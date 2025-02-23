package fpu

import spinal.core._
import spinal.lib._

object FpuUtils {
  def CountLeadingZeros(vec: UInt): UInt = {
    val width = vec.getWidth
    val lz = UInt(log2Up(width + 1) bits)
    lz := U(0, log2Up(width + 1) bits)
    for (i <- width - 1 downto 0) {
      when(vec(i)) {
        lz := U(width - 1 - i, log2Up(width + 1) bits)
      }
    }
    when(vec === 0) {
      lz := U(width, log2Up(width + 1) bits)
    }
    lz
  }

  def isSpecial(f: FloatUnpacked): (Bool, Bool, Bool, Bool) = {
    val isZero = f.mode === FloatMode.ZERO
    val isNaN = f.mode === FloatMode.NAN
    val isInf = f.mode === FloatMode.INF
    val isDenorm = f.mode === FloatMode.NORMAL && f.exponent < AFix(f.p.exponentMin, 0 exp)
    (isZero, isNaN, isInf, isDenorm)
  }

  def generateSpecialValue(mode: FloatMode.E, sign: Bool, param: FloatUnpackedParam): FloatUnpacked = {
    val f = FloatUnpacked(param)
    f.mode := mode
    f.sign := sign
    f.quiet := mode === FloatMode.NAN
    f.exponent := mode.mux(
      FloatMode.ZERO -> AFix(0, 0 exp),
      FloatMode.INF -> AFix(param.exponentMax, 0 exp),
      FloatMode.NAN -> AFix(param.exponentMax, 0 exp),
      FloatMode.NORMAL -> AFix(0, 0 exp)
    )
    f.mantissa := mode.mux(
      FloatMode.ZERO -> AFix(0, -param.mantissaWidth exp),
      FloatMode.INF -> AFix(0, -param.mantissaWidth exp),
      FloatMode.NAN -> AFix(FPUConfig.nanMaskValue << (param.mantissaWidth - 52), -param.mantissaWidth exp),
      FloatMode.NORMAL -> AFix(0, -param.mantissaWidth exp)
    )
    f
  }

  def clearExceptions(exceptions: Bundle): Unit = {
    exceptions.asInstanceOf[Bundle].elements.foreach {
      case ("invalidOp", invalidOp) => invalidOp := False
      case ("divideByZero", divideByZero) => divideByZero := False
      case ("overflow", overflow) => overflow := False
      case ("underflow", underflow) => underflow := False
      case ("inexact", inexact) => inexact := False
      case ("fpError", fpError) => fpError := False
      case ("excCode", excCode) => excCode := U(0, 3 bits)
      case _ =>
    }
  }

  def exponentDifference(expA: AFix, expB: AFix): (AFix, Bool) = {
    val diff = expA - expB
    val aGreater = expA >= expB
    (diff, aGreater)
  }

  def boothRecodeRadix4(multiplier: AFix, width: Int): (Vec[AFix], AFix) = {
    val partialCount = (width + 1) / 2 + 1
    val recoded = Vec(AFix(2 * width bits), partialCount - 1)
    val padded = Cat(multiplier.raw, U(0, 1 bit)).asUInt
    val correction = Reg(AFix(2 * width bits)) init(0)
    for (i <- 0 until partialCount - 1) {
      val bits = padded(2 * i + 2 downto 2 * i)
      recoded(i) := bits.mux(
        0 -> AFix(0, 0 exp),
        1 -> multiplier,
        2 -> multiplier,
        3 -> (multiplier << 1),
        4 -> -(multiplier << 1),
        5 -> -multiplier,
        6 -> -multiplier,
        7 -> AFix(0, 0 exp)
      )
      when(bits === 4 || bits === 5 || bits === 6) {
        correction := correction | AFix(1 << (2 * i), -width exp)
      }
    }
    (recoded, correction)
  }

  def carrySaveReduce7to2(partials: Vec[AFix], startIdx: Int, count: Int): (AFix, AFix) = {
    require(count <= 7, "T9000 7:2 array supports up to 7 inputs")
    val sum = Vec(partials.slice(startIdx, startIdx + count)).reduce(_ + _)
    (sum, AFix(0, -partials(0).bitWidth exp))
  }

  def interpolateRounding(p: AFix, roundMode: RoundMode.E, mantSize: Int): AFix = {
    val pPlus1 = p + AFix(1, -mantSize exp)
    val pPlus2 = p + AFix(2, -mantSize exp)
    val needsShift = p >= AFix(2, 0 exp)
    val guard = p.raw(1)
    val sticky = p.raw(0)
    val rounded = roundMode.mux(
      RoundMode.NEAREST -> Mux(guard && (sticky || p.raw(2)), needsShift ? pPlus2 | pPlus1, needsShift ? pPlus1 | p),
      RoundMode.ZERO -> Mux(needsShift, pPlus1, p),
      RoundMode.PLUS -> Mux(needsShift, pPlus2, pPlus1),
      RoundMode.MINUS -> Mux(needsShift, pPlus1.fixTo(0 exp, -mantSize exp), p.fixTo(0 exp, -mantSize exp))
    )
    Mux(needsShift, rounded >> 1, rounded).fixTo(0 exp, -mantSize exp)
  }

  def normalizeWithAFix(mant: AFix, exp: AFix, mantSize: Int): (AFix, AFix) = {
    val lz = CountLeadingZeros(mant.raw)
    val normMant = mant << lz
    val normExp = exp - AFix(lz.asSInt, 0 exp)
    (normMant.fixTo(0 exp, -mantSize exp), normExp)
  }

  def radix4CarryPropagateAdd(a: AFix, b: AFix, subtract: Bool): AFix = {
    val width = a.bitWidth
    val sum = Reg(AFix(width bits))
    val carry = Reg(AFix(width bits)) init(0)
    val bEffective = Mux(subtract, -b, b)
    for (i <- 0 until width / 2) {
      val aPair = a.raw(2 * i + 1 downto 2 * i)
      val bPair = bEffective.raw(2 * i + 1 downto 2 * i)
      val localSum = aPair.asSInt + bPair.asSInt + carry.raw(2 * i).asSInt
      sum.raw(2 * i + 1 downto 2 * i) := localSum(1 downto 0)
      carry.raw(2 * i + 2) := localSum(2)
    }
    sum.fixTo(a.maxExp exp, a.minExp exp)
  }

  def predictNormalizationDistance(a: AFix, b: AFix, isSub: Bool): UInt = {
    val width = a.bitWidth
    val pairCount = (width + 1) / 2
    val gpk = Vec(UInt(2 bits), pairCount)
    for (i <- 0 until pairCount - 1) {
      val aPair = a.raw(2 * i + 1 downto 2 * i)
      val bPair = b.raw(2 * i + 1 downto 2 * i)
      val diff = aPair.asSInt - bPair.asSInt
      gpk(i) := Mux(diff === 0, U(0), Mux(diff > 0, U(2), U(1)))
    }
    gpk(pairCount - 1) := Mux(a.raw(0) === b.raw(0), U(0), Mux(a.raw(0) > b.raw(0), U(2), U(1)))

    val potentialPoints = Vec(Bool(), pairCount)
    for (i <- 0 until pairCount - 2) {
      val triad = gpk(i + 2) ## gpk(i + 1) ## gpk(i)
      potentialPoints(i) := triad === B"001" || triad === B"010" || triad === B"100" || triad === B"101"
    }
    potentialPoints(pairCount - 1) := False
    potentialPoints(pairCount - 2) := False

    val normShift = UInt(log2Up(width + 1) bits)
    normShift := U(width, log2Up(width + 1) bits)
    for (i <- 0 until pairCount - 2) {
      when(potentialPoints(i)) {
        normShift := U((pairCount - 1 - i) * 2, log2Up(width + 1) bits)
      }
    }
    Mux(isSub, normShift, U(0, log2Up(width + 1) bits))
  }

  def progressiveCarryAssimilate(qPositive: AFix, qNegative: AFix, qDigit: SInt, width: Int): (AFix, AFix) = {
    val qPosNext = qPositive << 2 | Mux(qDigit >= 0, AFix(qDigit, 0 exp), AFix(0, 0 exp))
    val qNegNext = qNegative << 2 | Mux(qDigit < 0, AFix(-qDigit, 0 exp), AFix(0, 0 exp))
    (qPosNext.fixTo(0 exp, -width exp), qNegNext.fixTo(0 exp, -width exp))
  }
}