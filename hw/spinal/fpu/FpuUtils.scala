package fpu

import spinal.core._
import spinal.lib._

object FpuUtils {
  def CountLeadingZeros(vec: Bits): UInt = {
    val width = vec.getWidth
    val lz = UInt(log2Up(width + 1) bits)
    lz := 0
    for (i <- width - 1 downto 0) {
      when(vec(i)) {
        lz := (width - 1 - i)
      }
    }
    when(vec === 0) {
      lz := width
    }
    lz
  }

  def isSpecial(f: FloatUnpacked): (Bool, Bool, Bool, Bool) = {
    val isZero = f.mode === FloatMode.ZERO
    val isNaN = f.mode === FloatMode.NAN
    val isInf = f.mode === FloatMode.INF
    val isDenorm = f.mode === FloatMode.NORMAL && f.exponent < -FPUConfig.bias
    (isZero, isNaN, isInf, isDenorm)
  }

  def generateSpecialValue(mode: FloatMode.E, sign: Bool, param: FloatUnpackedParam): FloatUnpacked = {
    val f = FloatUnpacked(param)
    f.mode := mode
    f.sign := sign
    f.quiet := mode === FloatMode.NAN
    f.exponent := mode.mux(
      FloatMode.ZERO -> S(0, FPUConfig.exponentWidth + 2 bits),
      FloatMode.INF -> S(param.exponentMax, FPUConfig.exponentWidth + 2 bits),
      FloatMode.NAN -> S(param.exponentMax, FPUConfig.exponentWidth + 2 bits),
      FloatMode.NORMAL -> S(0, FPUConfig.exponentWidth + 2 bits)
    )
    f.mantissa := mode.mux(
      FloatMode.ZERO -> U(0, FPUConfig.mantissaWidth + 4 bits),
      FloatMode.INF -> U(0, FPUConfig.mantissaWidth + 4 bits),
      FloatMode.NAN -> U(FPUConfig.nanMaskValue << (FPUConfig.mantissaWidth - 52), FPUConfig.mantissaWidth + 4 bits),
      FloatMode.NORMAL -> U(0, FPUConfig.mantissaWidth + 4 bits)
    )
    f
  }

  def exponentDifference(expA: SInt, expB: SInt): (SInt, Bool) = {
    val diff = expA - expB
    val aGreater = expA >= expB
    (diff, aGreater)
  }

  def boothRecodeRadix4(multiplier: AFix, width: Int): (Vec[AFix], AFix) = {
    val partialCount = (width + 1) / 2 + 1
    val recoded = Vec(AFix.S(2 * width - 1 downto 0 bits), partialCount - 1)
    val padded = Cat(multiplier.asBits.resize(width), U(0, 1 bit)).asUInt
    val correction = Reg(AFix.S(2 * width - 1 downto 0 bits)) init(0)
    for (i <- 0 until partialCount - 1) {
      val bits = padded(2 * i + 2 downto 2 * i)
      recoded(i) := bits.mux(
        0 -> AFix.S(0, 2 * width - 1 downto 0 bits),
        1 -> multiplier,
        2 -> multiplier,
        3 -> (multiplier << 1),
        4 -> -(multiplier << 1),
        5 -> -multiplier,
        6 -> -multiplier,
        7 -> AFix.S(0, 2 * width - 1 downto 0 bits)
      )
      when(bits === 4 || bits === 5 || bits === 6) {
        correction := correction | AFix.S(1 << (2 * i), 2 * width - 1 downto 0 bits)
      }
    }
    (recoded, correction)
  }

  def carrySaveReduce7to2(partials: Vec[AFix], startIdx: Int, count: Int): (AFix, AFix) = {
    require(count <= 7, "T9000 7:2 array supports up to 7 inputs")
    val width = partials(0).maxExp - partials(0).minExp + 1
    val sum = Vec(partials.slice(startIdx, startIdx + count)).reduce(_ + _)
    (sum, AFix.S(0, width - 1 downto 0 bits))
  }

  def interpolateRounding(p: AFix, roundMode: FPUConfig.RoundMode.C, mantSize: Int): AFix = {
    val pPlus1 = p + AFix.U(U(1, mantSize bits))
    val pPlus2 = p + AFix.U(U(2, mantSize bits))
    val needsShift = p >= AFix.U(U(2, mantSize bits))
    val guard = p.asBits(1)
    val sticky = p.asBits(0)
    val rounded = roundMode.mux(
      FPUConfig.RoundMode.NEAREST -> Mux(guard && (sticky || p.asBits(2)), needsShift ? pPlus2 | pPlus1, needsShift ? pPlus1 | p),
      FPUConfig.RoundMode.ZERO -> Mux(needsShift, pPlus1, p),
      FPUConfig.RoundMode.PLUS -> Mux(needsShift, pPlus2, pPlus1),
      FPUConfig.RoundMode.MINUS -> Mux(needsShift, pPlus1.trim(mantSize bits), p.trim(mantSize bits))
    )
    Mux(needsShift, rounded >> 1, rounded).trim(mantSize bits)
  }

  def normalizeWithAFix(mant: AFix, exp: SInt, mantSize: Int): (AFix, SInt) = {
    val lz = CountLeadingZeros(mant.asBits)
    val normMant = mant << lz
    val normExp = exp - lz.asSInt.resize(FPUConfig.exponentWidth + 2)
    (normMant.trim(mantSize bits), normExp)
  }

  def radix4CarryPropagateAdd(a: AFix, b: AFix, subtract: Bool): AFix = {
    val width = a.maxExp - a.minExp + 1
    val sum = Reg(AFix.S(width - 1 downto 0 bits))
    val carry = Reg(AFix.S(width - 1 downto 0 bits)) init(0)
    val bEffective = Mux(subtract, -b, b)
    for (i <- 0 until width / 2) {
      val aPair = a.asBits(2 * i + 1 downto 2 * i)
      val bPair = bEffective.asBits(2 * i + 1 downto 2 * i)
      val localSum = aPair.asSInt + bPair.asSInt + carry.asBits(2 * i).asSInt
      sum.asBits(2 * i + 1 downto 2 * i) := localSum(1 downto 0)
      carry.asBits(2 * i + 2) := localSum(2)
    }
    sum
  }

  def predictNormalizationDistance(a: AFix, b: AFix, isSub: Bool): UInt = {
    val width = a.maxExp - a.minExp + 1
    val pairCount = (width + 1) / 2
    val gpk = Vec(UInt(2 bits), pairCount)
    for (i <- 0 until pairCount - 1) {
      val aPair = a.asBits(2 * i + 1 downto 2 * i)
      val bPair = b.asBits(2 * i + 1 downto 2 * i)
      val diff = aPair.asSInt - bPair.asSInt
      gpk(i) := Mux(diff === 0, U(0), Mux(diff.asBool, U(2), U(1)))
    }
    gpk(pairCount - 1) := Mux(a.asBits(0) === b.asBits(0), U(0), Mux(a.asBits(0), U(2), U(1)))

    val potentialPoints = Vec(Bool(), pairCount)
    for (i <- 0 until pairCount - 2) {
      val triad = gpk(i + 2) ## gpk(i + 1) ## gpk(i)
      potentialPoints(i) := triad === B"001" || triad === B"010" || triad === B"100" || triad === B"101"
    }
    potentialPoints(pairCount - 1) := False
    potentialPoints(pairCount - 2) := False

    val normShift = UInt(log2Up(width + 1) bits)
    normShift := width
    for (i <- 0 until pairCount - 2) {
      when(potentialPoints(i)) {
        normShift := (pairCount - 1 - i) * 2
      }
    }
    Mux(isSub, normShift, U(0, log2Up(width + 1) bits))
  }

  def progressiveCarryAssimilate(qPositive: AFix, qNegative: AFix, qDigit: SInt, width: Int): (AFix, AFix) = {
    val qPosNext = qPositive << 2 | Mux(qDigit >= 0, AFix.S(qDigit.resize(width)), AFix.S(U(0, width bits)))
    val qNegNext = qNegative << 2 | Mux(qDigit < 0, AFix.S(-qDigit.resize(width)), AFix.S(U(0, width bits)))
    (qPosNext.trim(width bits), qNegNext.trim(width bits))
  }
}