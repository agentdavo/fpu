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

  def generateSpecialValue(mode: FloatMode.E, sign: Bool): FloatUnpacked = {
    val f = FloatUnpacked()
    f.mode := mode
    f.sign := sign
    f.quiet := mode === FloatMode.NAN
    f.exponent := mode.mux(
      FloatMode.ZERO -> S(0, FPUConfig.exponentWidth + 2 bits),
      FloatMode.INF -> S(1024, FPUConfig.exponentWidth + 2 bits),
      FloatMode.NAN -> S(1024, FPUConfig.exponentWidth + 2 bits),
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

  def boothRecodeRadix4(multiplier: UInt, width: Int): (Vec[AFix], AFix) = {
    val partialCount = (width + 1) / 2 + 1
    val recoded = Vec(AFix.S((2 * width) bits), partialCount - 1)
    val padded = Cat(multiplier.resize(width), U(0, 1 bit)).asUInt
    val correction = Reg(AFix.S((2 * width) bits)) init(0)
    val multiplierAFix = AFix.U(multiplier.resize(width bits))
    for (i <- 0 until partialCount - 1) {
      val bits = padded(2 * i + 2 downto 2 * i)
      recoded(i) := bits.mux(
        0 -> AFix.S(U(0, (2 * width) bits)),
        1 -> multiplierAFix,
        2 -> multiplierAFix,
        3 -> (multiplierAFix << 1),
        4 -> -(multiplierAFix << 1),
        5 -> -multiplierAFix,
        6 -> -multiplierAFix,
        7 -> AFix.S(U(0, (2 * width) bits))
      )
      when(bits === 4 || bits === 5 || bits === 6) {
        correction := correction | AFix.S(U(1) << (2 * i), (2 * width) bits)
      }
    }
    (recoded, correction)
  }

  def carrySaveReduce7to2(partials: Vec[AFix], startIdx: Int, count: Int): (AFix, AFix) = {
    require(count <= 7, "T9000 7:2 array supports up to 7 inputs")
    val width = partials(0).getBitsWidth
    val sum = Vec(partials.slice(startIdx, startIdx + count)).reduce(_ + _)
    (sum, AFix.S(U(0, width bits)))
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

  def normalizeWithAFix(mant: UInt, exp: SInt, mantSize: Int): (UInt, SInt) = {
    val lz = CountLeadingZeros(mant.asBits.resize(mantSize))
    val normMant = (mant << lz).resize(mantSize)
    val normExp = exp - lz.asSInt.resize(FPUConfig.exponentWidth + 2)
    (normMant, normExp)
  }

  def radix4CarryPropagateAdd(a: UInt, b: UInt, subtract: Bool): UInt = {
    val width = a.getWidth
    val sum = Reg(UInt(width bits))
    val carry = Reg(UInt(width bits)) init(0)
    val aFix = AFix.U(a)
    val bFix = AFix.U(b)
    val bEffective = Mux(subtract, -bFix, bFix)
    for (i <- 0 until width / 2) {
      val aPair = aFix.asBits(2 * i + 1 downto 2 * i)
      val bPair = bEffective.asBits(2 * i + 1 downto 2 * i)
      val localSum = aPair.asSInt + bPair.asSInt + carry.asBits(2 * i).asSInt
      sum(2 * i + 1 downto 2 * i) := localSum(1 downto 0)
      carry(2 * i + 2) := localSum(2)
    }
    sum
  }

  def predictNormalizationDistance(a: UInt, b: UInt, isSub: Bool): UInt = {
    val width = a.getWidth
    val pairCount = (width + 1) / 2
    val gpk = Vec(UInt(2 bits), pairCount)
    val aFix = AFix.U(a)
    val bFix = AFix.U(b)
    for (i <- 0 until pairCount - 1) {
      val aPair = aFix.asBits(2 * i + 1 downto 2 * i)
      val bPair = bFix.asBits(2 * i + 1 downto 2 * i)
      val diff = aPair.asSInt - bPair.asSInt
      gpk(i) := Mux(diff === 0, U(0), Mux(diff.asBool, U(2), U(1)))
    }
    gpk(pairCount - 1) := Mux(aFix.asBits(0) === bFix.asBits(0), U(0), Mux(aFix.asBits(0), U(2), U(1)))

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
    val qNegNext = qNegative << 2 | Mux(qDigit < 0, AFix.S((-qDigit).resize(width)), AFix.S(U(0, width bits)))
    (qPosNext.trim(width bits), qNegNext.trim(width bits))
  }
}