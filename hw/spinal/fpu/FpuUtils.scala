package fpu

import spinal.core._

object FpuUtils {
  def CountLeadingZeros(vec: Bits): UInt = {
    val width = vec.getWidth
    val res = UInt(log2Up(width + 1) bits)
    res := 0
    for(i <- width-1 downto 0){
      when(vec(i)){
        res := (width - 1 - i)
      }
    }
    when(vec === 0){
      res := width
    }
    res
  }

  def isSpecial(f: FloatUnpacked): (Bool, Bool, Bool, Bool) = {
    (f.mode === FloatMode.ZERO,
     f.mode === FloatMode.NAN,
     f.mode === FloatMode.INF,
     False)
  }

  def generateSpecialValue(mode: FloatMode.E, sign: Bool): FloatUnpacked = {
    val f = FloatUnpacked()
    f.mode := mode
    f.sign := sign
    f.quiet := mode === FloatMode.NAN
    f.exponent := 0
    f.mantissa := 0
    f
  }

  def exponentDifference(expA: SInt, expB: SInt): (SInt, Bool) = {
    val diff = expA - expB
    (diff, expA >= expB)
  }

  def boothRecodeRadix4(multiplier: UInt, width: Int): (Vec[UInt], UInt) = {
    val count = (width + 1) / 2 + 1
    val vec = Vec(UInt((2 * width) bits), count - 1)
    for(v <- vec) v := 0
    (vec, U(0, (2 * width) bits))
  }

  def carrySaveReduce7to2(partials: Vec[UInt], startIdx: Int, count: Int): (UInt, UInt) = {
    val width = partials(0).getWidth
    val sum = partials.slice(startIdx, startIdx + count).reduce(_ + _)
    (sum, U(0, width bits))
  }

  def interpolateRounding(p: UInt, roundMode: RoundMode.C, mantSize: Int): UInt = {
    p
  }

  def normalizeWithAFix(mant: UInt, exp: SInt, mantSize: Int): (UInt, SInt) = {
    val lz = CountLeadingZeros(mant.asBits.resize(mantSize))
    ((mant << lz).resize(mantSize), exp - lz.asSInt.resize(exp.getWidth))
  }

  def radix4CarryPropagateAdd(a: UInt, b: UInt, subtract: Bool): UInt = {
    Mux(subtract, a - b, a + b)
  }

  def predictNormalizationDistance(a: UInt, b: UInt, isSub: Bool): UInt = {
    U(0, log2Up(a.getWidth + 1) bits)
  }

  def progressiveCarryAssimilate(qp: UInt, qn: UInt, qd: SInt, width: Int): (UInt, UInt) = {
    (qp, qn)
  }
}
