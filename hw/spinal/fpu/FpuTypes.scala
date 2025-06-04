package fpu

import spinal.core._
import spinal.lib._

case class FloatUnpackedParam(
  exponentMax: Int = 1024,
  exponentMin: Int = -1024,
  mantissaWidth: Int = 57
)

object FloatMode extends SpinalEnum {
  val NORMAL, ZERO, INF, NAN = newElement()
}

object FpuFormat extends SpinalEnum {
  val SINGLE, DOUBLE = newElement()
}

case class FloatUnpacked() extends Bundle {
  val mode = FloatMode()
  val quiet = Bool()
  val sign = Bool()
  val exponent = SInt(FPUConfig.exponentWidth + 2 bits)
  val mantissa = UInt(FPUConfig.mantissaWidth + 4 bits)
}

object FloatUnpacked {
  def apply(): FloatUnpacked = new FloatUnpacked()

  // IEEE 754 format constants
  private val expWidthSingle  = 8
  private val mantWidthSingle = 23
  private val biasSingle      = 127
  private val expWidthDouble  = 11
  private val mantWidthDouble = 52
  private val biasDouble      = 1023

  def fromIEEE754(bits: Bits, format: FpuFormat.C): FloatUnpacked = {
    val f = FloatUnpacked()

    val isDouble = format === FpuFormat.DOUBLE
    val sign = Mux(isDouble, bits(63), bits(31))
    val expBits = UInt(expWidthDouble bits)
    val mantBits = UInt(mantWidthDouble bits)
    val bias = UInt(11 bits)

    when(isDouble) {
      expBits := bits(62 downto 52).asUInt
      mantBits := bits(51 downto 0).asUInt
      bias := U(biasDouble, 11 bits)
    } otherwise {
      expBits := bits(30 downto 23).asUInt.resize(expWidthDouble)
      mantBits := bits(22 downto 0).asUInt.resize(mantWidthDouble)
      bias := U(biasSingle, 11 bits)
    }

    val exponent = expBits
    val mantissa = mantBits

    when(exponent === 0 && mantissa === 0) {
      f.mode := FloatMode.ZERO
      f.quiet := False
      f.sign := sign
      f.exponent := 0
      f.mantissa := 0
    } elsewhen(exponent === U((1 << expWidthDouble) - 1, expWidthDouble bits) && mantissa === 0) {
      f.mode := FloatMode.INF
      f.quiet := False
      f.sign := sign
      f.exponent := 0
      f.mantissa := 0
    } elsewhen(exponent === U((1 << expWidthDouble) - 1, expWidthDouble bits) && mantissa =/= 0) {
      f.mode := FloatMode.NAN
      f.quiet := mantissa(mantWidthDouble - 1)
      f.sign := sign
      f.exponent := 0
      f.mantissa := mantissa << (FPUConfig.mantissaWidth + 4 - mantWidthDouble)
    } otherwise {
      f.mode := FloatMode.NORMAL
      f.quiet := False
      f.sign := sign
      f.exponent := (exponent.asSInt - bias.asSInt).resize(FPUConfig.exponentWidth + 2)
      f.mantissa := Cat(U(1, 1 bits), mantissa).asUInt << (FPUConfig.mantissaWidth + 4 - mantWidthDouble - 1)
    }
    f
  }

  def toIEEE754(f: FloatUnpacked, format: FpuFormat.C): Bits = {
    val result = Bits(64 bits)
    result := 0
    when(format === FpuFormat.DOUBLE) {
      val exp = (f.exponent + S(biasDouble)).asUInt.resize(11)
      result := f.sign ## exp ## f.mantissa(51 downto 0)
    } otherwise {
      val exp = (f.exponent + S(biasSingle)).asUInt.resize(8)
      result(31) := f.sign
      result(30 downto 23) := exp.asBits
      result(22 downto 0) := f.mantissa(51 downto 29).asBits
    }
    result
  }
}

case class FPReg() extends Bundle {
  val value = Bits(64 bits)
  val typeTag = FpuFormat()
}

case class FPstatusReg() extends Bundle {
  val roundMode = RoundMode()
  val fpaType = FpuFormat()
  val fpbType = FpuFormat()
  val fpcType = FpuFormat()
  val reserved = Bits(24 bits)

  def set(roundMode: RoundMode.E, fpaType: FpuFormat.E, fpbType: FpuFormat.E, fpcType: FpuFormat.E, reserved: Bits): Unit = {
    this.roundMode := roundMode
    this.fpaType := fpaType
    this.fpbType := fpbType
    this.fpcType := fpcType
    this.reserved := reserved
  }
}