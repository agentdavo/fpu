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
  defaultEncoding = SpinalEnumEncoding("static")(
    NORMAL -> 0, ZERO -> 1, INF -> 2, NAN -> 3
  )
}

object FpuFormat extends SpinalEnum {
  val SINGLE, DOUBLE = newElement()
  defaultEncoding = SpinalEnumEncoding("static")(
    SINGLE -> 0, DOUBLE -> 1
  )
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

    val sign = bits(63)
    val expBits = format.mux(
      FpuFormat.SINGLE -> bits(62 downto (63 - expWidthSingle)),
      FpuFormat.DOUBLE -> bits(62 downto (63 - expWidthDouble))
    )
    val mantBits = format.mux(
      FpuFormat.SINGLE -> bits(mantWidthSingle - 1 downto 0),
      FpuFormat.DOUBLE -> bits(mantWidthDouble - 1 downto 0)
    )
    val bias = format.mux(
      FpuFormat.SINGLE -> biasSingle,
      FpuFormat.DOUBLE -> biasDouble
    )

    val exponent = expBits.asUInt
    val mantissa = mantBits.asUInt
    val expWidth = format.mux(
      FpuFormat.SINGLE -> expWidthSingle,
      FpuFormat.DOUBLE -> expWidthDouble
    )
    val mantWidth = format.mux(
      FpuFormat.SINGLE -> mantWidthSingle,
      FpuFormat.DOUBLE -> mantWidthDouble
    )

    when(exponent === 0 && mantissa === 0) {
      f.mode := FloatMode.ZERO
      f.quiet := False
      f.sign := sign
      f.exponent := 0
      f.mantissa := 0
    } elsewhen(exponent === ((1 << expWidth) - 1) && mantissa === 0) {
      f.mode := FloatMode.INF
      f.quiet := False
      f.sign := sign
      f.exponent := 0
      f.mantissa := 0
    } elsewhen(exponent === ((1 << expWidth) - 1) && mantissa =/= 0) {
      f.mode := FloatMode.NAN
      f.quiet := mantissa(mantWidth - 1)
      f.sign := sign
      f.exponent := 0
      f.mantissa := mantissa << (FPUConfig.mantissaWidth + 4 - mantWidth)
    } otherwise {
      f.mode := FloatMode.NORMAL
      f.quiet := False
      f.sign := sign
      f.exponent := (exponent.asSInt - bias).resize(FPUConfig.exponentWidth + 2)
      f.mantissa := Cat(U(1, 1 bits), mantissa).asUInt << (FPUConfig.mantissaWidth + 4 - mantWidth - 1)
    }
    f
  }

  def toIEEE754(f: FloatUnpacked, format: FpuFormat.C): Bits = {
    val result = Bits(64 bits)

    val expWidth = format.mux(
      FpuFormat.SINGLE -> expWidthSingle,
      FpuFormat.DOUBLE -> expWidthDouble
    )
    val mantWidth = format.mux(
      FpuFormat.SINGLE -> mantWidthSingle,
      FpuFormat.DOUBLE -> mantWidthDouble
    )
    val bias = format.mux(
      FpuFormat.SINGLE -> biasSingle,
      FpuFormat.DOUBLE -> biasDouble
    )

    val expAdjusted = f.exponent + bias
    val mantShifted = f.mantissa >> (FPUConfig.mantissaWidth + 4 - mantWidth - 1)
    val mantWithoutHidden = mantShifted(mantWidth - 1 downto 0)

    switch(f.mode) {
      is(FloatMode.ZERO) {
        result := f.sign ## B(0, expWidth bits) ## B(0, mantWidth bits) ## B(0, (64 - 1 - expWidth - mantWidth) bits)
      }
      is(FloatMode.INF) {
        result := f.sign ## B((1 << expWidth) - 1, expWidth bits) ## B(0, mantWidth bits) ## B(0, (64 - 1 - expWidth - mantWidth) bits)
      }
      is(FloatMode.NAN) {
        val mantTruncated = f.mantissa(mantWidth - 1 downto 0) | (f.quiet ? U(1 << (mantWidth - 1)) | U(0))
        result := f.sign ## B((1 << expWidth) - 1, expWidth bits) ## mantTruncated ## B(0, (64 - 1 - expWidth - mantWidth) bits)
      }
      is(FloatMode.NORMAL) {
        when(expAdjusted >= ((1 << expWidth) - 1)) {
          result := f.sign ## B((1 << expWidth) - 1, expWidth bits) ## B(0, mantWidth bits) ## B(0, (64 - 1 - expWidth - mantWidth) bits)
        } otherwise {
          result := f.sign ## expAdjusted.asUInt.resize(expWidth) ## mantWithoutHidden ## B(0, (64 - 1 - expWidth - mantWidth) bits)
        }
      }
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