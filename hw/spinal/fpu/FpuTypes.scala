package fpu

import spinal.core._
import spinal.lib._

// Enum and Config Definitions (unchanged from original thinking trace)
object FloatMode extends SpinalEnum {
  val NORMAL, ZERO, INF, NAN = newElement()
  defaultEncoding = SpinalEnumEncoding("T9000FloatMode")(
    NORMAL -> 0, ZERO -> 1, INF -> 2, NAN -> 3
  )
}

object FpuFormat extends SpinalEnum {
  val SINGLE, DOUBLE = newElement()
  defaultEncoding = SpinalEnumEncoding("T9000Format")(SINGLE -> 0, DOUBLE -> 1)
}

case class FPReg() extends Bundle {
  val value = Bits(64 bits)     // 64-bit floating-point value
  val typeTag = FpuFormat()     // SINGLE or DOUBLE
}

case class FloatUnpacked() extends Bundle {
  val mode = FloatMode()
  val quiet = Bool()
  val sign = Bool()
  val exponent = SInt(FPUConfig.exponentWidth + 2 bits)
  val mantissa = UInt(FPUConfig.mantissaWidth + 4 bits)
}

case class FloatUnpackedParam(
  exponentMax: Int = 1024,
  exponentMin: Int = -1024,
  mantissaWidth: Int = 57
)

object FloatUnpacked {
  def apply(): FloatUnpacked = new FloatUnpacked()

  def toFloatUnpacked(bits: Bits, format: FpuFormat.C): FloatUnpacked = {
    val f = FloatUnpacked()
    // Define constants outside hardware description
    val expWidthSingle = 8
    val mantWidthSingle = 23
    val expWidthDouble = 11
    val mantWidthDouble = 52

    val expWidth = format.mux(
      FpuFormat.SINGLE -> U(expWidthSingle, 4 bits),
      FpuFormat.DOUBLE -> U(expWidthDouble, 4 bits)
    )
    val mantWidth = format.mux(
      FpuFormat.SINGLE -> U(mantWidthSingle, 6 bits),
      FpuFormat.DOUBLE -> U(mantWidthDouble, 6 bits)
    )
    val bias = format.mux(
      FpuFormat.SINGLE -> U(127, 11 bits),
      FpuFormat.DOUBLE -> U(1023, 11 bits)
    )

    val sign = bits(63)
    val exp = bits(62 downto 63 - expWidth.asUInt).asUInt
    val mant = bits(mantWidth.asUInt - 1 downto 0).asUInt

    when(exp === 0 && mant === 0) {
      f := FpuUtils.generateSpecialValue(FloatMode.ZERO, sign)
    } elsewhen(exp === 0 && mant =/= 0) {
      f.mode := FloatMode.NORMAL
      f.sign := sign
      val (normMant, normExp) = FpuUtils.normalizeWithAFix(
        mant.resize(FPUConfig.mantissaWidth + 4),
        (-bias.asSInt).resize(FPUConfig.exponentWidth + 2),
        FPUConfig.mantissaWidth + 4
      )
      f.mantissa := normMant
      f.exponent := normExp
    } elsewhen(exp === ((1 << expWidth.asUInt) - 1) && mant === 0) {
      f := FpuUtils.generateSpecialValue(FloatMode.INF, sign)
    } elsewhen(exp === ((1 << expWidth.asUInt) - 1) && mant =/= 0) {
      f := FpuUtils.generateSpecialValue(FloatMode.NAN, sign)
    } otherwise {
      f.mode := FloatMode.NORMAL
      f.sign := sign
      f.exponent := (exp.asSInt - bias.asSInt).resize(FPUConfig.exponentWidth + 2)
      f.mantissa := Cat(U(1, 1 bit), mant, U(0, FPUConfig.mantissaWidth + 4 - mantWidth.asUInt - 1 bits)).asUInt.resize(FPUConfig.mantissaWidth + 4)
    }
    f
  }

  def toIEEE754(f: FloatUnpacked, format: FpuFormat.C): Bits = {
    val expWidthSingle = 8
    val mantWidthSingle = 23
    val expWidthDouble = 11
    val mantWidthDouble = 52

    val expWidth = format.mux(
      FpuFormat.SINGLE -> U(expWidthSingle, 4 bits),
      FpuFormat.DOUBLE -> U(expWidthDouble, 4 bits)
    )
    val mantWidth = format.mux(
      FpuFormat.SINGLE -> U(mantWidthSingle, 6 bits),
      FpuFormat.DOUBLE -> U(mantWidthDouble, 6 bits)
    )
    val bias = format.mux(
      FpuFormat.SINGLE -> U(127, 11 bits),
      FpuFormat.DOUBLE -> U(1023, 11 bits)
    )

    val result = Bits(64 bits)
    val expBits = Bits(expWidth.asUInt bits)
    val mantBits = Bits(mantWidth.asUInt bits)

    when(f.mode === FloatMode.ZERO) {
      expBits := 0
      mantBits := 0
    } elsewhen(f.mode === FloatMode.INF) {
      expBits := ((1 << expWidth.asUInt) - 1)
      mantBits := 0
    } elsewhen(f.mode === FloatMode.NAN) {
      expBits := ((1 << expWidth.asUInt) - 1)
      mantBits := f.mantissa(mantWidth.asUInt - 1 downto 0) | (f.quiet ? (U(1) << (mantWidth.asUInt - 1)) | U(0))
    } otherwise {
      val expAdjusted = f.exponent + bias.asSInt
      when(expAdjusted.asUInt > ((1 << expWidth.asUInt) - 1)) {
        expBits := ((1 << expWidth.asUInt) - 1)
        mantBits := 0
      } otherwise {
        expBits := expAdjusted.asUInt.resize(expWidth.asUInt)
        mantBits := f.mantissa(mantWidth.asUInt - 1 downto 0)
      }
    }
    result := f.sign ## expBits ## mantBits ## U(0, (64 - 1 - expWidth.asUInt - mantWidth.asUInt) bits)
    result
  }
}

case class FPstatusReg() extends Bundle {
  val roundMode = RoundMode()  // Fixed: Use RoundMode directly
  val fpaType = FpuFormat()
  val fpbType = FpuFormat()
  val fpcType = FpuFormat()
  val reserved = Bits(24 bits)

  def set(roundMode: RoundMode.E, fpaType: FpuFormat.E, fpbType: FpuFormat.E, fpcType: FpuFormat.E, reserved: BigInt): FPstatusReg = {
    this.roundMode := roundMode
    this.fpaType := fpaType
    this.fpbType := fpbType
    this.fpcType := fpcType
    this.reserved := B(reserved, 24 bits)
    this
  }
}