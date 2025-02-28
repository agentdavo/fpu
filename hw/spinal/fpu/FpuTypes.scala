package fpu

import spinal.core._
import spinal.lib._

case class FloatUnpackedParam(
  exponentMax: Int = 1024,
  exponentMin: Int = -1024,
  mantissaWidth: Int = 57
) {
  def union(other: FloatUnpackedParam) = FloatUnpackedParam(
    exponentMax max other.exponentMax,
    exponentMin min other.exponentMin,
    mantissaWidth max other.mantissaWidth
  )
}

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

case class FloatUnpacked(p: FloatUnpackedParam) extends Bundle {
  val mode = FloatMode()
  val quiet = Bool()
  val sign = Bool()
  val exponent = AFix.S(FPUConfig.exponentWidth + 1 downto -FPUConfig.exponentWidth - 2 bits)
  val mantissa = AFix.U(FPUConfig.mantissaWidth + 3 downto 0 bits)
}

object FloatUnpacked {
  def apply(exponentMax: Int, exponentMin: Int, mantissaWidth: Int): FloatUnpacked =
    FloatUnpacked(FloatUnpackedParam(exponentMax, exponentMin, mantissaWidth))

  def toFloatUnpacked(bits: Bits, format: FpuFormat.C): FloatUnpacked = {
    val f = FloatUnpacked(FloatUnpackedParam())
    val expWidth = format.mux(FpuFormat.SINGLE -> 8, FpuFormat.DOUBLE -> 11)
    val mantWidth = format.mux(FpuFormat.SINGLE -> 23, FpuFormat.DOUBLE -> 52)
    val bias = format.mux(FpuFormat.SINGLE -> 127, FpuFormat.DOUBLE -> 1023)
    val sign = bits(63)
    val exp = bits(62 downto (63 - expWidth)).asUInt
    val mant = bits(mantWidth - 1 downto 0).asUInt

    when(exp === 0 && mant === 0) {
      FpuUtils.generateSpecialValue(FloatMode.ZERO, sign, FloatUnpackedParam()) := f
    } elsewhen(exp === 0 && mant =/= 0) {
      f.mode := FloatMode.NORMAL
      f.sign := sign
      val (normMant, normExp) = FpuUtils.normalizeWithAFix(AFix.U(mant, mantWidth - 1 downto 0 bits), AFix.S(-bias, 11 downto -12 bits), FPUConfig.mantissaWidth + 4)
      f.mantissa := normMant
      f.exponent := normExp
    } elsewhen(exp === ((1 << expWidth) - 1) && mant === 0) {
      FpuUtils.generateSpecialValue(FloatMode.INF, sign, FloatUnpackedParam()) := f
    } elsewhen(exp === ((1 << expWidth) - 1) && mant =/= 0) {
      FpuUtils.generateSpecialValue(FloatMode.NAN, sign, FloatUnpackedParam()) := f
    } otherwise {
      f.mode := FloatMode.NORMAL
      f.sign := sign
      f.exponent := AFix.S(exp.asSInt - bias, 11 downto -12 bits)
      f.mantissa := AFix.U(Cat(U(1, 1 bit), mant, U(0, FPUConfig.mantissaWidth - mantWidth - 1 bits)).asUInt, FPUConfig.mantissaWidth + 3 downto 0 bits)
    }
    f
  }

  def toIEEE754(f: FloatUnpacked, format: FpuFormat.C): Bits = {
    val expWidth = format.mux(FpuFormat.SINGLE -> 8, FpuFormat.DOUBLE -> 11)
    val mantWidth = format.mux(FpuFormat.SINGLE -> 23, FpuFormat.DOUBLE -> 52)
    val bias = format.mux(FpuFormat.SINGLE -> 127, FpuFormat.DOUBLE -> 1023)
    val result = Bits(64 bits)
    val expBits = UInt(expWidth bits)
    val mantBits = UInt(mantWidth bits)

    when(f.mode === FloatMode.ZERO) {
      expBits := 0
      mantBits := 0
    } elsewhen(f.mode === FloatMode.INF) {
      expBits := (1 << expWidth) - 1
      mantBits := 0
    } elsewhen(f.mode === FloatMode.NAN) {
      expBits := (1 << expWidth) - 1
      mantBits := f.mantissa.asUInt(mantWidth - 1 downto 0) | (f.quiet ? U(1 << (mantWidth - 1)) | U(0))
    } otherwise {
      val expAdjusted = f.exponent + AFix.S(bias, 11 downto -12 bits)
      when(expAdjusted.asUInt > ((1 << expWidth) - 1)) {
        expBits := (1 << expWidth) - 1
        mantBits := 0
      } otherwise {
        expBits := expAdjusted.asUInt
        mantBits := f.mantissa.asUInt(mantWidth - 1 downto 0)
      }
    }
    result := f.sign ## expBits ## mantBits ## U(0, 64 - 1 - expWidth - mantWidth bits)
  }
}

case class FPReg() extends Bundle {
  val value = Bits(64 bits)
  val typeTag = FpuFormat()
}

case class MicroOpBundle() extends Bundle {
  val cmd = FPUCmd()
  val latencySingle = UInt(5 bits)
  val latencyDouble = UInt(5 bits)
  val shiftStack = Bool()
  val popStack = Bool()
  val writeResult = Bool()
  val memRead = Bool()
  val memWrite = Bool()
  val useInteger = Bool()
}

case class FPstatusReg() extends Bundle {
  val roundMode = FPUConfig.RoundMode()
  val fpaType = FpuFormat()
  val fpbType = FpuFormat()
  val fpcType = FpuFormat()
  val reserved = Bits(24 bits)

  def init(roundMode: FPUConfig.RoundMode.E, fpaType: FpuFormat.E, fpbType: FpuFormat.E, fpcType: FpuFormat.E, reserved: BigInt): FPstatusReg = {
    this.roundMode := roundMode
    this.fpaType := fpaType
    this.fpbType := fpbType
    this.fpcType := fpcType
    this.reserved := B(reserved, 24 bits)
    this
  }
}