package fpu

import spinal.core._
import spinal.lib._

// Configuration for unpacked float parameters
case class FloatUnpackedParam(
  exponentMax: Int = 1024,      // Maximum exponent value
  exponentMin: Int = -1024,     // Minimum exponent value
  mantissaWidth: Int = 57       // Width of mantissa
)

// Enum for float modes (normal, zero, infinity, NaN)
object FloatMode extends SpinalEnum {
  val NORMAL, ZERO, INF, NAN = newElement()
  defaultEncoding = SpinalEnumEncoding("static")(
    NORMAL -> 0, ZERO -> 1, INF -> 2, NAN -> 3
  )
}

// Enum for floating-point formats (single, double)
object FpuFormat extends SpinalEnum {
  val SINGLE, DOUBLE = newElement()
  defaultEncoding = SpinalEnumEncoding("static")(
    SINGLE -> 0, DOUBLE -> 1
  )
}

// Bundle representing an unpacked floating-point number
case class FloatUnpacked() extends Bundle {
  val mode = FloatMode()                        // Float mode (NORMAL, ZERO, INF, NAN)
  val quiet = Bool()                            // Quiet NaN flag
  val sign = Bool()                             // Sign bit
  val exponent = SInt(FPUConfig.exponentWidth + 2 bits)  // Exponent with extra bits for overflow
  val mantissa = UInt(FPUConfig.mantissaWidth + 4 bits)  // Mantissa with extra bits for precision

  // Default assignment method
  def assignNormal(sign: Bool, exponent: SInt, mantissa: UInt): FloatUnpacked = {
    this.mode := FloatMode.NORMAL
    this.quiet := False
    this.sign := sign
    this.exponent := exponent
    this.mantissa := mantissa
    this
  }
}

// Object with utility methods for FloatUnpacked
object FloatUnpacked {
  def apply(): FloatUnpacked = new FloatUnpacked()

  // Constants for IEEE 754 formats
  val expWidthSingle = 8
  val expWidthDouble = 11
  val mantWidthSingle = 23
  val mantWidthDouble = 52
  val biasSingle = 127
  val biasDouble = 1023

  // Convert IEEE 754 bits to FloatUnpacked
  def fromIEEE754(bits: Bits, format: FpuFormat.E): FloatUnpacked = {
    val result = FloatUnpacked()

    // Format-specific constants
    val (expWidth, mantWidth, bias) = format.mux(
      FpuFormat.SINGLE -> (expWidthSingle, mantWidthSingle, biasSingle),
      FpuFormat.DOUBLE -> (expWidthDouble, mantWidthDouble, biasDouble)
    )

    // Extract fields from 64-bit input
    val sign = bits(63)
    val exponentBits = bits(62 downto 63 - expWidth).asUInt
    val mantissaBits = bits(mantWidth - 1 downto 0).asUInt

    // Handle special cases
    when(exponentBits === 0 && mantissaBits === 0) {
      // Zero
      result.mode := FloatMode.ZERO
      result.quiet := False
      result.sign := sign
      result.exponent := 0
      result.mantissa := 0
    } elsewhen(exponentBits === ((1 << expWidth) - 1) && mantissaBits === 0) {
      // Infinity
      result.mode := FloatMode.INF
      result.quiet := False
      result.sign := sign
      result.exponent := 0
      result.mantissa := 0
    } elsewhen(exponentBits === ((1 << expWidth) - 1) && mantissaBits =/= 0) {
      // NaN
      result.mode := FloatMode.NAN
      result.quiet := mantissaBits(mantWidth - 1)  // MSB determines quiet/signaling
      result.sign := sign
      result.exponent := 0
      result.mantissa := mantissaBits << (FPUConfig.mantissaWidth + 4 - mantWidth)
    } otherwise {
      // Normal number
      val exponentAdjusted = exponentBits.asSInt - bias
      val mantissaWithHidden = Cat(U"1", mantissaBits).asUInt
      val mantissaShifted = mantissaWithHidden << (FPUConfig.mantissaWidth + 4 - mantWidth - 1)
      result.assignNormal(sign, exponentAdjusted.resize(FPUConfig.exponentWidth + 2), mantissaShifted)
    }
    result
  }

  // Convert FloatUnpacked to IEEE 754 bits
  def toIEEE754(f: FloatUnpacked, format: FpuFormat.E): Bits = {
    val result = Bits(64 bits)

    // Format-specific constants
    val (expWidth, mantWidth, bias) = format.mux(
      FpuFormat.SINGLE -> (expWidthSingle, mantWidthSingle, biasSingle),
      FpuFormat.DOUBLE -> (expWidthDouble, mantWidthDouble, biasDouble)
    )

    // Compute adjusted exponent and mantissa
    val expAdjusted = f.exponent + bias
    val mantissaShifted = f.mantissa >> (FPUConfig.mantissaWidth + 4 - mantWidth - 1)
    val mantissaWithoutHidden = mantissaShifted(mantWidth - 1 downto 0)

    // Pack based on mode
    switch(f.mode) {
      is(FloatMode.ZERO) {
        result := f.sign ## B(0, expWidth bits) ## B(0, mantWidth bits) ## B(0, 64 - 1 - expWidth - mantWidth bits)
      }
      is(FloatMode.INF) {
        result := f.sign ## B((1 << expWidth) - 1, expWidth bits) ## B(0, mantWidth bits) ## B(0, 64 - 1 - expWidth - mantWidth bits)
      }
      is(FloatMode.NAN) {
        val mantissaTruncated = f.mantissa(mantWidth - 1 downto 0) | (f.quiet ? U(1 << (mantWidth - 1)) | U(0))
        result := f.sign ## B((1 << expWidth) - 1, expWidth bits) ## mantissaTruncated ## B(0, 64 - 1 - expWidth - mantWidth bits)
      }
      is(FloatMode.NORMAL) {
        when(expAdjusted >= ((1 << expWidth) - 1)) {
          // Overflow to infinity
          result := f.sign ## B((1 << expWidth) - 1, expWidth bits) ## B(0, mantWidth bits) ## B(0, 64 - 1 - expWidth - mantWidth bits)
        } otherwise {
          result := f.sign ## expAdjusted.asUInt.resize(expWidth) ## mantissaWithoutHidden ## B(0, 64 - 1 - expWidth - mantWidth bits)
        }
      }
    }
    result
  }
}

// Floating-point register definition
case class FPReg() extends Bundle {
  val value = Bits(64 bits)       // 64-bit value
  val typeTag = FpuFormat()       // Format (SINGLE or DOUBLE)
}

// Floating-point status register
case class FPstatusReg() extends Bundle {
  val roundMode = RoundMode()     // Rounding mode (assumed defined elsewhere)
  val fpaType = FpuFormat()       // Format of operand A
  val fpbType = FpuFormat()       // Format of operand B
  val fpcType = FpuFormat()       // Format of operand C
  val reserved = Bits(24 bits)    // Reserved bits

  // Method to set values
  def set(
    roundMode: RoundMode.E,
    fpaType: FpuFormat.E,
    fpbType: FpuFormat.E,
    fpcType: FpuFormat.E,
    reserved: Bits = B(0, 24 bits)
  ): FPstatusReg = {
    this.roundMode := roundMode
    this.fpaType := fpaType
    this.fpbType := fpbType
    this.fpcType := fpcType
    this.reserved := reserved
    this
  }
}