package fpu

import spinal.core._
import spinal.lib._

case class FpuVCU(param: FloatUnpackedParam) extends Component {
  val io = new Bundle {
    val operandA = in Bits(64 bits)
    val operandB = in Bits(64 bits)
    val opcode = in Bits(2 bits) // 00=add, 01=sub/mul/div
    val isDouble = in Bool()
    val normOperandA = out(FloatUnpacked(param))
    val normOperandB = out(FloatUnpacked(param))
    val result = out(FloatUnpacked(param))
    val abort = out Bool()
    val restart = out Bool()
  }.setName("")

  // Convert to T9000 format
  val aBits = Mux(io.isDouble, io.operandA, Cat(io.operandA(31), io.operandA(30 downto 23).asUInt.resize(11), io.operandA(22 downto 0).asUInt.resize(52)))
  val bBits = Mux(io.isDouble, io.operandB, Cat(io.operandB(31), io.operandB(30 downto 23).asUInt.resize(11), io.operandB(22 downto 0).asUInt.resize(52)))

  // Unpack using updated FpuUtils
  val aUnpacked = FloatUnpacked.toFloatUnpacked(aBits, Mux(io.isDouble, FpuFormat.DOUBLE, FpuFormat.SINGLE), param)
  val bUnpacked = FloatUnpacked.toFloatUnpacked(bBits, Mux(io.isDouble, FpuFormat.DOUBLE, FpuFormat.SINGLE), param)

  // Check special values with FpuUtils
  val (isZeroA, isNaNA, isInfA, isDenormA) = FpuUtils.isSpecial(aUnpacked)
  val (isZeroB, isNaNB, isInfB, isDenormB) = FpuUtils.isSpecial(bUnpacked)

  io.normOperandA := aUnpacked
  io.normOperandB := bUnpacked
  io.result := aUnpacked // Default result
  io.abort := False
  io.restart := False

  // Handle special cases (T9000 Section 3)
  when(isNaNA || isNaNB) {
    io.abort := True
    io.result := FpuUtils.generateSpecialValue(FloatMode.NAN, (isNaNA && aUnpacked.sign) || (isNaNB && bUnpacked.sign), param)
  } elsewhen(isInfA || isInfB) {
    when(isInfA && isInfB && io.opcode === B"01" && aUnpacked.sign =/= bUnpacked.sign) {
      io.abort := True
      io.result := FpuUtils.generateSpecialValue(FloatMode.NAN, False, param) // Inf - Inf or Inf * -Inf
    } otherwise {
      io.abort := True
      io.result := FpuUtils.generateSpecialValue(FloatMode.INF, aUnpacked.sign || bUnpacked.sign, param)
    }
  } elsewhen(isZeroA && isZeroB) {
    io.abort := True
    io.result := FpuUtils.generateSpecialValue(FloatMode.ZERO, aUnpacked.sign && bUnpacked.sign, param)
  } elsewhen(isDenormA || isDenormB) {
    io.restart := True
    when(isDenormA) {
      val (normMant, normExp) = FpuUtils.normalizeWithAFix(aUnpacked.mantissa, aUnpacked.exponent, param.mantissaWidth)
      io.normOperandA.mantissa := normMant
      io.normOperandA.exponent := normExp
    }
    when(isDenormB) {
      val (normMant, normExp) = FpuUtils.normalizeWithAFix(bUnpacked.mantissa, bUnpacked.exponent, param.mantissaWidth)
      io.normOperandB.mantissa := normMant
      io.normOperandB.exponent := normExp
    }
  }
}