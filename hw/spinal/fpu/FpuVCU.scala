package fpu

import spinal.core._
import spinal.lib._

case class FpuVCU() extends Component {
  val io = new Bundle {
    val operandA = in Bits(64 bits)
    val operandB = in Bits(64 bits)
    val opcode = in Bits(2 bits)
    val isDouble = in Bool()
    val normOperandA = out(FloatUnpacked())
    val normOperandB = out(FloatUnpacked())
    val result = out(FloatUnpacked())
    val abort = out Bool()
    val restart = out Bool()
  }.setName("")

  val aBits = Mux(io.isDouble, io.operandA, Cat(io.operandA(31), io.operandA(30 downto 23).asUInt.resize(11), io.operandA(22 downto 0).asUInt.resize(52)))
  val bBits = Mux(io.isDouble, io.operandB, Cat(io.operandB(31), io.operandB(30 downto 23).asUInt.resize(11), io.operandB(22 downto 0).asUInt.resize(52)))

  val aUnpacked = FloatUnpacked.toFloatUnpacked(aBits, Mux(io.isDouble, FpuFormat.DOUBLE, FpuFormat.SINGLE))
  val bUnpacked = FloatUnpacked.toFloatUnpacked(bBits, Mux(io.isDouble, FpuFormat.DOUBLE, FpuFormat.SINGLE))

  val (isZeroA, isNaNA, isInfA, isDenormA) = FpuUtils.isSpecial(aUnpacked)
  val (isZeroB, isNaNB, isInfB, isDenormB) = FpuUtils.isSpecial(bUnpacked)

  io.normOperandA := aUnpacked
  io.normOperandB := bUnpacked
  io.result := FpuUtils.generateSpecialValue(FloatMode.ZERO, False)
  io.abort := False
  io.restart := False

  when(isNaNA || isNaNB) {
    io.abort := True
    io.result := FpuUtils.generateSpecialValue(FloatMode.NAN, (isNaNA && aUnpacked.sign) || (isNaNB && bUnpacked.sign))
  } elsewhen(isInfA || isInfB) {
    when(isInfA && isInfB && io.opcode === B"01" && aUnpacked.sign =/= bUnpacked.sign) {
      io.abort := True
      io.result := FpuUtils.generateSpecialValue(FloatMode.NAN, False)
    } otherwise {
      io.abort := True
      io.result := FpuUtils.generateSpecialValue(FloatMode.INF, aUnpacked.sign ^ (io.opcode === B"01" && bUnpacked.sign))
    }
  } elsewhen(isZeroA || isZeroB) {
    when(isZeroA && isZeroB) {
      io.abort := True
      io.result := FpuUtils.generateSpecialValue(FloatMode.ZERO, aUnpacked.sign && bUnpacked.sign)
    } elsewhen((isZeroA || isZeroB) && (isInfA || isInfB)) {
      io.abort := True
      io.result := FpuUtils.generateSpecialValue(FloatMode.NAN, False)
    } elsewhen(isZeroA && io.opcode === B"00") {
      io.abort := True
      io.result := bUnpacked
    } elsewhen(isZeroB && io.opcode === B"00") {
      io.abort := True
      io.result := aUnpacked
    }
  } elsewhen(isDenormA || isDenormB) {
    io.restart := True
    when(isDenormA) {
      val (normMant, normExp) = FpuUtils.normalizeWithAFix(aUnpacked.mantissa, aUnpacked.exponent, FPUConfig.mantissaWidth + 4)
      io.normOperandA.mantissa := normMant
      io.normOperandA.exponent := normExp
    }
    when(isDenormB) {
      val (normMant, normExp) = FpuUtils.normalizeWithAFix(bUnpacked.mantissa, bUnpacked.exponent, FPUConfig.mantissaWidth + 4)
      io.normOperandB.mantissa := normMant
      io.normOperandB.exponent := normExp
    }
  }
}