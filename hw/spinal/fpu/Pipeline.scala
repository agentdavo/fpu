package fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import spinal.core.sim._

class Pipeline extends Component {
  // Define pipeline nodes
  val n0 = Node()
  val n1 = Node()
  val n2 = Node()
  val n3 = Node()
  val n4 = Node()

  // Connect nodes with CtrlLinks
  val s01 = CtrlLink(n0, n1)
  val s12 = CtrlLink(n1, n2)
  val s23 = CtrlLink(n2, n3)
  val s34 = CtrlLink(n3, n4)

  // Payloads
  val CMD = Payload(FPUCmd())
  val CMD_VALUE = Payload(Bits(64 bits))
  val CMD_ADDR = Payload(UInt(32 bits))
  val MICRO_OP = Payload(MicroOpBundle())
  val IAREG = Payload(UInt(32 bits))
  val IBREG = Payload(UInt(32 bits))
  val ICREG = Payload(UInt(32 bits))
  val EXP_A = Payload(SInt(FPUConfig.exponentWidth + 2 bits))
  val EXP_B = Payload(SInt(FPUConfig.exponentWidth + 2 bits))
  val MANT_A = Payload(UInt(FPUConfig.mantissaWidth + 4 bits))
  val MANT_B = Payload(UInt(FPUConfig.mantissaWidth + 4 bits))
  val SIGN_A = Payload(Bool())
  val SIGN_B = Payload(Bool())
  val RESULT_EXP = Payload(SInt(FPUConfig.exponentWidth + 2 bits))
  val RESULT_SIGN = Payload(Bool())
  val RESULT_MANT = Payload(UInt(FPUConfig.mantissaWidth + 4 bits))
  val INT_RESULT = Payload(UInt(32 bits))
  val PARTIAL_PRODUCTS = Payload(Vec(AFix.S(2 * (FPUConfig.mantissaWidth + 4) - 1 downto 0 bits), 28))
  val STAGE1_SUM = Payload(AFix.S(2 * (FPUConfig.mantissaWidth + 4) - 1 downto 0 bits))
  val STAGE2_SUM = Payload(AFix.S(2 * (FPUConfig.mantissaWidth + 4) - 1 downto 0 bits))
  val MUL_OVERFLOW = Payload(Bool())

  // IO definition
  val io = new Bundle {
    val cmdIn = slave Stream new Bundle {
      val cmd = FPUCmd()
      val value = Bits(64 bits)
      val addr = UInt(32 bits)
      val integerA = UInt(32 bits)
      val integerB = UInt(32 bits)
      val integerC = UInt(32 bits)
    }
    val resultOut = master Flow new Bundle {
      val value = Bits(64 bits)
      val done = Bool()
      val integerResult = UInt(32 bits)
    }
    val mem = master(FPUConfig.MemoryInterface())
    val exceptions = new Bundle {
      val invalidOp = out Bool()
      val divideByZero = out Bool()
      val overflow = out Bool()
      val underflow = out Bool()
      val inexact = out Bool()
      val fpError = out Bool()
      val excCode = out UInt(3 bits)
    }
  }

  // Stack and registers
  val stack = Vec(Reg(FPReg()), FPUConfig.stackSize)
  for (s <- stack) {
    s.value.init(0)
    s.typeTag.init(FpuFormat.DOUBLE)
  }
  val areg = Reg(UInt(32 bits)) init(0)
  val breg = Reg(UInt(32 bits)) init(0)
  val creg = Reg(UInt(32 bits)) init(0)
  val exceptions = io.exceptions

  // Build method with plugins
  def build(): Unit = {
    val preprocessPlugin = new PreprocessPlugin()
    val vcuPlugin = new VCUPlugin()
    val adderPlugin = new AdderPlugin()
    val multiplierPlugin = new MultiplierPlugin()
    val dividerPlugin = new DividerPlugin()
    val outputPlugin = new OutputPlugin()
    Builder(preprocessPlugin :: vcuPlugin :: adderPlugin :: multiplierPlugin :: dividerPlugin :: outputPlugin :: s01 :: s12 :: s23 :: s34 :: Nil)
  }
}

// Preprocessing plugin (n1)
class PreprocessPlugin extends FiberPlugin {
  override def build(): Unit = {
    val pipeline = host[Pipeline]
    import pipeline._

    n1.on {
      EXP_A := CMD_VALUE(62 downto 52).asSInt.resize(FPUConfig.exponentWidth + 2)
      EXP_B := stack(0).value(62 downto 52).asSInt.resize(FPUConfig.exponentWidth + 2)
      MANT_A := Cat(U(1, 1 bit), CMD_VALUE(51 downto 0), U(0, 4 bits)).asUInt
      MANT_B := Cat(U(1, 1 bit), stack(0).value(51 downto 0), U(0, 4 bits)).asUInt
      SIGN_A := CMD_VALUE(63)
      SIGN_B := stack(0).value(63)
    }
  }
}

// VCU plugin (n2)
class VCUPlugin extends FiberPlugin {
  val vcu = new FpuVCU(FloatUnpackedParam())
  override def build(): Unit = {
    val pipeline = host[Pipeline]
    import pipeline._

    vcu.io.operandA := n2(CMD_VALUE)
    vcu.io.operandB := stack(0).value
    vcu.io.opcode := Mux(n2(CMD) === FPUCmd.fpadd, B"00", B"01")
    vcu.io.isDouble := stack(0).typeTag === FpuFormat.DOUBLE

    n2.on {
      when(isValid && (CMD === FPUCmd.fpadd || CMD === FPUCmd.fpsub || 
                       CMD === FPUCmd.fpmul || CMD === FPUCmd.fpdiv || 
                       CMD === FPUCmd.fpldnladddb || CMD === FPUCmd.fpldnladdsn ||
                       CMD === FPUCmd.fpldnlmuldb || CMD === FPUCmd.fpldnlmulsn)) {
        when(vcu.io.abort) {
          val (isZero, isNaN, isInf, _) = FpuUtils.isSpecial(vcu.io.result)
          when(isInf) {
            RESULT_EXP := S((1 << FPUConfig.exponentWidth) - 1, FPUConfig.exponentWidth + 2 bits)
            RESULT_SIGN := vcu.io.result.sign
            RESULT_MANT := U(0, FPUConfig.mantissaWidth + 4 bits)
            when(CMD === FPUCmd.fpdiv) {
              exceptions.divideByZero := True
              exceptions.fpError := True
              exceptions.excCode := ExceptionCodes.divideByZero
            } elsewhen(CMD === FPUCmd.fpsub && isNaN) {
              exceptions.invalidOp := True
              exceptions.fpError := True
              exceptions.excCode := ExceptionCodes.invalidOp
            }
          } elsewhen(isNaN) {
            RESULT_EXP := S((1 << FPUConfig.exponentWidth) - 1, FPUConfig.exponentWidth + 2 bits)
            RESULT_SIGN := vcu.io.result.sign
            RESULT_MANT := vcu.io.result.mantissa // Simplified; T9000 uses quiet NaN
            exceptions.invalidOp := True
            exceptions.fpError := True
            exceptions.excCode := ExceptionCodes.invalidOp
          } elsewhen(isZero) {
            RESULT_EXP := S(0, FPUConfig.exponentWidth + 2 bits)
            RESULT_SIGN := vcu.io.result.sign
            RESULT_MANT := U(0, FPUConfig.mantissaWidth + 4 bits)
          }
        }
      }
    }
  }
}

// Adder plugin (n2)
class AdderPlugin extends FiberPlugin {
  override def build(): Unit = {
    val pipeline = host[Pipeline]
    import pipeline._

    n2.on {
      when(isValid && (CMD === FPUCmd.fpadd || CMD === FPUCmd.fpsub || 
                       CMD === FPUCmd.fpldnladddb || CMD === FPUCmd.fpldnladdsn)) {
        val (expDiff, aGreater) = FpuUtils.exponentDifference(n2(EXP_A), n2(EXP_B))
        val alignedMantA = Mux(aGreater, n2(MANT_A), n2(MANT_A) >> expDiff.abs)
        val alignedMantB = Mux(aGreater, n2(MANT_B) >> expDiff.abs, n2(MANT_B))
        val sum = FpuUtils.radix4CarryPropagateAdd(
          AFix.U(alignedMantA, FPUConfig.mantissaWidth + 3 downto 0 bits),
          AFix.U(alignedMantB, FPUConfig.mantissaWidth + 3 downto 0 bits),
          CMD === FPUCmd.fpsub
        )
        val normShift = FpuUtils.predictNormalizationDistance(n2(MANT_A), n2(MANT_B), CMD === FPUCmd.fpsub)
        val (normMant, normExp) = FpuUtils.normalizeWithAFix(sum, Mux(aGreater, n2(EXP_A), n2(EXP_B)), FPUConfig.mantissaWidth + 4)
        RESULT_EXP := normExp
        RESULT_SIGN := n2(SIGN_A) ^ (CMD === FPUCmd.fpsub && !aGreater)
        RESULT_MANT := normMant.asUInt
        when(sum.asBits(1 downto 0) =/= 0) {
          exceptions.inexact := True
          exceptions.excCode := ExceptionCodes.inexact
        }
      }
    }
  }
}

// Divider plugin (n2)
class DividerPlugin extends FiberPlugin {
  override def build(): Unit = {
    val pipeline = host[Pipeline]
    import pipeline._

    n2.on {
      when(isValid && (CMD === FPUCmd.fpdiv || CMD === FPUCmd.fpsqrt)) {
        val divExpRaw = Mux(CMD === FPUCmd.fpsqrt,
          ((n2(EXP_A) - S(FPUConfig.bias, FPUConfig.exponentWidth + 2 bits)) >> 1) + S(FPUConfig.bias, FPUConfig.exponentWidth + 2 bits),
          n2(EXP_A) - n2(EXP_B) + S(FPUConfig.bias, FPUConfig.exponentWidth + 2 bits)
        )
        RESULT_EXP := divExpRaw
        RESULT_SIGN := n2(SIGN_A) ^ n2(SIGN_B)
        RESULT_MANT := n2(MANT_A) // Placeholder; needs Divider implementation
      }
    }
  }
}

// Multiplier plugin (n2-n4)
class MultiplierPlugin extends FiberPlugin {
  override def build(): Unit = {
    val pipeline = host[Pipeline]
    import pipeline._

    val multiplierStart = n2.isValid && (n2(CMD) === FPUCmd.fpmul || n2(CMD) === FPUCmd.fpldnlmuldb || n2(CMD) === FPUCmd.fpldnlmulsn)
    val isDouble = stack(0).typeTag === FpuFormat.DOUBLE
    val numPartialProducts = Mux(isDouble, U(28), U(13))

    n2.on {
      when(multiplierStart) {
        val (partials, correction) = FpuUtils.boothRecodeRadix4(AFix.U(n2(MANT_B), FPUConfig.mantissaWidth + 3 downto 0 bits), FPUConfig.mantissaWidth + 4)
        PARTIAL_PRODUCTS := Vec.tabulate(28)(i => 
          if (i < numPartialProducts.toInt) partials(i) << (2 * i) else correction << (2 * i)
        )
      }
    }

    n3.on {
      when(n2.isValid) {
        val (carry1, save1) = FpuUtils.carrySaveReduce7to2(n3(PARTIAL_PRODUCTS), 0, 7)
        val (carry2, save2) = FpuUtils.carrySaveReduce7to2(n3(PARTIAL_PRODUCTS), 7, 7)
        STAGE1_SUM := Mux(isDouble, carry1 + save1 + carry2 + save2, n3(PARTIAL_PRODUCTS).reduce(_ + _))
        when(isDouble) {
          val (carry3, save3) = FpuUtils.carrySaveReduce7to2(n3(PARTIAL_PRODUCTS), 14, 7)
          val (carry4, save4) = FpuUtils.carrySaveReduce7to2(n3(PARTIAL_PRODUCTS), 21, 7)
          STAGE2_SUM := n3(STAGE1_SUM) + carry3 + save3 + carry4 + save4
        } otherwise {
          STAGE2_SUM := n3(STAGE1_SUM)
        }
      }
    }

    n4.on {
      when(n3.isValid) {
        val roundedMant = FpuUtils.interpolateRounding(n4(STAGE2_SUM), FPUConfig.RoundMode.NEAREST, FPUConfig.mantissaWidth + 4)
        MUL_OVERFLOW := roundedMant.asUInt(FPUConfig.mantissaWidth + 3)
        RESULT_MANT := roundedMant.asUInt
        RESULT_EXP := n4(EXP_A) + n4(EXP_B) - S(FPUConfig.bias, FPUConfig.exponentWidth + 2 bits) + MUL_OVERFLOW.asSInt(2 bits)
        RESULT_SIGN := n4(SIGN_A) ^ n4(SIGN_B)
        when(roundedMant.asBits(1 downto 0) =/= 0) {
          exceptions.inexact := True
          exceptions.excCode := ExceptionCodes.inexact
        }
      }
    }
  }
}

// Output plugin (n4)
class OutputPlugin extends FiberPlugin {
  override def build(): Unit = {
    val pipeline = host[Pipeline]
    import pipeline._

    val latchedResultExp = Reg(SInt(FPUConfig.exponentWidth + 2 bits)) init(0)
    val latchedResultSign = Reg(Bool()) init(False)
    val latchedResultMant = Reg(UInt(FPUConfig.mantissaWidth + 4 bits)) init(0)
    val finalResult = Cat(latchedResultSign, latchedResultExp.resize(FPUConfig.exponentWidth bits), latchedResultMant(FPUConfig.mantissaWidth - 1 downto 0))
    val latencyCounter = Reg(UInt(5 bits)) init(0)
    val operationActive = Reg(Bool()) init(False)
    val latency = Mux(stack(0).typeTag === FpuFormat.DOUBLE, n4(MICRO_OP).latencyDouble, n4(MICRO_OP).latencySingle)

    n4.on {
      io.resultOut.valid := operationActive && latencyCounter === latency
      io.resultOut.payload.value := finalResult
      io.resultOut.payload.done := operationActive && latencyCounter === latency
      io.resultOut.payload.integerResult := INT_RESULT

      when(isValid && !operationActive) {
        latencyCounter := U(1, 5 bits)
        operationActive := True
        latchedResultExp := RESULT_EXP
        latchedResultSign := RESULT_SIGN
        latchedResultMant := RESULT_MANT
      } elsewhen(operationActive && latencyCounter < latency) {
        latencyCounter := latencyCounter + 1
      } otherwise {
        latencyCounter := U(0, 5 bits)
        operationActive := False
      }

      when(isValid && io.resultOut.payload.done) {
        when(exceptions.fpError) {
          when(exceptions.divideByZero) {
            io.resultOut.payload.value := Cat(RESULT_SIGN, U((1 << FPUConfig.exponentWidth) - 1, FPUConfig.exponentWidth bits), U(0, FPUConfig.mantissaWidth bits)).resize(64 bits)
          } elsewhen(exceptions.invalidOp) {
            io.resultOut.payload.value := Cat(RESULT_SIGN, U((1 << FPUConfig.exponentWidth) - 1, FPUConfig.exponentWidth bits), RESULT_MANT(FPUConfig.mantissaWidth - 1 downto 0)).resize(64 bits)
          }
        }
      }
    }
  }
}

// Basic pipeline simulation
object PipelineSim extends App {
  SimConfig.withFstWave.compile(new Pipeline()).doSim { dut =>
    dut.clockDomain.forkStimulus(period = 20)
    dut.clockDomain.assertReset()
    dut.clockDomain.waitSampling(1)
    dut.clockDomain.deassertReset()
    dut.io.cmdIn.valid #= false
    dut.io.mem.ready #= true
    dut.clockDomain.waitSampling(5)

    println("Testing basic pipeline with fpldzerosn")
    
    dut.io.cmdIn.cmd #= FPUCmd.fpldzerosn
    dut.io.cmdIn.value #= 0
    dut.io.cmdIn.addr #= 0x100
    dut.io.cmdIn.integerA #= 0
    dut.io.cmdIn.integerB #= 0
    dut.io.cmdIn.integerC #= 0
    dut.io.cmdIn.valid #= true
    dut.clockDomain.waitSampling(1)
    dut.io.cmdIn.valid #= false

    var cycles = 0
    val maxCycles = 10
    while (!dut.io.resultOut.done.toBoolean && cycles < maxCycles) {
      println(s"Cycle $cycles: " +
              s"n0.valid=${dut.n0.isValid.toBoolean}, " +
              s"n1.valid=${dut.n1.isValid.toBoolean}, " +
              s"n2.valid=${dut.n2.isValid.toBoolean}, " +
              s"n3.valid=${dut.n3.isValid.toBoolean}, " +
              s"n4.valid=${dut.n4.isValid.toBoolean}, " +
              s"resultOut.done=${dut.io.resultOut.done.toBoolean}, " +
              s"resultOut.value=0x${dut.io.resultOut.value.toBigInt.toString(16)}")
      dut.clockDomain.waitSampling()
      cycles += 1
    }

    val result = dut.io.resultOut.value.toBigInt
    val expected = BigInt(0)
    val stackTop = dut.stack(0).value.toBigInt
    assert(result == expected, s"Result mismatch: got 0x$result, expected 0x$expected")
    assert(stackTop == expected, s"Stack top mismatch: got 0x$stackTop, expected 0x$expected")
    assert(cycles <= 5, s"Pipeline took too long: $cycles cycles")
    println(s"PASS: Pipeline completed in $cycles cycles, result=0x$result, stack[0]=0x$stackTop")
  }
}