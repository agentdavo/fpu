package fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import spinal.core.sim._

class Pipeline extends Component {
  val n0 = Node()
  val n1 = Node()
  val n2 = Node()
  val n3 = Node()
  val n4 = Node()

  val s01 = CtrlLink(n0, n1)
  val s12 = CtrlLink(n1, n2)
  val s23 = CtrlLink(n2, n3)
  val s34 = CtrlLink(n3, n4)

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
  val PARTIAL_PRODUCTS = Payload(Vec(AFix.S((2 * (FPUConfig.mantissaWidth + 4)) bits), 28))
  val STAGE1_SUM = Payload(AFix.S((2 * (FPUConfig.mantissaWidth + 4)) bits))
  val STAGE2_SUM = Payload(AFix.S((2 * (FPUConfig.mantissaWidth + 4)) bits))
  val MUL_OVERFLOW = Payload(Bool())

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

  val stack = Vec(Reg(FPReg()), FPUConfig.stackSize)
  for (s <- stack) {
    s.value.init(0)
    s.typeTag.init(FpuFormat.DOUBLE)
  }
  val areg = Reg(UInt(32 bits)) init(0)
  val breg = Reg(UInt(32 bits)) init(0)
  val creg = Reg(UInt(32 bits)) init(0)
  val exceptions = io.exceptions

  val microcodePlugin = new MicrocodePlugin()
  val inputPlugin = new InputPlugin()
  val preprocessPlugin = new PreprocessPlugin()
  val vcuPlugin = new VCUPlugin()
  val adderPlugin = new AdderPlugin()
  val multiplierPlugin = new MultiplierPlugin()
  val dividerPlugin = new DividerPlugin()
  val outputPlugin = new OutputPlugin()

  Builder(s01, s12, s23, s34)
}

class MicrocodePlugin extends FiberPlugin {
  withPrefix("microcode")

  val MICROCODE = Payload(Bits(32 bits))

  override def build(pipeline: Pipeline): Unit = {
    import pipeline._

    val rom = Mem(Bits(32 bits), 256)
    val pc = Reg(UInt(8 bits)) init(0)

    n0.build {
      val cmd = n0(CMD)
      rom.write(pc + 1, B"32'h0", enable = False) // Dummy write
      n0(MICROCODE) := rom.readAsync(cmd.encoding.resize(8))
      pc := cmd.encoding.resize(8)
      n0(MICRO_OP).stageEnable := n0(MICROCODE)(4 downto 0)
    }
  }
}

class InputPlugin extends FiberPlugin {
  withPrefix("input")

  override def build(pipeline: Pipeline): Unit = {
    import pipeline._

    n0.build {
      when(io.cmdIn.valid && n0(MICRO_OP).stageEnable(0)) {
        n0(CMD) := io.cmdIn.cmd
        n0(CMD_VALUE) := io.cmdIn.value
        n0(CMD_ADDR) := io.cmdIn.addr
        n0(IAREG) := io.cmdIn.integerA
        n0(IBREG) := io.cmdIn.integerB
        n0(ICREG) := io.cmdIn.integerC
        val microOp = MicroOpBundle()
        microOp.cmd := io.cmdIn.cmd
        microOp.latencySingle := FPUConfig.latencyFor(io.cmdIn.cmd)._1
        microOp.latencyDouble := FPUConfig.latencyFor(io.cmdIn.cmd)._2
        microOp.shiftStack := FPUConfig.shiftStackFor(io.cmdIn.cmd)
        microOp.popStack := FPUConfig.popStackFor(io.cmdIn.cmd)
        microOp.writeResult := FPUConfig.writeResultFor(io.cmdIn.cmd)
        microOp.memRead := FPUConfig.memReadFor(io.cmdIn.cmd)
        microOp.memWrite := FPUConfig.memWriteFor(io.cmdIn.cmd)
        microOp.useInteger := FPUConfig.useIntegerFor(io.cmdIn.cmd)
        microOp.stageEnable := B"00001"
        n0(MICRO_OP) := microOp
        io.cmdIn.ready := True
        when(io.cmdIn.cmd === FPUCmd.fpldzerosn) {
          stack(0).value := 0
          stack(0).typeTag := FpuFormat.SINGLE
        }
      }
    }
  }
}

class PreprocessPlugin extends FiberPlugin {
  withPrefix("preprocess")

  override def build(pipeline: Pipeline): Unit = {
    import pipeline._

    n1.build {
      when(n1.isValid && n1(MICRO_OP).stageEnable(1)) {
        n1(EXP_A) := n1(CMD_VALUE)(62 downto 52).asSInt.resize(FPUConfig.exponentWidth + 2)
        n1(EXP_B) := stack(0).value(62 downto 52).asSInt.resize(FPUConfig.exponentWidth + 2)
        n1(MANT_A) := Cat(U(1, 1 bit), n1(CMD_VALUE)(51 downto 0), U(0, 4 bits)).asUInt
        n1(MANT_B) := Cat(U(1, 1 bit), stack(0).value(51 downto 0), U(0, 4 bits)).asUInt
        n1(SIGN_A) := n1(CMD_VALUE)(63)
        n1(SIGN_B) := stack(0).value(63)
      }
    }
  }
}

class VCUPlugin extends FiberPlugin {
  withPrefix("vcu")

  val vcu = new FpuVCU()

  override def build(pipeline: Pipeline): Unit = {
    import pipeline._

    vcu.io.operandA := n2(CMD_VALUE)
    vcu.io.operandB := stack(0).value
    vcu.io.opcode := Mux(n2(CMD) === FPUCmd.fpadd, B"00", B"01")
    vcu.io.isDouble := stack(0).typeTag === FpuFormat.DOUBLE

    n2.build {
      when(n2.isValid && n2(MICRO_OP).stageEnable(2) && (n2(CMD) === FPUCmd.fpadd || n2(CMD) === FPUCmd.fpsub || 
           n2(CMD) === FPUCmd.fpmul || n2(CMD) === FPUCmd.fpmul || 
           n2(CMD) === FPUCmd.fpldnladddb || n2(CMD) === FPUCmd.fpldnladdsn ||
           n2(CMD) === FPUCmd.fpldnlmuldb || n2(CMD) === FPUCmd.fpldnlmulsn)) {
        when(vcu.io.abort) {
          val (isZero, isNaN, isInf, _) = FpuUtils.isSpecial(vcu.io.result)
          when(isInf) {
            n2(RESULT_EXP) := S((1 << FPUConfig.exponentWidth) - 1, FPUConfig.exponentWidth + 2 bits)
            n2(RESULT_SIGN) := vcu.io.result.sign
            n2(RESULT_MANT) := U(0, FPUConfig.mantissaWidth + 4 bits)
            when(n2(CMD) === FPUCmd.fpdiv) {
              exceptions.divideByZero := True
              exceptions.fpError := True
              exceptions.excCode := ExceptionCodes.divideByZero
            } elsewhen(n2(CMD) === FPUCmd.fpsub && isNaN) {
              exceptions.invalidOp := True
              exceptions.fpError := True
              exceptions.excCode := ExceptionCodes.invalidOp
            }
          } elsewhen(isNaN) {
            n2(RESULT_EXP) := S((1 << FPUConfig.exponentWidth) - 1, FPUConfig.exponentWidth + 2 bits)
            n2(RESULT_SIGN) := vcu.io.result.sign
            n2(RESULT_MANT) := vcu.io.result.mantissa
            exceptions.invalidOp := True
            exceptions.fpError := True
            exceptions.excCode := ExceptionCodes.invalidOp
          } elsewhen(isZero) {
            n2(RESULT_EXP) := S(0, FPUConfig.exponentWidth + 2 bits)
            n2(RESULT_SIGN) := vcu.io.result.sign
            n2(RESULT_MANT) := U(0, FPUConfig.mantissaWidth + 4 bits)
          }
        }
      }
    }
  }
}

class AdderPlugin extends FiberPlugin {
  withPrefix("adder")

  override def build(pipeline: Pipeline): Unit = {
    import pipeline._

    n2.build {
      when(n2.isValid && n2(MICRO_OP).stageEnable(2) && (n2(CMD) === FPUCmd.fpadd || n2(CMD) === FPUCmd.fpsub || 
           n2(CMD) === FPUCmd.fpldnladddb || n2(CMD) === FPUCmd.fpldnladdsn)) {
        val (expDiff, aGreater) = FpuUtils.exponentDifference(n2(EXP_A), n2(EXP_B))
        val alignedMantA = Mux(aGreater, n2(MANT_A), n2(MANT_A) >> expDiff.abs)
        val alignedMantB = Mux(aGreater, n2(MANT_B) >> expDiff.abs, n2(MANT_B))
        val sum = FpuUtils.radix4CarryPropagateAdd(alignedMantA, alignedMantB, n2(CMD) === FPUCmd.fpsub)
        val normShift = FpuUtils.predictNormalizationDistance(n2(MANT_A), n2(MANT_B), n2(CMD) === FPUCmd.fpsub)
        val (normMant, normExp) = FpuUtils.normalizeWithAFix(sum, Mux(aGreater, n2(EXP_A), n2(EXP_B)), FPUConfig.mantissaWidth + 4)
        n2(RESULT_EXP) := normExp
        n2(RESULT_SIGN) := n2(SIGN_A) ^ (n2(CMD) === FPUCmd.fpsub && !aGreater)
        n2(RESULT_MANT) := normMant
        when(sum(1 downto 0) =/= 0) {
          exceptions.inexact := True
          exceptions.excCode := ExceptionCodes.inexact
        }
      }
    }
  }
}

class DividerPlugin extends FiberPlugin {
  withPrefix("divider")

  override def build(pipeline: Pipeline): Unit = {
    import pipeline._

    n2.build {
      when(n2.isValid && n2(MICRO_OP).stageEnable(2) && (n2(CMD) === FPUCmd.fpdiv || n2(CMD) === FPUCmd.fpsqrt)) {
        val divExpRaw = Mux(n2(CMD) === FPUCmd.fpsqrt,
          ((n2(EXP_A) - S(FPUConfig.bias, FPUConfig.exponentWidth + 2 bits)) >> 1) + S(FPUConfig.bias, FPUConfig.exponentWidth + 2 bits),
          n2(EXP_A) - n2(EXP_B) + S(FPUConfig.bias, FPUConfig.exponentWidth + 2 bits)
        )
        n2(RESULT_EXP) := divExpRaw
        n2(RESULT_SIGN) := n2(SIGN_A) ^ n2(SIGN_B)
        n2(RESULT_MANT) := n2(MANT_A) // Placeholder
      }
    }
  }
}

class MultiplierPlugin extends FiberPlugin {
  withPrefix("multiplier")

  override def build(pipeline: Pipeline): Unit = {
    import pipeline._

    val multiplierStart = n2.isValid && n2(MICRO_OP).stageEnable(2) && (n2(CMD) === FPUCmd.fpmul || n2(CMD) === FPUCmd.fpldnlmuldb || n2(CMD) === FPUCmd.fpldnlmulsn)
    val isDouble = stack(0).typeTag === FpuFormat.DOUBLE
    val numPartialProducts = Mux(isDouble, U(28, 6 bits), U(13, 6 bits))

    n2.build {
      when(multiplierStart) {
        val (partials, correction) = FpuUtils.boothRecodeRadix4(n2(MANT_A), FPUConfig.mantissaWidth + 4)
        n2(PARTIAL_PRODUCTS) := Vec.tabulate(28)(i => 
          if (i < numPartialProducts.toInt) partials(i) << (2 * i) else correction << (2 * i)
        )
      }
    }

    n3.build {
      when(n2.isValid && n3(MICRO_OP).stageEnable(3)) {
        val (carry1, save1) = FpuUtils.carrySaveReduce7to2(n3(PARTIAL_PRODUCTS), 0, 7)
        val (carry2, save2) = FpuUtils.carrySaveReduce7to2(n3(PARTIAL_PRODUCTS), 7, 7)
        n3(STAGE1_SUM) := Mux(isDouble, carry1 + save1 + carry2 + save2, n3(PARTIAL_PRODUCTS).reduce(_ + _))
        when(isDouble) {
          val (carry3, save3) = FpuUtils.carrySaveReduce7to2(n3(PARTIAL_PRODUCTS), 14, 7)
          val (carry4, save4) = FpuUtils.carrySaveReduce7to2(n3(PARTIAL_PRODUCTS), 21, 7)
          n3(STAGE2_SUM) := n3(STAGE1_SUM) + carry3 + save3 + carry4 + save4
        } otherwise {
          n3(STAGE2_SUM) := n3(STAGE1_SUM)
        }
      }
    }

    n4.build {
      when(n3.isValid && n4(MICRO_OP).stageEnable(4)) {
        val roundedMant = FpuUtils.interpolateRounding(n4(STAGE2_SUM), FPUConfig.RoundMode.NEAREST, FPUConfig.mantissaWidth + 4)
        n4(MUL_OVERFLOW) := roundedMant.asUInt(FPUConfig.mantissaWidth + 3)
        n4(RESULT_MANT) := roundedMant.asUInt
        n4(RESULT_EXP) := n4(EXP_A) + n4(EXP_B) - S(FPUConfig.bias, FPUConfig.exponentWidth + 2 bits) + n4(MUL_OVERFLOW).asSInt(2 bits)
        n4(RESULT_SIGN) := n4(SIGN_A) ^ n4(SIGN_B)
        when(roundedMant.asBits(1 downto 0) =/= 0) {
          exceptions.inexact := True
          exceptions.excCode := ExceptionCodes.inexact
        }
      }
    }
  }
}

class OutputPlugin extends FiberPlugin {
  withPrefix("output")

  override def build(pipeline: Pipeline): Unit = {
    import pipeline._

    val latchedResultExp = Reg(SInt(FPUConfig.exponentWidth + 2 bits)) init(0)
    val latchedResultSign = Reg(Bool()) init(False)
    val latchedResultMant = Reg(UInt(FPUConfig.mantissaWidth + 4 bits)) init(0)
    val finalResult = Cat(latchedResultSign, latchedResultExp.resize(FPUConfig.exponentWidth bits), latchedResultMant(FPUConfig.mantissaWidth - 1 downto 0))
    val latencyCounter = Reg(UInt(5 bits)) init(0)
    val operationActive = Reg(Bool()) init(False)
    val latency = Mux(stack(0).typeTag === FpuFormat.DOUBLE, n4(MICRO_OP).latencyDouble, n4(MICRO_OP).latencySingle)

    n4.build {
      io.resultOut.valid := operationActive && latencyCounter === latency
      io.resultOut.payload.value := finalResult
      io.resultOut.payload.done := operationActive && latencyCounter === latency
      io.resultOut.payload.integerResult := n4(INT_RESULT)

      when(n4.isValid && n4(MICRO_OP).stageEnable(4) && !operationActive) {
        latencyCounter := 1
        operationActive := True
        latchedResultExp := n4(RESULT_EXP)
        latchedResultSign := n4(RESULT_SIGN)
        latchedResultMant := n4(RESULT_MANT)
      } elsewhen(operationActive && latencyCounter < latency) {
        latencyCounter := latencyCounter + 1
      } otherwise {
        latencyCounter := 0
        operationActive := False
      }

      when(n4.isValid && io.resultOut.payload.done) {
        when(exceptions.fpError) {
          when(exceptions.divideByZero) {
            io.resultOut.payload.value := Cat(n4(RESULT_SIGN), U((1 << FPUConfig.exponentWidth) - 1, FPUConfig.exponentWidth bits), U(0, FPUConfig.mantissaWidth bits)).resize(64 bits)
          } elsewhen(exceptions.invalidOp) {
            io.resultOut.payload.value := Cat(n4(RESULT_SIGN), U((1 << FPUConfig.exponentWidth) - 1, FPUConfig.exponentWidth bits), n4(RESULT_MANT)(FPUConfig.mantissaWidth - 1 downto 0)).resize(64 bits)
          }
        }
      }
    }
  }
}

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