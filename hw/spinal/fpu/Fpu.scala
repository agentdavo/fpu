package fpu

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._

class Fpu extends Component with PluginHost {
  val pipeline = new Pipeline
  import pipeline._

  // Add plugins
  val inputPlugin = add(new InputPlugin)
  val preprocessPlugin = add(new PreprocessPlugin)
  val vcuPlugin = add(new VCUPlugin)
  val adderPlugin = add(new AdderPlugin)
  val multiplierPlugin = add(new MultiplierPlugin)
  val dividerPlugin = add(new DividerPlugin)
  val outputPlugin = add(new OutputPlugin)

  // Build the pipeline
  override def postBuild(): Unit = {
    super.postBuild()
    pipeline.build()
  }
}

// Input handling plugin (n0)
class InputPlugin extends FiberPlugin {
  override def build(): Unit = {
    val pipeline = host[Pipeline]
    import pipeline._

    n0.arbitrateFrom(io.cmdIn)
    val stage0 = new n0.Area {
      CMD := io.cmdIn.cmd
      CMD_VALUE := io.cmdIn.value
      CMD_ADDR := io.cmdIn.addr
      MICRO_OP := microcode.readAsync(io.cmdIn.cmd.asBits.asUInt)
      IAREG := io.cmdIn.integerA
      IBREG := io.cmdIn.integerB
      ICREG := io.cmdIn.integerC

      io.mem.addr := CMD_ADDR
      io.mem.read := MICRO_OP.memRead && isValid
      io.mem.write := MICRO_OP.memWrite && isValid
      io.mem.valid := (MICRO_OP.memRead || MICRO_OP.memWrite) && isValid
      io.mem.dataIn := stack(0).value

      when(isFiring) {
        FpuUtils.clearExceptions(exceptions)
        areg := IAREG
        breg := IBREG
        creg := ICREG
        when(MICRO_OP.shiftStack) {
          stack(2) := stack(1)
          stack(1) := stack(0)
          stack(0).value := CMD_VALUE
          stack(0).typeTag := Mux(CMD === FPUCmd.fpldnlmulsn, U(0, 2 bits), U(1, 2 bits))
        }
        when(MICRO_OP.popStack) {
          when(stack(0).typeTag === 0 && stack(1).typeTag === 0) {
            exceptions.invalidOp := True
            exceptions.fpError := True
            exceptions.excCode := ExceptionCodes.invalidOp
          } otherwise {
            stack(0) := stack(1)
            stack(1) := stack(2)
            stack(2).value := CMD_VALUE
            stack(2).typeTag := Mux(CMD === FPUCmd.fpldnladdsn || CMD === FPUCmd.fpldnlmulsn, U(0, 2 bits), U(1, 2 bits))
          }
        }
        fpStatus.fpaType := stack(0).typeTag
        fpStatus.fpbType := stack(1).typeTag
        fpStatus.fpcType := stack(2).typeTag
      }
    }
  }
}

// Preprocessing plugin (n1)
class PreprocessPlugin extends FiberPlugin {
  override def build(): Unit = {
    val pipeline = host[Pipeline]
    import pipeline._

    val stage1 = new n1.Area {
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
  override def build(): Unit = {
    val pipeline = host[Pipeline]
    import pipeline._

    val vcu = new FpuVCU(FloatUnpackedParam(exponentMax = 1024, exponentMin = -1024, mantissaWidth = FPUConfig.mantissaWidth + 4))
    vcu.io.operandA := n2(CMD_VALUE)
    vcu.io.operandB := stack(0).value
    vcu.io.opcode := Mux(n2(CMD) === FPUCmd.fpadd, B"00", B"01")
    vcu.io.isDouble := stack(0).typeTag === 1

    val stage2 = new n2.Area {
      when(isValid && (CMD === FPUCmd.fpadd || CMD === FPUCmd.fpsub || 
                       CMD === FPUCmd.fpmul || CMD === FPUCmd.fpdiv || 
                       CMD === FPUCmd.fpldnladddb || CMD === FPUCmd.fpldnladdsn ||
                       CMD === FPUCmd.fpldnlmuldb || CMD === FPUCmd.fpldnlmulsn)) {
        when(vcu.io.abort) {
          when(vcu.io.result.isInfinity) {
            RESULT_EXP := S((1 << FPUConfig.exponentWidth) - 1, FPUConfig.exponentWidth + 2 bits)
            RESULT_SIGN := vcu.io.result.sign
            RESULT_MANT := U(0, FPUConfig.mantissaWidth + 4 bits)
            when(CMD === FPUCmd.fpdiv) {
              exceptions.divideByZero := True
              exceptions.fpError := True
              exceptions.excCode := ExceptionCodes.divideByZero
            } elsewhen (CMD === FPUCmd.fpsub && vcu.io.result.isNan) {
              exceptions.invalidOp := True
              exceptions.fpError := True
              exceptions.excCode := ExceptionCodes.invalidOp
            }
          } elsewhen (vcu.io.result.isNan) {
            RESULT_EXP := S((1 << FPUConfig.exponentWidth) - 1, FPUConfig.exponentWidth + 2 bits)
            RESULT_SIGN := vcu.io.result.sign
            RESULT_MANT := FpuUtils.quietNaN(vcu.io.result.mantissa, FPUConfig.mantissaWidth + 4)
            exceptions.invalidOp := True
            exceptions.fpError := True
            exceptions.excCode := ExceptionCodes.invalidOp
          } elsewhen (vcu.io.result.isZero) {
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

    val adder = new DualAdder()
    adder.io.a := n2(MANT_A)
    adder.io.b := n2(MANT_B)
    adder.io.signA := n2(SIGN_A)
    adder.io.signB := n2(SIGN_B)
    adder.io.expA := n2(EXP_A)
    adder.io.expB := n2(EXP_B)
    adder.io.isSub := n2(CMD) === FPUCmd.fpsub

    val stage2 = new n2.Area {
      when(isValid && (CMD === FPUCmd.fpadd || CMD === FPUCmd.fpsub || 
                       CMD === FPUCmd.fpldnladddb || CMD === FPUCmd.fpldnladdsn) && !vcu.io.abort) {
        RESULT_EXP := adder.io.resultExp
        RESULT_SIGN := adder.io.resultSign
        RESULT_MANT := adder.io.sum
        when(adder.io.sum(1 downto 0) =/= 0) {
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

    val divider = new Divider()
    divider.io.dividend := n2(MANT_A)
    divider.io.divisor := n2(MANT_B)
    divider.io.isDouble := stack(0).typeTag === 1
    divider.io.isSqrt := n2(CMD) === FPUCmd.fpsqrt
    divider.io.start := n2.isValid && (n2(CMD) === FPUCmd.fpdiv || n2(CMD) === FPUCmd.fpsqrt) && !vcu.io.abort

    val stage2 = new n2.Area {
      when(divider.io.done) {
        val divExpRaw = Mux(CMD === FPUCmd.fpsqrt,
          ((EXP_A - S(FPUConfig.bias, FPUConfig.exponentWidth + 2 bits)) >> 1) + S(FPUConfig.bias, FPUConfig.exponentWidth + 2 bits),
          EXP_A - EXP_B + S(FPUConfig.bias, FPUConfig.exponentWidth + 2 bits)
        )
        RESULT_EXP := divExpRaw
        RESULT_SIGN := SIGN_A ^ SIGN_B
        RESULT_MANT := divider.io.quotient
        when(divider.io.remainder =/= 0) {
          exceptions.inexact := True
          exceptions.excCode := ExceptionCodes.inexact
        }
      }
    }
  }
}

class MultiplierPlugin extends FiberPlugin {
  // Define multiplier-specific payloads
  val PARTIAL_PRODUCTS = Payload(Vec.fill(28)(AFix(2 * (FPUConfig.mantissaWidth + 4) bits)))
  val STAGE1_SUM = Payload(AFix(2 * (FPUConfig.mantissaWidth + 4) bits))
  val STAGE2_SUM = Payload(AFix(2 * (FPUConfig.mantissaWidth + 4) bits))
  val MUL_OVERFLOW = Payload(Bool())

  // Build phase
  override def build(): Unit = {
    val pipeline = host[Pipeline]
    import pipeline._

    // Multiplier control signals
    val multiplierStart = n2.isValid && (n2(CMD) === FPUCmd.fpmul || n2(CMD) === FPUCmd.fpldnlmuldb || n2(CMD) === FPUCmd.fpldnlmulsn) && !n2.vcu.io.abort
    val isDouble = stack(0).typeTag === 1
    val numPartialProducts = Mux(isDouble, U(28), U(13))

    // Stage 2: Booth recoding
    val mulStage2 = new n2.Area {
      val aFix = AFix(n2(MANT_A), -(FPUConfig.mantissaWidth + 4) exp)
      val bFix = AFix(n2(MANT_B), -(FPUConfig.mantissaWidth + 4) exp)
      PARTIAL_PRODUCTS := Vec.tabulate(28)(i => {
        val bBits = Mux(isDouble,
          (if (i == 0) Cat(n2(MANT_B)(1 downto 0), U(0, 1 bit)) else n2(MANT_B)(2 * i + 1 downto 2 * i - 1)),
          (if (i < 13) (if (i == 0) Cat(n2(MANT_B)(25 downto 24), U(0, 1 bit)) else n2(MANT_B)(2 * i + 1 downto 2 * i - 1)) else U(0, 3 bits)))
        val boothDigit = bBits.mux(
          0 -> AFix(0, 0 exp),
          1 -> aFix,
          2 -> aFix,
          3 -> (aFix << 1),
          4 -> -(aFix << 1),
          5 -> -aFix,
          6 -> -aFix,
          7 -> AFix(0, 0 exp)
        )
        Mux(i < numPartialProducts, boothDigit << (2 * i), AFix(0, 0 exp))
      })

      val stage1Active = RegInit(False).simPublic()
      when(multiplierStart && !stage1Active) {
        stage1Active := True
      } otherwise {
        stage1Active := False
      }
    }

    // Stage 3: Sum partial products
    val mulStage3 = new n3.Area {
      val stage1Active = RegInit(False).simPublic()
      val stage2Active = RegInit(False).simPublic()
      val stage3Active = RegInit(False).simPublic()

      when(mulStage2.stage1Active && !stage1Active) {
        stage1Active := True
        STAGE1_SUM := Mux(isDouble,
          (0 until 14).map(i => n3(PARTIAL_PRODUCTS)(i)).reduce(_ + _),
          (0 until 13).map(i => n3(PARTIAL_PRODUCTS)(i)).reduce(_ + _)
        )
      } otherwise {
        stage1Active := False
      }

      when(stage1Active) {
        when(isDouble) {
          stage2Active := True
          STAGE2_SUM := n3(STAGE1_SUM)
        } otherwise {
          stage3Active := True
          STAGE2_SUM := n3(STAGE1_SUM)
        }
      }

      when(stage2Active) {
        stage3Active := True
        STAGE2_SUM := n3(STAGE2_SUM) + (14 until 28).map(i => n3(PARTIAL_PRODUCTS)(i)).reduce(_ + _)
      }
    }

    // Stage 4: Finalize
    val mulStage4 = new n4.Area {
      val stage3Active = RegInit(False).simPublic()
      when(mulStage3.stage3Active) {
        stage3Active := True
      } otherwise {
        stage3Active := False
      }

      when(stage3Active) {
        val p = n4(STAGE2_SUM).asSInt
        val pPlus1 = p + 1
        MUL_OVERFLOW := p(2 * (FPUConfig.mantissaWidth + 4) - 1)
        val mulMantFull = Mux(MUL_OVERFLOW,
          p(2 * (FPUConfig.mantissaWidth + 4) - 2 downto 2 * (FPUConfig.mantissaWidth + 4) - (FPUConfig.mantissaWidth + 4) - 2),
          p(2 * (FPUConfig.mantissaWidth + 4) - 3 downto 2 * (FPUConfig.mantissaWidth + 4) - (FPUConfig.mantissaWidth + 4) - 3)
        )
        val mulMantP = roundMode.mux(
          0 -> Mux(mulMantFull(1) && (mulMantFull(0) || mulMantFull(2)), pPlus1(2 * (FPUConfig.mantissaWidth + 4) - 3 downto 2 * (FPUConfig.mantissaWidth + 4) - (FPUConfig.mantissaWidth + 4) - 3), mulMantFull),
          1 -> mulMantFull,
          2 -> Mux(mulMantFull(1) && mulMantFull.msb, pPlus1(2 * (FPUConfig.mantissaWidth + 4) - 3 downto 2 * (FPUConfig.mantissaWidth + 4) - (FPUConfig.mantissaWidth + 4) - 3), mulMantFull),
          3 -> Mux(mulMantFull(1) && !mulMantFull.msb, pPlus1(2 * (FPUConfig.mantissaWidth + 4) - 3 downto 2 * (FPUConfig.mantissaWidth + 4) - (FPUConfig.mantissaWidth + 4) - 3), mulMantFull)
        ).asUInt
        RESULT_MANT := mulMantP.resize(FPUConfig.mantissaWidth + 4 bits)
        RESULT_EXP := n4(EXP_A) + n4(EXP_B) - S(FPUConfig.bias, FPUConfig.exponentWidth + 2 bits) + MUL_OVERFLOW.asSInt(2 bits)
        RESULT_SIGN := n4(SIGN_A) ^ n4(SIGN_B)
        when(mulMantP(1 downto 0) =/= 0) {
          exceptions.inexact := True
          exceptions.excCode := ExceptionCodes.inexact
        }
      }
    }

    // Expose for simulation
    mulStage2.stage1Active.simPublic()
    mulStage3.STAGE1_SUM.raw.simPublic()
    mulStage3.stage2Active.simPublic()
    mulStage4.STAGE2_SUM.raw.simPublic()
    mulStage4.stage3Active.simPublic()
    mulStage4.MUL_OVERFLOW.simPublic()
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
    val latency = Mux(stack(0).typeTag === 1, n4(MICRO_OP).latencyDouble, n4(MICRO_OP).latencySingle)

    val stage4 = new n4.Area {
      n4.arbitrateTo(io.resultOut)
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

      io.resultOut.payload.value := finalResult
      io.resultOut.payload.done := operationActive && latencyCounter === latency
      io.resultOut.payload.integerResult := INT_RESULT

      when(isValid && io.resultOut.payload.done) {
        when(exceptions.fpError) {
          when(exceptions.divideByZero) {
            io.resultOut.payload.value := Cat(RESULT_SIGN, U((1 << FPUConfig.exponentWidth) - 1, FPUConfig.exponentWidth bits), U(0, FPUConfig.mantissaWidth bits)).resize(64 bits)
          } elsewhen(exceptions.invalidOp) {
            io.resultOut.payload.value := Cat(RESULT_SIGN, U((1 << FPUConfig.exponentWidth) - 1, FPUConfig.exponentWidth bits), FpuUtils.quietNaN(RESULT_MANT(FPUConfig.mantissaWidth - 1 downto 0), FPUConfig.mantissaWidth + 4)).resize(64 bits)
          }
        }
      }
    }
  }
}