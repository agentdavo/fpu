package fpu

import spinal.core._
import spinal.lib._
import spinal.core.sim._

object FpuSimPipeline extends App {
  case class FPUTestVector(
    cmd: FPUCmd.E,
    value: BigInt,
    stack0: BigInt,
    stack0Type: Int,
    stack1: BigInt,
    stack1Type: Int,
    stack2: BigInt,
    stack2Type: Int,
    memData: Option[BigInt],
    integerA: Option[Int] = None,
    expected: BigInt,
    expectedType: Int,
    expectedCycles: Int,
    expectError: Boolean,
    expectMemWrite: Option[BigInt]
  )

  // Updated test vectors with T9000-compliant multiplier timing
  val testVectors = Seq(
    // Memory load/store operations
    FPUTestVector(FPUCmd.fpldnldbi, BigInt(0), BigInt(0), 1, BigInt(0), 1, BigInt(0), 1,
                  Some(BigInt("4008000000000000", 16)), Some(0x100), BigInt("4008000000000000", 16), 1, 2, false, None),
    FPUTestVector(FPUCmd.fpstnldb, BigInt(0), BigInt("4000000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt("4000000000000000", 16), 1, 2, false, Some(BigInt("4000000000000000", 16))),
    FPUTestVector(FPUCmd.fpldnlsni, BigInt(0), BigInt(0), 0, BigInt(0), 0, BigInt(0), 0,
                  Some(BigInt("40400000", 16)), Some(0x100), BigInt("4008000000000000", 16), 0, 2, false, None),

    // Addition/Subtraction (2 cycles)
    FPUTestVector(FPUCmd.fpadd, BigInt("3ff0000000000000", 16), BigInt("4000000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt("4008000000000000", 16), 1, 2, false, None),
    FPUTestVector(FPUCmd.fpadd, BigInt("3f80000000000000", 16), BigInt("4000000000000000", 16), 0, BigInt(0), 0, BigInt(0), 0,
                  None, None, BigInt("4040000000000000", 16), 0, 2, false, None),
    FPUTestVector(FPUCmd.fpsub, BigInt("4008000000000000", 16), BigInt("3ff0000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt("4000000000000000", 16), 1, 2, false, None),

    // Store single precision
    FPUTestVector(FPUCmd.fpstnlsn, BigInt(0), BigInt("4000000000000000", 16), 0, BigInt(0), 0, BigInt(0), 0,
                  None, None, BigInt("4000000000000000", 16), 0, 2, false, Some(BigInt("40000000", 16))),

    // Load memory operations
    FPUTestVector(FPUCmd.fpldnldb, BigInt(0), BigInt(0), 1, BigInt(0), 1, BigInt(0), 1,
                  Some(BigInt("4010000000000000", 16)), None, BigInt("4010000000000000", 16), 1, 2, false, None),
    FPUTestVector(FPUCmd.fpldnlsn, BigInt(0), BigInt(0), 0, BigInt(0), 0, BigInt(0), 0,
                  Some(BigInt("40000000", 16)), None, BigInt("4000000000000000", 16), 0, 2, false, None),

    // Multiplication (2 cycles single, 3 cycles double)
    FPUTestVector(FPUCmd.fpmul, BigInt("4000000000000000", 16), BigInt("4000000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt("4010000000000000", 16), 1, 3, false, None), // Double precision: 3 cycles
    FPUTestVector(FPUCmd.fpmul, BigInt("4000000000000000", 16), BigInt("4000000000000000", 16), 0, BigInt(0), 0, BigInt(0), 0,
                  None, None, BigInt("4080000000000000", 16), 0, 2, false, None), // Single precision: 2 cycles

    // Division/Square Root
    FPUTestVector(FPUCmd.fpdiv, BigInt("4010000000000000", 16), BigInt("4000000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt("4000000000000000", 16), 1, 15, false, None),
    FPUTestVector(FPUCmd.fpdiv, BigInt("4080000000000000", 16), BigInt("4000000000000000", 16), 0, BigInt(0), 0, BigInt(0), 0,
                  None, None, BigInt("4000000000000000", 16), 0, 7, false, None),
    FPUTestVector(FPUCmd.fpsqrt, BigInt(0), BigInt("4010000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt("4000000000000000", 16), 1, 15, false, None),
    FPUTestVector(FPUCmd.fpsqrt, BigInt(0), BigInt("4080000000000000", 16), 0, BigInt(0), 0, BigInt(0), 0,
                  None, None, BigInt("4000000000000000", 16), 0, 7, false, None),

    // Comparison and status operations
    FPUTestVector(FPUCmd.fprange, BigInt(0), BigInt("7ff0000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt(0), 1, 3, false, None),
    FPUTestVector(FPUCmd.fpnan, BigInt(0), BigInt("7ff8000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt(1), 0, 1, false, None),
    FPUTestVector(FPUCmd.fpordered, BigInt("3ff0000000000000", 16), BigInt("4000000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt(1), 0, 1, false, None),
    FPUTestVector(FPUCmd.fpnotfinite, BigInt(0), BigInt("7ff0000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt(1), 0, 1, false, None),
    FPUTestVector(FPUCmd.fpgt, BigInt("4000000000000000", 16), BigInt("3ff0000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt(1), 0, 1, false, None),
    FPUTestVector(FPUCmd.fpeq, BigInt("4000000000000000", 16), BigInt("4000000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt(1), 0, 1, false, None),
    FPUTestVector(FPUCmd.fpge, BigInt("4000000000000000", 16), BigInt("3ff0000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt(1), 0, 1, false, None),
    FPUTestVector(FPUCmd.fplg, BigInt("4000000000000000", 16), BigInt("3ff0000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt(1), 0, 1, false, None),

    // Integer conversion
    FPUTestVector(FPUCmd.fpi32tor32, BigInt(0), BigInt(0), 0, BigInt(0), 0, BigInt(0), 0,
                  None, Some(2), BigInt("40000000", 16), 0, 3, false, None),
    FPUTestVector(FPUCmd.fpi32tor64, BigInt(0), BigInt(0), 1, BigInt(0), 1, BigInt(0), 1,
                  None, Some(2), BigInt("4000000000000000", 16), 1, 3, false, None),
    FPUTestVector(FPUCmd.fpb32tor64, BigInt(0), BigInt(0), 1, BigInt(0), 1, BigInt(0), 1,
                  None, Some(2), BigInt("4000000000000000", 16), 1, 3, false, None),
    FPUTestVector(FPUCmd.fprtoi32, BigInt(0), BigInt("4004000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt(2), 0, 3, false, None),
    FPUTestVector(FPUCmd.fpstnli32, BigInt(0), BigInt(2), 0, BigInt(0), 0, BigInt(0), 0,
                  None, None, BigInt(2), 0, 2, false, Some(BigInt(2))),
    FPUTestVector(FPUCmd.fpint, BigInt(0), BigInt("400599999999999a", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt("4008000000000000", 16), 1, 3, false, None),

    // Stack operations
    FPUTestVector(FPUCmd.fpldzerosn, BigInt(0), BigInt(0), 0, BigInt(0), 0, BigInt(0), 0,
                  None, None, BigInt(0), 0, 1, false, None),
    FPUTestVector(FPUCmd.fpldzerodb, BigInt(0), BigInt(0), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt(0), 1, 1, false, None),
    FPUTestVector(FPUCmd.fpdup, BigInt(0), BigInt("4000000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt("4000000000000000", 16), 1, 1, false, None),
    FPUTestVector(FPUCmd.fprev, BigInt(0), BigInt("4000000000000000", 16), 1, BigInt("3ff0000000000000", 16), 1, BigInt(0), 1,
                  None, None, BigInt("3ff0000000000000", 16), 1, 1, false, None),

    // Load and operate
    FPUTestVector(FPUCmd.fpldnladddb, BigInt(0), BigInt("3ff0000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  Some(BigInt("4000000000000000", 16)), None, BigInt("4008000000000000", 16), 1, 3, false, None),
    FPUTestVector(FPUCmd.fpldnlmuldb, BigInt(0), BigInt("4000000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  Some(BigInt("4000000000000000", 16)), None, BigInt("4010000000000000", 16), 1, 3, false, None), // Double precision: 3 cycles
    FPUTestVector(FPUCmd.fpldnladdsn, BigInt(0), BigInt("3ff0000000000000", 16), 0, BigInt(0), 0, BigInt(0), 0,
                  Some(BigInt("3f800000", 16)), None, BigInt("4000000000000000", 16), 0, 3, false, None),
    FPUTestVector(FPUCmd.fpldnlmulsn, BigInt(0), BigInt("4000000000000000", 16), 0, BigInt(0), 0, BigInt(0), 0,
                  Some(BigInt("40000000", 16)), None, BigInt("4080000000000000", 16), 0, 2, false, None), // Single precision: 2 cycles

    // Rounding mode changes
    FPUTestVector(FPUCmd.fprn, BigInt(0), BigInt(0), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt(0), 1, 1, false, None),
    FPUTestVector(FPUCmd.fprp, BigInt(0), BigInt(0), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt(0), 1, 1, false, None),
    FPUTestVector(FPUCmd.fprm, BigInt(0), BigInt(0), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt(0), 1, 1, false, None),
    FPUTestVector(FPUCmd.fprz, BigInt(0), BigInt(0), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt(0), 1, 1, false, None),

    // Exponent scaling
    FPUTestVector(FPUCmd.fpdivby2, BigInt(0), BigInt("4010000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt("4000000000000000", 16), 1, 1, false, None),
    FPUTestVector(FPUCmd.fpmulby2, BigInt(0), BigInt("4000000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt("4010000000000000", 16), 1, 1, false, None),
    FPUTestVector(FPUCmd.fpexpdec32, BigInt(0), BigInt("4000000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt("3e70000000000000", 16), 1, 1, false, None),
    FPUTestVector(FPUCmd.fpexpinc32, BigInt(0), BigInt("4000000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt("41f0000000000000", 16), 1, 1, false, None),

    // Miscellaneous
    FPUTestVector(FPUCmd.fpabs, BigInt(0), BigInt("c000000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt("4000000000000000", 16), 1, 1, false, None),
    FPUTestVector(FPUCmd.fpchki32, BigInt(0), BigInt("4000000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt(1), 0, 1, false, None),
    FPUTestVector(FPUCmd.fpchki64, BigInt(0), BigInt("4000000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt(1), 0, 1, false, None),
    FPUTestVector(FPUCmd.fpstall, BigInt(0), BigInt("4000000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt("4000000000000000", 16), 1, 7, false, Some(BigInt("4000000000000000", 16))),
    FPUTestVector(FPUCmd.fpldall, BigInt(0), BigInt(0), 1, BigInt(0), 1, BigInt(0), 1,
                  Some(BigInt("4000000000000000", 16)), None, BigInt("4000000000000000", 16), 1, 7, false, None),
    FPUTestVector(FPUCmd.fpr32tor64, BigInt(0), BigInt("40000000", 16), 0, BigInt(0), 0, BigInt(0), 0,
                  None, None, BigInt("4000000000000000", 16), 1, 3, false, None),
    FPUTestVector(FPUCmd.fpr64tor32, BigInt(0), BigInt("4000000000000000", 16), 1, BigInt(0), 1, BigInt(0), 1,
                  None, None, BigInt("40000000", 16), 0, 3, false, None)
  )

  SimConfig.withFstWave.compile(new Fpu()).doSim { dut =>
    dut.clockDomain.forkStimulus(period = 20)
    dut.clockDomain.assertReset()
    dut.clockDomain.waitSampling(1)
    dut.clockDomain.deassertReset()
    dut.io.cmdIn.valid #= false
    dut.io.mem.ready #= true
    dut.clockDomain.waitSampling(5)

    def runCommand(test: FPUTestVector): (BigInt, Int, Boolean, Option[BigInt], Int) = {
      println(s"\n*** Testing ${test.cmd}: st0=0x${test.stack0.toString(16)} (${if (test.stack0Type == 1) "double" else "single"}), value=0x${test.value.toString(16)} ***")

      // Initialize stack and memory
      dut.stack(0).value #= test.stack0
      dut.stack(0).typeTag #= test.stack0Type
      dut.stack(1).value #= test.stack1
      dut.stack(1).typeTag #= test.stack1Type
      dut.stack(2).value #= test.stack2
      dut.stack(2).typeTag #= test.stack2Type
      test.memData.foreach(dut.io.mem.dataOut #= _)
      dut.clockDomain.waitSampling(1)

      // Send command
      dut.io.cmdIn.cmd #= test.cmd
      dut.io.cmdIn.value #= test.value
      dut.io.cmdIn.addr #= 0x100
      dut.io.cmdIn.integerA #= test.integerA.getOrElse(0).toLong
      dut.io.cmdIn.integerB #= 0L
      dut.io.cmdIn.integerC #= 0L
      dut.io.cmdIn.valid #= true
      dut.clockDomain.waitSampling(1)
      dut.io.cmdIn.valid #= false

      var cycles = 0
      val maxCycles = 50
      var memWrite: Option[BigInt] = None

      // Multiplier-specific signals
      val mulStage2 = dut.pipeline.multiplierPlugin.mulStage2
      val mulStage3 = dut.pipeline.multiplierPlugin.mulStage3
      val mulStage4 = dut.pipeline.multiplierPlugin.mulStage4

      while (!dut.io.resultOut.done.toBoolean && cycles < maxCycles) {
        if (dut.io.mem.valid.toBoolean && dut.io.mem.write.toBoolean) {
          memWrite = Some(dut.io.mem.dataIn.toBigInt)
        }
        println(s"Cycle $cycles: " +
                s"valid=${dut.io.resultOut.valid.toBoolean}, " +
                s"done=${dut.io.resultOut.done.toBoolean}, " +
                s"result=0x${dut.io.resultOut.value.toBigInt.toString(16)}, " +
                s"stage1Active=${mulStage2.stage1Active.toBoolean}, " +
                s"stage1Sum=0x${mulStage3.STAGE1_SUM.raw.toBigInt.toString(16)}, " +
                s"stage2Active=${mulStage3.stage2Active.toBoolean}, " +
                s"stage2Sum=0x${mulStage4.STAGE2_SUM.raw.toBigInt.toString(16)}, " +
                s"stage3Active=${mulStage4.stage3Active.toBoolean}, " +
                s"mulOverflow=${mulStage4.MUL_OVERFLOW.toBoolean}")
        dut.clockDomain.waitSampling()
        cycles += 1
      }

      val result = dut.io.resultOut.value.toBigInt
      val resultType = dut.stack(0).typeTag.toInt
      val errorOccurred = dut.io.exceptions.fpError.toBoolean
      println(s"Result: 0x${result.toString(16)}, Type: ${if (resultType == 1) "double" else "single"}, Cycles: $cycles, " +
              s"Error: $errorOccurred, MemWrite: ${memWrite.map(v => s"0x${v.toString(16)}").getOrElse("None")}, " +
              s"Exceptions: invalidOp=${dut.io.exceptions.invalidOp.toBoolean}, " +
              s"divideByZero=${dut.io.exceptions.divideByZero.toBoolean}")
      (result, resultType, errorOccurred, memWrite, cycles)
    }

    for (test <- testVectors) {
      val (result, resultType, errorOccurred, memWrite, cycles) = runCommand(test)
      val pass = (result == test.expected &&
                  resultType == test.expectedType &&
                  errorOccurred == test.expectError &&
                  memWrite == test.expectMemWrite &&
                  cycles <= test.expectedCycles)
      if (!pass) {
        println(s"FAIL: ${test.cmd} expected result=0x${test.expected.toString(16)}, " +
                s"type=${if (test.expectedType == 1) "double" else "single"}, " +
                s"cycles=${test.expectedCycles}, error=${test.expectError}, " +
                s"memWrite=${test.expectMemWrite.map(v => s"0x${v.toString(16)}").getOrElse("None")}, " +
                s"got result=0x${result.toString(16)}, type=${if (resultType == 1) "double" else "single"}, " +
                s"cycles=$cycles, error=$errorOccurred, memWrite=${memWrite.map(v => s"0x${v.toString(16)}").getOrElse("None")}")
      } else {
        println(s"PASS: ${test.cmd}")
      }
      dut.clockDomain.waitSampling(5)
    }
  }
}