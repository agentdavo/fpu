package fpu

import spinal.core._
import spinal.lib._
import spinal.core.sim._

object FpuSimPipeline extends App {
  val param = FloatUnpackedParam()

  case class FPUTestVector(
    cmd: FPUCmd.E,
    value: BigInt,
    stack0: BigInt,
    stack0Type: FpuFormat.E,
    stack1: BigInt,
    stack1Type: FpuFormat.E,
    stack2: BigInt,
    stack2Type: FpuFormat.E,
    memData: Option[BigInt],
    integerA: Option[Int] = None,
    expected: BigInt,
    expectedType: FpuFormat.E,
    expectedCycles: Int,
    expectError: Boolean,
    expectMemWrite: Option[BigInt]
  )

  val testVectors = Seq(
    FPUTestVector(FPUCmd.fpldnldbi, 0, 0, FpuFormat.DOUBLE, 0, FpuFormat.DOUBLE, 0, FpuFormat.DOUBLE, Some(BigInt("4008000000000000", 16)), Some(0x100), BigInt("4008000000000000", 16), FpuFormat.DOUBLE, 2, false, None),
    FPUTestVector(FPUCmd.fpadd, BigInt("3ff0000000000000", 16), BigInt("4000000000000000", 16), FpuFormat.DOUBLE, 0, FpuFormat.DOUBLE, 0, FpuFormat.DOUBLE, None, None, BigInt("4008000000000000", 16), FpuFormat.DOUBLE, 2, false, None),
    FPUTestVector(FPUCmd.fpmul, BigInt("4000000000000000", 16), BigInt("4000000000000000", 16), FpuFormat.DOUBLE, 0, FpuFormat.DOUBLE, 0, FpuFormat.DOUBLE, None, None, BigInt("4010000000000000", 16), FpuFormat.DOUBLE, 3, false, None),
    FPUTestVector(FPUCmd.fpmul, BigInt("4000000000000000", 16), BigInt("4000000000000000", 16), FpuFormat.SINGLE, 0, FpuFormat.SINGLE, 0, FpuFormat.SINGLE, None, None, BigInt("4080000000000000", 16), FpuFormat.SINGLE, 2, false, None),
    FPUTestVector(FPUCmd.fpdiv, BigInt("4010000000000000", 16), BigInt("4000000000000000", 16), FpuFormat.DOUBLE, 0, FpuFormat.DOUBLE, 0, FpuFormat.DOUBLE, None, None, BigInt("4000000000000000", 16), FpuFormat.DOUBLE, 15, false, None),
    FPUTestVector(FPUCmd.fpsqrt, 0, BigInt("4010000000000000", 16), FpuFormat.DOUBLE, 0, FpuFormat.DOUBLE, 0, FpuFormat.DOUBLE, None, None, BigInt("4000000000000000", 16), FpuFormat.DOUBLE, 15, false, None)
  )

  SimConfig.withFstWave.compile(new Pipeline()).doSim { dut =>
    dut.clockDomain.forkStimulus(period = 20)
    dut.clockDomain.assertReset()
    dut.clockDomain.waitSampling(1)
    dut.clockDomain.deassertReset()
    dut.io.cmdIn.valid #= false
    dut.io.mem.ready #= true
    dut.clockDomain.waitSampling(5)

    def runCommand(test: FPUTestVector): (BigInt, FpuFormat.E, Boolean, Option[BigInt], Int) = {
      println(s"\n*** Testing ${test.cmd}: st0=0x${test.stack0.toString(16)} (${test.stack0Type}), value=0x${test.value.toString(16)} ***")

      dut.stack(0).value #= test.stack0
      dut.stack(0).typeTag #= test.stack0Type
      dut.stack(1).value #= test.stack1
      dut.stack(1).typeTag #= test.stack1Type
      dut.stack(2).value #= test.stack2
      dut.stack(2).typeTag #= test.stack2Type
      test.memData.foreach(dut.io.mem.dataOut #= _)
      dut.clockDomain.waitSampling(1)

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

      while (!dut.io.resultOut.done.toBoolean && cycles < maxCycles) {
        if (dut.io.mem.valid.toBoolean && dut.io.mem.write.toBoolean) {
          memWrite = Some(dut.io.mem.dataIn.toBigInt)
        }
        println(s"Cycle $cycles: " +
                s"valid=${dut.io.resultOut.valid.toBoolean}, " +
                s"done=${dut.io.resultOut.done.toBoolean}, " +
                s"result=0x${dut.io.resultOut.value.toBigInt.toString(16)}")
        dut.clockDomain.waitSampling()
        cycles += 1
      }

      val result = dut.io.resultOut.value.toBigInt
      val resultType = dut.stack(0).typeTag
      val errorOccurred = dut.io.exceptions.fpError.toBoolean
      println(s"Result: 0x${result.toString(16)}, Type: ${resultType}, Cycles: $cycles, " +
              s"Error: $errorOccurred, MemWrite: ${memWrite.map(v => s"0x${v.toString(16)}").getOrElse("None")}")
      (result, resultType, errorOccurred, memWrite, cycles)
    }

    for (test <- testVectors) {
      val (result, resultType, errorOccurred, memWrite, cycles) = runCommand(test)
      val pass = (result == test.expected &&
                  resultType == test.expectedType &&
                  errorOccurred == test.expectError &&
                  memWrite == test.expectMemWrite &&
                  cycles == test.expectedCycles)
      if (!pass) {
        println(s"FAIL: ${test.cmd} expected result=0x${test.expected.toString(16)}, " +
                s"type=${test.expectedType}, " +
                s"cycles=${test.expectedCycles}, error=${test.expectError}, " +
                s"memWrite=${test.expectMemWrite.map(v => s"0x${v.toString(16)}").getOrElse("None")}, " +
                s"got result=0x${result.toString(16)}, type=${resultType}, " +
                s"cycles=$cycles, error=$errorOccurred, memWrite=${memWrite.map(v => s"0x${v.toString(16)}").getOrElse("None")}")
      } else {
        println(s"PASS: ${test.cmd}")
      }
      dut.clockDomain.waitSampling(5)
    }
  }
}