package fpu

import spinal.core._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin.FiberPlugin

// Base pipeline class with fixed stages
class Pipeline extends Component {
  // Define the five pipeline nodes
  val n0 = Node() // Input handling and stack management
  val n1 = Node() // Preprocessing (unpack operands)
  val n2 = Node() // Main computation
  val n3 = Node() // Intermediate pass-through
  val n4 = Node() // Output latching and result delivery

  // Connect nodes with StageLinks
  val s01 = StageLink(n0, n1)
  val s12 = StageLink(n1, n2)
  val s23 = StageLink(n2, n3)
  val s34 = StageLink(n3, n4)

  // Common payloads accessible across plugins
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
    s.typeTag.init(1)
  }
  val areg = Reg(UInt(32 bits)) init(0)
  val breg = Reg(UInt(32 bits)) init(0)
  val creg = Reg(UInt(32 bits)) init(0)

  val exceptions = io.exceptions

  // Base build method to instantiate plugins
  def build(): Unit = {
    Builder(s01, s12, s23, s34)
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

    // Test case: fpldzerosn (load zero single, 1 cycle, shifts stack)
    println("Testing basic pipeline with fpldzerosn")
    
    // Initialize inputs
    dut.io.cmdIn.cmd #= FPUCmd.fpldzerosn
    dut.io.cmdIn.value #= 0
    dut.io.cmdIn.addr #= 0x100
    dut.io.cmdIn.integerA #= 0
    dut.io.cmdIn.integerB #= 0
    dut.io.cmdIn.integerC #= 0
    dut.io.cmdIn.valid #= true
    dut.clockDomain.waitSampling(1)
    dut.io.cmdIn.valid #= false

    // Monitor pipeline stages and output
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

    // Check results
    val result = dut.io.resultOut.value.toBigInt
    val expected = BigInt(0) // fpldzerosn loads 0
    val stackTop = dut.stack(0).value.toBigInt
    assert(result == expected, s"Result mismatch: got 0x$result, expected 0x$expected")
    assert(stackTop == expected, s"Stack top mismatch: got 0x$stackTop, expected 0x$expected")
    assert(cycles <= 5, s"Pipeline took too long: $cycles cycles") // 5 stages max
    println(s"PASS: Pipeline completed in $cycles cycles, result=0x$result, stack[0]=0x$stackTop")
  }
}