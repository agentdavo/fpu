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