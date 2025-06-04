package fpu

import spinal.core._
import spinal.lib._
import spinal.core.sim._

class Pipeline extends Component {
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

  io.cmdIn.ready := True
  io.resultOut.valid := io.cmdIn.valid
  io.resultOut.done := io.cmdIn.valid
  io.resultOut.value := io.cmdIn.value
  io.resultOut.integerResult := 0
  io.mem.addr := io.cmdIn.addr
  io.mem.dataIn := io.cmdIn.value
  io.mem.read := False
  io.mem.write := False
  io.mem.valid := False

  io.exceptions.invalidOp := False
  io.exceptions.divideByZero := False
  io.exceptions.overflow := False
  io.exceptions.underflow := False
  io.exceptions.inexact := False
  io.exceptions.fpError := False
  io.exceptions.excCode := 0

  val stack = Vec(Reg(FPReg()), FPUConfig.stackSize)
  for (s <- stack) {
    s.value.simPublic()
    s.typeTag.simPublic()
    s.value init(0)
    s.typeTag init(FpuFormat.DOUBLE)
  }
}

object PipelineSim extends App {
  import spinal.core.sim._
  SimConfig.withFstWave.compile(new Pipeline()).doSim { dut =>
    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitSampling()
  }
}
