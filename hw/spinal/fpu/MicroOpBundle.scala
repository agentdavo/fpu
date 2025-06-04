package fpu

import spinal.core._

case class MicroOpBundle() extends Bundle {
  val cmd = FPUCmd()
  val latencySingle = UInt(6 bits)
  val latencyDouble = UInt(6 bits)
  val shiftStack = Bool()
  val popStack = Bool()
  val writeResult = Bool()
  val memRead = Bool()
  val memWrite = Bool()
  val useInteger = Bool()
  val stageEnable = Bits(5 bits)
}
