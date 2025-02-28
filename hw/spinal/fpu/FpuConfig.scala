package fpu

import spinal.core._
import spinal.lib._

object FPUCmd extends SpinalEnum {
  val fpldnlsn, fpldnldb, fpldnlsni, fpldnldbi, fpldzerosn, fpldzerodb, fpldnladdsn, fpldnladddb,
      fpldnlmulsn, fpldnlmuldb, fpstnlsn, fpstnldb, fpstnli32, fpentry, fprev, fpdup, fprn, fprz,
      fprp, fprm, fpadd, fpsub, fpmul, fpdiv, fpabs, fpexpinc32, fpexpdec32, fpmulby2, fpdivby2,
      fprtoi32, fpi32tor32, fpi32tor64, fpb32tor64, fpnoround, fpint, fpgt, fpeq, fpordered, fpnan,
      fpnotfinite, fpchki32, fpchki64, fpge, fplg, fpsqrt, fprem, fprange, fpr32tor64, fpr64tor32 = newElement()

  defaultEncoding = SpinalEnumEncoding("T9000Cmd")(
    fpldnlsn -> 0x8E, fpldnldb -> 0x8A, fpldnlsni -> 0x86, fpldnldbi -> 0x82, fpldzerosn -> 0x9F,
    fpldzerodb -> 0xA0, fpldnladdsn -> 0xAA, fpldnladddb -> 0xA6, fpldnlmulsn -> 0xAC, fpldnlmuldb -> 0xA8,
    fpstnlsn -> 0x88, fpstnldb -> 0x84, fpstnli32 -> 0x9E, fpentry -> 0xAB, fprev -> 0xA4, fpdup -> 0xA3,
    fprn -> 0xD0, fprz -> 0x06, fprp -> 0x04, fprm -> 0x05, fpadd -> 0x87, fpsub -> 0x89, fpmul -> 0x8B,
    fpdiv -> 0x8C, fpabs -> 0xDB, fpexpinc32 -> 0xDA, fpexpdec32 -> 0xD9, fpmulby2 -> 0xD2, fpdivby2 -> 0xD1,
    fprtoi32 -> 0x90, fpi32tor32 -> 0x96, fpi32tor64 -> 0x98, fpb32tor64 -> 0x9A, fpnoround -> 0x00,
    fpint -> 0xA1, fpgt -> 0x94, fpeq -> 0x95, fpordered -> 0x92, fpnan -> 0x91, fpnotfinite -> 0x93,
    fpchki32 -> 0x0E, fpchki64 -> 0x0F, fpge -> 0x97, fplg -> 0x9B, fpsqrt -> 0xD3, fprem -> 0xCF,
    fprange -> 0x8D, fpr32tor64 -> 0x07, fpr64tor32 -> 0x08
  )
}

object RoundMode extends SpinalEnum {
  val NEAREST, ZERO, PLUS, MINUS = newElement()
  defaultEncoding = SpinalEnumEncoding("T9000RoundMode")(NEAREST -> 0, ZERO -> 1, PLUS -> 2, MINUS -> 3)
}

object ExceptionCodes {
  val divideByZero = 1
  val overflow = 2
  val underflow = 3
  val invalidOp = 4
  val inexact = 5
}

object FPUConfig {
  val exponentWidth = 11
  val mantissaWidth = 57
  val stackSize = 3
  val nanMaskValue = BigInt("8000000000000", 16)
  val bias = 1023
  val clockFreqMHz = 50

  case class MemoryInterface() extends Bundle with IMasterSlave {
    val addr = UInt(32 bits)
    val dataIn = Bits(64 bits)
    val dataOut = Bits(64 bits)
    val read = Bool()
    val write = Bool()
    val valid = Bool()
    val ready = Bool()

    override def asMaster(): Unit = {
      out(addr, dataIn, read, write, valid)
      in(dataOut, ready)
    }
  }

  def latencyFor(cmd: FPUCmd.E): (Int, Int) = cmd match {
    case FPUCmd.fpadd => (2, 2)
    case FPUCmd.fpsub => (2, 2)
    case FPUCmd.fpmul => (2, 3)
    case FPUCmd.fpdiv => (5, 15)
    case FPUCmd.fpsqrt => (5, 15)
    case FPUCmd.fpldnlsn => (2, 2)
    case FPUCmd.fpldnldb => (2, 2)
    case FPUCmd.fpldnlsni => (2, 2)
    case FPUCmd.fpldnldbi => (2, 2)
    case FPUCmd.fpldzerosn => (1, 1)
    case FPUCmd.fpldzerodb => (1, 1)
    case FPUCmd.fpldnladdsn => (3, 3)
    case FPUCmd.fpldnladddb => (3, 3)
    case FPUCmd.fpldnlmulsn => (2, 2)
    case FPUCmd.fpldnlmuldb => (3, 3)
    case FPUCmd.fpstnlsn => (2, 2)
    case FPUCmd.fpstnldb => (2, 2)
    case FPUCmd.fpstnli32 => (2, 2)
    case FPUCmd.fpentry => (1, 1)
    case FPUCmd.fprev => (1, 1)
    case FPUCmd.fpdup => (1, 1)
    case FPUCmd.fprn => (1, 1)
    case FPUCmd.fprz => (1, 1)
    case FPUCmd.fprp => (1, 1)
    case FPUCmd.fprm => (1, 1)
    case FPUCmd.fpabs => (1, 1)
    case FPUCmd.fpexpinc32 => (2, 2)
    case FPUCmd.fpexpdec32 => (2, 2)
    case FPUCmd.fpmulby2 => (2, 2)
    case FPUCmd.fpdivby2 => (2, 2)
    case FPUCmd.fprtoi32 => (2, 4)
    case FPUCmd.fpi32tor32 => (2, 4)
    case FPUCmd.fpi32tor64 => (2, 2)
    case FPUCmd.fpb32tor64 => (2, 2)
    case FPUCmd.fpnoround => (2, 2)
    case FPUCmd.fpint => (2, 4)
    case FPUCmd.fpgt => (1, 1)
    case FPUCmd.fpeq => (1, 1)
    case FPUCmd.fpordered => (1, 1)
    case FPUCmd.fpnan => (1, 1)
    case FPUCmd.fpnotfinite => (1, 1)
    case FPUCmd.fpchki32 => (1, 1)
    case FPUCmd.fpchki64 => (1, 1)
    case FPUCmd.fpge => (2, 2)
    case FPUCmd.fplg => (2, 2)
    case FPUCmd.fprem => (74, 529)
    case FPUCmd.fprange => (10, 17)
    case FPUCmd.fpr32tor64 => (2, 2)
    case FPUCmd.fpr64tor32 => (2, 2)
  }

  def shiftStackFor(cmd: FPUCmd.E): Bool = cmd match {
    case FPUCmd.fpldnlsn | FPUCmd.fpldnldb | FPUCmd.fpldnlsni | FPUCmd.fpldnldbi |
         FPUCmd.fpldzerosn | FPUCmd.fpldzerodb | FPUCmd.fpdup => True
    case _ => False
  }

  def popStackFor(cmd: FPUCmd.E): Bool = cmd match {
    case FPUCmd.fpadd | FPUCmd.fpsub | FPUCmd.fpmul | FPUCmd.fpdiv | FPUCmd.fpldnladdsn |
         FPUCmd.fpldnladddb | FPUCmd.fpldnlmulsn | FPUCmd.fpldnlmuldb | FPUCmd.fpgt |
         FPUCmd.fpeq | FPUCmd.fpge | FPUCmd.fplg => True
    case _ => False
  }

  def writeResultFor(cmd: FPUCmd.E): Bool = cmd match {
    case FPUCmd.fpstnlsn | FPUCmd.fpstnldb | FPUCmd.fpstnli32 => False
    case _ => True
  }

  def memReadFor(cmd: FPUCmd.E): Bool = cmd match {
    case FPUCmd.fpldnlsn | FPUCmd.fpldnldb | FPUCmd.fpldnlsni | FPUCmd.fpldnldbi |
         FPUCmd.fpldnladdsn | FPUCmd.fpldnladddb | FPUCmd.fpldnlmulsn | FPUCmd.fpldnlmuldb => True
    case _ => False
  }

  def memWriteFor(cmd: FPUCmd.E): Bool = cmd match {
    case FPUCmd.fpstnlsn | FPUCmd.fpstnldb | FPUCmd.fpstnli32 => True
    case _ => False
  }

  def useIntegerFor(cmd: FPUCmd.E): Bool = cmd match {
    case FPUCmd.fpi32tor32 | FPUCmd.fpi32tor64 | FPUCmd.fpb32tor64 | FPUCmd.fprtoi32 |
         FPUCmd.fpchki32 | FPUCmd.fpchki64 => True
    case _ => False
  }
}