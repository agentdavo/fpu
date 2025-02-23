package fpu

import spinal.core._

object FPUCmd extends SpinalEnum {
  // Full T9000 opcode set from Tables 11.24–11.32
  val fpldnlsn, fpldnldb, fpldnlsni, fpldnldbi, fpldzerosn, fpldzerodb, fpldnladdsn, fpldnladddb,
      fpldnlmulsn, fpldnlmuldb, fpstnlsn, fpstnldb, fpstnli32, fpentry, fprev, fpdup, fprn, fprz,
      fprp, fprm, fpadd, fpsub, fpmul, fpdiv, fpabs, fpexpinc32, fpexpdec32, fpmulby2, fpdivby2,
      fprtoi32, fpi32tor32, fpi32tor64, fpb32tor64, fpnoround, fpint, fpgt, fpeq, fpordered, fpnan,
      fpnotfinite, fpchki32, fpchki64, fpge, fplg, fpsqrt, fprem, fprange, fpr32tor64, fpr64tor32 = newElement()

  defaultEncoding = SpinalEnumEncoding("T9000Cmd")(
    fpldnlsn -> 0x8E, fpldnldb -> 0x8A, fpldnlsni -> 0x86, fpldnldbi -> 0x82, fpldzerosn -> 0x9F,
    fpldzerodb -> 0xA0, fpldnladdsn -> 0xAA, fpldnladddb -> 0xA6, fpldnlmulsn -> 0xAC, fpldnlmuldb -> 0xA8,
    fpstnlsn -> 0x88, fpstnldb -> 0x84, fpstnli32 -> 0x9E, fpentry -> 0xAB, fprev -> 0xA4, fpdup -> 0xA3,
    fprn -> 0xD0, fprz -> 0x06, fprp -> 0x04, fprm -> 0x05, fpadd -> 0xS7, fpsub -> 0xS9, fpmul -> 0xSB,
    fpdiv -> 0xSC, fpabs -> 0xDB, fpexpinc32 -> 0xDA, fpexpdec32 -> 0xD9, fpmulby2 -> 0xD2, fpdivby2 -> 0xD1,
    fprtoi32 -> 0x90, fpi32tor32 -> 0x96, fpi32tor64 -> 0x98, fpb32tor64 -> 0x9A, fpnoround -> 0x00,
    fpint -> 0xA1, fpgt -> 0x94, fpeq -> 0x95, fpordered -> 0x92, fpnan -> 0x91, fpnotfinite -> 0x93,
    fpchki32 -> 0x0E, fpchki64 -> 0x0F, fpge -> 0x97, fplg -> 0x9B, fpsqrt -> 0xD3, fprem -> 0xCF,
    fprange -> 0xSD, fpr32tor64 -> 0x07, fpr64tor32 -> 0x08
  )
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
  val mantissaWidth = 57 // T9000: 53 + 4 guard bits (Section 5)
  val stackSize = 3      // T9000: 3-element stack (Section 2)
  val nanMaskValue = BigInt("8000000000000", 16)
  val bias = 1023
  val clockFreqMHz = 50  // T9000: 50 MHz (Section 1)

  // Latency aligned with T9000 Tables 11.24–11.32
  def latencyFor(cmd: FPUCmd.E): (Int, Int) = cmd match { // (single, double)
    case FPUCmd.fpadd => (2, 2)        // Table 11.30
    case FPUCmd.fpsub => (2, 2)        // Table 11.30
    case FPUCmd.fpmul => (2, 3)        // Table 11.30
    case FPUCmd.fpdiv => (5, 15)       // Table 11.30
    case FPUCmd.fpsqrt => (5, 15)      // Table 11.32
    case FPUCmd.fpldnlsn => (2, 2)     // Table 11.24
    case FPUCmd.fpldnldb => (2, 2)     // Table 11.24
    case FPUCmd.fpldnlsni => (2, 2)    // Table 11.24
    case FPUCmd.fpldnldbi => (2, 2)    // Table 11.24
    case FPUCmd.fpldzerosn => (1, 1)   // Table 11.24
    case FPUCmd.fpldzerodb => (1, 1)   // Table 11.24
    case FPUCmd.fpldnladdsn => (3, 3)  // Table 11.24
    case FPUCmd.fpldnladddb => (3, 3)  // Table 11.24
    case FPUCmd.fpldnlmulsn => (2, 2)  // Table 11.24
    case FPUCmd.fpldnlmuldb => (3, 3)  // Table 11.24
    case FPUCmd.fpstnlsn => (2, 2)     // Table 11.24
    case FPUCmd.fpstnldb => (2, 2)     // Table 11.24
    case FPUCmd.fpstnli32 => (2, 2)    // Table 11.24
    case FPUCmd.fpentry => (1, 1)      // Table 11.25 (assumed)
    case FPUCmd.fprev => (1, 1)        // Table 11.25
    case FPUCmd.fpdup => (1, 1)        // Table 11.25
    case FPUCmd.fprn => (1, 1)         // Table 11.26
    case FPUCmd.fprz => (1, 1)         // Table 11.26
    case FPUCmd.fprp => (1, 1)         // Table 11.26
    case FPUCmd.fprm => (1, 1)         // Table 11.26
    case FPUCmd.fpabs => (1, 1)        // Table 11.30
    case FPUCmd.fpexpinc32 => (2, 2)   // Table 11.30
    case FPUCmd.fpexpdec32 => (2, 2)   // Table 11.30
    case FPUCmd.fpmulby2 => (2, 2)     // Table 11.30
    case FPUCmd.fpdivby2 => (2, 2)     // Table 11.30
    case FPUCmd.fprtoi32 => (2, 4)     // Table 11.29
    case FPUCmd.fpi32tor32 => (2, 4)   // Table 11.29
    case FPUCmd.fpi32tor64 => (2, 2)   // Table 11.29
    case FPUCmd.fpb32tor64 => (2, 2)   // Table 11.29
    case FPUCmd.fpnoround => (2, 2)    // Table 11.29
    case FPUCmd.fpint => (2, 4)        // Table 11.29
    case FPUCmd.fpgt => (1, 1)         // Table 11.28
    case FPUCmd.fpeq => (1, 1)         // Table 11.28
    case FPUCmd.fpordered => (1, 1)    // Table 11.28
    case FPUCmd.fpnan => (1, 1)        // Table 11.28
    case FPUCmd.fpnotfinite => (1, 1)  // Table 11.28
    case FPUCmd.fpchki32 => (1, 1)     // Table 11.28
    case FPUCmd.fpchki64 => (1, 1)     // Table 11.28
    case FPUCmd.fpge => (2, 2)         // Table 11.32
    case FPUCmd.fplg => (2, 2)         // Table 11.32
    case FPUCmd.fprem => (74, 529)     // Table 11.32 (max)
    case FPUCmd.fprange => (10, 17)    // Table 11.32 (max)
    case FPUCmd.fpr32tor64 => (2, 2)   // Table 11.29
    case FPUCmd.fpr64tor32 => (2, 2)   // Table 11.29
  }

  // Pipeline control functions (T9000 Section 2, Figure 1)
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