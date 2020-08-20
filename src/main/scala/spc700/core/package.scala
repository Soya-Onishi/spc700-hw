package spc700

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util.Cat

package object core {
  class RegisterFile extends Bundle {
    val pc: UInt = UInt(16.W)
    val sp: UInt = UInt(8.W)

    val a: UInt = UInt(8.W)
    val x: UInt = UInt(8.W)
    val y: UInt = UInt(8.W)

    val psw: PSW = PSW()
  }

  object RegisterFile {
    def apply(): RegisterFile = {
      new RegisterFile()
    }
  }

  class PSW extends Bundle {
    val carry: Bool     = Bool()
    val zero: Bool      = Bool()
    val interrupt: Bool = Bool()
    val half: Bool      = Bool()
    val break: Bool     = Bool()
    val page: Bool      = Bool()
    val overflow: Bool  = Bool()
    val sign: Bool      = Bool()

    def get: UInt = Cat(sign, overflow, page, break, half, interrupt, zero, carry)
  }

  object PSW {
    def apply(): PSW = new PSW
  }

  object Addressing extends ChiselEnum {
    val AbsCalc,  AbsCall,  AbsJmp,  AbsRMW, AbsRMWBit, AbsIdxInd = Value
    val    AbsX,     AbsY,     Acc,  DpCalc,     DpRMW,    DpCMPW = Value
    val  DpWord,   DpINCW,    DpINC,   DpDp,     DpImm,    DpIndX,  DpIndY = Value
    val   DpRel,      DpX,  DpXRMW,  DpXRel,       DpY,      IndX = Value
    val IndXInc, IndXIndY,     Imm, BitMan,     Branch,    PSWMan = Value
    val RelDBNZ,  Special                                         = Value
  }


  object Ops extends ChiselEnum {
    val  ADD,  SUB,  MUL,  DIV,  MOV                             = Value
    val   OR,  AND,  EOR,  NOT,  CMP                             = Value
    val  ASL,  ROL,  LSR,  ROR,  DEC,  INC                       = Value
    val  CLR,  SET, CLRC, SETC, NOTC, CLRV, CLRP, SETP,  EI,  DI = Value
    val  DAA,  DAS,  XCN, TCLR, TSET                             = Value
    val  BPL,  BMI,  BVC,  BVS,  BCC,  BCS,  BNE,  BEQ, BBS, BBC = Value
    val CBNE, DBNZ                                               = Value
    val PUSH,  POP                                               = Value
    val  BRA,  JMP, CALL,TCALL,PCALL,  RET, RETI,  BRK           = Value
    val  NOP,SLEEP, STOP                                         = Value
  }

  object Instruction {
    def apply(): Instruction = new Instruction
  }
  class Instruction extends Bundle {
    val opcode = UInt(8.W)
    val ops = Ops()
    val addressing = Addressing()
  }
}
