package spc700.core

import chisel3._
import chisel3.util.switch
import chisel3.util.is
import chisel3.experimental.ChiselEnum

class Core extends Module {
  override val io = IO(new Bundle{
    val clk: Clock = Input(Clock())
    val rst: Reset = Input(Reset())

    // communicate to ram
    val ramWriteEn = Output(Bool())
    val ramReadData = Input(UInt(8.W))
    val ramReadAddr = Output(UInt(16.W))
    val ramWriteData = Output(UInt(8.W))
    val ramWriteAddr = Output(UInt(16.W))
  })

  val sleep = RegInit(Bool(), false.B)
  val regs = Reg(RegisterFile())
  val operand0 = Reg(UInt(16.W))
  val operand1 = Reg(UInt(16.W))


}

class Decoder extends Module {
  override val io = IO(new Bundle{
    val opcode = Input(UInt(8.W))
    val inst = Output(Operation())
  })

  io.inst.opcode := io.opcode
  switch(io.opcode) {
    is(0x00.U) { io.inst.ops :=   Ops.NOP; io.inst.addressing := Addressing.Special }
    is(0x01.U) { io.inst.ops := Ops.TCALL; io.inst.addressing := Addressing.Special }
  }
}

class RegisterFile extends Bundle {
  val pc: UInt = UInt(16.W)
  val sp: UInt = UInt(8.W)

  val a: UInt = UInt(8.W)
  val x: UInt = UInt(8.W)
  val y: UInt = UInt(8.W)
  val psw: UInt = UInt(8.W)
}

object RegisterFile {
  def apply(): RegisterFile = {
    new RegisterFile()
  }
}

object Addressing extends ChiselEnum {
  val AbsCalc,  AbsCall, AbsJump,  AbsRMW, AbsRMWBit, AbsIdxInd = Value
  val    AbsX,     AbsY,     Acc,  DpCalc,     DpRMW,    DpCMPW = Value
  val  DpWord,    DpINC,    DpDp,   DpImm,    DpIndX,    DpIndY = Value
  val   DpRel,      DpX,  DpXRMW,  DpXRel,       DpY,      IndX = Value
  val IndXInc, IndXIndY,     Imm, BitManA,   BitManB,    Branch = Value
  val RelDBNZ,  Special                                         = Value
}


object Ops extends ChiselEnum {
  val  ADD,  SUB,  MUL,  DIV                                   = Value
  val   OR,  AND,  EOR,  NOT,  CMP                             = Value
  val  ASL,  ROL,  LSR,  ROR,  DEC,  INC                       = Value
  val  CLR,  SET, CLRC, SETC, NOTC, CLRV, CLRP, SETP,  EI,  DI = Value
  val  DAA,  DAS,  XCN, TCLR, TSET                             = Value
  val  BPL,  BMI,  BVC,  BVS,  BCC,  BCS,  BNE,  BEQ, BBS, BBC = Value
  val CBNE, DBNZ                                               = Value
  val  BRA,  JMP, CALL,TCALL,PCALL,  RET, RETI,  BRK           = Value
  val  NOP,SLEEP, STOP                                         = Value
}

object Operation {
  def apply(): Operation = new Operation
}
class Operation extends Bundle {
  val opcode = UInt(8.W)
  val ops = Ops()
  val addressing = Addressing()
}


