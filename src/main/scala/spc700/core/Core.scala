package spc700.core

import chisel3._

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

  val regs = Reg(RegisterFile())
  val operand0 = Reg(UInt(16.W))
  val operand1 = Reg(UInt(16.W))

  val AbsCalc :: AbsCall :: AbsJump :: AbsRMW :: AbsRMWBit :: AbsIndexInd :: AbsX :: AbsY

}

class Decoder extends Module {
  override val io = IO(new Bundle{
    val opcode = Input(UInt(8.W))
    val inst =
  })
}

class RegisterFile extends Bundle {
  val pc = UInt(16.W)
  val sp = UInt(8.W)

  val a = UInt(8.W)
  val x = UInt(8.W)
  val y = UInt(8.W)
  val psw = UInt(8.W)
}

object RegisterFile {
  def apply(): RegisterFile = {
    new RegisterFile()
  }
}


