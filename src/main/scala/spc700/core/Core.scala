package spc700.core

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util.Enum
import chisel3.util.{is, switch}

class Core extends Module {
  override val io = IO(new Bundle{
    // communicate to ram
    val ramWriteEn = Output(Bool())
    val ramReadData = Input(UInt(8.W))
    val ramReadAddr = Output(UInt(16.W))
    val ramWriteData = Output(UInt(8.W))
    val ramWriteAddr = Output(UInt(16.W))
  })

  val fetch :: decode :: exec :: sleep :: Nil = Enum(4)
  val state = RegInit(fetch)
  val regs = Reg(RegisterFile())
  val operand0 = Reg(UInt(16.W))
  val operand1 = Reg(UInt(16.W))

  val opcode = Reg(UInt(8.W))

  val decoder = Module(new Decoder)
  val byteALU = Module(new ALU)

  io.ramWriteEn := false.B
  io.ramReadAddr := 0.U
  io.ramWriteData := DontCare
  io.ramWriteAddr := DontCare

  decoder.io.opcode := DontCare

  switch(state) {
    is(fetch) { fetchState() }
    is(decode) { decodeState() }
    is(exec) {}
    is(sleep) { /* nothing to do */ }
  }

  private def fetchState(): Unit = {
    // fetch instruction
    io.ramReadAddr := regs.pc
    opcode := io.ramReadData

    regs.pc := regs.pc + 1.U
    state := decode
  }

  private def decodeState(): Unit = {
    // decode instruction
    decoder.io.opcode := opcode

    // fetch next data
    io.ramReadAddr := regs.pc
    val data = io.ramReadData

    val inst = decoder.io.inst

    switch(inst.addressing) {
      is(Addressing.Imm)    {
        when(inst.opcode === 0x8F.U) {

        } .otherwise {
          execALU(inst.ops, )
        }
      }
      is(Addressing.Acc)    {

      }
      is(Addressing.Branch) {

      }
    }
  }

  private def execImm(inst: Operation, imm: UInt): Unit = {
    val register = RegType()
    register := RegType.A
    switch(inst.opcode) {
      is(0xCD.U) { register := RegType.X }
      is(0x8D.U) { register := RegType.Y }
      is(0xC8.U) { register := RegType.X }
      is(0xAD.U) { register := RegType.Y }
      is(0xDC.U) { register := RegType.Y }
      is(0xFC.U) { register := RegType.Y }
      is(0x1D.U) { register := RegType.X }
      is(0x3D.U) { register := RegType.X }
    }

    val src = UInt(8.W)
    src := DontCare
    switch(register) {
      is(RegType.A) { src := regs.a }
      is(RegType.X) { src := regs.x }
      is(RegType.Y) { src := regs.y }
    }

    byteALU.io.ops := inst.ops
    byteALU.io.op0 := src
    byteALU.io.op1 := imm

    switch(register) {
      is(RegType.A) { regs.a := byteALU.io.out }
      is(RegType.X) { regs.x := byteALU.io.out }
      is(RegType.Y) { regs.y := byteALU.io.out }
    }

    when(byteALU.io.carryEn)    { regs.psw.carry := byteALU.io.carryOut }
    when(byteALU.io.halfEn)     { regs.psw.half  := byteALU.io.halfOut  }
    when(byteALU.io.overflowEn) { regs.psw.overflow := byteALU.io.overflowOut }
    regs.psw.zero := byteALU.io.out === 0.U
    regs.psw.sign := byteALU.io.out.head(1).toBool()
  }

  private def execALU(op: Ops.Type, op0: UInt, op1: UInt): Unit = {

  }

  private def regType0(opcode: UInt): RegType.Type = {
    val source = RegType()
    source := RegType.A
    switch(opcode) {
      is(0xCD.U) { source := RegType.X }
      is(0x8D.U) { source := RegType.Y }
      is(0x5D.U) { source := RegType.X }
      is(0xFD.U) { source := regs.y }
      is(0x9D.U) { source := regs.x }
      is(0xBD.U) { source := regs.sp}
      is(0xF8.U) { source := regs.x }
      is(0xF9.U) { source := regs.x }
      is(0xE9.U) { source := regs.x }
      is(0xEB.U) { source := regs.y }
      is(0xFB.U) { source := regs.y }
      is(0xEC.U) { source := regs.y }
      is(0xC8.U) { source := regs.x }
      is(0x3E.U) { source := regs.x }
      is(0x1E.U) { source := regs.x }
      is(0xAD.U) { source := regs.y }
      is(0x7E.U) { source := regs.y }
      is(0x5E.U) { source := regs.y }
      is(0xDC.U) { source := regs.y }
      is(0xFC.U) { source := regs.y }
      is(0x1D.U) { source := regs.x }
      is(0x3D.U) { source := regs.x }
    }

    source
  }

  private def opDst(opcode: UInt): Unit = {

  }
}

object RegType extends ChiselEnum {
  val A, X, Y, SP = Value
}


