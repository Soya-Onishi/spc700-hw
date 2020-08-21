package spc700.ram

import chisel3._
import chisel3.util.{switch, is, SwitchContext}

class MemController extends Module {
  val io = IO(new Bundle {
    val readEn = Input(Bool())
    val writeEn = Input(Bool())
    val addr = Input(UInt(16.W))
    val wdata = Input(UInt(8.W))
    val rdata = Output(UInt(8.W))

    val renewControl = Output(Bool())
    val timerEn = Output(Vec(3, Bool()))
    val romWritable = Output(Bool())

    val setDSPRegister = Output(Bool())
    val dspAddr = Output(UInt(8.W))
    val wDspData = Output(UInt(8.W))
    val rdspData = Input(UInt(8.W))

    val setTimerDivider = Output(Vec(3, Bool()))
    val newDivider = Output(Vec(3, UInt(8.W)))

    val readTimer = Output(Vec(3, Bool()))
    val timerOut = Input(Vec(3, UInt(8.W)))
  })

  val mem = Mem(64 * 1024, UInt(8.W))
  val rom = VecInit(Seq.fill(64)(0.U(8.W)))
  val dspAddr = Reg(UInt(8.W))

  io.renewControl := false.B
  io.timerEn := DontCare
  io.romWritable := DontCare

  io.setTimerDivider := Vec(3, false.B)
  io.newDivider := DontCare

  io.readTimer := Vec(3, false.B)

  io.setDSPRegister := false.B
  io.dspAddr := dspAddr
  io.wDspData := DontCare

  when(io.readEn) {
    val readIO = io.addr >= 0x00F1.U & io.addr <= 0x00FF.U
    val readROM = io.addr >= 0xFFC0.U

    when(readIO) {
      io.rdata := readFromIO()
    }.elsewhen(readROM) {
      io.rdata := rom(io.addr(5, 0))
    }.otherwise {
      io.rdata := mem.read(io.addr)
    }
  }

  when(io.writeEn) {
    val writeIO = io.addr >= 0x00F1.U & io.addr <= 0x00FF.U

    when(writeIO) {
      writeToIO()
    }

    mem.write(io.addr, io.wdata)
  }

  private def readFromIO(): UInt = {
    def readTimer(idx: Int): UInt = {
      io.readTimer(idx) := true.B
      io.timerOut(idx)
    }

    val ret = UInt(8.W)
    ret := 0.U
    switch(io.addr) {
      is(0x00F2.U) { ret := dspAddr }
      is(0x00F3.U) { ret := io.rdspData }
      is(0x00F8.U) { ret := mem.read(0x00F8.U) }
      is(0x00F9.U) { ret := mem.read(0x00F9.U) }
      is(0x00FA.U) { ret := mem.read(0x00FA.U) }
      is(0x00FB.U) { ret := mem.read(0x00FB.U) }
      is(0x00FC.U) { ret := mem.read(0x00FC.U) }
      is(0x00FD.U) { ret := readTimer(0) }
      is(0x00FE.U) { ret := readTimer(1) }
      is(0x00FE.U) { ret := readTimer(2) }
    }

    ret
  }

  private def writeToIO(): Unit = {
    def writeTimer(idx: Int): Unit = {
      io.setTimerDivider(idx) := true.B
      io.newDivider(idx) := io.wdata
    }

    def writeToDSP(): Unit = {
      io.setDSPRegister := true.B
      io.wDspData := io.wdata
    }

    switch(io.addr) {
      is(0x00F1.U) { /*not implemented*/ }
      is(0x00F2.U) { dspAddr := io.wdata }
      is(0x00F3.U) { writeToDSP() }
      is(0x00FA.U) { writeTimer(0) }
      is(0x00FB.U) { writeTimer(1) }
      is(0x00FC.U) { writeTimer(2) }
    }
  }
}
