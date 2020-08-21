package spc700.dsp

import chisel3._
import chisel3.util.{switch, is, SwitchContext}

class DSP extends Module {
  val io = IO(new Bundle {
    val readAddr = Output(Vec(2, UInt(16.W)))
    val readData = Input(Vec(2, UInt(8.W)))

    val readDSPAddr = Input(UInt(8.W))
    val readDSPData = Output(UInt(8.W))
  })

  val counter = RegInit(0.U(5.W))
  val regArray = Mem(128, UInt(8.W))

  counter := counter + 1.U
  switch(counter) {
    is( 0.U) {}
    is( 1.U) {}
    is( 2.U) {}
    is( 3.U) {}
    is( 4.U) {}
    is( 5.U) {}
    is( 6.U) {}
    is( 7.U) {}
    is( 8.U) {}
    is( 9.U) {}
    is(10.U) {}
    is(11.U) {}
    is(12.U) {}
    is(13.U) {}
    is(14.U) {}
    is(15.U) {}
    is(16.U) {}
    is(17.U) {}
    is(18.U) {}
    is(19.U) {}
    is(20.U) {}
    is(21.U) {}
    is(22.U) {}
    is(23.U) {}
    is(24.U) {}
    is(25.U) {}
    is(26.U) {}
    is(27.U) {}
    is(28.U) {}
    is(29.U) {}
    is(30.U) {}
    is(31.U) {}
  }

  private def cycle0(): Unit = {

  }
}
