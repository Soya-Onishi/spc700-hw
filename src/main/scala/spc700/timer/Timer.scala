package spc700.timer

import chisel3._

class Timer(hz: Int) extends Module {
  val io = IO(new Bundle{
    val initEnable = Input(Bool())
    val initDivider = Input(UInt(8.W))

    val makeEnable  = Input(Bool())
    val makeDisable = Input(Bool())

    val readEn = Input(Bool())
    val writeEn = Input(Bool())

    val out = Output(UInt(8.W))
    val divider = Input(UInt(8.W))
  })

  val maxCount = hz match {
    case 8000 => 256
    case 64000 => 32
  }

  val enable = RegInit(io.initEnable)
  val cycleCount = RegInit(0.U)
  val divided = RegInit(io.initDivider)
  val divider = Reg(UInt(8.W))
  val count = RegInit(0.U(4.W))

  io.out := count

  when(enable) {
    cycleCount := cycleCount + 1.U
    when(cycleCount === (maxCount - 1).U) {
      cycleCount := 0.U
      divided := divided + 1.U

      when(divided + 1.U === divider) {
        count := count + 1.U
        divided := 0.U
      }
    }
  }

  when(io.makeEnable) {
    enable := true.B
    divided := 0.U
    cycleCount := 0.U
  }

  when(io.makeDisable) {
    enable := false.B
    count := 0.U
  }

  when(io.readEn) {
    count := 0.U
  }

  when(io.writeEn) {
    // In Fullsnes, divider is reloaded when making timer enable,
    // so this implementation may cause problems.
    divider := io.divider
  }
}
