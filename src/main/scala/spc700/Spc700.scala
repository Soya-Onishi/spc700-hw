package spc700

import chisel3._
import spc700.core.{Core, RegisterFile}
import spc700.ram.MemController
import spc700.timer.Timer

class Spc700 extends Module {
  val io = IO(new Bundle {
    val initReg = Input(RegisterFile())
    val initTimerEn = Input(Vec(3, Bool()))
    val initTimerDivider = Input(Vec(3, UInt(8.W)))
  })

  val core = Module(new Core)
  val mcu  = Module(new MemController)
  val timers = Vector(Module(new Timer(8000)), Module(new Timer(8000)), Module(new Timer(64000)))

  // connection between core and ram
  mcu.io.readEn       := core.io.ramReadEn
  mcu.io.writeEn      := core.io.ramWriteEn
  mcu.io.addr         := core.io.ramAddr
  mcu.io.wdata        := core.io.ramWriteData
  core.io.ramReadData := mcu.io.rdata

  // connection between timers and ram
  timers.zip(mcu.io.timerEn).foreach{ case (timer, en) => timer.io.makeEnable  := mcu.io.renewControl & en}
  timers.zip(mcu.io.timerEn).foreach{ case (timer, en) => timer.io.makeDisable := mcu.io.renewControl & !en}
  timers.zip(mcu.io.setTimerDivider).foreach{ case (timer, en) => timer.io.writeEn := en }
  timers.zip(mcu.io.newDivider).foreach{ case (timer, div) => timer.io.divider := div }
  timers.zip(mcu.io.readTimer).foreach{ case (timer, en) => timer.io.readEn := en }
  timers.zip(mcu.io.timerOut).foreach{ case (timer, out) => out := timer.io.out }

  // For now, DSP is not instantiated,
  // so ports related to DSP are DontCare
  mcu.io.addrFromDSP := DontCare
  mcu.io.rdspData := DontCare

  // initialization
  core.io.regInit := io.initReg
  timers.zip(io.initTimerEn).foreach{ case (timer, en) => timer.io.initEnable := en }
  timers.zip(io.initTimerDivider).foreach{ case (timer, div) => timer.io.initDivider := div }
}
