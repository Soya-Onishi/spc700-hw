package spc700.dsp

import chisel3._
import chisel3.util.{MuxLookup, Cat}
import chisel3.experimental.ChiselEnum

class DSPBlock extends Module {
  val io = IO(new Bundle {
    val beforeOut = Input(SInt(16.W))

    val brrEn = Input(Bool())
    val brrData = Input(UInt(8.W))
    val brrAddr = Output(UInt(16.W))
  })

  val srcn = Reg(UInt(8.W))
  val pitch = Reg(UInt(14.W))
  val pmon = Reg(Bool())
  val adsr = Reg(ADSRSettings())
  val gain = Reg(GainSettings())
  val leftVol = Reg(SInt(8.W))
  val rightVol = Reg(SInt(8.W))

  val brrHeader = Reg(BRRHeader())
  val brrBuffer = Reg(Vec(16, SInt(8.W)))
  val brrIdx = Reg(UInt(4.W))

  val pitchCounter = Reg(UInt(16.W))
  val requireNext = Bool()
  val srcAddr = Reg(UInt(16.W))
  val envelope = Reg(SInt(16.W))
  val adsrMode = RegInit(ADSRMode.Release)


  io.brrAddr := srcAddr
  when(io.brrEn) {
    when(brrIdx === 0.U) {
      brrHeader.set(io.brrData)
    }.otherwise {
      val lower = io.brrData(3, 0).asSInt()
      val upper = io.brrData(7, 4).asSInt()
      val lowerIdx = Cat(brrIdx - 1.U, 0.U(1.W))
      val upperIdx = Cat(brrIdx - 1.U, 1.U(1.W))

      brrBuffer(lowerIdx) := lower
      brrBuffer(upperIdx) := upper
    }

    brrIdx  := Mux(brrIdx === 8.U, 0.U, brrIdx + 1.U)
    srcAddr := srcAddr + 1.U
  }

}

class BRRHeader extends Bundle {
  val shiftAmount = UInt(4.W)
  val filter = FilterMode()
  val loop = LoopFlag()

  def set(byte: UInt): Unit = {
    this.shiftAmount := byte(7, 4)
    this.filter := MuxLookup(byte(3, 2), FilterMode.NoFilter, Seq(
      1.U -> FilterMode.UseOld,
      2.U -> FilterMode.UseAll0,
      3.U -> FilterMode.UseAll1,
    ))
    this.loop := MuxLookup(byte(1, 0), LoopFlag.Normal, Seq(
      1.U -> LoopFlag.Mute,
      3.U -> LoopFlag.Loop,
    ))
  }
}

object BRRHeader {
  def apply(): BRRHeader = new BRRHeader
}

object FilterMode extends ChiselEnum {
  val NoFilter, UseOld, UseAll0, UseAll1 = Value
}

object LoopFlag extends ChiselEnum {
  val Normal, Mute, Loop = Value
}

class ADSRSettings extends Bundle {
  val isADSRMode = Bool()
  val attack = UInt(4.W)
  val decay = UInt(3.W)
  val sustain = UInt(5.W)
  val boundary = UInt(3.W)
}

object ADSRSettings {
  def apply() = new ADSRSettings
}

class GainSettings extends Bundle {
  val isCustom = Bool()
  val remains = UInt(7.W)
}

object GainSettings {
  def apply() = new GainSettings
}

object ADSRMode extends ChiselEnum {
  val Attack, Decay, Sustain, Release = Value
}