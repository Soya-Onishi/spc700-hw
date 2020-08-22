package spc700.dsp

import chisel3._
import chisel3.util.{Cat, MuxCase, MuxLookup, SwitchContext, is, switch}
import chisel3.experimental.ChiselEnum

class DSPBlock(val idx: Int) extends Module {
  val io = IO(new Bundle {
    val beforeOut = Input(SInt(16.W))
    val leftOut   = Output(SInt(16.W))
    val rightOut  = Output(SInt(16.W))

    val brrEn = Input(Bool())
    val brrData = Input(UInt(8.W))
    val brrAddr = Output(UInt(16.W))

    val keyedOn = Input(Bool())
    val sampleClock = Input(Bool())

    val gaussianIdx = Output(UInt(8.W))
    val gaussianFilter = Input(Vec(4, UInt(12.W)))
  })

  val dir  = Reg(UInt(8.W))
  val srcn = Reg(UInt(8.W))
  val pitch = Reg(UInt(14.W))
  val pmon = Reg(Bool())
  val adsr = Reg(ADSRSettings())
  val gain = Reg(GainSettings())
  val leftVol = Reg(SInt(8.W))
  val rightVol = Reg(SInt(8.W))

  val brrHeader = Reg(BRRHeader())
  val brrBuffer = Reg(Vec(16, SInt(16.W)))
  val brrIdx = Reg(UInt(4.W))

  val pitchCounter = Reg(UInt(16.W))
  val baseSrcAddr = dir * 0x100.U + srcn * 4.U
  val srcAddr = Reg(UInt(16.W))
  val adsrMode = RegInit(ADSRMode.Release)
  val keyOnCounter = Reg(UInt(3.W))
  val fetchable = RegInit(true.B)
  val envelope = Reg(SInt(16.W))
  val envCounter = Reg(UInt(12.W))

  //                //
  // DSP operations //
  //                //

  // apply gaussian interpolation filter
  val sampleIdx = pitchCounter.head(4)
  val gaussianIdx = pitchCounter.tail(4)
  io.gaussianIdx := gaussianIdx
  val sample = applyGaussianFilter(sampleIdx)

  // apply envelope
  val envelopedSample = sample * envelope

  // apply left and right volume
  val leftSample = leftVol * envelopedSample
  val rightSample = rightVol * envelopedSample

  // output new sample
  io.leftOut  := leftSample.head(16).asSInt()
  io.rightOut := rightSample.head(16).asSInt()

  // fetch new BRR sample/header
  io.brrAddr := srcAddr
  when(io.brrEn & fetchable) {
    when(brrIdx === 0.U) {
      brrHeader.set(io.brrData)
    }.otherwise {
      val first  = io.brrData(7, 4).asSInt()
      val second = io.brrData(3, 0).asSInt()
      val baseIdx   = (brrIdx - 1.U)(2, 0)
      val firstIdx  = Cat(baseIdx, 0.U(1.W))
      val secondIdx = Cat(baseIdx, 1.U(1.W))

      val oldIdx   = (firstIdx - 1.U)(3, 0)
      val olderIdx = (firstIdx - 2.U)(3, 0)
      val old   = brrBuffer(oldIdx)
      val older = brrBuffer(olderIdx)

      val filteredFirst  = filterSample(first, old, older)
      val filteredSecond = filterSample(second, filteredFirst, old)

      brrBuffer(firstIdx)  := filteredFirst
      brrBuffer(secondIdx) := filteredSecond
    }

    when(brrIdx === 8.U) {
      fetchable := false.B
    }

    brrIdx  := Mux(brrIdx === 8.U, 0.U, brrIdx + 1.U)
    srcAddr := srcAddr + 1.U
  }

  // when key on, src address are set by (DIR * 0x100 + SRCN * 4)
  // and after key on, this dsp are muted in force until 5 cycles
  when(io.keyedOn) {
    srcAddr := baseSrcAddr
    keyOnCounter := 4.U
  }

  when(io.sampleClock) {
    keyOnCounter := Mux(keyOnCounter === 0.U, 0.U, keyOnCounter - 1.U)
    val additionalPitch = getAdditionalPitch()
    pitchCounter := pitchCounter + additionalPitch

    val requireNext = (pitchCounter + additionalPitch).head(1).toBool()
    when(requireNext) {
      fetchable := true.B
    }
  }

  private def filterSample(sample: SInt, old: SInt, older: SInt): SInt = {
    val ret = SInt(16.W)
    ret := MuxLookup(brrHeader.filter.asUInt(), sample, Seq(
      FilterMode.UseOld.asUInt() -> {
        val oldFilter = old + Cat(-old, 0.S(4.W)).asSInt()

        sample + oldFilter
      },
      FilterMode.UseAll0.asUInt() -> {
        val oldFilter   = old * 2.S + Cat(-old * 3.S, 0.S(5.W)).asSInt()
        val olderFilter = -older + Cat(older, 0.S(4.W)).asSInt()

        sample + oldFilter + olderFilter
      },
      FilterMode.UseAll1.asUInt() -> {
        val oldFilter   = old * 2.S + Cat(-old * 13.S, 0.S(6.W)).asSInt()
        val olderFilter = -older + Cat(older * 3.S, 0.S(4.W)).asSInt()

        sample + oldFilter + olderFilter
      },
    ))

    ret
  }

  private def getAdditionalPitch(): UInt = {
    val ret = UInt(16.W)
    val additional = idx match {
      case 0 => pitch
      case _ => Mux(!pmon, pitch, {
        val factor = (io.beforeOut >> 4) + 0x400.S
        val tmp = (pitch.asSInt() * factor) >> 10
        val ret0 = UInt(16.W)
        ret0 := MuxCase(tmp.asSInt(), Seq(
          (tmp > 0x3FFF.S) -> { 0x3FFF.U },
          (tmp < 0.S)      -> {    0x0.U },
        ))

        ret0
      })
    }
    ret := additional
    ret
  }

  private def applyGaussianFilter(sampleIdx: UInt): SInt = {
    val oldest = brrBuffer((sampleIdx - 3.U) & 7.U)
    val older  = brrBuffer((sampleIdx - 2.U) & 7.U)
    val old    = brrBuffer((sampleIdx - 1.U) & 7.U)
    val sample = brrBuffer(sampleIdx)

    val factor0 = ((oldest * io.gaussianFilter(3).asSInt()) >> 10).asSInt()
    val factor1 = (( older * io.gaussianFilter(2).asSInt()) >> 10).asSInt()
    val factor2 = ((   old * io.gaussianFilter(1).asSInt()) >> 10).asSInt()
    val factor3 = ((sample * io.gaussianFilter(0).asSInt()) >> 10).asSInt()

    val tmp = factor0 + factor1 + factor2 + factor3
    val out = SInt(16.W)
    out := MuxCase(tmp, Seq(
      (tmp >  0x7FFF.S) ->  0x7FFF.S,
      (tmp < -0x8000.S) -> -0x8000.S,
    ))

    out
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