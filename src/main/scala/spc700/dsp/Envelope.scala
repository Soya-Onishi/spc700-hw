package spc700.dsp

import chisel3._
import chisel3.util.{Cat, MuxCase, MuxLookup, is, switch, SwitchContext}

class Envelope extends Module {
  val io = IO(new Bundle {
    val renewEnvelope = Input(Bool())
    val adsrSettings = Input(ADSRSettings())
    val gainSettings = Input(GainSettings())

    val envelope = Output(SInt(16.W))
  })

  val rateTable = VecInit(Seq(
       0, 2048, 1536, 1280,
    1024,  768,  640,  512,
     384,  320,  256,  192,
     160,  128,   96,   80,
      64,   48,   40,   32,
      24,   20,   16,   12,
      10,    8,    6,    5,
       4,    3,    2,    1,
  ).map(_.U))

  val counterOffset = VecInit(Seq(
      1, 0, 1040,
    536, 0, 1040,
    536, 0, 1040,
    536, 0, 1040,
    536, 0, 1040,
    536, 0, 1040,
    536, 0, 1040,
    536, 0, 1040,
    536, 0, 1040,
    536, 0, 1040,
    0,
    0
  ).map(_.U))

  val level = Reg(SInt(16.W))
  val adsrMode = RegInit(ADSRMode.Release)
  val counter = RegInit(0.U(12.W))
  io.envelope := level

  when(io.renewEnvelope) {
    val isADSR = io.adsrSettings.isADSRMode
    val isRelease = adsrMode === ADSRMode.Release
    val rate = UInt(5.W)
    val step = SInt(16.W)
    val immediate = Bool()

    immediate := false.B
    when(isADSR | isRelease) {
      val (r, s) = runADSRMode()
      rate := r
      step := s
    }.otherwise {
      val (r, s, imm) = runGainMode()
      rate := r
      step := s
      immediate := imm
    }

    val clippedLevel = clipLevel(step)
    val newMode = renewState(clippedLevel, rate, immediate)
    val newLevel = renewLevel(clippedLevel, rate, immediate)

    level := newLevel
    adsrMode := newMode
  }

  private def runADSRMode(): (UInt, SInt) = {
    val rate = UInt(5.W)
    val step = SInt(16.W)

    rate := DontCare
    step := DontCare
    switch(adsrMode) {
      is(ADSRMode.Attack) {
        val attackRate = Cat(io.adsrSettings.attack, 0.U(1.W))
        rate := attackRate
        step := Mux(attackRate === 31.U, 1024.S, 32.S)
      }
      is(ADSRMode.Decay) {
        rate := Cat(io.adsrSettings.decay, 0.U(2.W))
        step := -(((level - 1.S) >> 8).asSInt() + 1.S)
      }
      is(ADSRMode.Sustain) {
        rate := io.adsrSettings.sustain
        step := -(((level - 1.S) >> 8).asSInt() + 1.S)
      }
      is(ADSRMode.Release) {
        rate := 31.U
        step := -8.S
      }
    }

    (rate, step)
  }

  private def runGainMode(): (UInt, SInt, Bool) = {
    val rate = UInt(5.W)
    val step = SInt(16.W)
    val immediate = Bool()

    when(io.gainSettings.isCustom) {
      val gainMode = io.gainSettings.remains.head(2)
      val gainRate = io.gainSettings.remains.tail(2)

      immediate := false.B
      rate := gainRate
      step := MuxLookup(gainMode, -32.S, Seq(
        1.U -> -(((level - 1.S) >> 8).asSInt + 1.S),
        2.U -> 32.S,
        3.U -> Mux(level < 600.S, 32.S, 8.S)
      ))
    } .otherwise {
      immediate := true.B
      rate := 0.U
      step := io.gainSettings.remains.asSInt() * 16.S
    }

    (rate, step, immediate)
  }

  private def clipLevel(step: SInt): SInt = {
    val ret = SInt(16.W)
    val newLevel = level + step

    ret := MuxCase(newLevel, Seq(
      (newLevel > 0x800.S) -> 0x7FF.S,
      (newLevel <     0.S) ->     0.S,
    ))

    ret
  }

  private def renewState(newLevel: SInt, rate: UInt, immediate: Bool): ADSRMode.Type = {
    val boundary = io.adsrSettings.boundary + 1.U * 0x100.U
    val isReachDecay    = newLevel >= 0x7E0.S
    val isReachBoundary = newLevel <= boundary.asSInt()

    MuxCase(adsrMode, Seq(
      (   isReachDecay & adsrMode === ADSRMode.Attack) -> ADSRMode.Decay,
      (isReachBoundary & adsrMode ===  ADSRMode.Decay) -> ADSRMode.Sustain,
    ))
  }

  private def renewLevel(newLevel: SInt, rate: UInt, immediate: Bool): SInt = {
    // I don't know whether this implementation is correct or not
    val isRenew = (counter + counterOffset(rate)) === rateTable(rate) | immediate

    when(isRenew) {
      counter := 0.U
    }

    Mux(isRenew, level, newLevel)
  }
}