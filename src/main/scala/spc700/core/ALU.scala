package spc700.core

import chisel3._
import chisel3.util.{switch, is}
import chisel3.util.Cat

class ALU extends Module {
  val io = IO(new Bundle {
    val ops = Input(Ops())
    val op0 = Input(UInt(8.W))
    val op1 = Input(UInt(8.W))
    val carryIn = Input(Bool())

    val carryEn = Output(Bool())
    val carryOut = Output(Bool())
    val halfEn = Output(Bool())
    val halfOut = Output(Bool())
    val overflowEn = Output(Bool())
    val overflowOut = Output(Bool())

    val out = Output(UInt(8.W))
  })

  io.carryEn     := false.B
  io.halfEn      := false.B
  io.overflowEn  := false.B
  io.carryOut    := DontCare
  io.halfOut     := DontCare
  io.overflowOut := DontCare
  io.out         := DontCare

  switch(io.ops) {
    is(Ops.ADD) { adc() }
    is(Ops.SUB) { sbc() }
    is(Ops.AND) { and() }
    is(Ops.OR)  { or() }
    is(Ops.EOR) { eor() }
    is(Ops.CMP) { cmp() }
    is(Ops.MOV) { mov() }
    is(Ops.ASL) { asl() }
    is(Ops.ROL) { rol() }
    is(Ops.LSR) { lsr() }
    is(Ops.ROR) { ror() }
    is(Ops.INC) { inc() }
    is(Ops.DEC) { dec() }
  }

  private def adc(): Unit = {
    val aLower = io.op0(3, 0)
    val aUpper = io.op0(7, 4)
    val bLower = io.op1(3, 0)
    val bUpper = io.op1(7, 4)

    val lower = aLower + bLower + io.carryIn.asUInt()
    val half = lower.head(1)
    val lowerRes = lower.tail(1)

    val upper = aUpper + bUpper + half
    val carry = upper.head(1)
    val upperRes = upper.tail(1)

    val a = io.op0(7)
    val b = io.op1(7)
    val r = upperRes.head(1)

    val overflow = !(a ^ b) & (a ^ r)

    setCarry(carry.toBool())
    setOverflow(overflow.toBool())
    setHalf(half.toBool())

    io.out := Cat(upperRes, lowerRes)
  }

  private def sbc(): Unit = {
    val aLower = io.op0(3, 0)
    val aUpper = io.op0(7, 4)
    val bLower = io.op1(3, 0)
    val bUpper = io.op1(7, 4)

    val lower = aLower - bLower - !io.carryIn.asUInt()
    val half = !lower.head(1)
    val lowerRes = lower.tail(1)

    val upper = aUpper - bUpper - half
    val carry = !upper.head(1)
    val upperRes = upper.tail(1)

    val a = io.op0(7)
    val b = io.op1(7)
    val r = upperRes.head(1)
    val overflow = (a ^ b) & (a ^ r)

    setCarry(carry.toBool())
    setOverflow(overflow.toBool())
    setHalf(half.toBool())

    io.out := Cat(upperRes, lowerRes)
  }

  private def cmp(): Unit = {
    val res = io.op0 - io.op1

    setCarry(res.head(1).toBool())
    io.out := io.op0
  }

  private def or(): Unit = {
    io.out := io.op0 | io.op1
  }

  private def and(): Unit = {
    io.out := io.op0 & io.op1
  }

  private def eor(): Unit = {
    io.out := io.op0 ^ io.op1
  }

  private def mov(): Unit = {
    io.out := io.op1
  }

  private def asl(): Unit = {
    setCarry(io.op0.head(1).toBool())
    io.out := Cat(io.op0.tail(1), 0.U(1))
  }

  private def rol(): Unit = {
    setCarry(io.op0.head(1).toBool())
    io.out := Cat(io.op0.tail(1), io.carryIn.asUInt())
  }

  private def lsr(): Unit = {
    setCarry(io.op0.head(1).toBool())
    io.out := Cat(0.U(1), io.op0(7, 1))
  }

  private def ror(): Unit = {
    setCarry(io.op0.head(1).toBool())
    io.out := Cat(io.carryIn.asUInt(), io.op0(7, 1))
  }

  private def inc(): Unit = {
    io.out := io.op0 + 1.U
  }

  private def dec(): Unit = {
    io.out := io.op0 - 1.U
  }

  private def setCarry(c: Bool): Unit = {
    io.carryEn := true.B
    io.carryOut := c
  }

  private def setHalf(h: Bool): Unit = {
    io.halfEn := true.B
    io.halfOut := h
  }

  private def setOverflow(v: Bool): Unit = {
    io.overflowEn := true.B
    io.overflowOut := v
  }
}
