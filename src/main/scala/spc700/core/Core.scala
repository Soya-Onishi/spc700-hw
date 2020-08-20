package spc700.core

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util.{Cat, Enum, is, switch, SwitchContext}

class Core extends Module {
  override val io = IO(new Bundle{
    // communicate to ram
    val ramWriteEn = Output(Bool())
    val ramReadData = Input(UInt(8.W))
    val ramReadAddr = Output(UInt(16.W))
    val ramWriteData = Output(UInt(8.W))
    val ramWriteAddr = Output(UInt(16.W))
  })

  val fetch :: decode :: exec :: jump :: sleep :: Nil = Enum(4)
  val globalState = RegInit(fetch)
  val regs = Reg(RegisterFile())
  val operand0 = Reg(UInt(16.W))
  val operand1 = Reg(UInt(16.W))

  val opcode = Reg(UInt(8.W))
  val inst = Reg(Instruction())
  val readData0 = Reg(UInt(8.W))

  val jmpOffset = Reg(SInt(16.W))

  val decoder = Module(new Decoder)
  val byteALU = Module(new ALU)

  io.ramWriteEn := false.B
  io.ramReadAddr := 0.U
  io.ramWriteData := DontCare
  io.ramWriteAddr := DontCare

  decoder.io.opcode := DontCare

  switch(globalState) {
    is(fetch) { fetchState() }
    is(decode) { decodeState() }
    is(exec) { }
    is(jump) { jumpState() }
    is(sleep) { /* nothing to do */ }
  }

  private def fetchState(): Unit = {
    // fetch instruction
    io.ramReadAddr := regs.pc
    opcode := io.ramReadData

    regs.pc := regs.pc + 1.U
    globalState := decode
  }

  private def decodeState(): Unit = {
    // decode instruction
    decoder.io.opcode := opcode

    // fetch next data
    io.ramReadAddr := regs.pc
    val data = io.ramReadData

    val inst = decoder.io.inst
    this.inst := decoder.io.inst

    when(inst.addressing === Addressing.Imm) {
      execImm(inst, data)
    }.elsewhen(inst.addressing === Addressing.Acc) {
      execAcc(inst)
    }.elsewhen(inst.addressing === Addressing.Branch) {
      execBranchByPSW(inst, data)
    }.elsewhen(inst.addressing === Addressing.PSWMan) {
      manPSWDecode(inst.ops)
    }.otherwise{
      regs.pc := regs.pc + 1.U
      readData0 := data
      globalState := exec
    }
  }

  private def execImm(inst: Instruction, imm: UInt): Unit = {
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

    val out = runByteALU(inst.ops, src, imm)
    switch(register) {
      is(RegType.A) { regs.a := out }
      is(RegType.X) { regs.x := out }
      is(RegType.Y) { regs.y := out }
    }

    // next instruction
    regs.pc := regs.pc + 1.U
    globalState := fetch
  }

  private def execAcc(inst: Instruction): Unit = {
    val src = RegType()
    val dst = RegType()
    src := RegType.A
    dst := RegType.A
    switch(inst.opcode) {
      is(0xDC.U) { src := RegType.Y; dst := RegType.Y }
      is(0xFC.U) { src := RegType.Y; dst := RegType.Y }
      is(0x1D.U) { src := RegType.X; dst := RegType.X }
      is(0x3D.U) { src := RegType.X; dst := RegType.X }
      is(0x5D.U) {                   dst := RegType.X }
      is(0x7D.U) { src := RegType.X                   }
      is(0x9D.U) { src := RegType.SP;dst := RegType.X }
      is(0xBD.U) { src := RegType.X; dst := RegType.SP}
      is(0xDD.U) { src := RegType.Y                   }
      is(0xFD.U) {                   dst := RegType.Y }
    }

    val operand = UInt(8.W)
    operand := regs.a
    switch(src) {
      is(RegType.X) { operand := regs.x }
      is(RegType.Y) { operand := regs.y }
      is(RegType.SP){ operand := regs.sp}
    }

    val out = runByteALU(inst.ops, operand, operand)
    switch(dst) {
      is( RegType.A) {  regs.a := out }
      is( RegType.X) {  regs.x := out }
      is( RegType.Y) {  regs.y := out }
      is(RegType.SP) { regs.sp := out }
    }

    globalState := fetch
  }

  private def execBranchByPSW(inst: Instruction, offset: UInt): Unit = {
    val cond =  Bool()
    cond := false.B
    switch(inst.ops) {
      is(Ops.BPL) { cond := !regs.psw.sign }
      is(Ops.BMI) { cond :=  regs.psw.sign }
      is(Ops.BVC) { cond := !regs.psw.overflow }
      is(Ops.BVS) { cond :=  regs.psw.overflow }
      is(Ops.BCC) { cond := !regs.psw.carry }
      is(Ops.BCS) { cond :=  regs.psw.carry }
      is(Ops.BNE) { cond := !regs.psw.zero }
      is(Ops.BEQ) { cond :=  regs.psw.zero }
      is(Ops.BRA) { cond := true.B }
    }

    regs.pc := regs.pc + 1.U
    when(cond) {
      jmpOffset := offset.asSInt()
      globalState := jump
    } .otherwise {
      globalState := fetch
    }
  }

  private def manPSWDecode(ops: Ops.Type): Unit = {
    switch(ops) {
      is(Ops.CLRP) { regs.psw.page  := false.B }
      is(Ops.SETP) { regs.psw.page  := true.B  }
      is(Ops.CLRC) { regs.psw.carry := false.B }
      is(Ops.SETC) { regs.psw.carry := true.B  }
      is(Ops.NOTC) { regs.psw.carry := !regs.psw.carry }
      is(Ops.EI)   { regs.psw.interrupt := true.B }
      is(Ops.DI)   { regs.psw.interrupt := false.B }
      is(Ops.CLRV) { regs.psw.overflow := false.B; regs.psw.half := false.B }
    }

    when(ops === Ops.EI | ops === Ops.DI | ops === Ops.NOTC) {
      globalState := exec
    } .otherwise {
      globalState := fetch
    }
  }

  private def manPSWExec(): Unit = {
    regs.pc := regs.pc + 1.U
    globalState := fetch
  }

  private def runAbsArith(): Unit = {
    val readAbsH :: readOperand :: writeRam :: Nil = Enum(3)
    val absArithState = RegInit(readAbsH)
    val absL = readData0
    val absH = Reg(UInt(8.W))
    val op0Reg = RegType()
    val op0 = UInt(8.W)
    val op1 = UInt(8.W)
    val stored = Reg(UInt(8.W))
    val isStore = Bool()
    val opcode = this.inst.opcode

    op0Reg := RegType.A
    switch(opcode) {
      is(0xE9.U) { op0Reg := RegType.X }
      is(0xEC.U) { op0Reg := RegType.Y }
      is(0xC9.U) { op0Reg := RegType.X }
      is(0xCC.U) { op0Reg := RegType.Y }
      is(0x1E.U) { op0Reg := RegType.X }
      is(0x5E.U) { op0Reg := RegType.Y }
    }

    op0 := regs.a
    op1 := DontCare
    switch(op0Reg) {
      is(RegType.X) { op0 := regs.x }
      is(RegType.Y) { op0 := regs.y }
    }

    isStore := opcode === 0xC5.U | opcode === 0xC9.U | opcode === 0xCC.U

    switch(absArithState) {
      is(readAbsH) {
        io.ramReadAddr := regs.pc
        absH := io.ramReadData
        regs.pc := regs.pc + 1.U

        absArithState := readOperand
      }
      is(readOperand) {
        io.ramReadAddr := Cat(absH, absL)
        op1 := io.ramReadData

        when(isStore) {
          stored := op0
          absArithState := writeRam
        } .otherwise {
          val out = runByteALU(this.inst.ops, op0, op1)
          switch(op0Reg) {
            is(RegType.A) { regs.a := out }
            is(RegType.X) { regs.x := out }
            is(RegType.Y) { regs.y := out }
          }

          globalState := fetch
          absArithState := readAbsH
        }
      }
      is(writeRam) {
        io.ramWriteEn   := true.B
        io.ramWriteAddr := Cat(absH, absL)
        io.ramWriteData := stored

        globalState := fetch
        absArithState := readAbsH
      }
    }
  }

  private def runAbsCall(): Unit = {
    val readNewPCH :: none0 :: pushPCH :: pushPCL :: none1 :: renewPC :: Nil = Enum(6)
    val newPCL = readData0
    val newPCH = Reg(UInt(8.W))
    val absCallState = RegInit(readNewPCH)

    switch(absCallState) {
      is(readNewPCH) {
        io.ramReadAddr := regs.pc

        regs.pc := regs.pc + 1.U
        newPCH := io.ramReadData
        absCallState := none0
      }
      is(none0) {
        absCallState := pushPCH
      }
      is(pushPCH) {
        val pcUpper = regs.pc(15, 8)
        pushStack(pcUpper)
        absCallState := pushPCL
      }
      is(pushPCL) {
        val pcLower = regs.pc(7, 0)
        pushStack(pcLower)
        absCallState := none1
      }
      is(none1) {
        absCallState := renewPC
      }
      is(renewPC) {
        regs.pc := Cat(newPCH, newPCL)

        absCallState := readNewPCH
        globalState := fetch
      }
    }
  }

  private def runAbsJmp(): Unit = {
    io.ramReadAddr := regs.pc
    val newPCL = readData0
    val newPCH = io.ramReadData

    regs.pc := Cat(newPCH, newPCL)
    globalState := fetch
  }

  private def runAbsRMW(): Unit = {
    val fetchAddrH :: fetchData :: storeResult ::Nil = Enum(3)
    val addrL = readData0
    val addrH = Reg(UInt(8.W))
    val result = Reg(UInt(8.W))
    val absRMWState = RegInit(fetchAddrH)

    switch(absRMWState) {
      is(fetchAddrH) {
        addrH := readAbs(regs.pc)
        regs.pc := regs.pc + 1.U
        absRMWState := fetchData
      }
      is(fetchData) {
        val readData = readAbs(Cat(addrH, addrL))

        result := runByteALU(this.inst.ops, readData, readData)
        absRMWState := storeResult
      }
      is(storeResult) {
        writeAbs(Cat(addrH, addrL), result)

        absRMWState := fetchAddrH
        globalState := fetch
      }
    }
  }

  private def runAbsRMWBit(): Unit = {
    val fetchAddrH :: addX :: fetchPCL :: fetchPCH :: Nil = Enum(4)
    val addrL = readData0
    val addrH = Reg(UInt(8.W))
    val newPCL = Reg(UInt(8.W))
    val fetchAddr = Reg(UInt(16.W))
    val absRMWBitState = RegInit(fetchAddrH)

    switch(absRMWBitState) {
      is(fetchAddrH) {
        addrH := readAbs(regs.pc)
        regs.pc := regs.pc + 1.U
        absRMWBitState := addX
      }
      is(addX) {
        fetchAddr := Cat(addrH, addrL) + regs.x
        absRMWBitState := fetchPCL
      }
      is(fetchPCL) {
        newPCL := readAbs(fetchAddr)
        fetchAddr := fetchAddr + 1.U
        absRMWBitState := fetchPCH
      }
      is(fetchPCH) {
        val newPCH = readAbs(fetchAddr)
        regs.pc := Cat(newPCH, newPCL)

        absRMWBitState := fetchAddrH
        globalState := fetch
      }
    }
  }

  private def jumpState(): Unit = {
    val jump0 :: jump1 :: Nil = Enum(2)
    val jmpState = RegInit(jump0)

    switch(jmpState) {
      is(jump0) { jmpState := jump1 }
      is(jump1) {
        jmpState := jump0
        globalState := fetch
        regs.pc := regs.pc.asSInt() + jmpOffset
      }
    }
  }

  private def runByteALU(op: Ops.Type, op0: UInt, op1: UInt): UInt = {
    byteALU.io.ops := op
    byteALU.io.op0 := op0
    byteALU.io.op1 := op1

    when(byteALU.io.carryEn)    { regs.psw.carry := byteALU.io.carryOut }
    when(byteALU.io.halfEn)     { regs.psw.half  := byteALU.io.halfOut  }
    when(byteALU.io.overflowEn) { regs.psw.overflow := byteALU.io.overflowOut }
    regs.psw.zero := byteALU.io.out === 0.U
    regs.psw.sign := byteALU.io.out.head(1).toBool()

    byteALU.io.out
  }

  private def readAbs(abs: UInt): UInt = {
    io.ramReadAddr := abs

    io.ramReadData
  }

  private def writeAbs(abs: UInt, data: UInt): Unit = {
    io.ramWriteEn   := true.B
    io.ramWriteAddr := abs
    io.ramWriteData := data
  }

  private def readDp(dpAddr: UInt): UInt = {
    io.ramReadAddr := Cat(regs.psw.page, dpAddr)

    io.ramReadData
  }

  private def writeDp(dpAddr: UInt, data: UInt): Unit = {
    io.ramWriteEn := true.B
    io.ramWriteAddr := Cat(regs.psw.page, dpAddr)
    io.ramWriteData := data
  }

  private def pushStack(data: UInt): Unit = {
    io.ramWriteEn := true.B
    io.ramWriteAddr := Cat(1.U, regs.sp)
    io.ramWriteData := data

    regs.sp := regs.sp - 1.U
  }

  private def popStack(): UInt = {
    regs.sp := regs.sp + 1.U
    io.ramReadAddr := Cat(1.U, regs.sp + 1.U)

    io.ramReadData
  }
}

object RegType extends ChiselEnum {
  val A, X, Y, SP = Value
}


