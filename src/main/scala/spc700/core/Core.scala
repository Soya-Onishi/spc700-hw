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

  // run Absolute + Index addressing mode
  // Abs+X or Abs+Y
  // idx is regs.x or regs.y
  private def runAbsIndex(idx: UInt): Unit = {
    val fetchAbsH :: addIndex :: fetchData :: storeResult :: Nil = Enum(4)
    val absL = readData0
    val absH = Reg(UInt(8.W))
    val addr = Reg(UInt(16.W))
    val absXState = RegInit(fetchAbsH)
    val isStore = inst.opcode === 0xD5.U | inst.opcode === 0xD6.U

    switch(absXState) {
      is(fetchAbsH) {
        absH := readAbs(regs.pc)
        regs.pc := regs.pc + 1.U
        absXState := addIndex
      }
      is(addIndex) {
        addr := Cat(absH, absL) + idx
        absXState := fetchData
      }
      is(fetchData) {
        val op1 = readAbs(addr)
        when(isStore) {
          absXState := storeResult
        }.otherwise {
          val out = runByteALU(inst.ops, regs.a, op1)
          regs.a := out

          absXState := fetchAbsH
          globalState := fetch
        }
      }
      is(storeResult) {
        writeAbs(addr, regs.a)

        absXState := fetchAbsH
        globalState := fetch
      }
    }
  }

  private def runDpArith(): Unit = {
    val fetchData :: storeResult :: Nil = Enum(2)
    val regType = RegType()
    val dpArithState = RegInit(fetchData)
    val isStore = inst.opcode === 0xC4.U | inst.opcode === 0xD8.U | inst.opcode === 0xCB.U

    regType := RegType.A
    switch(inst.opcode) {
      is(0xF8.U) { regType := RegType.X }
      is(0xEB.U) { regType := RegType.Y }
      is(0xD8.U) { regType := RegType.X }
      is(0xCB.U) { regType := RegType.Y }
      is(0x3E.U) { regType := RegType.X }
      is(0x7E.U) { regType := RegType.Y }
    }

    val op0 = UInt(8.W)
    op0 := regs.a
    switch(regType) {
      is(RegType.X) { op0 := regs.x }
      is(RegType.Y) { op0 := regs.y }
    }

    switch(dpArithState) {
      is(fetchData) {
        val op1 = readDp(readData0)
        when(isStore) {
          dpArithState := storeResult
        } .otherwise {
          val out = runByteALU(inst.ops, op0, op1)
          switch(regType) {
            is(RegType.A) { regs.a := out }
            is(RegType.X) { regs.x := out }
            is(RegType.Y) { regs.y := out }
          }

          globalState := fetch
        }
      }
      is(storeResult) {
        writeDp(readData0, op0)

        dpArithState := fetchData
        globalState := fetch
      }
    }
  }

  private def runDpRMW(): Unit = {
    val fetchData :: storeResult :: Nil = Enum(2)
    val dpRMWState = RegInit(fetchData)
    val fetchedData = Reg(UInt(8.W))
    val isBitMan = inst.ops === Ops.CLR | inst.ops === Ops.SET
    val bitmanIdx = inst.opcode(7, 5)
    val dpAddr = readData0

    switch(dpRMWState) {
      is(fetchData) {
        fetchedData := readDp(dpAddr)
        dpRMWState := storeResult
      }
      is(storeResult) {
        val out = UInt(8.W)
        out := DontCare
        when(isBitMan) {
          switch(inst.ops) {
            is(Ops.CLR) { out := bitManipulation(fetchedData, false.B, bitmanIdx) }
            is(Ops.SET) { out := bitManipulation(fetchedData,  true.B, bitmanIdx) }
          }
        }.otherwise {
          out := runByteALU(inst.ops, fetchedData, fetchedData)
        }

        writeDp(dpAddr, out)

        dpRMWState := fetchData
        globalState := fetch
      }
    }
  }

  private def runDpCMPW(): Unit = {
    val fetchL :: fetchH :: Nil = Enum(2)
    val dataL = Reg(UInt(8.W))
    val dpCMPWState = RegInit(fetchL)
    val addrH = Reg(UInt(8.W))
    val dpAddr = readData0

    switch(dpCMPWState) {
      is(fetchL) {
        dataL := readDp(dpAddr)
        addrH := dpAddr + 1.U
        dpCMPWState := fetchH
      }
      is(fetchH) {
        val dataH = readDp(addrH)
        val word = Cat(dataH, dataL)
        val ya = Cat(regs.y, regs.a)
        val ret = ya - word

        regs.psw.sign  := ret(15).toBool()
        regs.psw.zero  := ret === 0.U
        regs.psw.carry := ret.head(1).toBool()
      }
    }
  }

  // execute ADDW, MOVW or SUBW
  private def runDpWord(): Unit = {
    val fetchL :: calc :: fetchH :: Nil = Enum(3)
    val dpWordState = RegInit(fetchL)
    val dataL = Reg(UInt(8.W))
    val dpAddrH = Reg(UInt(8.W))
    val dpAddrL = readData0
    val isStore = inst.opcode === 0xDA.U

    switch(dpWordState) {
      is(fetchL) {
        dataL := readDp(dpAddrL)
        dpAddrH := dpAddrL + 1.U

        dpWordState := calc
      }
      is(calc) {
        when(isStore) {
          writeDp(dpAddrL, regs.a)
        } .otherwise {
          switch(inst.ops) {
            is(Ops.ADD) { dataL := runByteALU(inst.ops, regs.a, dataL, false.B) }
            is(Ops.SUB) { dataL := runByteALU(inst.ops, regs.a, dataL,  true.B) }
            is(Ops.MOV) { dataL := runByteALU(inst.ops, regs.a, dataL) }
          }
        }

        dpWordState := fetchH
      }
      is(fetchH) {
        when(isStore) {
          writeDp(dpAddrH, regs.y)
        } .otherwise {
          val dataH = readDp(dpAddrH)
          val res   = UInt(8.W)
          res := DontCare
          switch(inst.ops) {
            is(Ops.ADD) { res := runByteALU(inst.ops, regs.y, dataH) }
            is(Ops.SUB) { res := runByteALU(inst.ops, regs.y, dataH) }
            is(Ops.MOV) { res := runByteALU(inst.ops, regs.y, dataH) }
          }

          regs.a := dataL
          regs.y := res

          dpWordState := fetchL
          globalState := fetch
        }
      }
    }
  }

  private def runDpINCW(): Unit = {
    val fetchL :: storeL :: fetchH :: storeH :: Nil = Enum(4)
    val dpINCWState = RegInit(fetchL)

    val dataL = Reg(UInt(8.W))
    val dataH = Reg(UInt(8.W))
    val outL = Reg(UInt(8.W))
    val dpAddrL = readData0
    val dpAddrH = Reg(UInt(8.W))
    val carry = Reg(Bool())
    val isINC = inst.ops === Ops.INC

    switch(dpINCWState) {
      is(fetchL) {
        dataL := readDp(dpAddrL)
        dpAddrH := dpAddrL + 1.U

        dpINCWState := storeL
      }
      is(storeL) {
        val out = Mux(isINC, dataL + 1.U, dataL - 1.U)
        outL := out
        carry := out.head(1)
        writeDp(dpAddrL, out.tail(1))

        dpINCWState := fetchH
      }
      is(fetchH) {
        dataH := readDp(dpAddrH)

        dpINCWState := storeH
      }
      is(storeH) {
        val out = Mux(isINC, dataH + carry.asUInt(), dataH - !carry.asUInt())
        val outH = out.tail(1)
        val result = Cat(outH, outL)

        writeDp(dpAddrH, outH)

        regs.psw.sign := result.head(1).toBool()
        regs.psw.zero := result === 0.U
        dpINCWState := fetchL
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

  private def runByteALU(op: Ops.Type, op0: UInt, op1: UInt, carryIn: Bool = regs.psw.carry): UInt = {
    byteALU.io.ops := op
    byteALU.io.op0 := op0
    byteALU.io.op1 := op1
    byteALU.io.carryIn := carryIn

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

  private def bitManipulation(src: UInt, bit: Bool, idx: UInt): UInt = {
    val srcBits = VecInit(src.toBools())
    val ret = UInt(8.W)

    switch(idx) {
      is(0.U) { srcBits(0) := bit }
      is(1.U) { srcBits(1) := bit }
      is(2.U) { srcBits(2) := bit }
      is(3.U) { srcBits(3) := bit }
      is(4.U) { srcBits(4) := bit }
      is(5.U) { srcBits(5) := bit }
      is(6.U) { srcBits(6) := bit }
      is(7.U) { srcBits(7) := bit }
    }

    ret := srcBits.asUInt()
    ret
  }
}

object RegType extends ChiselEnum {
  val A, X, Y, SP = Value
}


