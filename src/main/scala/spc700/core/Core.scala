package spc700.core

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util.{Cat, Enum, MuxCase, MuxLookup, SwitchContext, is, switch}

class Core(initReg: RegValue) extends Module {
  override val io = IO(new Bundle{
    // communicate to ram
    val ramReadEn = Output(Bool())
    val ramWriteEn = Output(Bool())
    val ramAddr = Output(UInt(16.W))
    val ramReadData = Input(UInt(8.W))
    val ramWriteData = Output(UInt(8.W))

    val regInit = Input(RegisterFile())

    val pcOut = Output(UInt(16.W))
  })

  val fetch :: decode :: exec :: jump :: sleep :: Nil = Enum(5)
  val globalState = RegInit(fetch)
  val operand0 = Reg(UInt(16.W))
  val operand1 = Reg(UInt(16.W))

  val init = Wire(RegisterFile())
  init.a := initReg.a.U
  init.x := initReg.x.U
  init.y := initReg.y.U
  init.pc := initReg.pc.U
  init.sp := initReg.sp.U
  init.psw.set(initReg.psw.U)
  val regs = RegInit(init)

  val opcode = Reg(UInt(8.W))
  val inst = Reg(Instruction())
  val readData0 = Reg(UInt(8.W))

  val jmpOffset = Reg(SInt(16.W))

  val decoder = Module(new Decoder)
  val byteALU = Module(new ALU)

  // submodule ALU and Decoder initialize
  decoder.io := DontCare
  byteALU.io := DontCare

  // IO initialize
  io.ramWriteEn := false.B
  io.ramReadEn := false.B
  io.ramAddr := DontCare
  io.ramWriteData := DontCare
  io.pcOut := regs.pc

  when(reset.asBool()) {
    printf(p"resetting...\n pc: 0x${Hexadecimal(io.regInit.pc)}, a: 0x${Hexadecimal(io.regInit.a)}, x: 0x${Hexadecimal(io.regInit.x)}, y: 0x${Hexadecimal(io.regInit.y)}, sp: 0x${Hexadecimal(io.regInit.sp)}, psw: 0b${Binary(io.regInit.psw.get)}\n")
  }

  switch(globalState) {
    is(fetch) { fetchState() }
    is(decode) { decodeState(globalState === decode) }
    is(exec) { execState(globalState === exec) }
    is(jump) { jumpState(globalState === jump) }
    is(sleep) { /* nothing to do */ }
  }

  private def fetchState(): Unit = {
    // fetch instruction
    val op = Wire(UInt(8.W))
    op := readAbs(regs.pc)
    opcode := op

    printf(p"[0x${Hexadecimal(op)}] pc: 0x${Hexadecimal(regs.pc)}, a: 0x${Hexadecimal(regs.a)}, x: 0x${Hexadecimal(regs.x)}, y: 0x${Hexadecimal(regs.y)}, sp: 0x${Hexadecimal(regs.sp)}, psw: 0b${Binary(regs.psw.get)}\n")

    regs.pc := regs.pc + 1.U
    globalState := decode
  }

  private def decodeState(enable: Bool): Unit = {
    // decode instruction
    decoder.io.opcode := opcode

    // fetch next data
    val data = readAbs(regs.pc)

    val inst = decoder.io.inst
    this.inst := decoder.io.inst

    when(inst.addressing === Addressing.Imm) {
      execImm(inst, data, enable & inst.addressing === Addressing.Imm)
    }.elsewhen(inst.addressing === Addressing.Acc) {
      execAcc(inst, enable & inst.addressing === Addressing.Acc)
    }.elsewhen(inst.addressing === Addressing.Branch) {
      execBranchByPSW(inst, data, enable & inst.addressing === Addressing.Branch)
    }.elsewhen(inst.addressing === Addressing.PSWMan) {
      manPSWDecode(inst.ops, enable & inst.addressing === Addressing.PSWMan)
    }.elsewhen(inst.ops === Ops.NOP){
      regs.pc := regs.pc + 1.U
      globalState := fetch
    }.otherwise{
      regs.pc := regs.pc + 1.U
      readData0 := data
      globalState := exec
    }
  }

  private def execState(enable: Bool): Unit = {
    switch(inst.addressing){
      is(Addressing.PSWMan) { manPSWExec(enable & inst.addressing === Addressing.PSWMan) }
      is(Addressing.AbsCalc) { runAbsArith(enable & inst.addressing === Addressing.AbsCalc) }
      is(Addressing.AbsCall) { runAbsCall(enable & inst.addressing === Addressing.AbsCall) }
      is(Addressing.AbsJmp) { runAbsJmp(enable & inst.addressing === Addressing.AbsJmp) }
      is(Addressing.AbsRMW) { runAbsRMW(enable & inst.addressing === Addressing.AbsRMW) }
      is(Addressing.AbsRMWBit) { runAbsRMWBit(enable & inst.addressing === Addressing.AbsRMWBit) }
      is(Addressing.AbsIdxInd) {
        val opcodes = Vector(0xF6, 0xD6, 0x16, 0x36, 0x56, 0x76, 0x96, 0xB6, 0xD6, 0xF6)
        val isRequireY = opcodes
          .map(inst.opcode === _.U)
          .foldLeft(false.B){ case (left, right) => left | right }

        val idx = Mux(isRequireY, regs.y, regs.x)
        runAbsIndex(idx, enable & inst.addressing === Addressing.AbsIdxInd)
      }
      is(Addressing.DpCalc) { runDpArith(enable & inst.addressing === Addressing.DpCalc) }
      is(Addressing.DpRMW) { runDpRMW(enable & inst.addressing === Addressing.DpRMW) }
      is(Addressing.DpCMPW) { runDpCMPW(enable & inst.addressing === Addressing.DpCMPW) }
      is(Addressing.DpWord) { runDpWord(enable & inst.addressing === Addressing.DpWord) }
      is(Addressing.DpINCW) { runDpINCW(enable & inst.addressing === Addressing.DpINCW) }
      is(Addressing.DpDp) { runDpDp(enable & inst.addressing === Addressing.DpDp) }
      is(Addressing.DpImm) { runDpImm(enable & inst.addressing === Addressing.DpImm) }
      is(Addressing.DpIndX) { runDpIndX(enable & inst.addressing === Addressing.DpIndX) }
      is(Addressing.DpIndY) { runDpIndY(enable & inst.addressing === Addressing.DpIndY) }
      is(Addressing.DpRel) { runDpRel(enable & inst.addressing === Addressing.DpRel) }
      is(Addressing.DpX) { runDpIdx(regs.x, enable & inst.addressing === Addressing.DpX) }
      is(Addressing.DpY) { runDpIdx(regs.y, enable & inst.addressing === Addressing.DpY) }
      is(Addressing.DpXRMW) { runDpXRMW(enable & inst.addressing === Addressing.DpXRMW) }
      is(Addressing.DpXRel) { runDpXRel(enable & inst.addressing === Addressing.DpXRel) }
      is(Addressing.IndX) { runIndX(needInc = false, enable & inst.addressing === Addressing.IndX) }
      is(Addressing.IndXInc) { runIndX(needInc = true, enable & inst.addressing === Addressing.IndXInc) }
      is(Addressing.IndXIndY) { runIndXIndY(enable & inst.addressing === Addressing.IndXIndY) }
      is(Addressing.BitMan) { runBitMan(enable & inst.addressing === Addressing.BitMan) }
      is(Addressing.RelDBNZ) { runRelDBNZ(enable & inst.addressing === Addressing.RelDBNZ) }
      is(Addressing.Special) {
        switch(inst.ops) {
          is(Ops.MUL) { runMUL(enable & inst.ops === Ops.MUL) }
          is(Ops.DIV) { runDIV(enable & inst.ops === Ops.DIV) }
          is(Ops.DAA) { runDAA(enable & inst.ops === Ops.DAA) }
          is(Ops.DAS) { runDAS(enable & inst.ops === Ops.DAS) }
          is(Ops.XCN) { runXCN(enable & inst.ops === Ops.XCN) }
          is(Ops.BRK) { runBRK(enable & inst.ops === Ops.BRK) }
          is(Ops.RET) { runRET(enable & inst.ops === Ops.RET) }
          is(Ops.RETI) { runRETI(enable & inst.ops === Ops.RETI) }
          is(Ops.PUSH) { runPUSH(enable & inst.ops === Ops.PUSH) }
          is(Ops.POP) { runPOP(enable & inst.ops === Ops.POP) }
          is(Ops.TCALL) { runTCALL(enable & inst.ops === Ops.TCALL) }
          is(Ops.PCALL) { runPCALL(enable & inst.ops === Ops.PCALL) }
          is(Ops.SLEEP) { globalState := sleep }
          is(Ops.STOP) { globalState := sleep }
        }
      }
    }
  }

  private def execImm(inst: Instruction, imm: UInt, enable: Bool): Unit = {
    val register = WireInit(RegType(), DontCare)

    when(enable) {
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

      val src = WireInit(UInt(8.W), DontCare)
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
  }

  private def execAcc(inst: Instruction, enable: Bool): Unit = {
    val src = WireInit(RegType(), RegType.A)
    val dst = WireInit(RegType(), RegType.A)

    when(enable) {
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

      val operand = WireInit(UInt(8.W), regs.a)
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
  }

  private def execBranchByPSW(inst: Instruction, offset: UInt, enable: Bool): Unit = {
    val cond =  WireInit(Bool(), false.B)

    when(enable) {
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
  }

  private def manPSWDecode(ops: Ops.Type, enable: Bool): Unit = {
    when(enable) {
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
  }

  private def manPSWExec(enable: Bool): Unit = {
    when(enable) {
      globalState := fetch
    }
  }

  private def runAbsArith(enable: Bool): Unit = {
    val readAbsH :: readOperand :: writeRam :: Nil = Enum(3)
    val absArithState = RegInit(readAbsH)
    val absL = readData0
    val absH = Reg(UInt(8.W))
    val op0Reg = WireInit(RegType(), DontCare)
    val op0 = WireInit(UInt(8.W), DontCare)
    val op1 = WireInit(UInt(8.W), DontCare)
    val stored = Reg(UInt(8.W))
    val isStore = WireInit(Bool(), DontCare)
    val opcode = this.inst.opcode

    when(enable) {
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
          absH := readAbs(regs.pc)
          regs.pc := regs.pc + 1.U

          absArithState := readOperand
        }
        is(readOperand) {
          op1 := readAbs(Cat(absH, absL))

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
          writeAbs(Cat(absH, absL), stored)

          globalState := fetch
          absArithState := readAbsH
        }
      }
    }
  }

  private def runAbsCall(enable: Bool): Unit = {
    val readNewPCH :: none0 :: pushPCH :: pushPCL :: none1 :: renewPC :: Nil = Enum(6)
    val newPCL = readData0
    val newPCH = Reg(UInt(8.W))
    val absCallState = RegInit(readNewPCH)

    when(enable) {
      switch(absCallState) {
        is(readNewPCH) {
          regs.pc := regs.pc + 1.U
          newPCH := readAbs(regs.pc)
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
  }

  private def runAbsJmp(enable: Bool): Unit = {
    when(enable) {
      val newPCL = readData0
      val newPCH = readAbs(regs.pc)

      regs.pc := Cat(newPCH, newPCL)
      globalState := fetch
    }
  }

  private def runAbsRMW(enable: Bool): Unit = {
    val fetchAddrH :: fetchData :: storeResult ::Nil = Enum(3)
    val addrL = readData0
    val addrH = Reg(UInt(8.W))
    val result = Reg(UInt(8.W))
    val absRMWState = RegInit(fetchAddrH)

    when(enable) {
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
  }

  private def runAbsRMWBit(enable: Bool): Unit = {
    val fetchAddrH :: addX :: fetchPCL :: fetchPCH :: Nil = Enum(4)
    val addrL = readData0
    val addrH = Reg(UInt(8.W))
    val newPCL = Reg(UInt(8.W))
    val fetchAddr = Reg(UInt(16.W))
    val absRMWBitState = RegInit(fetchAddrH)

    when(enable) {
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
  }

  // run Absolute + Index addressing mode
  // [Abs+X] or [Abs+Y]
  // idx is regs.x or regs.y
  private def runAbsIndex(idx: UInt, enable: Bool): Unit = {
    val fetchAbsH :: addIndex :: fetchData :: storeResult :: Nil = Enum(4)
    val absL = readData0
    val absH = Reg(UInt(8.W))
    val addr = Reg(UInt(16.W))
    val absXState = RegInit(fetchAbsH)
    val isStore = inst.opcode === 0xD5.U | inst.opcode === 0xD6.U

    when(enable) {
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
  }

  private def runDpArith(enable: Bool): Unit = {
    val fetchData :: storeResult :: Nil = Enum(2)
    val regType = WireInit(RegType(), RegType.A)
    val dpArithState = RegInit(fetchData)
    val isStore = inst.opcode === 0xC4.U | inst.opcode === 0xD8.U | inst.opcode === 0xCB.U

    when(enable) {
      switch(inst.opcode) {
        is(0xF8.U) { regType := RegType.X }
        is(0xEB.U) { regType := RegType.Y }
        is(0xD8.U) { regType := RegType.X }
        is(0xCB.U) { regType := RegType.Y }
        is(0x3E.U) { regType := RegType.X }
        is(0x7E.U) { regType := RegType.Y }
      }

      val op0 = WireInit(UInt(8.W), regs.a)
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
  }

  private def runDpRMW(enable: Bool): Unit = {
    val fetchData :: storeResult :: Nil = Enum(2)
    val dpRMWState = RegInit(fetchData)
    val fetchedData = Reg(UInt(8.W))
    val isBitMan = inst.ops === Ops.CLR | inst.ops === Ops.SET
    val bitmanIdx = inst.opcode(7, 5)
    val dpAddr = readData0

    when(enable) {
      switch(dpRMWState) {
        is(fetchData) {
          fetchedData := readDp(dpAddr)
          dpRMWState := storeResult
        }
        is(storeResult) {
          val out = WireInit(UInt(8.W), DontCare)
          when(isBitMan) {
            switch(inst.ops) {
              is(Ops.CLR) { out := setBit(fetchedData, false.B, bitmanIdx) }
              is(Ops.SET) { out := setBit(fetchedData,  true.B, bitmanIdx) }
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
  }

  private def runDpCMPW(enable: Bool): Unit = {
    val fetchL :: fetchH :: Nil = Enum(2)
    val dataL = Reg(UInt(8.W))
    val dpCMPWState = RegInit(fetchL)
    val addrH = Reg(UInt(8.W))
    val dpAddr = readData0

    when(enable) {
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

          regs.psw.sign := ret(15).asBool()
          regs.psw.zero := ret === 0.U
          regs.psw.carry := ret.head(1).asBool()
        }
      }
    }
  }

  // execute ADDW, MOVW or SUBW
  private def runDpWord(enable: Bool): Unit = {
    val fetchL :: calc :: fetchH :: Nil = Enum(3)
    val dpWordState = RegInit(fetchL)
    val dataL = Reg(UInt(8.W))
    val dpAddrH = Reg(UInt(8.W))
    val dpAddrL = readData0
    val isStore = inst.opcode === 0xDA.U

    when(enable) {
      switch(dpWordState) {
        is(fetchL) {
          dataL := readDp(dpAddrL)
          dpAddrH := dpAddrL + 1.U

          dpWordState := calc
        }
        is(calc) {
          when(isStore) {
            writeDp(dpAddrL, regs.a)
          }.otherwise {
            switch(inst.ops) {
              is(Ops.ADD) {
                dataL := runByteALU(inst.ops, regs.a, dataL, false.B)
              }
              is(Ops.SUB) {
                dataL := runByteALU(inst.ops, regs.a, dataL, true.B)
              }
              is(Ops.MOV) {
                dataL := runByteALU(inst.ops, regs.a, dataL)
              }
            }
          }

          dpWordState := fetchH
        }
        is(fetchH) {
          when(isStore) {
            writeDp(dpAddrH, regs.y)
          }.otherwise {
            val dataH = readDp(dpAddrH)
            val res = WireInit(UInt(8.W), DontCare)
            switch(inst.ops) {
              is(Ops.ADD) {
                res := runByteALU(inst.ops, regs.y, dataH)
              }
              is(Ops.SUB) {
                res := runByteALU(inst.ops, regs.y, dataH)
              }
              is(Ops.MOV) {
                res := runByteALU(inst.ops, regs.y, dataH)
              }
            }

            regs.a := dataL
            regs.y := res

            dpWordState := fetchL
            globalState := fetch
          }
        }
      }
    }
  }

  private def runDpINCW(enable: Bool): Unit = {
    val fetchL :: storeL :: fetchH :: storeH :: Nil = Enum(4)
    val dpINCWState = RegInit(fetchL)

    val dataL = Reg(UInt(8.W))
    val dataH = Reg(UInt(8.W))
    val outL = Reg(UInt(8.W))
    val dpAddrL = readData0
    val dpAddrH = Reg(UInt(8.W))
    val carry = Reg(Bool())
    val isINC = inst.ops === Ops.INC

    when(enable) {
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

          regs.psw.sign := result.head(1).asBool()
          regs.psw.zero := result === 0.U
          dpINCWState := fetchL
          globalState := fetch
        }
      }
    }
  }

  def runDpDp(enable: Bool): Unit = {
    val fetchFirst :: fetchNextAddr :: calculation :: storeResult :: Nil = Enum(4)
    val dpdpState = RegInit(fetchFirst)
    val secondData = Reg(UInt(8.W))
    val secondAddr = readData0
    val firstAddr = Reg(UInt(8.W))
    val result = Reg(UInt(8.W))

    when(enable) {
      switch(dpdpState) {
        is(fetchFirst) {
          secondData := readDp(secondAddr)
          dpdpState := fetchNextAddr
        }
        is(fetchNextAddr) {
          firstAddr := readAbs(regs.pc)
          regs.pc := regs.pc + 1.U
          dpdpState := calculation
        }
        is(calculation) {
          // first data is not needed for MOV
          val firstData = WireInit(UInt(8.W), DontCare)
          when(inst.ops =/= Ops.MOV) {
            firstData := readDp(firstAddr)
          }

          result := runByteALU(inst.ops, firstData, secondData)

          // Only MOV is finished at this state
          dpdpState := storeResult
          when(inst.ops === Ops.MOV) {
            writeDp(firstAddr, secondData)
            dpdpState := fetchFirst
            globalState := fetch
          }
        }
        is(storeResult) {
          when(inst.ops =/= Ops.CMP) {
            writeDp(firstAddr, result)
          }

          dpdpState := fetchFirst
          globalState := fetch
        }
      }
    }
  }

  def runDpImm(enable: Bool): Unit = {
    val fetchDpAddr :: fetchData :: storeResult :: Nil = Enum(3)
    val dpImmState = RegInit(fetchDpAddr)
    val imm = readData0
    val dpAddr = Reg(UInt(8.W))
    val dpData = Reg(UInt(8.W))

    when(enable) {
      switch(dpImmState) {
        is(fetchDpAddr) {
          dpAddr := readAbs(regs.pc)
          regs.pc := regs.pc + 1.U
          dpImmState := fetchData
        }
        is(fetchData) {
          dpData := readDp(dpAddr)
          dpImmState := storeResult
        }
        is(storeResult) {
          val out = runByteALU(inst.ops, dpData, imm)
          when(inst.ops =/= Ops.CMP) {
            writeDp(dpAddr, out)
          }

          dpImmState := fetchDpAddr
          globalState := fetch
        }
      }
    }
  }


  // For [[dp+X]]
  def runDpIndX(enable: Bool): Unit = {
    val addX :: fetchL :: fetchH :: arith :: storeRam :: Nil = Enum(5)
    val dpIndXState = RegInit(addX)
    val dp = readData0
    val dpAddr = Reg(UInt(8.W))
    val addrL = Reg(UInt(8.W))
    val addrH = Reg(UInt(8.W))
    val regType = WireInit(RegType(), RegType.A)

    when(enable) {
      switch(inst.opcode) {
        is(0xF9.U) { regType := RegType.X }
        is(0xFB.U) { regType := RegType.Y }
        is(0xDB.U) { regType := RegType.Y }
        is(0xD9.U) { regType := RegType.X }
      }
      val op = Wire(UInt(8.W))
      op := MuxLookup(regType.asUInt(), regs.a, Seq(
        RegType.X.asUInt() -> regs.x,
        RegType.Y.asUInt() -> regs.y
      ))

      val isStore = inst.opcode === 0xC7.U

      switch(dpIndXState) {
        is(addX) {
          dpAddr := dp + regs.x
          dpIndXState := fetchL
        }
        is(fetchL) {
          addrL := readDp(dpAddr)
          dpAddr := dpAddr + 1.U
          dpIndXState := fetchH
        }
        is(fetchH) {
          addrH := readDp(dpAddr)
          dpIndXState := arith
        }
        is(arith) {
          val addr = Cat(addrH, addrL)
          val data = readAbs(addr)

          when(isStore) {
            dpIndXState := storeRam
          }.otherwise {
            val out = runByteALU(inst.ops, op, data)
            switch(regType) {
              is(RegType.A) { regs.a := out }
              is(RegType.X) { regs.x := out }
              is(RegType.Y) { regs.y := out }
            }

            dpIndXState := addX
            globalState := fetch
          }
        }
        is(storeRam) {
          val addr = Cat(addrH, addrL)
          writeAbs(addr, op)

          dpIndXState := addX
          globalState := fetch
        }
      }
    }
  }

  private def runDpIndY(enable: Bool): Unit = {
    val fetchAddrL :: fetchAddrH :: addY :: arith :: storeRam :: Nil = Enum(5)
    val dpIndYState = RegInit(fetchAddrL)
    val dpAddr = readData0
    val addrL = Reg(UInt(8.W))
    val addrH = Reg(UInt(8.W))
    val addrY = Reg(UInt(8.W))
    val isStore = inst.opcode === 0xD7.U

    when(enable) {
      switch(dpIndYState) {
        is(fetchAddrL) {
          addrL := readDp(dpAddr)
          dpIndYState := fetchAddrH
        }
        is(fetchAddrH) {
          addrH := readDp(dpAddr + 1.U)
          dpIndYState := addY
        }
        is(addY) {
          addrY := Cat(addrH, addrL) + regs.y
          dpIndYState := arith
        }
        is(arith) {
          val data = readAbs(addrY)

          when(isStore) {
            dpIndYState := storeRam
          } .otherwise {
            regs.a := runByteALU(inst.ops, regs.a, data)
            dpIndYState := fetchAddrL
            globalState := fetch
          }
        }
        is(storeRam) {
          writeAbs(addrY, regs.a)
          dpIndYState := fetchAddrL
          globalState := fetch
        }
      }
    }
  }

  private def runDpRel(enable: Bool): Unit = {
    val fetchOp :: calcCond :: fetchOffset :: Nil = Enum(3)
    val dpRelState = RegInit(fetchOp)
    val operand = Reg(UInt(8.W))
    val dpAddr = readData0
    val idx = inst.opcode.head(3)

    def isBranch(): Bool = {
      val ret = Wire(Bool())
      ret := MuxLookup(inst.ops.asUInt(), false.B, Seq(
         Ops.BBS.asUInt() ->  getBit(idx, operand),
         Ops.BBC.asUInt() -> !getBit(idx, operand),
        Ops.CBNE.asUInt() ->  (regs.a =/= operand),
        Ops.DBNZ.asUInt() ->  (regs.y =/= operand),
      ))

      ret
    }

    when(enable) {
      switch(dpRelState) {
        is(fetchOp) {
          operand := readDp(dpAddr)
          dpRelState := calcCond
        }
        is(calcCond) {
          when(inst.ops === Ops.DBNZ) {
            operand := operand - 1.U
            writeAbs(dpAddr, operand - 1.U)
          }

          dpRelState := fetchOffset
        }
        is(fetchOffset) {
          jmpOffset := readAbs(regs.pc).asSInt()
          regs.pc := regs.pc + 1.U

          when(isBranch()) {
            globalState := jump
          } .otherwise {
            globalState := fetch
          }

          dpRelState := fetchOp
        }
      }
    }
  }

  // for [dp+X] or [dp+Y] addressing mode
  private def runDpIdx(idx: UInt, enable: Bool): Unit = {
    val addIdx :: fetchData :: storeRam :: Nil = Enum(3)
    val dpIdxState = RegInit(addIdx)
    val isStore = inst.opcode === 0xD4.U | inst.opcode === 0xDB.U | inst.opcode === 0xD9.U
    val dp = readData0
    val dpAddr = Reg(UInt(8.W))
    val regType = WireInit(RegType(), DontCare)

    when(enable) {
      regType := MuxLookup(inst.opcode, RegType.A, Seq(
        0xF9.U -> RegType.X,
        0xFB.U -> RegType.Y,
        0xDB.U -> RegType.Y,
        0xD9.U -> RegType.X,
      ))
      val operand = Wire(UInt(8.W))
      operand := MuxLookup(regType.asUInt(), regs.a, Seq(
        RegType.X.asUInt() -> regs.x,
        RegType.Y.asUInt() -> regs.y,
      ))

      switch(dpIdxState) {
        is(addIdx) {
          dpAddr := dp + idx
          dpIdxState := fetchData
        }
        is(fetchData) {
          val data = readDp(dpAddr)
          when(isStore) {
            dpIdxState := storeRam
          } .otherwise {
            val out = runByteALU(inst.ops, operand, data)
            switch(regType) {
              is(RegType.A) { regs.a := out }
              is(RegType.X) { regs.x := out }
              is(RegType.Y) { regs.y := out }
            }

            dpIdxState := addIdx
            globalState := fetch
          }
        }
        is(storeRam) {
          writeDp(dpAddr, operand)

          dpIdxState := addIdx
          globalState := fetch
        }
      }
    }
  }

  private def runDpXRMW(enable: Bool): Unit = {
    val addX :: fetchData :: storeResult :: Nil = Enum(3)
    val dpXRMWState = RegInit(addX)
    val dp = readData0
    val dpAddr = Reg(UInt(8.W))
    val result = Reg(UInt(8.W))

    when(enable) {
      switch(dpXRMWState) {
        is(addX) {
          dpAddr := dp + regs.x
          dpXRMWState := fetchData
        }
        is(fetchData) {
          val data = readDp(dpAddr)
          result := runByteALU(inst.ops, data, data)
          dpXRMWState := storeResult
        }
        is(storeResult) {
          writeDp(dpAddr, result)
          dpXRMWState := addX
          globalState := fetch
        }
      }
    }
  }

  // for CBNE
  private def runDpXRel(enable: Bool): Unit = {
    val addX :: fetchData :: none :: execBranch :: Nil = Enum(4)
    val dpXRelState = RegInit(addX)
    val dp = readData0
    val dpAddr = Reg(UInt(8.W))
    val data = Reg(UInt(8.W))

    when(enable) {
      switch(dpXRelState) {
        is(addX) {
          dpAddr := dp + regs.x
          dpXRelState := fetchData
        }
        is(fetchData) {
          data := readDp(dpAddr)
          dpXRelState := none
        }
        is(none) {
          dpXRelState := execBranch
        }
        is(execBranch) {
          jmpOffset := readDp(regs.pc).asSInt()
          regs.pc := regs.pc + 1.U

          val isBranch = regs.a =/= data
          when(isBranch) {
            globalState := jump
          } .otherwise {
            globalState := fetch
          }

          dpXRelState := addX
        }
      }
    }
  }

  private def runIndX(needInc: Boolean, enable: Bool): Unit = {
    val fetchData :: storeRam :: Nil = Enum(2)
    val indXState = RegInit(fetchData)
    val isStore = inst.opcode === 0xC6.U

    when(enable) {
      switch(indXState) {
        is(fetchData) {
          val data = readDp(regs.x)

          indXState := storeRam
          when(!isStore) {
            val out = runByteALU(inst.ops, regs.a, data)
            regs.a := out

            if(needInc)
              regs.x := regs.x + 1.U

            indXState := fetchData
            globalState := fetch
          }
        }
        is(storeRam) {
          writeDp(regs.x, regs.a)

          if(needInc)
            regs.x := regs.x + 1.U

          indXState := fetchData
          globalState := fetch
        }
      }
    }
  }

  private def runIndXIndY(enable: Bool): Unit = {
    val fetchY :: fetchX :: storeResult :: Nil = Enum(3)
    val indXIndYState = RegInit(fetchY)
    val dataX = Reg(UInt(8.W))
    val dataY = Reg(UInt(8.W))

    when(enable) {
      switch(indXIndYState) {
        is(fetchY) {
          dataY := readDp(regs.y)
          indXIndYState := fetchX
        }
        is(fetchX) {
          dataX := readDp(regs.x)
          indXIndYState := storeResult
        }
        is(storeResult) {
          val out = runByteALU(inst.ops, dataX, dataY)
          when(inst.ops =/= Ops.CMP) {
            writeDp(regs.x, out)
          }

          indXIndYState := fetchY
          globalState   := fetch
        }
      }
    }
  }

  private def runBitMan(enable: Bool): Unit = {
    val fetchAddrH :: fetchData :: writeRamNOT :: writeRamMOV :: Nil = Enum(4)
    val addrL = readData0
    val addrH = Reg(UInt(5.W))
    val idx   = Reg(UInt(3.W))
    val stored = Reg(UInt(8.W))
    val bitManState = RegInit(fetchAddrH)

    val needRev    = inst.opcode === 0x2A.U | inst.opcode === 0x6A.U
    val isStoreMOV = inst.opcode === 0xCA.U
    val isNOT      = inst.opcode === 0xEA.U
    val isDstRam   = isNOT | isStoreMOV
    val only4Cycle = inst.opcode === 0xAA.U | inst.opcode === 0x4A.U | inst.opcode === 0x6A.U

    when(enable) {
      switch(bitManState) {
        is(fetchAddrH) {
          val fetched = readAbs(regs.pc)
          regs.pc := regs.pc + 1.U

          addrH := fetched.tail(3)
          idx   := fetched.head(3)
          bitManState := fetchData
        }
        is(fetchData) {
          val data = readAbs(Cat(addrH, addrL))
          val bit = getBit(idx, data)
          val res0 = Wire(Bool())
          res0 := MuxLookup(inst.ops.asUInt(), regs.psw.carry, Seq(
            Ops.AND.asUInt() -> (regs.psw.carry & bit),
            Ops.OR.asUInt()  -> (regs.psw.carry | bit),
            Ops.EOR.asUInt() -> (regs.psw.carry ^ bit),
            Ops.NOT.asUInt() -> (!bit),
            Ops.MOV.asUInt() -> Mux(isStoreMOV, regs.psw.carry, bit),
          ))
          val res1 = Mux(needRev, !res0, res0).asBool()
          stored := setBit(data, res1, idx)

          when(!isDstRam) {
            regs.psw.carry := res1
          }

          bitManState := writeRamNOT
          when(only4Cycle) {
            bitManState := fetchAddrH
            globalState := fetch
          }
        }
        is(writeRamNOT) {
          bitManState := fetchAddrH
          globalState := fetch

          when(isNOT) {
            writeAbs(Cat(addrH, addrL), stored)
          }
          when(isStoreMOV) {
            bitManState := writeRamMOV
            globalState := exec
          }
        }
        is(writeRamMOV) {
          writeAbs(Cat(addrH, addrL), stored)

          bitManState := fetchAddrH
          globalState := fetch
        }
      }
    }
  }

  private def runRelDBNZ(enable: Bool): Unit = {
    val decY :: execCond :: Nil = Enum(2)
    val relDBNZState = RegInit(decY)
    val isBranch = regs.y === 0.U

    when(enable) {
      switch(relDBNZState) {
        is(decY) {
          regs.y := regs.y - 1.U
          relDBNZState := execCond
        }
        is(execCond) {
          jmpOffset := readAbs(regs.pc).asSInt()
          regs.pc := regs.pc + 1.U

          relDBNZState := decY
          when(isBranch) {
            globalState := jump
          }.otherwise {
            globalState := fetch
          }
        }
      }
    }
  }

  private def runMUL(enable: Bool): Unit = {
    val mul :: wait :: Nil = Enum(2)
    val mulState = RegInit(mul)
    val waitCounter = RegInit(0.U)

    when(enable) {
      switch(mulState) {
        is(mul) {
          val result = regs.y * regs.a
          regs.a := result( 7, 0)
          regs.y := result(15, 8)
          regs.psw.sign := result(15)
          regs.psw.zero := result === 0.U

          mulState := wait
        }
        is(wait) {
          waitCounter := waitCounter + 1.U
          when(waitCounter === 4.U) {
            waitCounter := 0.U
            mulState := mul
            globalState := fetch
          }
        }
      }
    }
  }

  private def runDIV(enable: Bool): Unit = {
    val setPSW :: normalDiv :: erroneousDiv0 :: erroneousDiv1 :: wait :: Nil = Enum(5)
    val divState = RegInit(setPSW)
    val dividedForDiv = Reg(UInt(16.W))
    val dividedForMod = Reg(UInt(16.W))
    val divider = Reg(UInt(16.W))
    val waitCounter = RegInit(0.U)

    when(enable) {
      switch(divState) {
        is(setPSW) {
          regs.psw.overflow := regs.y >= regs.x
          regs.psw.half := regs.y(3, 0)

          when(regs.y < Cat(regs.x, 0.U(1.W))) {
            divState := normalDiv
          }.otherwise {
            divState := erroneousDiv0
          }
        }
        is(normalDiv) {
          val ya = Cat(regs.y, regs.a)
          regs.a := ya / regs.x
          regs.y := ya % regs.x

          divState := wait
        }
        is(erroneousDiv0) {
          val ya = Cat(regs.y, regs.a)
          val operand = ya - Cat(regs.x, 0.U(9.W))
          dividedForDiv := 255.U - operand
          dividedForMod := regs.x + operand
          divider := 256.U - regs.x

          divState := erroneousDiv1
        }
        is(erroneousDiv1) {
          regs.a := dividedForDiv / divider
          regs.y := dividedForMod % divider

          waitCounter := 1.U
          divState := wait
        }
        is(wait) {
          waitCounter := waitCounter + 1.U

          when(waitCounter === 7.U) {
            regs.psw.zero := regs.a === 0.U
            regs.psw.zero := regs.a.head(1).asBool()

            waitCounter := 0.U
            divState := setPSW
            globalState := fetch
          }
        }
      }
    }
  }

  private def runDAA(enable: Bool): Unit = {
    val firstCond = regs.psw.carry | regs.a > 0x99.U
    val tmp = WireInit(UInt(8.W), regs.a)

    when(enable) {
      when(firstCond) {
        tmp := regs.a + 0x60.U
        regs.psw.carry := true.B
      }

      val secondCond = regs.psw.half | tmp(3, 0) > 0x09.U
      val ret = WireInit(UInt(8.W), tmp)
      when(secondCond) {
        ret := tmp + 0x06.U
      }

      regs.a        := ret
      regs.psw.sign := ret.head(1).asBool()
      regs.psw.zero := ret === 0.U

      globalState := fetch
    }
  }

  private def runDAS(enable: Bool): Unit = {
    val firstCond = !regs.psw.carry | regs.a > 0x99.U
    val tmp = WireInit(UInt(8.W), regs.a)

    when(enable) {
      when(firstCond) {
        tmp := regs.a - 0x60.U
        regs.psw.carry := false.B
      }

      val secondCond = !regs.psw.half | regs.a(3, 0) > 0x09.U
      val ret = WireInit(UInt(8.W), tmp)
      when(secondCond) {
        ret := tmp - 0x06.U
      }

      regs.a        := ret
      regs.psw.sign := ret.head(1).asBool()
      regs.psw.zero := ret === 0.U

      globalState := fetch
    }
  }

  private def runXCN(enable: Bool): Unit = {
    val waitCount = RegInit(0.U)

    when(enable) {
      waitCount := waitCount + 1.U
      when(waitCount === 2.U) {
        val ret = Cat(regs.a(3, 0), regs.a(7, 4))
        regs.a := ret
        regs.psw.sign := ret.head(1).asBool()
        regs.psw.zero := ret === 0.U

        waitCount := 0.U
        globalState := fetch
      }
    }
  }

  private def runBRK(enable: Bool): Unit = {
    val writePCH :: writePCL :: writePSW :: renewPSW :: fetchPCL :: fetchPCH :: Nil = Enum(6)
    val brkState = RegInit(writePCH)
    val pcL = Reg(UInt(8.W))

    when(enable) {
      switch(brkState) {
        is(writePCH) {
          pushStack(regs.pc(15, 8))
          brkState := writePCL
        }
        is(writePCL) {
          pushStack(regs.pc(7, 0))
          brkState := writePSW
        }
        is(writePSW) {
          pushStack(regs.psw.get)
          brkState := renewPSW
        }
        is(renewPSW) {
          regs.psw.break     := true.B
          regs.psw.interrupt := false.B
          brkState := fetchPCL
        }
        is(fetchPCL) {
          pcL := readAbs(0xFFDE.U)
          brkState := fetchPCH
        }
        is(fetchPCH) {
          regs.pc := Cat(readAbs(0xFFDF.U), pcL)
          brkState := writePCH
          globalState := fetch
        }
      }
    }
  }

  private def runRETI(enable: Bool): Unit = {
    val none :: popPSW :: popPCL :: popPCH :: Nil = Enum(4)
    val retiState = RegInit(none)
    val pcL = Reg(UInt(8.W))

    when(enable) {
      switch(retiState) {
        is(none) { retiState := popPSW }
        is(popPSW) {
          val psw = popStack()
          regs.psw.set(psw)

          retiState := popPCL
        }
        is(popPCL) {
          pcL := popStack()
          retiState := popPCH
        }
        is(popPCH) {
          regs.pc := Cat(popStack(), pcL)
          retiState := none
          globalState := fetch
        }
      }
    }
  }

  private def runPUSH(enable: Bool): Unit = {
    val push :: none :: Nil = Enum(2)
    val pushState = RegInit(push)
    val pushed = MuxLookup(inst.opcode, regs.a, Seq(
      0x2D.U -> regs.a,
      0x4D.U -> regs.x,
      0x6D.U -> regs.y,
      0x0D.U -> regs.psw.get,
    ))

    when(enable) {
      switch(pushState) {
        is(push) {
          pushStack(pushed.asUInt())
          pushState := none
        }
        is(none) {
          pushState := push
          globalState := fetch
        }
      }
    }
  }

  private def runPOP(enable: Bool): Unit = {
    val none :: pop :: Nil = Enum(2)
    val popState = RegInit(none)

    when(enable) {
      switch(popState) {
        is(none) { popState := pop }
        is(pop) {
          val data = popStack()
          switch(inst.opcode) {
            is(0xAE.U) { regs.a := data }
            is(0xCE.U) { regs.x := data }
            is(0xEE.U) { regs.y := data }
            is(0x8E.U) { regs.psw.set(data) }
          }
        }
      }
    }
  }

  private def runTCALL(enable: Bool): Unit = {
    val none0 :: pushPCH :: pushPCL :: calcAddr :: fetchPCL :: fetchPCH :: Nil = Enum(6)
    val fetchAddr = Reg(UInt(16.W))
    val tcallState = RegInit(none0)
    val pcL = Reg(UInt(8.W))

    when(enable) {
      switch(tcallState) {
        is(none0) { tcallState := pushPCH }
        is(pushPCH) {
          pushStack(regs.pc(15, 8))
          tcallState := pushPCL
        }
        is(pushPCL) {
          pushStack(regs.pc(7, 0))
          tcallState := calcAddr
        }
        is(calcAddr) {
          val idx = Cat(inst.opcode(7, 4), 0.U(1.W))
          fetchAddr := 0xFFDE.U - idx
          tcallState := fetchPCL
        }
        is(fetchPCL) {
          pcL := readAbs(fetchAddr)
          fetchAddr := fetchAddr + 1.U
          tcallState := fetchPCH
        }
        is(fetchPCH) {
          regs.pc := Cat(readAbs(fetchAddr), pcL)

          tcallState  := none0
          globalState := fetch
        }
      }
    }
  }

  private def runPCALL(enable: Bool): Unit = {
    val none :: pushPCH :: pushPCL :: setPC :: Nil = Enum(4)
    val pcallState = RegInit(none)

    when(enable) {
      switch(pcallState) {
        is(none) { pcallState := pushPCH }
        is(pushPCH) {
          pushStack(regs.pc(15, 8))
          pcallState := pushPCL
        }
        is(pushPCL) {
          pushStack(regs.pc(7, 0))
          pcallState := setPC
        }
        is(setPC) {
          regs.pc := Cat(0xFF.U, inst.opcode(3, 0))
          pcallState := none
          globalState := fetch
        }
      }
    }
  }

  private def runRET(enable: Bool): Unit = {
    val none :: popPCL :: popPCH :: Nil = Enum(3)
    val retState = RegInit(none)
    val pcl = Reg(UInt(8.W))

    when(enable) {
      switch(retState) {
        is(none) { retState := popPCL }
        is(popPCL) {
          pcl := popStack()
          retState := popPCH
        }
        is(popPCH) {
          regs.pc := Cat(popStack(), pcl)
          retState    := none
          globalState := fetch
        }
      }
    }
  }

  private def jumpState(enable: Bool): Unit = {
    val jump0 :: jump1 :: Nil = Enum(2)
    val jmpState = RegInit(jump0)

    when(enable) {
      switch(jmpState) {
        is(jump0) {
          jmpState := jump1
        }
        is(jump1) {
          jmpState := jump0
          globalState := fetch
          regs.pc := (regs.pc.asSInt() + jmpOffset).asUInt()
        }
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
    regs.psw.sign := byteALU.io.out.head(1).asBool()

    byteALU.io.out
  }

  private def readAbs(abs: UInt): UInt = {
    io.ramReadEn := true.B
    io.ramAddr   := abs

    io.ramReadData
  }

  private def writeAbs(abs: UInt, data: UInt): Unit = {
    io.ramWriteEn   := true.B
    io.ramAddr      := abs
    io.ramWriteData := data
  }

  private def readDp(dpAddr: UInt): UInt = {
    io.ramReadEn := true.B
    io.ramAddr   := Cat(regs.psw.page, dpAddr)

    io.ramReadData
  }

  private def writeDp(dpAddr: UInt, data: UInt): Unit = {
    io.ramWriteEn   := true.B
    io.ramAddr      := Cat(regs.psw.page, dpAddr)
    io.ramWriteData := data
  }

  private def pushStack(data: UInt): Unit = {
    io.ramWriteEn   := true.B
    io.ramAddr      := Cat(1.U, regs.sp)
    io.ramWriteData := data

    regs.sp := regs.sp - 1.U
  }

  private def popStack(): UInt = {
    regs.sp := regs.sp + 1.U
    io.ramReadEn := true.B
    io.ramAddr := Cat(1.U, regs.sp + 1.U)

    io.ramReadData
  }

  private def getBit(idx: UInt, byte: UInt): Bool = {
    val bits = byte.asBools()
    MuxLookup(idx, false.B, Seq(
      0.U -> bits(0),
      1.U -> bits(1),
      2.U -> bits(2),
      3.U -> bits(3),
      4.U -> bits(4),
      5.U -> bits(5),
      6.U -> bits(6),
      7.U -> bits(7),
    ))
  }

  private def setBit(src: UInt, bit: Bool, idx: UInt): UInt = {
    val srcBits = VecInit(src.asBools())
    val ret = Wire(UInt(8.W))

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

case class RegValue(a: Int, x: Int, y: Int, sp: Int, psw: Int, pc: Int)