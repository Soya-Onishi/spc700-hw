package spc700.core

import chisel3._

class Decoder extends Module {
  override val io = IO(new Bundle{
    val opcode = Input(UInt(8.W))
    val inst = Output(Instruction())
  })

  io.inst.opcode := io.opcode
  io.inst.ops := DontCare
  io.inst.addressing := DontCare
  (0x00 until 0xFF).foreach{op =>
    when(io.opcode === op.U) {
      io.inst.ops := Decoder.ops(op)
      io.inst.addressing := Decoder.addressing(op)
    }
  }
}

object Decoder {
  def ops(idx: Int): Ops.Type = {
    val upper = (idx >> 4) & 0x0F
    val lower = idx & 0x0F

    (upper, lower) match {
      case (upper,   0x0) =>
        upper match {
          case 0x0 => Ops.NOP
          case 0x1 => Ops.BPL
          case 0x2 => Ops.CLRP
          case 0x3 => Ops.BMI
          case 0x4 => Ops.SETP
          case 0x5 => Ops.BVC
          case 0x6 => Ops.CLRC
          case 0x7 => Ops.BVS
          case 0x8 => Ops.SETC
          case 0x9 => Ops.BCC
          case 0xA => Ops.EI
          case 0xB => Ops.BCS
          case 0xC => Ops.DI
          case 0xD => Ops.BNE
          case 0xE => Ops.CLRV
          case 0xF => Ops.BEQ
          case _   => throw new MatchError(s"upper must be between 0x0 to 0xF, actual: $upper")
        }
      case (    _,   0x1) => Ops.TCALL
      case (upper,   0x2) if upper % 2 == 0 => Ops.SET
      case (    _,   0x2) => Ops.CLR
      case (upper,   0x3) if upper % 2 == 0 => Ops.BBS
      case (    _,   0x3) => Ops.BBC
      case (upper, lower) if lower >= 0x4 && lower <= 0x9 =>
        upper match {
          case 0x0 | 0x1 => Ops.OR
          case 0x2 | 0x3 => Ops.AND
          case 0x4 | 0x5 => Ops.EOR
          case 0x6 | 0x7 => Ops.CMP
          case 0x8 | 0x9 => Ops.ADD
          case 0xA | 0xB => Ops.SUB
          case 0xC if lower ==  0x8 => Ops.CMP
          case _         => Ops.MOV
        }
      case (upper,   0xA) =>
        upper match {
          case 0x0 | 0x2 => Ops.OR
          case 0x1       => Ops.DEC
          case 0x3       => Ops.INC
          case 0x4 | 0x6 => Ops.AND
          case 0x5       => Ops.CMP
          case 0x7       => Ops.ADD
          case 0x8       => Ops.EOR
          case 0x9       => Ops.SUB
          case 0xA | 0xB | 0xC | 0xD | 0xF => Ops.MOV
          case 0xE       => Ops.NOT
          case _ => throw new MatchError(s"upper must be between 0x0 to 0xF, actual: $upper")
        }
      case (upper, lower) if lower >= 0xB && lower <= 0xC =>
        upper match {
          case 0x0 | 0x1 => Ops.ASL
          case 0x2 | 0x3 => Ops.ROL
          case 0x4 | 0x5 => Ops.LSR
          case 0x6 | 0x7 => Ops.ROR
          case 0x8 | 0x9 => Ops.DEC
          case 0xA | 0xB => Ops.INC
          case 0xC | 0xE => Ops.MOV
          case 0xD if lower == 0xB => Ops.MOV
          case 0xD       => Ops.DEC
          case 0xF if lower == 0xB => Ops.MOV
          case 0xF       => Ops.INC
          case _ => throw new MatchError(s"upper must be between 0x0 to 0xF, actual: $upper")
        }
      case (upper, 0xD) =>
        upper match {
          case 0x0 | 0x2 | 0x4 | 0x6 => Ops.PUSH
          case 0x1                   => Ops.DEC
          case 0x3                   => Ops.INC
          case 0x5 | 0x7 | 0x8 | 0x9 | 0xB | 0xC | 0xD | 0xF => Ops.MOV
          case 0xA                   => Ops.CMP
          case 0xE                   => Ops.NOTC
          case _ => throw new MatchError(s"upper must be between 0x0 to 0xF, actual: $upper")
        }
      case (upper, 0xE) =>
        upper match {
          case 0x0 => Ops.TSET
          case 0x2 => Ops.CBNE
          case 0x4 => Ops.TCLR
          case 0x6 => Ops.DBNZ
          case 0x9 => Ops.DIV
          case 0xB => Ops.DAS
          case 0xD => Ops.CBNE
          case 0xF => Ops.DBNZ
          case _ if upper <= 0x7 => Ops.CMP
          case _ if upper >= 0x8 => Ops.POP
          case _ => throw new MatchError(s"upper must be between 0x0 to 0xF, actual: $upper")
        }
      case (upper, 0xF) =>
        upper match {
          case 0x0 => Ops.BRK
          case 0x1 => Ops.JMP
          case 0x2 => Ops.BRA
          case 0x3 => Ops.CALL
          case 0x4 => Ops.PCALL
          case 0x5 => Ops.JMP
          case 0x6 => Ops.RET
          case 0x7 => Ops.RETI
          case 0x8 => Ops.MOV
          case 0x9 => Ops.XCN
          case 0xA => Ops.MOV
          case 0xB => Ops.MOV
          case 0xC => Ops.MUL
          case 0xD => Ops.DAA
          case 0xE => Ops.SLEEP
          case 0xF => Ops.STOP
        }
    }
  }

  def addressing(idx: Int): Addressing.Type = {
    val upper = (idx >> 4) & 0x0F
    val lower = idx & 0x0F

    (upper, lower) match {
      case (  0x0, 0x0) => Addressing.Special
      case (upper, 0x0) if upper % 2 == 1 => Addressing.Branch
      case (    _, 0x0) => Addressing.Special
      case (    _, 0x1) => Addressing.Special
      case (    _, 0x2) => Addressing.AbsRMWBit
      case (    _, 0x3) => Addressing.DpRel
      case (upper, 0x4) if upper % 2 == 0 => Addressing.DpCalc
      case (    _, 0x4) => Addressing.DpX
      case (upper, 0x5) if upper % 2 == 0 => Addressing.AbsCalc
      case (    _, 0x5) => Addressing.AbsX
      case (upper, 0x6) if upper % 2 == 0 => Addressing.IndX
      case (    _, 0x6) => Addressing.AbsY
      case (upper, 0x7) if upper % 2 == 0 => Addressing.DpIndX
      case (    _, 0x7) => Addressing.DpIndY
      case (upper, 0x8) if upper % 2 == 0 => Addressing.Imm
      case (    _, 0x8) => Addressing.DpImm
      case (upper, 0x9) if upper % 2 == 0 && upper <= 0xA => Addressing.DpDp
      case (upper, 0x9) if upper % 2 == 0 => Addressing.AbsCalc
      case (    _, 0x9) => Addressing.DpY
      case (upper, 0xA) if upper % 2 == 0 => Addressing.BitMan
      case (  0xF, 0xA) => Addressing.DpDp
      case (upper, 0xA) =>
        upper match {
          case 0x1 | 0x3             => Addressing.DpINCW
          case 0x5                   => Addressing.DpCMPW
          case 0x7 | 0x9 | 0xB | 0xD => Addressing.DpWord
        }
      case (upper, 0xB) if upper % 2 == 0 && upper <= 0xB => Addressing.DpRMW
      case (upper, 0xB) if upper <= 0xB => Addressing.DpXRMW
      case (  0xC, 0xB) => Addressing.DpCalc
      case (  0xD, 0xB) => Addressing.DpX
      case (  0xE, 0xB) => Addressing.DpCalc
      case (  0xF, 0xB) => Addressing.DpX
      case (upper, 0xC) if upper % 2 == 0 && upper <= 0xB => Addressing.AbsRMW
      case (upper, 0xC) if upper <= 0xB => Addressing.Acc
      case (  0xC, 0xC) => Addressing.AbsCalc
      case (  0xD, 0xC) => Addressing.Acc
      case (  0xE, 0xC) => Addressing.AbsCalc
      case (  0xF, 0xC) => Addressing.Acc
      case (upper, 0xD) =>
        upper match {
          case 0x0 | 0x2 | 0x4 | 0x6             => Addressing.Special
          case 0x1 | 0x3                         => Addressing.Acc
          case 0x5 | 0x7 | 0x9 | 0xB | 0xD | 0xF => Addressing.Acc
          case 0x8 | 0xA | 0xC                   => Addressing.Imm
          case 0xE                               => Addressing.PSWMan
        }
      case (upper, 0xE) =>
        upper match {
          case 0x0 => Addressing.AbsRMWBit
          case 0x1 => Addressing.AbsCalc
          case 0x2 => Addressing.Special
          case 0x3 => Addressing.DpCalc
          case 0x4 => Addressing.AbsRMWBit
          case 0x5 => Addressing.AbsCalc
          case 0x6 => Addressing.Special
          case 0x7 => Addressing.DpCalc
          case   _ => Addressing.Special
        }
      case (upper, 0xF) =>
        upper match {
          case 0x0 => Addressing.Special
          case 0x1 => Addressing.AbsJmp
          case 0x2 => Addressing.Branch
          case 0x3 => Addressing.Special
          case 0x4 => Addressing.Special
          case 0x5 => Addressing.AbsJmp
          case 0x6 => Addressing.Special
          case 0x7 => Addressing.Special
          case 0x8 => Addressing.DpImm
          case 0x9 => Addressing.Special
          case 0xA => Addressing.IndXInc
          case 0xB => Addressing.IndXInc
          case 0xC => Addressing.Special
          case 0xD => Addressing.Special
          case 0xE => Addressing.Special
          case 0xF => Addressing.Special
        }
    }
  }
}


