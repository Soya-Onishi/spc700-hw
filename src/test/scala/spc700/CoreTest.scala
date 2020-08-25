package spc700

import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.JavaConverters._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}
import spc700.core.RegValue


class CoreTest(chip: Spc700) extends PeekPokeTester(chip) {
  step(100)
}

class CoreTester extends ChiselFlatSpec {
  val timerPattern = "([01]),([0-9a-fA-F]+)".r

  val regPaths   = Paths.get("./spc/reg.hex")
  val timerPaths = Paths.get("./spc/timer.hex")
  val regs = Files.readAllLines(regPaths).asScala.toVector.map(Integer.parseInt(_, 16))
  val (timerEnables, timerDividers) = Files.readAllLines(timerPaths)
    .asScala
    .toVector
    .map(str => timerPattern.findFirstMatchIn(str).map(m => (m.group(1), m.group(2))).toVector.head)
    .map{ case (en, divider) => (Integer.parseInt(en), Integer.parseInt(divider, 16)) }
    .unzip

  val Vector(reg_pc, reg_a, reg_x, reg_y, reg_sp, reg_psw) = regs

  val reg = RegValue(reg_a, reg_x, reg_y, reg_sp, reg_psw, reg_pc)

  behavior of "Core"
  it should s"10000 cycles works correctly in verilator" in {
    Driver(() => new Spc700(reg), "verilator")(c => new CoreTest(c)) should be (true)
  }
}
