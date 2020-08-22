package spc700

import java.nio.file.Files
import java.nio.file.Paths
import scala.collection.JavaConverters._
import chisel3.iotesters.{PeekPokeTester, Driver, ChiselFlatSpec}


class CoreTest(chip: Spc700) extends PeekPokeTester(chip) {
  val timerPattern = "([01]),([0-9a-fA-F]+)".r

  val regPaths   = Paths.get("./spc/reg.hex")
  val timerPaths = Paths.get("./spc/timer.hex")
  val regs = Files.readAllLines(regPaths).asScala.toVector.map(Integer.parseInt(_, 16))
  val (timerEnables, timerDividers) = Files.readAllLines(timerPaths)
    .asScala
    .toVector
    .map(str => timerPattern.findAllIn(str).matchData.map(m => (m.group(0), m.group(1))).toVector.head)
    .map{ case (en, divider) => (Integer.parseInt(en), Integer.parseInt(divider, 16)) }
    .unzip

  val Vector(pc, a, x, y, sp, psw) = regs


  poke(chip.io.pc, pc)
  poke( chip.io.a,  a)
  poke( chip.io.x,  x)
  poke( chip.io.y,  y)
  poke(chip.io.sp, sp)
  poke(chip.io.psw,psw)

  timerEnables.zip(chip.io.initTimerEn).foreach{ case (en, timer) => poke(timer, en) }
  timerDividers.zip(chip.io.initTimerDivider).foreach{ case (div, timer) => poke(timer, div) }
  reset()
  step(100)
}

class CoreTester extends ChiselFlatSpec {
  behavior of "Core"
  backends foreach { backend =>
    it should s"10000 cycles works correctly in $backend" in {
      Driver(() => new Spc700, backend)(c => new CoreTest(c)) should be (true)
    }
  }
}
