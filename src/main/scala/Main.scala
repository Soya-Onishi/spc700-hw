import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import spc700.Spc700
import spc700.core.RegValue

object Main extends App {
  val reg = RegValue(0, 0, 0, 0, 0, 0)

  (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => new Spc700(reg))))
}
