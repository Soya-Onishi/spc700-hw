import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import spc700.Spc700

object Main extends App {
  (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => new Spc700)))
}
