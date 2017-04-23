package scala.meta.example
import scala.meta._

object NativeMain {
  def main(args: Array[String]): Unit = args.toList match {
    case path :: Nil =>
      val input = Input.File(AbsolutePath.fromAbsoluteOrRelative(path))
      val ast = dialects.Sbt0137(input).tokenize.get
      println(new String(input.chars))
      println(ast.structure)
//      println(args.toList)
      println(q"val x = 2".structure)
    case _ =>
      ???
  }
}
