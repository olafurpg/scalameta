package scala.meta.example
import scala.meta._

object NativeMain {
  def main(args: Array[String]): Unit = args.toList match {
    case path :: Nil =>
      val input = Input.File(AbsolutePath.fromAbsoluteOrRelative(path))
      val ast = input.parse[Source].get
      println(new String(input.chars))
      println(ast.structure)
    case _ =>
      ???
  }
}
