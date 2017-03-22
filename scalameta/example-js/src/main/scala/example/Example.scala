package tutorial.webapp

import scala.meta._
import scala.meta.semantic.v1.Address
import scala.meta.semantic.v1.Database
import scala.meta.semantic.v1.Location
import scala.scalajs.js.JSApp

object TutorialApp extends JSApp {
  def main(): Unit = {
    case class Foo(a: Any)
    val x = Foo(2)
    x match {
      case Foo(b: Byte) => println("BYTE!")
      case Foo(b: Short) => println("SHORT!")
      case Foo(b) => println(s"ELSE: $b")
    }
//    val start = System.currentTimeMillis()
//    val code =
//      """|object a  {
//         |  val Foo(1) = 2
//         |}
//      """.stripMargin.parse[Stat].get
//    val end = System.currentTimeMillis()
//    val db = Database(
//      Map(Location(new Address.Snippet("val x = 2"), 1, 2) -> Symbol("_root_.a.")),
//      messages = Nil
//    )
//    val bytes = db.toBinary
//    val db2 = Database.fromBinary(bytes)
//    println(db)
//    println(db)
//    println(q"{ val x = 2; x }")
//    println(q"val x = 2".structure)
//    println(code.syntax)
//    println(code.structure)
//    logger.elem(end - start)
  }
}
