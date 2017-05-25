package scala.meta
package paiges
package test

import org.scalameta.logger
import org.scalatest.FunSuite

class PrettyStructureSuite extends FunSuite {
  def check(original: Tree, expected: String): Unit = {
    test(logger.revealWhitespace(original.syntax)) {
      val obtained = original.show[PrettyStructure].trim
//      logger.elem(obtained)
      assert(obtained == expected.trim)
    }
  }

  check(
    q"def foo[T <: S](a: Int = 2): List[T] = ???",
    // Ideally, I want single line formatting to be `foo(1)` instead of `foo( 1 )`.
    // I also would like there to be a line break between `, Seq`.
    """Defn.Def(
      |  Nil, Term.Name( "foo" ), Seq(
      |    Type.Param(
      |      Nil, Type.Name( "T" ), Nil, Type.Bounds( None, Some( Type.Name( "S" ) ) ),
      |      Nil, Nil
      |    )
      |  ), Seq(
      |    Seq(
      |      Term.Param(
      |        Nil, Term.Name( "a" ), Some( Type.Name( "Int" ) ), Some( Lit.Int( 2 ) )
      |      )
      |    )
      |  ), Some( Type.Apply( Type.Name( "List" ), Seq( Type.Name( "T" ) ) ) ),
      |  Term.Name( "???" )
      |)""".stripMargin
  )
}
