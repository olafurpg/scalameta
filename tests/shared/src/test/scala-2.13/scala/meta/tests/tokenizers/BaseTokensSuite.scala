package scala.meta.tests.tokenizers

import munit.FunSuite
import scala.meta.parsers.Parse
import scala.meta.internal.inputs._
import munit.TestOptions
import scala.meta.internal.trees.Origin

abstract class BaseTokensSuite extends FunSuite {
  import scala.meta._
  def defaultDialect: Dialect = dialects.Scala213

  def check[T <: Tree: Parse](code: TestOptions, expected: String): Unit = {
    test(code) {
      implicit val D = defaultDialect
      val tree = code.name.parse[T].get
      val tokens = tree.collect { case t =>
        assertNotEquals(t.origin, Origin.None: Origin)
        val syntax = t.syntax
        if (syntax.isEmpty) {
          s"${t.productPrefix}\n  ${t.pos.lineContent}\n  ${t.pos.lineCaret}"
        } else {
          s"${t.productPrefix} ${syntax}"
        }
      }
      val obtained = tokens.mkString("\n")
      assertNoDiff(obtained, expected)
    }
  }
}
