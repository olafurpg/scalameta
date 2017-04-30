package scala.meta.tests

import org.scalactic.Equality
import org.scalatest.FunSuiteLike
import org.scalatest.exceptions.TestFailedException

  /** Dummy diff helper since Google diffing library is Java only.  */
trait DiffAssertions extends FunSuiteLike {

  implicit val strEquals = new Equality[String] {
    override def areEqual(a: String, b: Any): Boolean = b match {
      case b: String =>
        assertNoDiff(a, b)
      case _ =>
        false
    }
  }

  case class DiffFailure(title: String, expected: String, obtained: String, diff: String)
      extends TestFailedException(title + "\n" + error2message(obtained, expected), 1)

  def header[T](t: T): String = {
    val line = s"=" * (t.toString.length + 3)
    s"$line\n=> $t\n$line"
  }

  def error2message(obtained: String, expected: String): String = {
    val sb = new StringBuilder
    if (obtained.length < 1000) {
      sb.append(s"""
                   #${header("Obtained")}
                   #${trailingSpace(obtained)}
         """.stripMargin('#'))
    }
    sb.append(s"""
                 #${header("Diff")}
                 #${trailingSpace(compareContents(obtained, expected))}
         """.stripMargin('#'))
    sb.toString()
  }

  def assertNoDiff(obtained: String, expected: String, title: String = ""): Boolean = {
    val result = compareContents(obtained, expected)
    if (result.isEmpty) true
    else {
      throw DiffFailure(title, expected, obtained, result)
    }
  }

  def trailingSpace(str: String): String = str.replaceAll(" \n", "∙\n")

  def compareContents(original: String, revised: String): String = {
    """${original.diff(revised)}${revised.diff(original)}"""
  }

}
