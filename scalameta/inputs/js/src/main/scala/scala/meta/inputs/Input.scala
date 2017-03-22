package scala.meta
package inputs

import org.scalameta.adt.{Liftables => AdtLiftables}
import org.scalameta.data._
import org.scalameta.invariants._
import scala.meta.common._
import scala.meta.internal.inputs._

trait Input extends Optional with Product with Serializable with InternalInput {
  def location: String = "<input>"
  def chars: Array[Char]
}

object Input {
  @none object None extends Input {
    lazy val chars = new Array[Char](0)
    override def toString = "Input.None"
  }

  @data class String(s: scala.Predef.String) extends Input {
    lazy val chars = s.toArray
    override def toString = "Input.String(\"" + s + "\")"
  }

  // NOTE: `start` and `end` are String.substring-style,
  // i.e. `start` is inclusive and `end` is not.
  // Therefore Slice.end can point to the last character of input plus one.
  @data class Slice(input: Input, start: Int, end: Int) extends Input {
    lazy val chars = input.chars.slice(start, end)
    override def toString = s"Input.Slice($input, $start, $end)"
  }

  implicit val charsToInput: Convert[Array[Char], Input] = Convert(chars => Input.String(new scala.Predef.String(chars)))
  implicit val stringToInput: Convert[scala.Predef.String, Input] = Convert(Input.String(_))
}

// NOTE: Need this code in this very file in order to avoid issues with knownDirectSubclasses.
// Without this, compilation order may unexpectedly affect compilation success.
private[meta] trait InputLiftables extends AdtLiftables {
  implicit lazy val liftInput: u.Liftable[Input] = u.Liftable[Input] { input =>
    import u._
    val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"
    q"_root_.scala.meta.inputs.Input.String(${new String(input.chars)})"
  }
}
