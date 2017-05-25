package scala.meta
package paiges

import scala.meta.prettyprinters.Show
import org.scalameta.logger
import org.typelevel.paiges._

trait PrettyStructure[T] extends Show[T]

object PrettyStructure {
  case class PaigesOptions(maxColumn: Int = 80)
  object PaigesOptions {
    implicit val default = PaigesOptions()
  }
  import Doc._
  private val dot = text(".")
  private val leftParen = text("(")
  private val rightParen = text(")")
  private def toDoc(tree: Tree): Doc = tree match {
    case Lit(_) => str(tree.syntax)
    case Term.Name(name) => text(name)
    case Term.Select(qual, name) =>
      toDoc(qual) + dot + toDoc(name)
    case Term.Apply(fun, args) =>
      toDoc(fun) +
        Doc.intercalate(comma + lineOrSpace, args.map(toDoc)).bracketBy(leftParen, rightParen)
  }

  implicit def showPrettyStructure[T <: Tree]: PrettyStructure[T] =
    new PrettyStructure[T] {
      def apply(t: T): Show.Result = {
        val structure = t.show[Structure]
        val stat = structure.parse[Stat].get
        Show.Str(toDoc(stat).render(80))
      }
    }
}
