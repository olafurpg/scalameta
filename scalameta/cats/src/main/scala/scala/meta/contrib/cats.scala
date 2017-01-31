package scala.meta
package contrib
import scala.language.implicitConversions

import _root_.cats._

object cats {
  implicit def scalametaEqInstance[A <: Tree](e: Equal[A]): Eq[A] =
    new Eq[A] { override def eqv(x: A, y: A): Boolean = e.equal(x, y) }
  def syntaxShow: Show[Tree]    = new Show[Tree] { override def show(f: Tree) = f.syntax    }
  def structureShow: Show[Tree] = new Show[Tree] { override def show(f: Tree) = f.structure }
}
