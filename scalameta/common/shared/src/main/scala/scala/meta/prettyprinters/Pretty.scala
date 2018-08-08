package scala.meta
package prettyprinters

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "don't know how to show[Syntax] for ${T}")
trait Pretty[T] extends Show[T]
object Pretty {
  def apply[T](f: T => Show.Result): Pretty[T] = new Pretty[T] { def apply(input: T) = f(input) }
}
