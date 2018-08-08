package scala.meta.internal.prettyprinters

import scala.meta.Tree
import scala.meta.prettyprinters.Pretty

object TreePretty {
  def apply[T <: Tree](settings: PrettySettings): Pretty[T] = {
    Pretty { (tree: T) =>
      ???
    }
  }
}
