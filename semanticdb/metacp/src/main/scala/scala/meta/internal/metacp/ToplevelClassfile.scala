package scala.meta.internal.metacp

import scala.meta.internal.io._
import scala.meta.io._
import scala.tools.asm.tree._
import scala.tools.nsc

final case class ToplevelClassfile(
    base: AbsolutePath,
    path: AbsolutePath,
    node: ClassNode,
    global: nsc.Global) {
  def uri: String = PathIO.toUnix(path.toRelative(base).toString)
}
