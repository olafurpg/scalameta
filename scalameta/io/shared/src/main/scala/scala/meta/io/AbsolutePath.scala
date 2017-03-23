package scala.meta.io

import PlatformIO._

sealed abstract class AbsolutePath(val path: String) extends Serializable {
  override def toString: String = path.stripPrefix(workingDirectory.path + fileSeparator)
  def /(other: String): AbsolutePath =
    new AbsolutePath(path + fileSeparator + other) {}
}

object AbsolutePath {
  def unapply(arg: AbsolutePath): Option[String] = Some(arg.path)
  def apply(file: java.io.File): AbsolutePath =
    new AbsolutePath(file.getAbsolutePath) {}
  def apply(path: String): AbsolutePath =
    if (isAbsolutePath(path)) {
      new AbsolutePath(path) {}
    } else workingDirectory / path
}
