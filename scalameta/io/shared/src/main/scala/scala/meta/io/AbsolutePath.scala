package scala.meta.io

import PlatformIO._

sealed abstract class AbsolutePath(val str: String) extends Serializable {
  override def toString: String = str.stripPrefix(workingDirectory.str + fileSeparator)
  def /(other: String): AbsolutePath =
    new AbsolutePath(str + fileSeparator + other) {}
}

object AbsolutePath {
  def apply(file: java.io.File): AbsolutePath =
    new AbsolutePath(file.getAbsolutePath) {}
  def apply(path: String): AbsolutePath =
    if (isAbsolutePath(path)) {
      new AbsolutePath(path) {}
    } else workingDirectory / path
}
