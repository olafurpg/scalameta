package scala.meta.io

import PlatformIO._

sealed abstract class AbsolutePath(val str: String) extends Serializable {
  def /(other: String): AbsolutePath =
    new AbsolutePath(str + fileSeparator + other) {}
}

object AbsolutePath {
  def apply(path: String): Option[AbsolutePath] =
    if (isAbsolutePath(path)) Some(new AbsolutePath(path) {})
    else None
  def get(path: String): AbsolutePath =
    if (isAbsolutePath(path)) {
      new AbsolutePath(path) {}
    } else {
      throw new IllegalArgumentException(s"Path is not absolute! path=$path")
    }
}
