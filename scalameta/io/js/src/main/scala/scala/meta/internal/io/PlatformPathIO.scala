package scala.meta.internal.io

import scala.meta.io._

object PlatformPathIO {
  def workingDirectory: AbsolutePath =
    if (PlatformIO.isNode) AbsolutePath(JSProcess.cwd())
    else AbsolutePath(fileSeparator)

  def fileSeparator: String =
    JSPath.sep

  def pathSeparator: String =
    JSPath.delimiter

  def isAbsolutePath(path: String): Boolean =
    JSPath.isAbsolute(path)

  def normalizePath(path: String): String =
    JSPath.normalize(path)

  def resolve(path1: AbsolutePath, path2: RelativePath): AbsolutePath =
    AbsolutePath(JSPath.resolve(path1.toString, path2.toString))

  def resolve(path1: RelativePath, path2: RelativePath): RelativePath =
    RelativePath(JSPath.resolve(path1.toString, path2.toString))
}
