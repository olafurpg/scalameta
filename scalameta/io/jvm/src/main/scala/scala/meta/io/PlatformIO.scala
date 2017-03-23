package scala.meta.io

import java.io.File

object PlatformIO {
  def workingDirectory: AbsolutePath = AbsolutePath(sys.props("user.dir"))
  def read(path: AbsolutePath): String =
    scala.io.Source.fromFile(new File(path.path)).mkString
  def fileSeparator: String = File.separator
  def isAbsolutePath(path: String): Boolean = new File(path).isAbsolute
}
