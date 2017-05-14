package scala.meta.internal.io

import java.nio.charset.Charset
import java.nio.file.Files
import scala.meta.io._

object PlatformFileIO {
  def listFiles(path: AbsolutePath): Array[RelativePath] =
    path.toFile.listFiles().map { x =>
      org.scalameta.logger.elem(x)
      RelativePath(x)
    }

  def readAllBytes(path: AbsolutePath): Array[Byte] =
    Files.readAllBytes(path.toNIO)
  def slurp(path: AbsolutePath, charset: Charset): String =
    new String(readAllBytes(path), charset)
}
