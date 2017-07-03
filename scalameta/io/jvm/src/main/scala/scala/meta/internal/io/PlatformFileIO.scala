package scala.meta.internal.io

import java.io.ByteArrayInputStream
import java.io.InputStream
import java.net.URI
import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Path
import java.security.DigestInputStream
import java.security.MessageDigest
import java.util.stream.Collectors
import javax.xml.bind.DatatypeConverter
import scala.io.Source
import scala.meta.io._

object PlatformFileIO {
  def readAllBytes(uri: URI): Array[Byte] =
    InputStreamIO.readBytes(uri.toURL.openStream())

  def readAllBytes(path: AbsolutePath): Array[Byte] =
    Files.readAllBytes(path.toNIO)

  def slurp(path: AbsolutePath, charset: Charset): String =
    scala.io.Source.fromFile(path.toFile)(scala.io.Codec(charset)).mkString

  def listFiles(path: AbsolutePath): ListFiles =
    new ListFiles(path, Option(path.toFile.list()).toList.flatten.map(RelativePath.apply))

  def isFile(path: AbsolutePath): Boolean =
    Files.isRegularFile(path.path)

  def isDirectory(path: AbsolutePath): Boolean =
    Files.isDirectory(path.path)

  def listAllFilesRecursively(root: AbsolutePath): ListFiles = {
    import scala.collection.JavaConverters._
    val relativeFiles = Files
      .walk(root.toNIO)
      .collect(Collectors.toList[Path])
      .asScala
      .collect {
        case path if Files.isRegularFile(path) =>
          RelativePath(root.path.relativize(path))
      }
    new ListFiles(root, relativeFiles)
  }

  def sha256(path: AbsolutePath): String = {
    sha256(Files.newInputStream(path.toNIO))
  }

  def sha256(input: String): String = {
    sha256(new ByteArrayInputStream(input.getBytes))
  }

  def sha256(is: InputStream): String = {
    val algorithm = MessageDigest.getInstance("SHA-256")
    val dis = new DigestInputStream(is, algorithm)
    try {
      Source.fromInputStream(dis).foreach(_ => ())
      val bytes = algorithm.digest()
      DatatypeConverter.printHexBinary(bytes)
    } finally {
      dis.close()
    }
  }
}
