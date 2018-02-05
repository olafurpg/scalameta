package scala.meta.cli.metacp

import java.nio.file.Files
import java.nio.file.Path
import scala.tools.scalap.Main
import scala.tools.scalap.scalax.rules.scalasig.ByteCode
import scala.tools.scalap.scalax.rules.scalasig.ScalaSigParser
import scala.tools.scalap.scalax.rules.scalasig._
import scala.util.control.NonFatal
import org.langmeta.internal.io.FileIO
import org.langmeta.internal.io.PathIO
import org.langmeta.io.AbsolutePath
import org.langmeta.io.Classpath


object Metacp {

  def process(classpath: String): Int = {
    pprint.log(classpath)
    for {
      path <- Classpath(classpath).shallow
      root = IOUtil.rootPath(path.toNIO)
      if Files.isDirectory(root)
      path <- FileIO.listAllFilesRecursively(AbsolutePath(root))
      if PathIO.extension(path.toNIO) == "class"
    } yield {
      try {
        handlePath(path.toNIO)
      } catch {
        case NonFatal(e) =>
        // new Exception(s"Error handling $path", e) with NoStackTrace{}.printStackTrace()
      }
    }
    1
  }

  def handlePath(path: Path): Unit = {
    val bytes = Files.readAllBytes(path)
    val byteCode = ByteCode(bytes)
    val classFile = ClassFileParser.parse(byteCode)
    Main.decompileScala()
    ScalaSigParser.parse(classFile) match {
      case Some(sig) =>
        pprint.log(sig)
      case None =>
        for {
          method <- classFile.methods
        } {
          val name =
            new String(byteCode.bytes, method.nameIndex, method.descriptorIndex - method.nameIndex)
          pprint.log(name)

        }
      // Ignore
    }

  }

}
