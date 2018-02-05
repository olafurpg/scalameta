package scala.meta.cli

import java.io._
import java.net._
import java.nio.channels._
import java.nio.file._
import scala.meta.internal.semanticdb.scalac._
import scala.tools.nsc.{Main => ScalacMain}

object Metac {
  def main(args: Array[String]): Unit = {
    sys.exit(process(args))
  }

  def process(args: Array[String]): Int = {
    val semanticdbArgs =
      Array("-Xplugin:" + pluginClasspath, "-Xplugin-require:semanticdb", "-Yrangepos")
    val stopAfterSemanticdb = Array("-Ystop-after:semanticdb-typer")
    val scalacArgs = args ++ semanticdbArgs ++ stopAfterSemanticdb
    ScalacMain.process(scalacArgs)
    if (ScalacMain.reporter.hasErrors) 1 else 0
  }

  def pluginClasspath: String = classOf[SemanticdbPlugin].getClassLoader match {
    case null =>
      val manifestDir = Files.createTempDirectory("semanticdb-scalac_")
      val resourceUrl = classOf[SemanticdbPlugin].getResource("/scalac-plugin.xml")
      val resourceChannel = Channels.newChannel(resourceUrl.openStream())
      val manifestStream = new FileOutputStream(manifestDir.resolve("scalac-plugin.xml").toFile)
      manifestStream.getChannel.transferFrom(resourceChannel, 0, Long.MaxValue)
      manifestStream.close()
      manifestDir.toString
    case cl: URLClassLoader => cl.getURLs.map(_.getFile).mkString(File.pathSeparator)
    case cl => sys.error(s"unsupported classloader: $cl")
  }

}
