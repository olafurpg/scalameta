package scala.meta.internal.sbthost

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.collection.mutable
import scala.reflect.internal.util.Position
import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.reporters.Reporter
import scala.meta.internal.semantic.{schema => s}
import scala.reflect.internal.util.OffsetPosition
import scala.tools.nsc.reporters.StoreReporter

case class SbthostConfig(sourceroot: Path, targetroot: Path) {
  val metainf = targetroot.resolve("META-INF")
  val semanticdb = metainf.resolve("semanticdb")
  def relativePath(path: Path) = sourceroot.relativize(path)
  def semanticdbPath(relativePath: Path) = {
    val sibling =
      relativePath.getFileName.toString.stripSuffix(".scala") + ".semanticdb"
    semanticdb
      .resolve(relativePath)
      .resolveSibling(sibling)
      .toAbsolutePath
  }
}

class SbthostPlugin(val global: Global) extends Plugin {
  val name = "sbthost"
  val description = "Compiler plugin for sbt v1.0 migration."
  val components = List[PluginComponent](SbthostComponent)
  val config = SbthostConfig(
    sourceroot = Paths.get(sys.props("user.dir")),
    targetroot = Paths.get(
      global.settings.outputDirs.getSingleOutput
        .map(_.file.toURI)
        .getOrElse(new File(global.settings.d.value).toURI))
  )
  private object SbthostComponent extends PluginComponent {
    val global = SbthostPlugin.this.global
    import global._
    override val runsAfter = List("typer")
    override val runsRightAfter = Some("typer")
    val phaseName = "sbthost"
    def getMessages(source: SourceFile) =
      global.reporter match {
        case reporter: StoreReporter =>
          reporter.infos.withFilter(_.pos.source == source).map { info =>
            val range = info.pos match {
              case p: RangePosition => s.Range(p.start, p.end)
              case p: OffsetPosition => s.Range(p.start, -1)
              case _ => s.Range(-1, -1)
            }
            val severity = info.severity.id match {
              case 0 => s.Message.Severity.INFO
              case 1 => s.Message.Severity.WARNING
              case 2 => s.Message.Severity.ERROR
              case els => s.Message.Severity.UNKNOWN
            }
            s.Message(Some(range), severity, info.msg)
          }
        case els =>
          global.reporter.warning(NoPosition, s"Unknown reporter $els")
          Nil
      }
    override def newPhase(prev: Phase) = new StdPhase(prev) {
      def apply(unit: global.CompilationUnit): Unit = {
        val messages = getMessages(unit.source)
        val source = Paths.get(unit.source.file.file.getAbsoluteFile.toURI)
        val filename = config.relativePath(source)
        val attributes = s.Attributes(
          filename = filename.toString,
          contents = unit.source.content.mkString,
          dialect = "Scala210", // TODO
          names = Nil,
          messages = messages.toSeq,
          denotations = Nil,
          sugars = Nil
        )
        val outFile =
          config.semanticdbPath(filename)
        global.reporter.info(unit.targetPos,
                             s"""Persisting attributes for ${unit.source} to $outFile
                                |$attributes""".stripMargin,
                             true)
        val semanticdbOutFile = config.semanticdbPath(filename)
        semanticdbOutFile.toFile.getParentFile.mkdirs()
        global.reporter.info(NoPosition, semanticdbOutFile.toFile.exists().toString, true)
        Files.write(semanticdbOutFile, attributes.toByteArray)
        global.reporter.info(unit.targetPos, s"UNIT: $unit", true)
      }
    }
  }
}
