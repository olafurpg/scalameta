package scala.meta.internal.sbthost

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.collection.mutable.ListBuffer
import scala.meta.internal.semantic.{schema => s}
import scala.reflect.internal.util.OffsetPosition
import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.Global
import scala.tools.nsc.Phase

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
// NOTE: This is proof-of-concept to show that we can emit .semanticdb files
// for 2.10. The emitted data is not as accurate as in scalahost, my hypothesis
// is that it's still better than many other hacks.
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
    def jvmSignature(sym: MethodSymbol): String = {
      def encode(tpe: Type): String = {
        val TypeRef(_, sym, args) = tpe
        require(args.isEmpty || sym == definitions.ArrayClass)
        if (sym == definitions.UnitClass) "V"
        else if (sym == definitions.BooleanClass) "Z"
        else if (sym == definitions.CharClass) "C"
        else if (sym == definitions.ByteClass) "B"
        else if (sym == definitions.ShortClass) "S"
        else if (sym == definitions.IntClass) "I"
        else if (sym == definitions.FloatClass) "F"
        else if (sym == definitions.LongClass) "J"
        else if (sym == definitions.DoubleClass) "D"
        else if (sym == definitions.ArrayClass) "[" + encode(args.head)
        else "L" + sym.fullName.replace(".", "/") + ";"
      }
      val MethodType(params, ret) = sym.info.erasure
      val jvmRet = if (!sym.isConstructor) ret else definitions.UnitClass.toType
      "(" + params.map(param => encode(param.info)).mkString("") + ")" + encode(jvmRet)
    }
    def prettySymbol(symbol: Symbol): String = {
      val buffer = new StringBuilder
      buffer.append("_root_.")
      buffer.append(symbol.fullName)
      if (symbol.isMethod)
        buffer.append(jvmSignature(symbol.asMethod))
      buffer.append(".")
      buffer.toString()
    }
    def getNames(unit: global.CompilationUnit): Seq[s.ResolvedName] = {
      val buffer = ListBuffer.newBuilder[s.ResolvedName]
      def isValidSymbol(symbol: Symbol) =
        symbol.ne(null) && symbol != NoSymbol
      object traverser extends Traverser {
        override def traverse(tree: Tree): Unit = {
          if (tree.pos.isDefined && isValidSymbol(tree.symbol)) {
            val range = s.Range(tree.pos.point, -1)
            buffer += s.ResolvedName(Some(range), prettySymbol(tree.symbol))
          }
          super.traverse(tree)
        }
      }
      traverser(unit.body)
      buffer.result()
    }
    override def newPhase(prev: Phase) = new StdPhase(prev) {
      def apply(unit: global.CompilationUnit): Unit = {
        val source = Paths.get(unit.source.file.file.getAbsoluteFile.toURI)
        val filename = config.relativePath(source)
        val attributes = s.Attributes(
          filename = filename.toString,
          contents = unit.source.content.mkString,
          dialect = "Scala210", // TODO
          names = getNames(unit),
          messages = getMessages(unit.source).toSeq,
          denotations = Nil,
          sugars = Nil
        )
        val semanticdbOutFile = config.semanticdbPath(filename)
        semanticdbOutFile.toFile.getParentFile.mkdirs()
        Files.write(semanticdbOutFile, attributes.toByteArray)
      }
    }
  }
}
