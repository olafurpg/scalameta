package scala.meta.internal.sbthost

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.reflect.internal.util._

class SbthostPlugin(val global: Global) extends Plugin {
  val name = "sbthost"
  val description = "Compiler plugin for sbt v1.0 migration."
  val components = List[PluginComponent](SbthostComponent)
  private object SbthostComponent extends PluginComponent {
    val global = SbthostPlugin.this.global
    import global._
    override val runsAfter = List("typer")
    override val runsRightAfter = Some("typer")
    val phaseName = "sbthost"
    override def newPhase(prev: Phase) = new GlobalPhase(prev) {
      override def run() = {
        global.reporter.info(NoPosition, s"UNIT:!!!!", true)
        val count = global.currentRun.units.length
        val x: Iterator[Seq[String]] = global.currentRun.units.map(_.source.file.path.split("/"))
        var start = x.next()
        for (next <- x) {
          for (i <- next.indices) {
            if (i < start.length && start.lift(i) != next.lift(i)) {
              start = start.take(i)
            }
          }
        }
        val cwd = new java.io.File("").getAbsolutePath.split("/")
        if (start.startsWith(cwd)) start = start.drop(cwd.length)
        val msg = s"Compiling $count files in ${start.mkString("/")}"
        val spaces = " " * ((90 - msg.length) / 2)
        global.reporter.echo(
          scala.Console.BLUE_B +
            spaces + msg + spaces +
            scala.Console.RESET
        )
      }
      def name: String = phaseName
      def apply(unit: global.CompilationUnit): Unit = {
        
        global.reporter.info(unit.targetPos, s"UNIT: $unit", true)
      }
    }
  }
}
