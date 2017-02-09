package scala.meta.internal
package scalahost

import java.io._
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins.PluginComponent
import scala.compat.Platform.EOL
import scala.meta.semantic.v1.Database
import scala.meta.internal.semantic.v1._
import scala.meta.internal.scalahost.v1.online.{Mirror => OnlineMirror}

import java.util.Properties

import org.scalameta.logger

trait ScalahostPipeline { self: ScalahostPlugin =>

  object ScalahostComponent extends PluginComponent {
    val global: ScalahostPipeline.this.global.type = ScalahostPipeline.this.global
    val runsAfter = List("typer")
    override val runsRightAfter = Some("typer")
    val phaseName = "scalameta"
    override val description = "compute the scala.meta semantic database"
    def newPhase(_prev: Phase) = new ScalahostPhase(_prev)
    val config = new Properties()
    sys.props
      .get("scalahost.properties")
      .map(new File(_))
      .filter(_.isFile)
      .foreach { file =>
        val configFile = file.getAbsoluteFile
        config.load(new FileInputStream(configFile))
        logger.debug(s"Loading custom configuration $configFile")
      }

    class ScalahostPhase(prev: Phase) extends StdPhase(prev) {
      override def apply(unit: g.CompilationUnit): Unit = {
        // TODO: compute and persist the database for every top-level class/module
      }

      override def run(): Unit = {
        super.run()
        val database = new OnlineMirror(global).database
        logger.elem(config.toString)
        if (config.get("writeToEachFile") != null) database.writeToEachFile()
        else {
          val databaseFile =
            new File(new File(global.settings.d.value), "semanticdb").getAbsoluteFile
          logger.debug(s"Writing database to $databaseFile")
          val prevDatabase = if (databaseFile.exists) Database(databaseFile) else Database()
          val mergedDatabase = prevDatabase.append(database)
          val allowedAddrs = global.currentRun.units.map(_.source.toAddr).toSet
          val trimmedDatabase = Database(
            mergedDatabase.symbols.filterKeys(k => allowedAddrs.contains(k.addr)))
          trimmedDatabase.toFile(databaseFile)
        }
      }
    }
  }
}
