package scala.meta.cli

import java.nio.file.Files
import scala.tools.nsc.Settings
import scala.tools.nsc.Global
import org.scalameta.logger

object Metacp {

  def process(args: List[String]): Int = {
    val settings = new Settings()
    val out = Files.createTempDirectory("metacp")
    settings.processArguments(
      "-Xplugin-require:semanticdb" ::
        "-Xplugin:" + Metac.pluginClasspath ::
        args,
      processAll = true
    )
    settings.d.value = out.toString
    val g = new Global(settings)
    val run = new g.Run
    g.phase = run.parserPhase
    g.globalPhase = run.parserPhase
    pprint.log(g.rootMirror.RootClass.info.members)
    g.classPath.asURLs
    logger.elem(settings, g.classPath)
    2
  }

}
