package scala.meta.cli

import scala.meta.internal.semanticdb.scalac.SemanticdbPlugin
import scala.tools.nsc.Settings
import scala.tools.nsc.Global
import org.scalameta.logger

object Metacp {

  def process(args: List[String]): Int = {
    val settings = new Settings()
    settings.processArguments(args, processAll = true)
    val global = new Global(settings)
    val mods = args.collect {
      case arg if arg.startsWith("-P:semanticdb:") => arg.stripPrefix("-P:semanticdb:")
    }
    val semanticdb = new SemanticdbPlugin(global)
    global.classPath.
    semanticdb.init(mods, settings.errorFn)
    logger.elem(settings, semanticdb.config.syntax)
    2
  }

}
