package scala.meta.tests.pretty

import utest.framework._
import utest.runner._
import utest.ufansi

class PrettyprintersFramework extends Framework {
  override def formatSingle(
      path: Seq[String],
      r: Result
  ): Option[ufansi.Str] = {
    super.formatSingle(Seq(path.last), r)
  }
}
