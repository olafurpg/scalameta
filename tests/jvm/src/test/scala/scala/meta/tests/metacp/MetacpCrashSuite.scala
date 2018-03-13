package scala.meta.tests.metacp

import scala.meta._
import scala.meta.cli.Metacp
import scala.meta.metacp.Reporter
import scala.meta.metacp.Settings

// TODO(olafur): move to slow/integration tests.
class MetacpCrashSuite extends BaseMetacpSuite {

  checkMetacp("scala-library", () => scalaLibraryClasspath)
  checkMetacp(scalameta)
  checkMetacp(akka)
  checkMetacp(spark)
  checkMetacp(jdk)

}
class XXX extends BaseMetacpSuite {
  checkMetacp(akkaStream211)
}
