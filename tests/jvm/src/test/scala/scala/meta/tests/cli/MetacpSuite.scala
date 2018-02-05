package scala.meta.tests.cli

import scala.meta.cli.Metacp

class MetacpSuite extends BaseCliSuite {
  test("scala-library") {
    val args = List(
      "-P:semanticdb:mode:slim",
      "-classpath",
      scalaLibraryJar
    )
    Metacp.process(args)
  }

}
