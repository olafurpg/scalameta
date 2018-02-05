package scala.meta.tests.cli

import org.scalatest.FunSuite

import scala.meta.cli.metacp.Metacp

class MetacpSuite extends FunSuite {
  val scalaLibraryJar = sys.props("sbt.paths.scalalibrary.classes")
  if (scalaLibraryJar == null) sys.error("sbt.paths.scalalibrary.classes not set. broken build?")

  test("scala-library") {
    Metacp.process(
      "/Users/ollie/.coursier/cache/v1/https/repo1.maven.org/maven2/com/google/protobuf/protobuf-java/3.5.0/protobuf-java-3.5.0.jar")
  }

}
