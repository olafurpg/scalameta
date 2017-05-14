package scala.meta.tests.io

import scala.meta.inputs.Input
import scala.meta.internal.io.FileIO
import scala.meta.internal.io.PathIO
import scala.meta.io.RelativePath

import org.scalatest.FunSuite

class IOSuite extends FunSuite {

  val buildSbt = RelativePath("build.sbt").toAbsolute

  test("PathIO.workingDirectory") {
    val obtained = PathIO.workingDirectory.toString
    // assuming we never run tests from root directory, check that the
    // returned value is not the default value "/" when running outside node.
    assert(obtained != "/")
  }

  test("PathIO.pathSeparator") {
    val obtained = PathIO.pathSeparator
    assert(obtained == ":" || obtained == ";")
  }

  test("PathIO.fileSeparator") {
    val obtained = PathIO.fileSeparator
    assert(obtained == "/" || obtained == "\\")
  }

  test("PathIO.isAbsolute") {
    val obtained = PathIO.isAbsolutePath(PathIO.workingDirectory.toString)
    assert(obtained)
  }

  test("FileIO.listFiles") {
    val obtained = FileIO.listFiles(PathIO.workingDirectory)
    org.scalameta.logger.elem(obtained.toList)
    assert(obtained.contains("build.sbt"))
    assert(!obtained.contains("."))
  }

  test("FileIO.readAllBytes") {
    val obtained = new String(FileIO.readAllBytes(buildSbt))
    assert(obtained.contains("project"))
  }

  test("Input.File.slurp") {
    val obtained = new String(Input.File(buildSbt).chars)
    assert(obtained.contains("project"))
  }

}
