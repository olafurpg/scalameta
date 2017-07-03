package scala.meta.tests.io

import scala.meta.internal.io.PlatformFileIO
import scala.meta.internal.io.PlatformPathIO
import scala.meta.io.RelativePath
import org.scalatest.FunSuite

class JVMIOSuite extends FunSuite {

  // testing two files to assert the sha is different.

  test("FileIO.sha256(file1)") {
    val file1 =
      PlatformPathIO.workingDirectory.resolve(
        "scalameta/scalameta/jvm/src/test/resources/file1.txt")
    val obtained1 = PlatformFileIO.sha256(file1)
    assert(obtained1 == "6DCD640909856AB495B92D0B265FA94A6C8A0F8436435324404711A5B26E1427")
  }

  test("FileIO.sha257(file1)") {
    val file2 =
      PlatformPathIO.workingDirectory.resolve(
        "scalameta/scalameta/jvm/src/test/resources/file2.txt")
    val obtained2 = PlatformFileIO.sha256(file2)
    assert(obtained2 == "B58D6041072344AE2FAF75E0D52399DB58FE2583F191533529C5B6CEF9F25FD8")
  }

}
