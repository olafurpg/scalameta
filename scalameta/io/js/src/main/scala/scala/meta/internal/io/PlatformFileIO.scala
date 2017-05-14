package scala.meta.internal.io

import java.nio.charset.Charset
import scala.meta.io._

object PlatformFileIO {

  def listFiles(path: AbsolutePath): Array[RelativePath] = {
    val jsArray = JSFs.readdirSync(path.toString)
    val result = new Array[String](jsArray.length)
    jsArray.copyToArray(result)
    result
  }

  def readAllBytes(path: AbsolutePath): Array[Byte] = PlatformIO.inNodeJS {
    val jsArray = JSFs.readFileSync(path.toString)
    val len = jsArray.length
    val result = new Array[Byte](len)
    var curr = 0
    // Can't use copyToArray because of .toByte.
    while (curr < len) { result(curr) = jsArray(curr).toByte; curr += 1 }
    result
  }

  def slurp(path: AbsolutePath, charset: Charset): String = PlatformIO.inNodeJS {
    JSFs.readFileSync(path.toString, charset.toString)
  }
}
