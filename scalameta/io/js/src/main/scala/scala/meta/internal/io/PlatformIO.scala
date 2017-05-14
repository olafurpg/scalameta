package scala.meta.internal.io

import scala.meta.io._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.annotation.JSImport.Namespace

/** Facade for npm package "shelljs".
  *
  * @see https://www.npmjs.com/package/shelljs
  */
@js.native
@JSImport("shelljs", Namespace)
object JSShell extends js.Any {

  /** Returns the current directory. */
  def pwd(): js.Object = js.native
}

/** Facade for native nodejs module "fs".
  *
  * @see https://nodejs.org/api/fs.html
  */
@js.native
@JSImport("fs", Namespace)
object JSFs extends js.Any {

  /** Returns the file contents as Buffer using blocking APIs.
    *
    * NOTE. The actual return value is a nodejs Buffer, not a js.Array. However,
    * their APIs are the same for square bracket index access and .length.
    */
  def readFileSync(path: String): js.Array[Int] = js.native

  /** Returns the file contents as String using blocking APIs. */
  def readFileSync(path: String, encoding: String): String = js.native

  /** Writes file contents using blocking apis. */
  def writeFileSync(path: String, contents: String): Unit = js.native

  /** Writes file contents using blocking apis. */
  def readdirSync(path: String): js.Array[String] = js.native
}


/** Facade for native nodejs module "process".
  *
  * @see https://nodejs.org/api/process.html
  */
@js.native
@JSImport("process", Namespace)
object JSProcess extends js.Any {
  def cwd(): String = js.native
}
/** Facade for native nodejs module "path".
  *
  * @see https://nodejs.org/api/path.html
  */
@js.native
@JSImport("path", Namespace)
object JSPath extends js.Any {
  def sep: String = js.native
  def delimiter: String = js.native
  def isAbsolute(path: String): Boolean = js.native
  def resolve(paths: String*): String = js.native
  def normalize(path: String): String = js.native
}

object PlatformIO {
  private[io] def isNode = JSFs != null
  def inNodeJS[T](f: => T): T =
    if (PlatformIO.isNode) f
    else {
      throw new IllegalStateException("This operation is not supported in this environment.")
    }
}
