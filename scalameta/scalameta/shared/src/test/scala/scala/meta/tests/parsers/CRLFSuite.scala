package scala.meta
package tests.parsers

import org.scalameta.logger
import org.scalatest.FunSuite

class CRLFSuite extends FunSuite {

  test("file with CRLF parses OK") {
    val CRLF = " s\"nope\"\r\n "
    val noCRLF = " s\"nope\"\n "
    def reveal(a: String) = logger.revealWhitespace(a)
    def d(tokens: Tokens) = tokens.map(x => s"${x.productPrefix} ${reveal(x.structure)}").mkString
    println(d(CRLF.tokenize.get))
    println(d(noCRLF.tokenize.get))
  }

}
