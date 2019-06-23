package scala.meta
package internal
package tokenizers

import LegacyToken._
import Chars._
import scala.meta.inputs._
import scala.annotation.tailrec

trait LegacyTokenData {
  /** the input that is currently being tokenized */
  var input: Input = null

  /** the next token */
  var token: LegacyToken = EMPTY

  /** the offset of the first character of the current token */
  var offset: Offset = 0

  /** the offset of the character following the token preceding this one */
  var lastOffset: Offset = 0

  /** the offset of the last character of the current token */
  var endOffset: Offset = 0

  /** the name of an identifier */
  var name: String = null

  /** the string value of a literal */
  var strVal: String = null

  /** the base of a number */
  var base: Int = 0

  def copyFrom(td: LegacyTokenData): this.type = {
    this.input = td.input
    this.token = td.token
    this.offset = td.offset
    this.lastOffset = td.lastOffset
    this.endOffset = td.endOffset
    this.name = td.name
    this.strVal = td.strVal
    this.base = td.base
    this
  }

  override def toString = s"{token = $token, position = $offset..$endOffset, lastOffset = $lastOffset, name = $name, strVal = $strVal, base = $base}"

  lazy val reporter: Reporter = Reporter(input)
  import reporter._

      /** Convert current strVal to char value
     */
    def charVal: Char = if (strVal.length > 0) strVal.charAt(0) else 0

    /** Convert current strVal, base to long value.
     *  This is tricky because of max negative value.
     *
     *  Conversions in base 10 and 16 are supported. As a permanent migration
     *  path, attempts to write base 8 literals except `0` emit a verbose error.
     */
    def intVal(negated: Boolean): Long = {
      def malformed: Long = {
        if (base == 8) syntaxError("Decimal integer literals may not have a leading zero. (Octal syntax is obsolete.)", at = offset)
        else syntaxError("malformed integer number", at = offset)
        0
      }
      def tooBig: Long = {
        syntaxError("integer number too large", at = offset)
        0
      }
      def intConvert: Long = {
        val len = strVal.length
        if (len == 0) {
          if (base != 8) syntaxError("missing integer number", at = offset)  // e.g., 0x;
          0
        } else {
          val divider     = if (base == 10) 1 else 2
          val limit: Long = if (token == LONGLIT) Long.MaxValue else Int.MaxValue
          @tailrec def convert(value: Long, i: Int): Long =
            if (i >= len) value
            else {
              val c = strVal.charAt(i)
              if (isNumberSeparator(c)) convert(value, i + 1)
              else {
                val d = digit2int(c, base)
                if (d < 0)
                  malformed
                else if (value < 0 ||
                    limit / (base / divider) < value ||
                    limit - (d / divider) < value * (base / divider) &&
                    !(negated && limit == value * base - 1 + d))
                  tooBig
                else
                  convert(value * base + d, i + 1)
              }
            }
          val result = convert(0, 0)
          if (base == 8) malformed else if (negated) -result else result
        }
      }
      if (token == CHARLIT && !negated) charVal.toLong else intConvert
    }

    def intVal: Long = intVal(negated = false)
    def longVal: Long = intVal

    private val zeroFloat = raw"[0.]+(?:[eE][+-]?[0-9]+)?[fFdD]?".r

    /** Convert current strVal, base to float value.
     */
    def floatVal(negated: Boolean): Float = {
      val text = removeNumberSeparators(strVal)
      try {
        val value: Float = java.lang.Float.parseFloat(text)
        if (value > Float.MaxValue)
          syntaxError("floating point number too large", at = offset)
        if (value == 0.0f && !zeroFloat.pattern.matcher(text).matches)
          syntaxError("floating point number too small", at = offset)
        if (negated) -value else value
      } catch {
        case _: NumberFormatException =>
          syntaxError("malformed floating point number", at = offset)
          0.0f
      }
    }

    def floatVal: Float = floatVal(negated = false)

    /** Convert current strVal, base to double value.
     */
    def doubleVal(negated: Boolean): Double = {
      val text = removeNumberSeparators(strVal)
      try {
        val value: Double = java.lang.Double.parseDouble(text)
        if (value > Double.MaxValue)
          syntaxError("double precision floating point number too large", at = offset)
        if (value == 0.0d && !zeroFloat.pattern.matcher(text).matches)
          syntaxError("double precision floating point number too small", at = offset)
        if (negated) -value else value
      } catch {
        case _: NumberFormatException =>
          syntaxError("malformed double precision floating point number", at = offset)
          0.0
      }
    }

  def doubleVal: Double = doubleVal(negated = false)

  @inline private def isNumberSeparator(c: Char): Boolean = c == '_' //|| c == '\''

  @inline private def removeNumberSeparators(s: String): String =
    if (s.indexOf('_') > 0) s.replaceAllLiterally("_", "") /*.replaceAll("'","")*/ else s
}