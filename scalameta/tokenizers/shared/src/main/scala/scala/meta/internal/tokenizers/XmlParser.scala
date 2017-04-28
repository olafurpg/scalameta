package scala.meta.internal.tokenizers

/**
  * Copy-pasta from this lihaoyi comment:
  * https://github.com/scalameta/fastparse/pull/1#issuecomment-244940542*/
import scala.annotation.switch
import scala.meta.Dialect
import scala.meta.inputs.Input

import fastparse.all._

class XmlParser(Block: P0,
                Patterns: P0 = Fail,
                WL: P0 = CharsWhile(_.isWhitespace).opaque("whitespace")) {

  val XmlExpr: P0 = P( Xml.XmlContent.rep(min = 1, sep = WL.?) )
  val XmlPattern: P0 = P( Xml.ElemPattern )

  private[this] object Xml {
    val Element   = P( TagHeader ~/ ("/>" | ">" ~/ Content ~/ ETag ) ) // FIXME tag must be balanced
    val TagHeader = P( "<" ~ Name ~/ (WL ~ Attribute).rep ~ WL.? )
    val ETag      = P( "</" ~ Name ~ WL.? ~ ">" )

    val Attribute = P( Name ~ Eq ~/ AttValue )
    val Eq        = P( WL.? ~ "=" ~ WL.? )
    val AttValue  = P(
      "\"" ~/ (CharQ | Reference).rep ~ "\"" |
        "'" ~/ (CharA | Reference).rep ~ "'" |
        ScalaExpr
    )

    val Content        = P( (CharData | Reference | ScalaExpr | XmlContent).rep )
    val XmlContent: P0 = P( Unparsed | CDSect | PI | Comment | Element )

    val ScalaExpr = P( "{" ~ Block ~ "}" )

    val Unparsed = P( UnpStart ~/ UnpData ~ UnpEnd )
    val UnpStart = P( "<xml:unparsed" ~/ (WL ~ Attribute).rep ~ WL.? ~ ">" )
    val UnpEnd   = P( "</xml:unparsed>" )
    val UnpData  = P( (!UnpEnd ~ AnyChar).rep )

    val CDSect  = P( CDStart ~/ CData ~ CDEnd )
    val CDStart = P( "<![CDATA[" )
    val CData   = P( (!"]]>" ~ Char).rep )
    val CDEnd   = P( "]]>" )

    val Comment = P( "<!--" ~/ ComText ~ "-->" )
    val ComText = P( (!"--" ~ Char).rep ~ ("-" ~ &("--")).? )

    val PI         = P( "<?" ~ PITarget ~ PIProcText.? ~ "?>" )
    val PITarget   = P( !(("X" | "x") ~ ("M" | "m") ~ ("L" | "l")) ~ Name )
    val PIProcText = P( WL ~ (!"?>" ~ Char).rep )

    val Reference = P( EntityRef | CharRef )
    val EntityRef = P( "&" ~ Name ~/ ";" )
    val CharRef   = P( "&#" ~ Num ~/ ";" | "&#x" ~ HexNum ~/ ";" )
    val Num       = P( CharIn('0' to '9').rep )
    val HexNum    = P( CharIn('0' to '9', 'a' to 'f', 'A' to 'F').rep )

    val CharData = P( (!"{" ~ Char1 | "{{").rep(1) )

    val Char   = P( AnyChar )
    val Char1  = P( !("<" | "&") ~ Char )
    val CharQ  = P( !"\"" ~ Char1 )
    val CharA  = P( !"'" ~ Char1 )

    val Name      = P( NameStart ~ NameChar.rep )
    val NameStart = P( CharPred(isNameStart) ).opaque("NameStart")
    val NameChar  = P( CharPred(isNameChar) ).opaque("NameChar")

    val ElemPattern: P0 = P( TagPHeader ~/ ("/>" | ">" ~/ ContentP ~/ ETag ) )
    val TagPHeader      = P( "<" ~ Name ~ WL.?  )

    val ContentP: P0  = P( ( CharDataP | ScalaPatterns | ElemPattern ).rep )
    val ScalaPatterns = P( "{" ~ Patterns ~ "}" )
    val CharDataP     = P( "&" ~ CharData.? | CharData ) // matches weirdness of scalac parser on xml reference.

    //================================================================================
    // From `scala.xml.parsing.TokenTests`
    //================================================================================

    /**
     * {{{
     *  NameChar ::= Letter | Digit | '.' | '-' | '_' | ':'
     *             | CombiningChar | Extender
     *  }}}
     *  See [4] and Appendix B of XML 1.0 specification.
     */
    def isNameChar(ch: Char) = {
      import java.lang.Character._
      // The constants represent groups Mc, Me, Mn, Lm, and Nd.

      isNameStart(ch) || (getType(ch).toByte match {
        case COMBINING_SPACING_MARK |
          ENCLOSING_MARK | NON_SPACING_MARK |
          MODIFIER_LETTER | DECIMAL_DIGIT_NUMBER => true
        case _ => ".-:" contains ch
      })
    }

    /**
     * {{{
     *  NameStart ::= ( Letter | '_' )
     *  }}}
     *  where Letter means in one of the Unicode general
     *  categories `{ Ll, Lu, Lo, Lt, Nl }`.
     *
     *  We do not allow a name to start with `:`.
     *  See [3] and Appendix B of XML 1.0 specification
     */
    def isNameStart(ch: Char) = {
      import java.lang.Character._

      getType(ch).toByte match {
        case LOWERCASE_LETTER |
          UPPERCASE_LETTER | OTHER_LETTER |
          TITLECASE_LETTER | LETTER_NUMBER => true
        case _ => ch == '_'
      }
    }
  }
}

case class RangePosition(from: Int, to: Int)

/** Collects start and end positions of scala expressions inside xml literals.
  *
  * Doesn't really parse scala expressions, only reads until the curly brace
  * balance hits 0.
  */
class ScalaExprPositionParser(dialect: Dialect) extends Parser[Unit] {
  private val splicePositions = Seq.newBuilder[RangePosition]
  def getSplicePositions = splicePositions.result()

  def parseRec(cfg: ParseCtx, index: Int) = {
    var current = 0
    var curlyBraceCount = 1
    val scanner =
      new LegacyScanner(Input.String(cfg.input.slice(index, cfg.input.length)), dialect)
    scanner.reader.nextChar()
    while (curlyBraceCount > 0) {
      scanner.nextToken()
      current += scanner.curr.endOffset - scanner.curr.offset + 1
      (scanner.curr.token: @switch) match {
        case LegacyToken.LBRACE => curlyBraceCount += 1
        case LegacyToken.RBRACE => curlyBraceCount -= 1
        case _ =>
      }
    }
    current -= 1 // account for }
    splicePositions += RangePosition(index, index + current)
    success(cfg.success, (), index + current, Set.empty, false)
  }
}
