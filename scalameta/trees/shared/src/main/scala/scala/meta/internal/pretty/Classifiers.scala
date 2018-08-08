package scala.meta.internal.pretty

import scala.meta.internal.classifiers.classifier
import scala.meta.classifiers._
import scala.meta.tokens.Token._
import scala.meta.tokens.Token

@classifier
trait Trivia
object Trivia {
  def unapply(token: Token): Boolean = {
    token.is[Whitespace] || token.is[Comment] || token.is[BOF] || token.is[EOF]
  }
}

@classifier
trait Whitespace
object Whitespace {
  def unapply(token: Token): Boolean = {
    token.is[Space] || token.is[Tab] || token.is[CR] || token.is[LF] ||
      token.is[FF]
  }
}
