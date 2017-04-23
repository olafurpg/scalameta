package scala.meta
package internal
package tokenizers

import scala.collection.mutable
import scala.meta.inputs._
import scala.meta.tokens._

import java.{util => ju}

object PlatformTokenizerCache {
  // NOTE: Manipulated by tokenization code in the ScalametaTokenizer class.
  // Caching just in toTokenize wouldn't be enough, because someone could call the tokenizer directly.
  val megaCache: ju.Map[Dialect, mutable.Map[Input, Tokens]] =
    new ju.HashMap[Dialect, mutable.Map[Input, Tokens]]()
  val miniCacheSyncRoot = new Object
  def newUnsyncResult: mutable.Map[Input, Tokens] = mutable.HashMap.empty[Input, Tokens]
  def putIfAbsent(dialect: Dialect,
                  cache: mutable.Map[Input, Tokens]): mutable.Map[Input, Tokens] =
    megaCache.putIfAbsent(dialect, cache)
}
