package scala.meta.tests.pretty

import scala.collection.GenIterable
import scala.meta.testkit.CorpusFile
import scala.meta.testkit.Phase
import scala.util.control.NonFatal

object SyntaxAnalysis {
  def run[T](corpus: GenIterable[CorpusFile])(f: CorpusFile => Unit): Unit =
    Phase.run("syntax analysis") {
      def analyze(file: CorpusFile): Unit = {
        try {
          f(file)
        } catch {
          case NonFatal(e) =>
            // unexpected errors are printed in the console.
            println(s"Unexpected error analysing file: $file")
            println(s"Error: ${e.getClass.getName} $e")
            val stack = e.getStackTrace.take(10) // print small stacktrace
            stack.foreach(println)
        }
      }
      corpus.foreach(analyze)
    }
}
