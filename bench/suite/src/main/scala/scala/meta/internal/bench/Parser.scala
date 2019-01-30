package scala.meta.internal.bench

import java.nio.charset.StandardCharsets
import org.openjdk.jmh.annotations._
import scala.meta.io.AbsolutePath
import java.util.concurrent.TimeUnit
import scala.meta._
import scala.meta.internal.io.FileIO
import scala.meta.internal.io.PathIO

@State(Scope.Benchmark)
class Parser {

  var jars: List[AbsolutePath] = Nil

  @Setup
  def setup(): Unit = {
    jars = LibrarySources.all.flatMap(_.sources.entries)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def parseSources(): Int = {
    var nodes = 0
    jars.foreach { jar =>
      FileIO.withJarFileSystem(jar, create = false, close = true) { root =>
        FileIO.listAllFilesRecursively(root).foreach { path =>
          if (PathIO.extension(path.toNIO) == "scala") {
            val text = FileIO.slurp(path, StandardCharsets.UTF_8)
            text
              .parse[Source]
              .toOption
              .foreach(_.traverse {
                case _ =>
                  nodes += 1
              })
          }
        }
      }
    }
    nodes
  }
}

object Parser {
  def main(args: Array[String]): Unit = {
    val p = new Parser()
    p.setup()
    val nodes = p.parseSources()
    pprint.log(nodes)
  }
}
