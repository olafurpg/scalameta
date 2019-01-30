package scala.meta.internal.bench

import java.nio.charset.StandardCharsets
import org.openjdk.jmh.annotations._
import scala.meta.io.AbsolutePath
import java.util.concurrent.TimeUnit
import scala.meta._
import scala.meta.internal.io.FileIO
import scala.meta.internal.io.InputStreamIO
import scala.meta.internal.io.PathIO

@State(Scope.Benchmark)
class Parser {

  var jars: List[AbsolutePath] = Nil

  var genjs: String = ""

  @Setup
  def setup(): Unit = {
    val bytes = InputStreamIO.readBytes(this.getClass.getResourceAsStream("/GenJSCode.scala"))
    genjs = new String(bytes, StandardCharsets.UTF_8)
    jars = LibrarySources.all.flatMap(_.sources.entries)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def parseSources(): Int = {
    var n = 0
    jars.foreach { jar =>
      FileIO.withJarFileSystem(jar, create = false, close = true) { root =>
        FileIO.listAllFilesRecursively(root).foreach { path =>
          if (PathIO.extension(path.toNIO) == "scala") {
            val text = FileIO.slurp(path, StandardCharsets.UTF_8)
//            n += text.linesIterator.length
            text
              .parse[Source]
              .toOption
              .foreach(_.traverse {
                case _ =>
                  n += 1
              })
          }
        }
      }
    }
    n
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.MILLISECONDS)
  def parseGenjs(): Unit = {
    genjs.parse[Source].get
  }
}

object Parser {
  def main(args: Array[String]): Unit = {
    val p = new Parser()
    p.setup()
    pprint.log(p.parseSources())
    sys.exit(0)
  }
}

import com.geirsson.coursiersmall.CoursierSmall
import com.geirsson.coursiersmall.Dependency
import com.geirsson.coursiersmall.Settings
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath

case class LibrarySources(
    name: String,
    classpath: Classpath,
    sources: Classpath
)

object LibrarySources {
  def all: List[LibrarySources] = {
    val settings = new Settings()
      .withDependencies(
        List(
          new Dependency(
            "org.scalameta",
            "scalameta_2.12",
            "4.1.0"
          ),
          new Dependency(
            "com.typesafe.akka",
            "akka-testkit_2.12",
            "2.5.9"
          ),
          new Dependency(
            "org.apache.spark",
            "spark-sql_2.11",
            "2.2.1"
          ),
          new Dependency(
            "org.eclipse.jetty",
            "jetty-servlet",
            "9.3.11.v20160721"
          ),
          new Dependency(
            "org.apache.kafka",
            "kafka_2.12",
            "1.0.0"
          ),
          new Dependency(
            "org.apache.flink",
            "flink-parent",
            "1.4.1"
          ),
          new Dependency(
            "io.grpc",
            "grpc-all",
            "1.10.0"
          ),
          new Dependency(
            "io.buoyant",
            "linkerd-core_2.12",
            "1.4.3"
          )
        )
      )
      .withClassifiers(List("sources", "_"))
    val jars = CoursierSmall.fetch(settings)
    val (sources, classpath) =
      jars.partition(_.getFileName.toString.endsWith("-sources.jar"))
    List(
      LibrarySources(
        "suite",
        Classpath(classpath.map(AbsolutePath(_))),
        Classpath(sources.map(AbsolutePath(_)))
      )
    )
  }
}
