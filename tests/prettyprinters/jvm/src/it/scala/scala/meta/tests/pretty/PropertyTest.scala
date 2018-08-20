package scala.meta.tests.pretty

import java.nio.charset.StandardCharsets
import java.nio.file._
import java.util.concurrent.atomic.AtomicInteger
import me.tongfei.progressbar.{ProgressBarStyle, ProgressBar => PB}
import org.scalameta.logger
import org.scalatest.tagobjects.Slow
import scala.meta._
import scala.meta.tests.BuildInfo
import scala.meta.testkit.Corpus
import scala.collection.concurrent.TrieMap
import scala.util.control.NonFatal

sealed trait PropertyResult
case object Success extends PropertyResult
case class Failure(explanation: String) extends PropertyResult

abstract class PropertyTest(name: String) extends org.scalatest.FunSuite {

  def check(file: Input.File, url: String): PropertyResult

  private val failed = TrieMap.empty[String, Boolean]
  private val regressions = TrieMap.empty[String, Boolean]
  private val nl = "\n"
  private val root = BuildInfo.resourceDirectory.toPath

  private val coverageFile = root.resolve(s"coverage-${name}.txt")
  private val todoFile = root.resolve(s"todo-${name}.diff")

  if (Files.exists(todoFile)) {
    Files.delete(todoFile)
  }

  private val previouslyFailed: Set[String] = {
    val input = new String(Files.readAllBytes(coverageFile))
    input.split(nl).filterNot(_ == "").toSet
  }

  private def fileList(in: TrieMap[String, Boolean], sep: String): String =
    in.keys.toSeq.sorted.mkString("", sep, sep)
  val prefix = Paths.get("target", "repos")

  test(name, Slow) {
    val failureCount = new AtomicInteger(0)
    val successCount = new AtomicInteger(0)

    val corpus = Corpus
      .files(Corpus.fastparse)
      .toBuffer
      .par

    val progress = new PB(
      "Formatting",
      corpus.size,
      1000,
      System.out,
      ProgressBarStyle.UNICODE_BLOCK
    )
    progress.start()

    SyntaxAnalysis.run[Unit](corpus) { file =>
      try {
        val jFile = file.jFile
        val input = Input.File(jFile, StandardCharsets.UTF_8)
        val githubUrl = file.githubUrl

        check(input, githubUrl) match {
          case Success => successCount.incrementAndGet()
          case Failure(explanation) => {
            val failures = failureCount.incrementAndGet()
            failed(githubUrl) = true

            if (failures < 100) {
              logger.elem(explanation)
            }

            val relpath = prefix.relativize(file.jFile.toPath).toString

            if (!previouslyFailed.contains(githubUrl) &&
                !previouslyFailed.contains(relpath)) {
              regressions(githubUrl) = true
              print(Console.RED)
              println("*************************")
              println("Regression: " + file.jFile)
              println("*************************")
              print(Console.RESET)
            } else {
              Files.write(
                todoFile,
                (explanation + nl).toString.getBytes("utf-8"),
                StandardOpenOption.CREATE,
                StandardOpenOption.APPEND
              )
            }
          }
        }
      } catch {
        case NonFatal(_) => ()
        case _: StackOverflowError =>
          println(s"\nStack overflow at ${file.githubUrl}")
      }

      progress.synchronized {
        progress.step()

        val currentFailures = failureCount.get
        val currentSuccess = successCount.get

        val progressMessage =
          if (currentFailures > 100) {
            val rate = (currentSuccess.toDouble / (currentFailures + currentSuccess).toDouble) * 100.0
            f"Success: ${rate}%.2f%%"
          } else {
            s"Failures: $currentFailures"
          }

        progress.setExtraMessage(progressMessage)
      }
    }

    progress.stop()

    if (regressions.isEmpty) {
      Files.write(
        coverageFile,
        fileList(failed, nl).getBytes("utf-8"),
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
    }

    if (regressions.nonEmpty) {
      val sep = nl + "  "
      val regressionList = fileList(regressions, sep)
      sys.error("Regressions:" + sep + regressionList)
    }
  }
}
