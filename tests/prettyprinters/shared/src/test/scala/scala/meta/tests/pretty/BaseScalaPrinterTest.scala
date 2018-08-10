package scala.meta.tests.pretty

import scala.meta._
import scala.meta.parsers.Parse
import scala.collection.JavaConverters._
import org.scalameta.logger
import scala.util.control.NonFatal


abstract class BaseScalaPrinterTest extends DiffSuite with TestHelpers {

  def checkType(
      original: String,
      options: InternalOptions = defaultOptions
  ): Unit = {
    check(original, options.copy(parser = Parse.parseType))
  }

  def checkPat(
      original: String,
      options: InternalOptions = defaultOptions
  ): Unit = {
    check(original, options.copy(parser = Parse.parsePat))
  }

  def checkCase(
      original: String,
      options: InternalOptions = defaultOptions
  ): Unit = {
    check(original, options.copy(parser = Parse.parseCase))
  }

  def checkCaseStructural(
      original: String,
      options: InternalOptions = defaultOptions
  ): Unit = {
    checkStructural(original, options.copy(parser = Parse.parseCase))
  }

  def checkSource(
      original: String,
      options: InternalOptions = defaultOptions
  ): Unit = {
    check(
      original,
      options.copy(parser = Parse.parseSource, dialect = dialects.Scala212)
    )
  }

  def checkSourceStructural(
      original: String,
      options: InternalOptions = defaultOptions
  ): Unit = {
    checkStructural(
      original,
      options.copy(parser = Parse.parseSource, dialect = dialects.Scala212)
    )
  }

  def checkEnumerator(
      original: String,
      options: InternalOptions = defaultOptions
  ): Unit = {
    check(original, options.copy(parser = Parse.parseEnumerator))
  }

  def check(original: String, options: InternalOptions = defaultOptions): Unit = {
    check(original, original, options)
  }

  def checkStructural(
      original: String,
      options: InternalOptions = defaultOptions
  ): Unit = {
    checkStructural(original, original, options)
  }

  def check(original: String, expected: String): Unit = {
    check(original, expected, defaultOptions)
  }

  def checkStructural(
      original: String,
      expected: String,
      options: InternalOptions
  ): Unit = {
    checkFromString(
      original,
      expected,
      options,
      structuralOnly = true
    )
  }

  def check(
      original: String,
      expected: String,
      options: InternalOptions
  ): Unit = {
    checkFromString(
      original,
      expected,
      options,
      structuralOnly = false
    )
  }

  private def checkFromString(
      original2: String,
      expected2: String,
      options: InternalOptions,
      structuralOnly: Boolean
  ): Unit = {
    val original = original2.stripMargin.replace("'''", "\"\"\"")
    val expected = expected2.stripMargin.replace("'''", "\"\"\"")
    val testName = logger.revealWhitespace(original)
    test(testName) {
      val originalTree = options.getRoot(original)
      val formattedCode = printTree(originalTree, options)
      val formattedTree =
        try {
          options.getRoot(formattedCode)
        } catch {
          case NonFatal(e) => {
            throw new Exception("formattedCode: \n" + formattedCode, e)
          }
        }

      isSameTree(testName, originalTree, formattedTree) match {
        case Left(astDiff) =>
          sys.error(
            s"""|## AST changed ##
                |- diff -
                |$astDiff
                |
                |- formatted -
                |$formattedCode
                |
                |- expected -
                |$expected
                |
                |---------------------------------""".stripMargin
          )

        case Right(()) =>
          if (!structuralOnly) {
            assertNoDiff(formattedCode, expected)
            val reformattedCode = printTree(formattedTree, options)
            assertNoDiff(
              formattedCode,
              reformattedCode,
              "Idempotency violated!"
            )
          }
      }
    }
  }

  def check(input: Input): Unit = {
    checkTreeSource(input.parse[Source].get)
  }

  def checkTreeSource(root: Tree): Unit = {
    val testName = root.syntax
    val options = defaultOptions.copy(
      parser = Parse.parseSource,
      dialect = dialects.Scala212
    )

    test(testName) {
      val obtained = printTree(root, options)
      val root2 = options.getRoot(obtained).children.head
      isSameTree(testName, root, root2) match {
        case Left(astDiff) =>
          sys.error(
            s"""|## AST changed ##
                |- diff -
                |$astDiff
                |
                |- obtained -
                |${root2.syntax}
                |
                |${root2.structure} 
                |
                |- expected -
                |${root2.syntax}
                |
                |${root.structure}
                |
                |---------------------------------""".stripMargin
          )
        case Right(()) => ()
      }
    }
  }

  def check(tree: Tree, expected: String): Unit = {
    assertNoDiff(printTree(tree), expected)
  }

}
