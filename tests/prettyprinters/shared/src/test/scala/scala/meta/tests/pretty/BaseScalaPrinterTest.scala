package scala.meta.tests.pretty

import scala.meta._
import scala.meta.parsers.Parse
import scala.meta.testkit.AnyDiff
import scala.meta.testkit.StructurallyEqual
import scala.meta.transversers.Transformer
import scala.collection.JavaConverters._
import difflib.DiffUtils
import org.scalameta.logger
import scala.meta.internal.pretty.TreePrinter
import scala.meta.internal.trees.Origin
import scala.util.control.NonFatal

case class InternalOptions(
    maxColumn: Int,
    dialect: Dialect = dialects.Scala212,
    parser: Parse[_ <: Tree] = Parse.parseSource
) {
  def getRoot(code: String): Tree = {
    dialect(code).parse(parser).get
  }
}

object TestMethods extends TestMethods
trait TestMethods {

  val defaultOptions: InternalOptions = InternalOptions(100).copy(
    dialect = dialects.Sbt1.copy(
      allowTypeLambdas = true,
      allowAndTypes = true,
      allowImplicitFunctionTypes = true
    ),
    parser = Parse.parseStat
  )

  val dotty = defaultOptions.copy(dialect = dialects.Dotty)

  private val options = defaultOptions

  def prettyPrint(tree: Tree): String =
    TreePrinter.printTree(tree).render(options.maxColumn)

  def unified(filename: String, original: String, modified: String): String = {
    def lines(s: String) = s.replaceAllLiterally("\r\n", "\n").split("\r").toSeq.asJava
    val diff = difflib.DiffUtils.diff(lines(original), lines(modified))
    DiffUtils
      .generateUnifiedDiff(
        filename,
        filename + "-formatted",
        lines(original),
        diff,
        3
      )
      .asScala
      .mkString("\n")
  }
  def getDiff(filename: String, original: Tree, modified: Tree): String = {
    val originalCode = original.withOrigin(Origin.None).syntax
    val modifiedCode = modified.withOrigin(Origin.None).syntax
    val result = unified(filename, originalCode, modifiedCode)
    result
  }

  def printTree(root: Tree, options: InternalOptions = defaultOptions): String = {
    TreePrinter.printTree(root).render(options.maxColumn)
  }

  object normalizeTransform extends Transformer {
    import scala.meta._

    val transform: PartialFunction[Tree, Tree] = {
      case Source(List(t)) => t
      case Term.Block(a :: Nil) if !a.is[Defn] => a
      case Term.ApplyInfix(lhs, op, targs, args) =>
        if (targs.isEmpty) q"$lhs.$op(..$args)"
        else q"$lhs.$op[..$targs](..$args)"
      case Term.Block((f @ Term.PartialFunction(_)) :: Nil) =>
        f
      case f @ Term.Function(_, Term.Block(_ :: _ :: _)) if !f.parent.exists(_.is[Term.Block]) =>
        Term.Block(f :: Nil)
    }

    override def apply(tree: Tree): Tree = {
      super.apply(transform.lift(tree).map(this.apply).getOrElse(tree))
    }
  }

  def normalize(tree: Tree): Tree = {
    val input = tree.tokens.head.input match {
      case scala.meta.Input.VirtualFile(path, _) => path
      case _ => "<input>"
    }
    try {
      normalizeTransform(tree)
    } catch {
      case e: UnsupportedOperationException =>
        throw new IllegalArgumentException(s"Failed to transform $input", e)
    }
  }

  def isStructurallyEqual(a: Tree, b: Tree): Either[AnyDiff, Unit] =
    StructurallyEqual(normalize(a), normalize(b))

  def isSameTree(filename: String, a: Tree, b: Tree): Either[String, Unit] = {
    isStructurallyEqual(a, b).left.map(_ => getDiff(filename, a, b))
  }


}

abstract class BaseScalaPrinterTest extends DiffSuite with TestMethods {

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
