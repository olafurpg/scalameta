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