package scala.meta.tests.parsers.dotty

import scala.meta._
import scala.meta.tests.tokenizers.BaseTokensSuite

class Scala3TokensSuite extends BaseTokensSuite {
  override def defaultDialect: Dialect = dialects.Scala3

  check[Stat](
    "extension [A, B](i: A)(using a: F[A], G[B]) def isZero = i == 0",
    """|Defn.ExtensionGroup extension [A, B](i: A)(using a: F[A], G[B]) def isZero = i == 0
       |Type.Bounds extension [A→←, B](i: A)(using a: F[A], G[B]) def isZero = i == 0
       |Type.Bounds extension [A, B→←](i: A)(using a: F[A], G[B]) def isZero = i == 0
       |Term.Param a: F[A]
       |Mod.Using extension [A, B](i: A)(using →←a: F[A], G[B]) def isZero = i == 0
       |Type.Apply F[A]
       |Term.Param G[B]
       |Mod.Using extension [A, B](i: A)(using a: F[A], →←G[B]) def isZero = i == 0
       |Name.Anonymous G
       |Type.Apply G[B]
       |Defn.Def def isZero = i == 0
       |Term.ApplyInfix i == 0
       |""".stripMargin
  )

  // This tests exists to document the symmetry between positions for
  // `Mod.Implicit` (test below) and `Mod.Using` (test above).
  check[Stat](
    "def foo(implicit a: A, b: B): Unit",
    """|Decl.Def def foo(implicit a: A, b: B): Unit
       |Term.Param a: A
       |Mod.Implicit def foo(implicit →←a: A, b: B): Unit
       |Term.Param b: B
       |Mod.Implicit def foo(implicit a: A, →←b: B): Unit
       |""".stripMargin
  )

  check[Stat](
    "enum Day[T](e: T) extends A with B { case Monday, Tuesday }",
    """|Defn.Enum enum Day[T](e: T) extends A with B { case Monday, Tuesday }
       |Type.Bounds enum Day[T→←](e: T) extends A with B { case Monday, Tuesday }
       |Ctor.Primary (e: T)
       |Template A with B { case Monday, Tuesday }
       |Self enum Day[T](e: T) extends A with B { →←case Monday, Tuesday }
       |Defn.RepeatedEnumCase case Monday, Tuesday
       |""".stripMargin
  )
  check[Stat](
    "class Day[T](e: T) extends A with B { val Monday = 42 }",
    """|Defn.Class class Day[T](e: T) extends A with B { val Monday = 42 }
       |Type.Bounds class Day[T→←](e: T) extends A with B { val Monday = 42 }
       |Ctor.Primary (e: T)
       |Template A with B { val Monday = 42 }
       |Self class Day[T](e: T) extends A with B { →←val Monday = 42 }
       |Defn.Val val Monday = 42
       |Pat.Var Monday
       |""".stripMargin
  )
  check[Stat](
    "inline given intOrd as Ord[Int] { def f(): Int = 1 }",
    """|Defn.Given inline given intOrd as Ord[Int] { def f(): Int = 1 }
       |Type.Apply Ord[Int]
       |Template { def f(): Int = 1 }
       |Self inline given intOrd as Ord[Int] { →←def f(): Int = 1 }
       |Defn.Def def f(): Int = 1
       |""".stripMargin
  )
  check[Stat](
    "export a.b",
    """|Export export a.b
       |Importer a.b
       |""".stripMargin
  )
  check[Stat](
    "export A.{ b, c, d, _ }",
    """|Export export A.{ b, c, d, _ }
       |Importer A.{ b, c, d, _ }
       |""".stripMargin
  )
  check[Stat](
    "export given a.b",
    """|ExportGiven export given a.b
       |Importer a.b
       |""".stripMargin
  )
  check[Stat](
    "import Instances.{ im, given Ordering[?] }",
    """|Import import Instances.{ im, given Ordering[?] }
       |Importer Instances.{ im, given Ordering[?] }
       |Importee.Given given Ordering[?]
       |Type.Apply Ordering[?]
       |Type.Placeholder ?
       |Type.Bounds import Instances.{ im, given Ordering[?→←] }
       |""".stripMargin
  )
  check[Stat](
    "import File.given",
    """|Import import File.given
       |Importer File.given
       |Importee.GivenAll given
       |""".stripMargin
  )
  check[Stat](
    "export given A.{ b, c, d, _ }",
    """|ExportGiven export given A.{ b, c, d, _ }
       |Importer A.{ b, c, d, _ }
       |""".stripMargin
  )
  check[Type]("A & B", "Type.And A & B")
  check[Type]("A | B", "Type.Or A | B")

}
