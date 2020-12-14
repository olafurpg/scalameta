package scala.meta.tests.parsers.dotty

import scala.meta._
import scala.meta.tests.tokenizers.BaseTokensSuite

class Scala3TokensSuite extends BaseTokensSuite {
  override def defaultDialect: Dialect = dialects.Scala3

  check[Stat](
    "extension [T](i: T)(using t: F[T]) def isZero = i == 0",
    """|Defn.ExtensionGroup extension [T](i: T)(using t: F[T]) def isZero = i == 0
       |Term.Param i: T
       |Term.Name i
       |Type.Name T
       |Type.Param T
       |Type.Name T
       |Type.Bounds
       |  extension [T](i: T)(using t: F[T]) def isZero = i == 0
       |              ^
       |Term.Param t: F[T]
       |Mod.Using
       |  extension [T](i: T)(using t: F[T]) def isZero = i == 0
       |                            ^
       |Term.Name t
       |Type.Apply F[T]
       |Type.Name F
       |Type.Name T
       |Defn.Def def isZero = i == 0
       |Term.Name isZero
       |Term.ApplyInfix i == 0
       |Term.Name i
       |Term.Name ==
       |Lit.Int 0
       |""".stripMargin
  )
  check[Stat](
    "enum Day[T](e: T) extends A with B { case Monday, Tuesday }",
    """|Defn.Enum enum Day[T](e: T) extends A with B { case Monday, Tuesday }
       |Type.Name Day
       |Type.Param T
       |Type.Name T
       |Type.Bounds
       |  enum Day[T](e: T) extends A with B { case Monday, Tuesday }
       |            ^
       |Ctor.Primary (e: T)
       |Name.Anonymous
       |  enum Day[T](e: T) extends A with B { case Monday, Tuesday }
       |             ^
       |Term.Param e: T
       |Term.Name e
       |Type.Name T
       |Template A with B { case Monday, Tuesday }
       |Init A
       |Type.Name A
       |Name.Anonymous
       |  enum Day[T](e: T) extends A with B { case Monday, Tuesday }
       |                              ^
       |Init B
       |Type.Name B
       |Name.Anonymous
       |  enum Day[T](e: T) extends A with B { case Monday, Tuesday }
       |                                     ^
       |Self
       |  enum Day[T](e: T) extends A with B { case Monday, Tuesday }
       |                                       ^
       |Name.Anonymous
       |  enum Day[T](e: T) extends A with B { case Monday, Tuesday }
       |                                       ^
       |Defn.RepeatedEnumCase case Monday, Tuesday
       |Term.Name Monday
       |Term.Name Tuesday
       |""".stripMargin
  )
  check[Stat](
    "class Day[T](e: T) extends A with B { val Monday = 42 }",
    """|Defn.Class class Day[T](e: T) extends A with B { val Monday = 42 }
       |Type.Name Day
       |Type.Param T
       |Type.Name T
       |Type.Bounds
       |  class Day[T](e: T) extends A with B { val Monday = 42 }
       |             ^
       |Ctor.Primary (e: T)
       |Name.Anonymous
       |  class Day[T](e: T) extends A with B { val Monday = 42 }
       |              ^
       |Term.Param e: T
       |Term.Name e
       |Type.Name T
       |Template A with B { val Monday = 42 }
       |Init A
       |Type.Name A
       |Name.Anonymous
       |  class Day[T](e: T) extends A with B { val Monday = 42 }
       |                               ^
       |Init B
       |Type.Name B
       |Name.Anonymous
       |  class Day[T](e: T) extends A with B { val Monday = 42 }
       |                                      ^
       |Self
       |  class Day[T](e: T) extends A with B { val Monday = 42 }
       |                                        ^
       |Name.Anonymous
       |  class Day[T](e: T) extends A with B { val Monday = 42 }
       |                                        ^
       |Defn.Val val Monday = 42
       |Pat.Var Monday
       |Term.Name Monday
       |Lit.Int 42
       |""".stripMargin
  )
  // check[Defn.Given]("inline given intOrd as Ord[Int] { def f(): Int = 1 }")
  // check[Export]("export a.b")
  // check[Export]("export A.{ b, c, d, _ }")
  // check[ExportGiven]("export given a.b")
  // check[Importee.Given]("import Instances.{ im, given Ordering[?] }")
  // check[Importee.GivenAll]("import File.given")
  // check[ExportGiven]("export given A.{ b, c, d, _ }")
  // checkType[Type.And]("A & B")
  // checkType[Type.Or]("A | B")
  // checkType[Type.Or]("A | B")

}
