package scala.meta.tests.parsers.dotty

import scala.meta._
import scala.meta.tests.tokenizers.BaseTokenizerCoverageSuite

class Scala3TokenizerCoverageSuite extends BaseTokenizerCoverageSuite {
  override def defaultDialect: Dialect = dialects.Scala3

  check[Defn.ExtensionGroup]("extension [→T←](→i: T←)(using →t: F[T]←) →def isZero = i == 0←")
  check[Defn.Enum]("enum →Day←[→T←]→(e: T)← extends →A with B { case Monday, Tuesday }←")
  check[Defn.Class]("class →Day←[→T←]→(e: T)← extends →A with B { val Monday = 42 }←")
  check[Defn.Given]("→inline← given →intOrd← as →Ord[Int]← →{ def f(): Int = 1 }←")
  check[Export]("export →a.b←")
  check[Export]("export →A.{ b, c, d, _ }←")
  check[ExportGiven]("export given →a.b←")
  check[Importee.Given]("import →Instances.{ im, given Ordering[?] }←")
  check[Importee.GivenAll]("import →File.given←")
  check[ExportGiven]("export given →A.{ b, c, d, _ }←")
  checkType[Type.And]("→A← & →B←")
  checkType[Type.Or]("→A← | →B←")
  checkType[Type.Or]("→A← | →B←")

}
