package scala.meta.internal.scalacp

sealed trait SymbolLookup
final case object PackageLookup extends SymbolLookup
final case object JavaLookup extends SymbolLookup
final case object MissingLookup extends SymbolLookup
final case class ScalaLookup(kind: ScalaLookup.Kind) extends SymbolLookup
object ScalaLookup {
  sealed trait Kind
  case object Term extends Kind
  case object Type extends Kind
  case object Package extends Kind
  case object Method extends Kind
}
