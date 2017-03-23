package scala.meta.semantic.v1

import org.scalameta.adt._
import org.scalameta.data.data

@data class CompilerMessage(location: Location, severity: Severity, message: String) {
  def syntax = s"$severity $message"
}

@root trait Severity {
  import Severity._
  def id: Int = this match {
    case Unknown(id) => id
    case Info => 1
    case Warning => 2
    case Error => 3
  }
}
object Severity {
  private val Message = "(\\d+)?(\\w+) (.*)".r
  def fromId(id: Int): Severity = id match {
    case 1 => Info
    case 2 => Warning
    case 3 => Error
    case _ => Unknown(id)
  }

  @leaf object Info extends Severity
  @leaf object Warning extends Severity
  @leaf object Error extends Severity
  @leaf class Unknown(n: Int) extends Severity
}
