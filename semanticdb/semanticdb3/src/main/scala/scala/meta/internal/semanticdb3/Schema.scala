// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!
//
// Protofile syntax: PROTO3

package scala.meta.internal.semanticdb3.semanticdb3

sealed trait Schema extends _root_.scalapb.GeneratedEnum {
  type EnumType = Schema
  def isLegacy: _root_.scala.Boolean = false
  def isSemanticdb3: _root_.scala.Boolean = false
  def companion: _root_.scalapb.GeneratedEnumCompanion[Schema] = scala.meta.internal.semanticdb3.semanticdb3.Schema
}

object Schema extends _root_.scalapb.GeneratedEnumCompanion[Schema] {
  implicit def enumCompanion: _root_.scalapb.GeneratedEnumCompanion[Schema] = this
  @SerialVersionUID(0L)
  case object LEGACY extends Schema {
    val value = 0
    val index = 0
    val name = "LEGACY"
    override def isLegacy: _root_.scala.Boolean = true
  }
  
  @SerialVersionUID(0L)
  case object SEMANTICDB3 extends Schema {
    val value = 3
    val index = 1
    val name = "SEMANTICDB3"
    override def isSemanticdb3: _root_.scala.Boolean = true
  }
  
  @SerialVersionUID(0L)
  final case class Unrecognized(value: _root_.scala.Int) extends Schema with _root_.scalapb.UnrecognizedEnum
  
  lazy val values = scala.collection.Seq(LEGACY, SEMANTICDB3)
  def fromValue(value: _root_.scala.Int): Schema = value match {
    case 0 => LEGACY
    case 3 => SEMANTICDB3
    case __other => Unrecognized(__other)
  }
  def javaDescriptor: _root_.com.google.protobuf.Descriptors.EnumDescriptor = Semanticdb3Proto.javaDescriptor.getEnumTypes.get(0)
  def scalaDescriptor: _root_.scalapb.descriptors.EnumDescriptor = Semanticdb3Proto.scalaDescriptor.enums(0)
}