// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!
//
// Protofile syntax: PROTO3

package scala.meta.internal.semanticdb3.semanticdb3

@SerialVersionUID(0L)
final case class Synthetic(
    range: _root_.scala.Option[scala.meta.internal.semanticdb3.semanticdb3.Range] = None,
    text: _root_.scala.Option[scala.meta.internal.semanticdb3.semanticdb3.TextDocument] = None
    ) extends scalapb.GeneratedMessage with scalapb.Message[Synthetic] with scalapb.lenses.Updatable[Synthetic] {
    @transient
    private[this] var __serializedSizeCachedValue: _root_.scala.Int = 0
    private[this] def __computeSerializedValue(): _root_.scala.Int = {
      var __size = 0
      if (range.isDefined) {
        val __value = range.get
        __size += 1 + _root_.com.google.protobuf.CodedOutputStream.computeUInt32SizeNoTag(__value.serializedSize) + __value.serializedSize
      };
      if (text.isDefined) {
        val __value = text.get
        __size += 1 + _root_.com.google.protobuf.CodedOutputStream.computeUInt32SizeNoTag(__value.serializedSize) + __value.serializedSize
      };
      __size
    }
    final override def serializedSize: _root_.scala.Int = {
      var read = __serializedSizeCachedValue
      if (read == 0) {
        read = __computeSerializedValue()
        __serializedSizeCachedValue = read
      }
      read
    }
    def writeTo(`_output__`: _root_.com.google.protobuf.CodedOutputStream): _root_.scala.Unit = {
      range.foreach { __v =>
        val __m = __v
        _output__.writeTag(1, 2)
        _output__.writeUInt32NoTag(__m.serializedSize)
        __m.writeTo(_output__)
      };
      text.foreach { __v =>
        val __m = __v
        _output__.writeTag(2, 2)
        _output__.writeUInt32NoTag(__m.serializedSize)
        __m.writeTo(_output__)
      };
    }
    def mergeFrom(`_input__`: _root_.com.google.protobuf.CodedInputStream): scala.meta.internal.semanticdb3.semanticdb3.Synthetic = {
      var __range = this.range
      var __text = this.text
      var _done__ = false
      while (!_done__) {
        val _tag__ = _input__.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __range = Option(_root_.scalapb.LiteParser.readMessage(_input__, __range.getOrElse(scala.meta.internal.semanticdb3.semanticdb3.Range.defaultInstance)))
          case 18 =>
            __text = Option(_root_.scalapb.LiteParser.readMessage(_input__, __text.getOrElse(scala.meta.internal.semanticdb3.semanticdb3.TextDocument.defaultInstance)))
          case tag => _input__.skipField(tag)
        }
      }
      scala.meta.internal.semanticdb3.semanticdb3.Synthetic(
          range = __range,
          text = __text
      )
    }
    def getRange: scala.meta.internal.semanticdb3.semanticdb3.Range = range.getOrElse(scala.meta.internal.semanticdb3.semanticdb3.Range.defaultInstance)
    def clearRange: Synthetic = copy(range = None)
    def withRange(__v: scala.meta.internal.semanticdb3.semanticdb3.Range): Synthetic = copy(range = Option(__v))
    def getText: scala.meta.internal.semanticdb3.semanticdb3.TextDocument = text.getOrElse(scala.meta.internal.semanticdb3.semanticdb3.TextDocument.defaultInstance)
    def clearText: Synthetic = copy(text = None)
    def withText(__v: scala.meta.internal.semanticdb3.semanticdb3.TextDocument): Synthetic = copy(text = Option(__v))
    def getFieldByNumber(__fieldNumber: _root_.scala.Int): _root_.scala.Any = {
      (__fieldNumber: @_root_.scala.unchecked) match {
        case 1 => range.orNull
        case 2 => text.orNull
      }
    }
    def getField(__field: _root_.scalapb.descriptors.FieldDescriptor): _root_.scalapb.descriptors.PValue = {
      require(__field.containingMessage eq companion.scalaDescriptor)
      (__field.number: @_root_.scala.unchecked) match {
        case 1 => range.map(_.toPMessage).getOrElse(_root_.scalapb.descriptors.PEmpty)
        case 2 => text.map(_.toPMessage).getOrElse(_root_.scalapb.descriptors.PEmpty)
      }
    }
    def toProtoString: _root_.scala.Predef.String = _root_.scalapb.TextFormat.printToUnicodeString(this)
    def companion = scala.meta.internal.semanticdb3.semanticdb3.Synthetic
}

object Synthetic extends scalapb.GeneratedMessageCompanion[scala.meta.internal.semanticdb3.semanticdb3.Synthetic] {
  implicit def messageCompanion: scalapb.GeneratedMessageCompanion[scala.meta.internal.semanticdb3.semanticdb3.Synthetic] = this
  def fromFieldsMap(__fieldsMap: scala.collection.immutable.Map[_root_.com.google.protobuf.Descriptors.FieldDescriptor, _root_.scala.Any]): scala.meta.internal.semanticdb3.semanticdb3.Synthetic = {
    require(__fieldsMap.keys.forall(_.getContainingType() == javaDescriptor), "FieldDescriptor does not match message type.")
    val __fields = javaDescriptor.getFields
    scala.meta.internal.semanticdb3.semanticdb3.Synthetic(
      __fieldsMap.get(__fields.get(0)).asInstanceOf[_root_.scala.Option[scala.meta.internal.semanticdb3.semanticdb3.Range]],
      __fieldsMap.get(__fields.get(1)).asInstanceOf[_root_.scala.Option[scala.meta.internal.semanticdb3.semanticdb3.TextDocument]]
    )
  }
  implicit def messageReads: _root_.scalapb.descriptors.Reads[scala.meta.internal.semanticdb3.semanticdb3.Synthetic] = _root_.scalapb.descriptors.Reads{
    case _root_.scalapb.descriptors.PMessage(__fieldsMap) =>
      require(__fieldsMap.keys.forall(_.containingMessage == scalaDescriptor), "FieldDescriptor does not match message type.")
      scala.meta.internal.semanticdb3.semanticdb3.Synthetic(
        __fieldsMap.get(scalaDescriptor.findFieldByNumber(1).get).flatMap(_.as[_root_.scala.Option[scala.meta.internal.semanticdb3.semanticdb3.Range]]),
        __fieldsMap.get(scalaDescriptor.findFieldByNumber(2).get).flatMap(_.as[_root_.scala.Option[scala.meta.internal.semanticdb3.semanticdb3.TextDocument]])
      )
    case _ => throw new RuntimeException("Expected PMessage")
  }
  def javaDescriptor: _root_.com.google.protobuf.Descriptors.Descriptor = Semanticdb3Proto.javaDescriptor.getMessageTypes.get(25)
  def scalaDescriptor: _root_.scalapb.descriptors.Descriptor = Semanticdb3Proto.scalaDescriptor.messages(25)
  def messageCompanionForFieldNumber(__number: _root_.scala.Int): _root_.scalapb.GeneratedMessageCompanion[_] = {
    var __out: _root_.scalapb.GeneratedMessageCompanion[_] = null
    (__number: @_root_.scala.unchecked) match {
      case 1 => __out = scala.meta.internal.semanticdb3.semanticdb3.Range
      case 2 => __out = scala.meta.internal.semanticdb3.semanticdb3.TextDocument
    }
    __out
  }
  lazy val nestedMessagesCompanions: Seq[_root_.scalapb.GeneratedMessageCompanion[_]] = Seq.empty
  def enumCompanionForFieldNumber(__fieldNumber: _root_.scala.Int): _root_.scalapb.GeneratedEnumCompanion[_] = throw new MatchError(__fieldNumber)
  lazy val defaultInstance = scala.meta.internal.semanticdb3.semanticdb3.Synthetic(
  )
  implicit class SyntheticLens[UpperPB](_l: _root_.scalapb.lenses.Lens[UpperPB, scala.meta.internal.semanticdb3.semanticdb3.Synthetic]) extends _root_.scalapb.lenses.ObjectLens[UpperPB, scala.meta.internal.semanticdb3.semanticdb3.Synthetic](_l) {
    def range: _root_.scalapb.lenses.Lens[UpperPB, scala.meta.internal.semanticdb3.semanticdb3.Range] = field(_.getRange)((c_, f_) => c_.copy(range = Option(f_)))
    def optionalRange: _root_.scalapb.lenses.Lens[UpperPB, _root_.scala.Option[scala.meta.internal.semanticdb3.semanticdb3.Range]] = field(_.range)((c_, f_) => c_.copy(range = f_))
    def text: _root_.scalapb.lenses.Lens[UpperPB, scala.meta.internal.semanticdb3.semanticdb3.TextDocument] = field(_.getText)((c_, f_) => c_.copy(text = Option(f_)))
    def optionalText: _root_.scalapb.lenses.Lens[UpperPB, _root_.scala.Option[scala.meta.internal.semanticdb3.semanticdb3.TextDocument]] = field(_.text)((c_, f_) => c_.copy(text = f_))
  }
  final val RANGE_FIELD_NUMBER = 1
  final val TEXT_FIELD_NUMBER = 2
}
