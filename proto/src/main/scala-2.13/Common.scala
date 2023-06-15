package proto

import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.reflect.macros.blackbox.Context

trait Common {
  val c: Context
  import c.universe._
  import definitions._

  def error[A](msg: String): A = c.abort(c.enclosingPosition, msg)
  def evalTyped[A](expr: c.Expr[A]) = c.eval(c.Expr[A](c.untypecheck(expr.tree.duplicate)))
  
  private[proto] case class FieldInfo(name: TermName, sizeName: TermName, prepareName: TermName, readName: TermName, getter: c.Tree, tpe: c.Type, num: Int, defaultValue: Option[c.Tree], nonPacked: Boolean = false)  

  val PrepareType: c.Type = typeOf[Prepare]
  val CodedOutputStreamType: c.Type = typeOf[CodedOutputStream]
  val CodedInputStreamType: c.Type = typeOf[CodedInputStream]
  val ArrayByteType: c.Type = typeOf[Array[Byte]]
  val ArraySeqByteType: c.Type = typeOf[scala.collection.immutable.ArraySeq[Byte]]
  val BytesType: c.Type = typeOf[Bytes]
  val NType: c.Type = c.typeOf[N]
  val RestrictedNType: c.Type = c.typeOf[RestrictedN]
  val ItetableType: c.Type = typeOf[scala.collection.Iterable[Unit]]
  val ArrayType: c.Type = typeOf[Array[_]]

  def messageCodecFor(t: c.Type): c.Type = appliedType(typeOf[MessageCodec[Unit]].typeConstructor, t)
  def builder(t1: c.Type, t2: c.Type): c.Type = appliedType(typeOf[scala.collection.mutable.Builder[Unit, Unit]].typeConstructor, t1, t2)

  val commonTypes: List[c.Type] =
    StringClass.selfType :: IntClass.selfType :: LongClass.selfType :: BooleanClass.selfType :: DoubleClass.selfType :: FloatClass.selfType :: ArrayByteType :: ArraySeqByteType :: Nil 

  val packedTypes: List[c.Type] =
    IntClass.selfType :: LongClass.selfType :: BooleanClass.selfType :: DoubleClass.selfType :: FloatClass.selfType :: Nil 

  implicit class TypeOps (t: c.Type) {

    def isOption: Boolean = t.typeConstructor =:= OptionClass.selfType.typeConstructor
    def isTrait: Boolean = t.typeSymbol.isClass && t.typeSymbol.asClass.isTrait && t.typeSymbol.asClass.isSealed
    def isCaseClass: Boolean = t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass
    def constructorParams: List[TermSymbol] = t.typeSymbol.asClass.primaryConstructor.asMethod.paramLists.flatten.map(_.asTerm)
    def typeArgsToReplace: List[(c.Type, c.Type)] = t.typeSymbol.asClass.primaryConstructor.owner.asClass.typeParams.map(_.asType.toType).zip(t.typeArgs)
    def knownDirectSubclasses: List[c.Type] = t.typeSymbol.asClass.knownDirectSubclasses.toList.map(_.asType.toType)//.filter(isCaseClass)
    def knownFinalSubclasses: List[c.Type] = t.knownDirectSubclasses.flatMap{
      case t if t.isTrait => t.knownDirectSubclasses
      case t => List(t)
    }

    def isCommonType: Boolean = commonTypes.exists(_ =:= t)
    def isPackedType: Boolean = packedTypes.exists(_ =:= t)
    def isIterable: Boolean = t.baseClasses.exists(_.asType.toType.typeConstructor <:< ItetableType.typeConstructor) && !(t =:= ArraySeqByteType)
    def iterableArgument: c.Type = if (t.isIterable) t.baseType(ItetableType.typeSymbol).typeArgs(0) else error(s"It isn't Iterable (argument) type: $t")

    def isArray: Boolean = t.baseClasses.exists(_.asType.toType.typeConstructor <:< ArrayType.typeConstructor) && !(t =:= ArrayByteType)
    def arrayArgument: c.Type = if (t.isArray) t.baseType(ArrayType.typeSymbol).typeArgs(0) else error(s"It isn't Array (argument) type: $t")

    def isRepeated: Boolean = t.isIterable || t.isArray
    def repeatedArgument: c.Type = 
      if (t.isIterable) t.iterableArgument
      else if (t.isArray) t.arrayArgument
      else error(s"It isn't repated type: $t")
  }
  
  object common {
    def tag(field: FieldInfo): Int = 
      if (field.nonPacked && field.tpe.isRepeated && field.tpe.repeatedArgument.isCommonType)
        field.num << 3 | wireType(field.tpe.repeatedArgument)
      else field.num << 3 | wireType(field.tpe)

    def wireType(t: c.Type): Int = 
      if ( t =:= IntClass.selfType 
        || t =:= LongClass.selfType
        || t =:= BooleanClass.selfType
         ) {
        0
      } else if (t =:= DoubleClass.selfType) {
        1
      } else if (t =:= FloatClass.selfType) {
        5
      } else if (t.typeConstructor =:= OptionClass.selfType.typeConstructor) {
        val tpe1 = t.typeArgs(0)
        wireType(tpe1)
      } else if ( t =:= StringClass.selfType
               || t =:= ArrayByteType
               || t =:= ArraySeqByteType
               || t =:= BytesType
               || t.isCaseClass
               || t.isTrait
               || t.isRepeated
                ) {
        2
      } else {
        //todo; add condition case for generics
        2
      } //else { c.error(c.enclosingPosition, s"no wiretype for ${t}") }

    def sizeFun(field: FieldInfo): Option[c.Tree] =
      if (field.tpe =:= IntClass.selfType) {
        Some(q"${CodedOutputStreamType.typeSymbol.companion}.computeInt32SizeNoTag(${field.getter})")
      } else if (field.tpe =:= LongClass.selfType) {
        Some(q"${CodedOutputStreamType.typeSymbol.companion}.computeInt64SizeNoTag(${field.getter})")
      } else if (field.tpe =:= BooleanClass.selfType) {
        Some(q"1")
      } else if (field.tpe =:= DoubleClass.selfType) {
        Some(q"8")
      } else if (field.tpe =:= FloatClass.selfType) {
        Some(q"4")
      } else if (field.tpe =:= StringClass.selfType) {
        Some(q"${CodedOutputStreamType.typeSymbol.companion}.computeStringSizeNoTag(${field.getter})")
      } else if (field.tpe =:= ArrayByteType) {
        Some(q"${CodedOutputStreamType.typeSymbol.companion}.computeByteArraySizeNoTag(${field.getter})")
      } else if (field.tpe =:= ArraySeqByteType) {
        Some(q"${CodedOutputStreamType.typeSymbol.companion}.computeByteArraySizeNoTag(${field.getter}.toArray[Byte])")
      } else if (field.tpe =:= BytesType) {
        Some(q"${CodedOutputStreamType.typeSymbol.companion}.computeByteArraySizeNoTag(${field.getter}.unsafeArray)")
      } else {
        None
      }

    def writeFun(field: FieldInfo, os: TermName): Option[c.Tree] =
      if (field.tpe =:= IntClass.selfType) {
        Some(q"${os}.writeInt32NoTag(${field.getter})")
      } else if (field.tpe =:= LongClass.selfType) {
        Some(q"${os}.writeInt64NoTag(${field.getter})")
      } else if (field.tpe =:= BooleanClass.selfType) {
        Some(q"${os}.writeBoolNoTag(${field.getter})")
      } else if (field.tpe =:= DoubleClass.selfType) {
        Some(q"${os}.writeDoubleNoTag(${field.getter})")
      } else if (field.tpe =:= FloatClass.selfType) {
        Some(q"${os}.writeFloatNoTag(${field.getter})")
      } else if (field.tpe =:= StringClass.selfType) {
        Some(q"${os}.writeStringNoTag(${field.getter})")
      } else if (field.tpe =:= ArrayByteType) {
        Some(q"${os}.writeByteArrayNoTag(${field.getter})")
      } else if (field.tpe =:= ArraySeqByteType) {
        Some(q"${os}.writeByteArrayNoTag(${field.getter}.toArray[Byte])")
      } else if (field.tpe =:= BytesType) {
        Some(q"${os}.writeByteArrayNoTag(${field.getter}.unsafeArray)")
      } else {
        None
      }

    def readFun(field: FieldInfo, is: TermName): Option[c.Tree] =
      if (field.tpe =:= IntClass.selfType) {
        Some(q"${is}.readInt32")
      } else if (field.tpe =:= LongClass.selfType) {
        Some(q"${is}.readInt64")
      } else if (field.tpe =:= BooleanClass.selfType) {
        Some(q"${is}.readBool")
      } else if (field.tpe =:= DoubleClass.selfType) {
        Some(q"${is}.readDouble")
      } else if (field.tpe =:= FloatClass.selfType) {
        Some(q"${is}.readFloat")
      } else if (field.tpe =:= StringClass.selfType) {
        Some(q"${is}.readString")
      } else if (field.tpe =:= ArrayByteType) {
        Some(q"${is}.readByteArray")
      } else if (field.tpe =:= ArraySeqByteType) {
        Some(q"${ArraySeqByteType.typeSymbol.companion}.unsafeWrapArray(${is}.readByteArray)")
      } else if (field.tpe =:= BytesType) {
        Some(q"${BytesType.typeSymbol.companion}.unsafeWrap(${is}.readByteArray)")
      } else {
        None
      }
    }
}
