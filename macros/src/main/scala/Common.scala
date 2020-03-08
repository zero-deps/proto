package zd
package proto

import proto.api.{MessageCodec, Prepare, N, RestrictedN}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.quoted._
import scala.quoted.matching._
import scala.internal.quoted.showName
import scala.collection.immutable.ArraySeq
import zd.proto.Bytes

trait Common {
  implicit val qctx: QuoteContext
  import qctx.tasty.{_, given _}
  import qctx.tasty.defn._

  private[proto] case class FieldInfo(
    name: String
  , num: Int
  , tpe: Type
  , tpt: TypeTree
  , getter: Symbol
  , sizeSym: Symbol
  , prepareSym: Symbol
  , prepareOptionSym: Symbol
  , prepareArraySym: Symbol
  , defaultValue: Option[Term]
  )

  def (field: FieldInfo) tag: Int = field.num << 3 | wireType(field.tpe)

  def wireType(t: Type): Int =
    if      t.isInt || t.isLong || t.isBoolean then 0
    else if t.isDouble then 1
    else if t.isFloat then 5
    else if t.isOption then wireType(t.optionArgument)
    else if t.isString || 
            t.isArrayByte || 
            t.isArraySeqByte || 
            t.isBytesType || 
            t.isCaseClass ||
            t.isSealedTrait ||
            t.isIterable then 2
    else 2

  def writeFun(os: Expr[CodedOutputStream], t: Type, getterTerm: Term): Expr[Unit] =
    val getValue = getterTerm.seal
    if      t.isInt then '{ ${os}.writeInt32NoTag(${getValue.cast[Int]}) }
    else if t.isLong then '{ ${os}.writeInt64NoTag(${getValue.cast[Long]}) }
    else if t.isBoolean then '{ ${os}.writeBoolNoTag(${getValue.cast[Boolean]}) }
    else if t.isDouble then '{ ${os}.writeDoubleNoTag(${getValue.cast[Double]}) }
    else if t.isFloat then '{ ${os}.writeFloatNoTag(${getValue.cast[Float]}) }
    else if t.isString then '{ ${os}.writeStringNoTag(${getValue.cast[String]}) }
    else if t.isArrayByte then '{ ${os}.writeByteArrayNoTag(${getValue.cast[Array[Byte]]}) }
    else if t.isArraySeqByte then '{ ${os}.writeByteArrayNoTag(${getValue.cast[ArraySeq[Byte]]}.toArray[Byte]) }
    else if t.isBytesType then '{ ${os}.writeByteArrayNoTag(${getValue.cast[Bytes]}.unsafeArray) }
    else qctx.throwError(s"Unsupported common type: ${t.typeSymbol.name}")

  def sizeFun(t: Type, getterTerm: Term): Expr[Int] =
    val getValue = getterTerm.seal
    if      t.isInt then '{ CodedOutputStream.computeInt32SizeNoTag(${getValue.cast[Int]}) }
    else if t.isLong then '{ CodedOutputStream.computeInt64SizeNoTag(${getValue.cast[Long]}) }
    else if t.isBoolean then Expr(1)
    else if t.isDouble then Expr(8)
    else if t.isFloat then Expr(4)
    else if t.isString then '{ CodedOutputStream.computeStringSizeNoTag(${getValue.cast[String]}) }
    else if t.isArrayByte then '{ CodedOutputStream.computeByteArraySizeNoTag(${getValue.cast[Array[Byte]]}) }
    else if t.isArraySeqByte then '{ CodedOutputStream.computeByteArraySizeNoTag(${getValue.cast[ArraySeq[Byte]]}.toArray[Byte]) }
    else if t.isBytesType then '{ CodedOutputStream.computeByteArraySizeNoTag(${getValue.cast[Bytes]}.unsafeArray) }
    else qctx.throwError(s"Unsupported common type: ${t.typeSymbol.name}")

  def readFun(t: Type, is: Expr[CodedInputStream]): Expr[Any] =
    if      t.isInt then '{ ${is}.readInt32 }
    else if t.isLong then '{ ${is}.readInt64 }
    else if t.isBoolean then '{ ${is}.readBool }
    else if t.isDouble then '{ ${is}.readDouble }
    else if t.isFloat then '{ ${is}.readFloat }
    else if t.isString then '{ ${is}.readString }
    else if t.isArrayByte then '{ ${is}.readByteArray }
    else if t.isArraySeqByte then '{ ArraySeq.unsafeWrapArray(${is}.readByteArray) }
    else if t.isBytesType then '{ Bytes.unsafeWrap(${is}.readByteArray) }
    else qctx.throwError(s"Unsupported common type: ${t.typeSymbol.name}")

  val ArrayByteType: Type = typeOf[Array[Byte]]
  val ArraySeqByteType: Type = typeOf[ArraySeq[Byte]]
  val BytesType: Type = typeOf[Bytes]
  val NTpe: Type = typeOf[N]
  val RestrictedNType: Type = typeOf[RestrictedN]
  val ItetableType: Type = typeOf[scala.collection.Iterable[Any]]
  val PrepareType: Type = typeOf[Prepare]
  val CodedInputStreamType: Type = typeOf[CodedInputStream]
  def (t: Type) isNType: Boolean = t =:= NTpe
  def (t: Type) isCaseClass: Boolean = t.typeSymbol.flags.is(Flags.Case)
  def (t: Type) isSealedTrait: Boolean = t.typeSymbol.flags.is(Flags.Sealed & Flags.Trait)
  def (t: Type) isIterable: Boolean = t <:< ItetableType && !t.isArraySeqByte
  def unitLiteral: Literal = Literal(Constant(()))
  def defaultMethodName(i: Int): String = s"$$lessinit$$greater$$default$$${i+1}"

  def builderType: Type = typeOf[scala.collection.mutable.Builder[Unit, Unit]]
  def appliedBuilderType(t1: Type, t2: Type): Type = builderType match
    case AppliedType(tycon,_) => AppliedType(tycon, List(t1, t2))
    case _ => ???
  def optionType: Type = typeOf[Option[Any]]

  def appliedOptionType(t: Type): Type = optionType match
    case AppliedType(tycon,_) => AppliedType(tycon, List(t))

  val commonTypes: List[Type] =
    StringType :: IntType :: LongType :: BooleanType :: DoubleType :: FloatType :: ArrayByteType :: ArraySeqByteType :: BytesType :: Nil 

  def (t: Type) isString: Boolean = t =:= StringType
  def (t: Type) isInt: Boolean = t =:= IntType
  def (t: Type) isLong: Boolean = t =:= LongType
  def (t: Type) isBoolean: Boolean = t =:= BooleanType
  def (t: Type) isDouble: Boolean = t =:= DoubleType
  def (t: Type) isFloat: Boolean = t =:= FloatType
  def (t: Type) isArrayByte: Boolean = t =:= ArrayByteType
  def (t: Type) isArraySeqByte: Boolean = t =:= ArraySeqByteType
  def (t: Type) isBytesType: Boolean = t =:= BytesType

  def (t: Type) isOption: Boolean = t match
    case AppliedType(t1, _) if t1.typeSymbol == OptionClass => true
    case _ => false

  def (t: Type) optionArgument: Type = t match
    case AppliedType(t1, args) if t1.typeSymbol == OptionClass => args.head.asInstanceOf[Type]
    case _ => qctx.throwError(s"It isn't Option type: ${t.typeSymbol.name}")

  def (t: Type) iterableArgument: Type = t match
    case AppliedType(_, args) if t.isIterable => args.head.asInstanceOf[Type]
    case _ => qctx.throwError(s"It isn't Iterable type: ${t.typeSymbol.name}")

  def (t: Type) iterableBaseType: Type = t match
    case AppliedType(t1, _) if t.isIterable => t1
    case _ => qctx.throwError(s"It isn't Iterable type: ${t.typeSymbol.name}")

  def (t: Type) isCommonType: Boolean = commonTypes.exists(_ =:= t)

  def (t: Type) restrictedNums: List[Int] =
    val aName = RestrictedNType.typeSymbol.name
    val tName = t.typeSymbol.fullName
    t.typeSymbol.annots.collect{ case Apply(Select(New(tpt),_), List(Typed(Repeated(args,_),_))) if tpt.tpe =:= RestrictedNType => args } match
      case List(Nil) => qctx.throwError(s"empty annotation ${aName} for `${tName}`")
      case List(xs) =>
        val nums = xs.collect{
          case Literal(Constant(n: Int)) => n
          case x => qctx.throwError(s"wrong annotation ${aName} for `${tName}` $x")
        }
        if (nums.size != nums.distinct.size) qctx.throwError(s"nums not unique in annotation ${aName} for `${tName}`")
        nums
      case Nil => Nil
      case _ => qctx.throwError(s"multiple ${aName} annotations applied for `${tName}`")
}