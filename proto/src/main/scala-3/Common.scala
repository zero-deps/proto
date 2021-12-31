package proto

import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.quoted.*
import scala.collection.immutable.ArraySeq
import scala.annotation.*

trait Common:
  implicit val qctx: Quotes
  import qctx.reflect.{*, given}
  import qctx.reflect.defn.*
  import report.*

  extension (x: TypeRepr)
    private[proto] def matchable: TypeRepr & Matchable = x.asInstanceOf[TypeRepr & Matchable]

  private[proto] case class FieldInfo(
    name: String
  , num: Int
  , sym: Symbol
  , tpe: TypeRepr & Matchable
  , getter: Term => Term
  , sizeSym: Symbol
  , prepareSym: Symbol
  , prepareOptionSym: Symbol = Symbol.noSymbol
  , prepareArraySym: Symbol = Symbol.noSymbol
  , defaultValue: Option[Term]
  , isCaseObject: Boolean = false
  , nonPacked: Boolean = false
  ):
    def tag: Int = 
      if nonPacked && tpe.isIterable && tpe.iterableArgument.matchable.isCommonType then 
        num << 3 | wireType(tpe.iterableArgument.matchable)
      else num << 3 | wireType(tpe)
  
  def wireType(t: TypeRepr & Matchable): Int =
    if      t.isInt || t.isLong || t.isBoolean then 0
    else if t.isDouble then 1
    else if t.isFloat then 5
    else if t.isOption then wireType(t.optionArgument.matchable)
    else if t.isString || 
            t.isArrayByte || 
            t.isArraySeqByte || 
            t.isCaseType ||
            t.isSealedTrait ||
            t.isIterable then 2
    else 2

  def writeFun(os: Expr[CodedOutputStream], t: TypeRepr & Matchable, getterTerm: Term)(using Quotes): Expr[Unit] =
    if      t.isInt then '{ ${os}.writeInt32NoTag(${getterTerm.asExprOf[Int]}) }
    else if t.isLong then '{ ${os}.writeInt64NoTag(${getterTerm.asExprOf[Long]}) }
    else if t.isBoolean then '{ ${os}.writeBoolNoTag(${getterTerm.asExprOf[Boolean]}) }
    else if t.isDouble then '{ ${os}.writeDoubleNoTag(${getterTerm.asExprOf[Double]}) }
    else if t.isFloat then '{ ${os}.writeFloatNoTag(${getterTerm.asExprOf[Float]}) }
    else if t.isString then '{ ${os}.writeStringNoTag(${getterTerm.asExprOf[String]}) }
    else if t.isArrayByte then '{ ${os}.writeByteArrayNoTag(${getterTerm.asExprOf[Array[Byte]]}) }
    else if t.isArraySeqByte then '{ ${os}.writeByteArrayNoTag(${getterTerm.asExprOf[ArraySeq[Byte]]}.toArray[Byte]) }
    else errorAndAbort(s"Unsupported common type: ${t.typeSymbol.name}")

  def sizeFun(t: TypeRepr & Matchable, getterTerm: Term): Expr[Int] =
    if      t.isInt then '{ CodedOutputStream.computeInt32SizeNoTag(${getterTerm.asExprOf[Int]}) }
    else if t.isLong then '{ CodedOutputStream.computeInt64SizeNoTag(${getterTerm.asExprOf[Long]}) }
    else if t.isBoolean then Expr(1)
    else if t.isDouble then Expr(8)
    else if t.isFloat then Expr(4)
    else if t.isString then '{ CodedOutputStream.computeStringSizeNoTag(${getterTerm.asExprOf[String]}) }
    else if t.isArrayByte then '{ CodedOutputStream.computeByteArraySizeNoTag(${getterTerm.asExprOf[Array[Byte]]}) }
    else if t.isArraySeqByte then '{ CodedOutputStream.computeByteArraySizeNoTag(${getterTerm.asExprOf[ArraySeq[Byte]]}.toArray[Byte]) }
    else errorAndAbort(s"Unsupported common type: ${t.typeSymbol.name}")

  def readFun(t: TypeRepr & Matchable, is: Expr[CodedInputStream])(using Quotes): Term =
    if      t.isInt then '{ ${is}.readInt32 }.asTerm
    else if t.isLong then '{ ${is}.readInt64 }.asTerm
    else if t.isBoolean then '{ ${is}.readBool }.asTerm
    else if t.isDouble then '{ ${is}.readDouble }.asTerm
    else if t.isFloat then '{ ${is}.readFloat }.asTerm
    else if t.isString then '{ ${is}.readString.nn }.asTerm
    else if t.isArrayByte then '{ ${is}.readByteArray.nn }.asTerm
    else if t.isArraySeqByte then '{ ArraySeq.unsafeWrapArray(${is}.readByteArray.nn) }.asTerm
    else errorAndAbort(s"Unsupported common type: ${t.typeSymbol.name}")

  val ArrayByteType: TypeRepr = TypeRepr.of[Array[Byte]]
  val ArraySeqByteType: TypeRepr = TypeRepr.of[ArraySeq[Byte]]
  val NTpe: TypeRepr = TypeRepr.of[N]
  val RestrictedNType: TypeRepr = TypeRepr.of[RestrictedN]
  val ItetableType: TypeRepr = TypeRepr.of[scala.collection.Iterable[?]]
  val PrepareType: TypeRepr = TypeRepr.of[Prepare]
  val CodedInputStreamType: TypeRepr = TypeRepr.of[CodedInputStream]
  
  extension (s: Symbol)
    def constructorParams: List[Symbol] = s.primaryConstructor.paramSymss.find(_.headOption.fold(false)( _.isTerm)).getOrElse(Nil)
    def tpe: TypeRepr =
      s.tree match
        case x: ClassDef => x.constructor.returnTpt.tpe
        case ValDef(_,tpt,_) => tpt.tpe
        case Bind(_, pattern: Term) => pattern.tpe

  def unitLiteral: Literal = Literal(UnitConstant())
  def defaultMethodName(i: Int): String = s"$$lessinit$$greater$$default$$${i+1}"

  def unitExpr: Expr[Unit] = unitLiteral.asExprOf[Unit]

  def builderType: TypeRepr = TypeRepr.of[scala.collection.mutable.Builder]
    
  def OptionType: TypeRepr = TypeRepr.of[Option]

  def Some_Apply(tpe: TypeRepr, value: Term): Term =
    Select.unique(Ref(SomeModule.companionModule), "apply")
      .appliedToType(tpe)
      .appliedTo(value)

  val commonTypes: List[TypeRepr] =
    TypeRepr.of[String] :: TypeRepr.of[Int] :: TypeRepr.of[Long] :: TypeRepr.of[Boolean] :: TypeRepr.of[Double] :: TypeRepr.of[Float] :: ArrayByteType :: ArraySeqByteType :: Nil 

  extension (t: TypeRepr)
    def isNType: Boolean = t =:= NTpe
    def isCaseClass: Boolean = t.typeSymbol.flags.is(Flags.Case)
    def isCaseObject: Boolean = t.termSymbol.flags.is(Flags.Case)
    def isCaseType: Boolean = t.isCaseClass || t.isCaseObject
    def isSealedTrait: Boolean = t.typeSymbol.flags.is(Flags.Sealed) && t.typeSymbol.flags.is(Flags.Trait)
    def isIterable: Boolean = t <:< ItetableType && !t.isArraySeqByte
    def isString: Boolean = t =:= TypeRepr.of[String]
    def isInt: Boolean = t =:= TypeRepr.of[Int]
    def isLong: Boolean = t =:= TypeRepr.of[Long]
    def isBoolean: Boolean = t =:= TypeRepr.of[Boolean]
    def isDouble: Boolean = t =:= TypeRepr.of[Double]
    def isFloat: Boolean = t =:= TypeRepr.of[Float]
    def isArrayByte: Boolean = t =:= ArrayByteType
    def isArraySeqByte: Boolean = t =:= ArraySeqByteType
    def isCommonType: Boolean = commonTypes.exists(_ =:= t)

    def typeArgsToReplace: Map[String, TypeRepr] =
      t.typeSymbol.primaryConstructor.paramSymss
      .find(_.headOption.fold(false)( _.isType))
      .map(_.map(_.name).zip(t.typeArgs)).getOrElse(Nil)
      .toMap

    def replaceTypeArgs(map: Map[String, TypeRepr]): TypeRepr = t.matchable match
      case AppliedType(t1, args)  => t1.appliedTo(args.map(_.matchable.replaceTypeArgs(map)))
      case _ => map.getOrElse(t.typeSymbol.name, t)

    def isOption: Boolean = t.matchable match
      case AppliedType(t1, _) if t1.typeSymbol == OptionClass => true
      case _ => false

    def typeArgs: List[TypeRepr] = t.dealias.matchable match
      case AppliedType(t1, args)  => args
      case _ => Nil

    def optionArgument: TypeRepr = t.matchable match
      case AppliedType(t1, args) if t1.typeSymbol == OptionClass => args.head
      case _ => errorAndAbort(s"It isn't Option type: ${t.typeSymbol.name}")

    def iterableArgument: TypeRepr = t.baseType(ItetableType.typeSymbol).matchable match
      case AppliedType(_, args) if t.isIterable => args.head
      case _ => errorAndAbort(s"It isn't Iterable (argument) type: ${t.typeSymbol.name}")

    def iterableBaseType: TypeRepr = t.dealias.matchable match
      case AppliedType(t1, _) if t.isIterable => t1
      case _ => errorAndAbort(s"It isn't Iterable (base) type: ${t.typeSymbol.name}")

    def restrictedNums: List[Int] =
      val aName = RestrictedNType.typeSymbol.name
      val tName = t.typeSymbol.fullName
      t.typeSymbol.annotations.collect{ case Apply(Select(New(tpt),_), List(Typed(Repeated(args,_),_))) if tpt.tpe =:= RestrictedNType => args } match
        case List(Nil) => errorAndAbort(s"empty annotation ${aName} for `${tName}`")
        case List(xs) =>
          val nums = xs.collect{
            case Literal(IntConstant(n)) => n
            case x => errorAndAbort(s"wrong annotation ${aName} for `${tName}` $x")
          }
          if nums.size != nums.distinct.size then errorAndAbort(s"nums not unique in annotation ${aName} for `${tName}`")
          nums
        case Nil => Nil
        case _ => errorAndAbort(s"multiple ${aName} annotations applied for `${tName}`")
      
    def knownFinalSubclasses: List[Symbol] =
      @tailrec def loop(q: List[Symbol], acc: List[Symbol]): List[Symbol] = q match
        case Nil => acc
        case x :: xs if x.tpe.isSealedTrait => loop(x.tpe.typeSymbol.children ++ xs, acc)
        case x :: xs => loop(xs, x :: acc)
      loop(t.typeSymbol.children, Nil)

  def mkIfStatement(branches: List[(Term, Term)], elseBranch: Term): Term =
    branches match
      case (cond, thenp) :: xs =>
        If(cond, thenp, mkIfStatement(xs, elseBranch))
      case Nil => elseBranch

end Common
