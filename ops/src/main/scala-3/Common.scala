package proto

import scala.quoted.*
import scala.collection.immutable.ArraySeq
import compiletime.asMatchable
import scala.annotation.*

trait Common:
  implicit val qctx: Quotes
  import qctx.reflect.{*, given}
  import qctx.reflect.defn.*
  import report.*

  val ArrayByteType: TypeRepr = TypeRepr.of[Array[Byte]]
  val ArraySeqByteType: TypeRepr = TypeRepr.of[ArraySeq[Byte]]
  val NTpe: TypeRepr = TypeRepr.of[N]
  val ItetableType: TypeRepr = TypeRepr.of[scala.collection.Iterable[?]]
  val ArrayType: TypeRepr = TypeRepr.of[Array[?]]

  extension (s: Symbol)
    def constructorParams: List[Symbol] = s.primaryConstructor.paramSymss.find(_.headOption.fold(false)( _.isTerm)).getOrElse(Nil)
    def tpe: TypeRepr =
      s.tree match
        case x: ClassDef => x.constructor.returnTpt.tpe
        case ValDef(_,tpt,_) => tpt.tpe
        case Bind(_, pattern: Term) => pattern.tpe

  def defaultMethodName(i: Int): String = s"$$lessinit$$greater$$default$$${i+1}"

  val commonTypes: List[TypeRepr] =
    TypeRepr.of[String] :: TypeRepr.of[Int] :: TypeRepr.of[Long] :: TypeRepr.of[Boolean] :: TypeRepr.of[Double] :: TypeRepr.of[Float] :: ArrayByteType :: ArraySeqByteType :: Nil 

  val packedTypes: List[TypeRepr] =
    TypeRepr.of[Int] :: TypeRepr.of[Long] :: TypeRepr.of[Boolean] :: TypeRepr.of[Double] :: TypeRepr.of[Float] :: Nil

  extension (t: TypeRepr)
    def isNType: Boolean = t =:= NTpe
    def isCaseClass: Boolean = t.typeSymbol.flags.is(Flags.Case)
    def isCaseObject: Boolean = t.termSymbol.flags.is(Flags.Case)
    def isCaseType: Boolean = t.isCaseClass || t.isCaseObject
    def isSealedTrait: Boolean = t.typeSymbol.flags.is(Flags.Sealed) && t.typeSymbol.flags.is(Flags.Trait)
    def isIterable: Boolean = t <:< ItetableType && !t.isArraySeqByte
    def isArray: Boolean = t <:< ArrayType && !t.isArrayByte
    def isRepeated: Boolean = t.isIterable || t.isArray
    def isString: Boolean = t =:= TypeRepr.of[String]
    def isInt: Boolean = t =:= TypeRepr.of[Int]
    def isLong: Boolean = t =:= TypeRepr.of[Long]
    def isBoolean: Boolean = t =:= TypeRepr.of[Boolean]
    def isDouble: Boolean = t =:= TypeRepr.of[Double]
    def isFloat: Boolean = t =:= TypeRepr.of[Float]
    def isArrayByte: Boolean = t =:= ArrayByteType
    def isArraySeqByte: Boolean = t =:= ArraySeqByteType
    def isCommonType: Boolean = commonTypes.exists(_ =:= t)
    def isPackedType: Boolean = packedTypes.exists(_ =:= t)

    def isOption: Boolean = t.asMatchable match
      case AppliedType(t1, _) if t1.typeSymbol == OptionClass => true
      case _ => false

    def typeArgs: List[TypeRepr] = t.dealias.asMatchable match
      case AppliedType(t1, args)  => args
      case _ => Nil

    def optionArgument: TypeRepr = t.asMatchable match
      case AppliedType(t1, args) if t1.typeSymbol == OptionClass => args.head
      case _ => errorAndAbort(s"It isn't Option type: ${t.typeSymbol.name}")

    def knownFinalSubclasses: List[Symbol] =
      @tailrec def loop(q: List[Symbol], acc: List[Symbol]): List[Symbol] = q match
        case Nil => acc
        case x :: xs if x.tpe.isSealedTrait => loop(x.tpe.typeSymbol.children ++ xs, acc)
        case x :: xs => loop(xs, x :: acc)
      loop(t.typeSymbol.children, Nil)

  def findN(x: Symbol): Option[Int] = {
    x.annotations.collect{
      case Apply(Select(New(tpt), _), List(Literal(IntConstant(num)))) 
        if tpt.tpe.asMatchable.isNType => num
    } match
      case List(x) => Some(x)
      case _ => None
  }