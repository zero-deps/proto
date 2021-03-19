package proto

import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.quoted.*
import scala.collection.immutable.ArraySeq

//todo; optimisation for case object (don't create prepare)
//todo; optimisation for MessageCodec (add .size/.write and use these in proto.api.encode instead of .prepare)
//todo; optimisation for string (write custom .size/.write for string to prevent double time .size computation)
//todo; remove .read exception and rewrite all the protobuf methods that throws exceptions

inline def caseCodecAuto[A]: MessageCodec[A] = ${Macro.caseCodecAuto[A]}
inline def caseCodecNums[A](inline nums: (String, Int)*): MessageCodec[A] = ${Macro.caseCodecNums[A]('nums)}
inline def caseCodecIdx[A]: MessageCodec[A] = ${Macro.caseCodecIdx[A]}
inline def classCodecAuto[A]: MessageCodec[A] = ${Macro.classCodecAuto[A]}
inline def classCodecNums[A](inline nums: (String, Int)*)(inline constructor: Any): MessageCodec[A] = ${Macro.classCodecNums[A]('nums)('constructor)}
inline def sealedTraitCodecAuto[A]: MessageCodec[A] = ${Macro.sealedTraitCodecAuto[A]}
inline def sealedTraitCodecNums[A](inline nums: (String, Int)*): MessageCodec[A] = ${Macro.sealedTraitCodecNums[A]('nums)}
inline def enumByN[A]: MessageCodec[A] = ${Macro.enumByN[A]}

object Macro:
  def caseCodecAuto[A: Type](using qctx: Quotes): Expr[MessageCodec[A]] = Impl().caseCodecAuto[A]

  def caseCodecNums[A: Type](numsExpr: Expr[Seq[(String, Int)]])(using qctx: Quotes): Expr[MessageCodec[A]] = Impl().caseCodecNums[A](numsExpr)

  def caseCodecIdx[A: Type](using qctx: Quotes): Expr[MessageCodec[A]] = Impl().caseCodecIdx[A]

  def classCodecAuto[A: Type](using qctx: Quotes): Expr[MessageCodec[A]] = Impl().classCodecAuto[A]

  def classCodecNums[A: Type](numsExpr: Expr[Seq[(String, Int)]])(constructorExpr: Expr[Any])(using qctx: Quotes): Expr[MessageCodec[A]] = Impl().classCodecNums[A](numsExpr)(constructorExpr)

  def enumByN[A: Type](using qctx: Quotes): Expr[MessageCodec[A]] = Impl().enumByN[A]

  def sealedTraitCodecAuto[A: Type](using qctx: Quotes): Expr[MessageCodec[A]] = Impl().sealedTraitCodecAuto[A]

  def sealedTraitCodecNums[A: Type](numsExpr: Expr[Seq[(String, Int)]])(using qctx: Quotes): Expr[MessageCodec[A]] = Impl().sealedTraitCodecNums[A](numsExpr)
end Macro

private class Impl(using val qctx: Quotes) extends BuildCodec:
  import qctx.reflect.{*, given}
  import qctx.reflect.defn.*
  import report.*

  def caseCodecAuto[A: Type]: Expr[MessageCodec[A]] =
    val a_tpe = getCaseClassType[A].matchable
    val aTypeSymbol = a_tpe.typeSymbol
    val typeName = aTypeSymbol.fullName
    val params: List[Symbol] = aTypeSymbol.constructorParams
    val nums: List[(String, Int)] = params.map(p =>
      p.annotations.collect{
        case Apply(Select(New(tpt), _), List(Literal(IntConstant(num))))
          if tpt.tpe.matchable.isNType =>
            p.name -> num 
      } match
        case List(x) => x
        case Nil => throwError(s"missing ${NTpe.typeSymbol.name} annotation for `${typeName}`")
        case _ => throwError(s"multiple ${NTpe.typeSymbol.name} annotations applied for `${typeName}`")
    )
    messageCodec(a_tpe, nums, params, restrictDefaults=true)

  def caseCodecNums[A: Type](numsExpr: Expr[Seq[(String, Int)]]): Expr[MessageCodec[A]] =
    val nums: Seq[(String, Int)] = numsExpr.valueOrError
    val a_tpe = getCaseClassType[A].matchable
    val aTypeSymbol = a_tpe.typeSymbol
    val params: List[Symbol] = aTypeSymbol.constructorParams
    messageCodec(a_tpe, nums, params, restrictDefaults=false)

  def caseCodecIdx[A: Type]: Expr[MessageCodec[A]] =
    val a_tpe = getCaseClassType[A].matchable
    val aTypeSymbol = a_tpe.typeSymbol
    val params: List[Symbol] = aTypeSymbol.constructorParams
    val nums: List[(String, Int)] = params.zipWithIndex.map{case (p, idx) => (p.name, idx + 1) }
    messageCodec(a_tpe, nums, params, restrictDefaults=false)

  def classCodecAuto[A: Type]: Expr[MessageCodec[A]] =
    val a_tpe = TypeRepr.of[A].matchable
    val aTypeSymbol = a_tpe.typeSymbol
    val typeName = aTypeSymbol.fullName
    val params: List[Symbol] = aTypeSymbol.constructorParams
    val nums: List[(String, Int)] =
      params.map(p =>
        p.annotations.collect{
          case Apply(Select(New(tpt), _), List(Literal(IntConstant(num))))
            if tpt.tpe.matchable.isNType =>
              p.name -> num
        } match
          case List(x) => x
          case Nil => throwError(s"missing ${NTpe.typeSymbol.name} annotation for `${typeName}`")
          case _ => throwError(s"multiple ${NTpe.typeSymbol.name} annotations applied for `${typeName}`")
      )
    messageCodec(a_tpe, nums, params, restrictDefaults=true)

  def classCodecNums[A: Type](
    numsExpr: Expr[Seq[(String, Int)]]
  )(
    constructor: Expr[Any]
  ): Expr[MessageCodec[A]] =
    val nums: Seq[(String, Int)] = numsExpr.valueOrError
    val a_tpe = TypeRepr.of[A].matchable
    val aTypeSymbol = a_tpe.typeSymbol
    val typeName = aTypeSymbol.fullName
    val members: List[Symbol] = aTypeSymbol.memberFields ++ aTypeSymbol.memberMethods
    val params: List[Symbol] =
      nums.map{ case (name, num) =>
        members.find(_.name == name) match
          case Some(s) if s.isTerm => s
          case Some(s) => throwError(s"`${typeName}` field `${name}` is not a term")
          case None => throwError(s"`${typeName}` has no field `${name}`")
      }.toList
    messageCodec(a_tpe, nums, params, restrictDefaults=false, constructor=Some(constructor.asTerm))

  def messageCodec[A: Type](
    a_tpe: TypeRepr & Matchable
  , nums: Seq[(String, Int)]
  , cParams: List[Symbol]
  , restrictDefaults: Boolean
  , constructor: Option[Term]=None
  ): Expr[MessageCodec[A]] =
    val aTypeSym = a_tpe.typeSymbol
    val aTypeCompanionSym = aTypeSym.companionModule
    val typeName = aTypeSym.fullName
    
    if nums.exists(_._2 < 1) then throwError(s"nums ${nums} should be > 0")
    if nums.size != cParams.size then throwError(s"nums size ${nums} not equal to `${typeName}` constructor params size ${cParams.size}")
    if nums.groupBy(_._2).exists(_._2.size != 1) then throwError(s"nums ${nums} should be unique")
    val restrictedNums = a_tpe.restrictedNums
    val typeArgsToReplace: Map[String, TypeRepr] = a_tpe.typeArgsToReplace

    val fields: List[FieldInfo] = cParams.zipWithIndex.map{ case (s, i) =>
      val (name, tpe) = s.tree match  
        case ValDef(v_name, v_tpt, v_rhs) =>
          val tpe1 = v_tpt.tpe.matchable
          (v_name, tpe1.replaceTypeArgs(typeArgsToReplace))
        case DefDef(d_name, _, d_tpt, d_rhs) =>
          val tpe1 = d_tpt.tpe.matchable
          (d_name, tpe1.replaceTypeArgs(typeArgsToReplace))
        case _ => throwError(s"wrong param definition of case class `${typeName}`")

      val defaultValue: Option[Term] = 
        if s.flags.is(Flags.HasDefault) then
          aTypeCompanionSym.memberMethod(defaultMethodName(i)) match
            case List(x) =>
              if tpe.matchable.isOption && restrictDefaults then
                throwError(s"`${name}: ${tpe.typeSymbol.fullName}`: default value for Option isn't allowed")
              else if tpe.matchable.isIterable && restrictDefaults then
                throwError(s"`${name}: ${tpe.typeSymbol.fullName}`: default value for collections isn't allowed")
              else
                Some(Select(Ref(aTypeCompanionSym), x))
            case _ => throwError(s"`${name}: ${tpe.typeSymbol.fullName}`: default value method not found")
        else None
      val num: Int =
        nums.collectFirst{ case (name1, num1) if name1 == name =>
          if restrictedNums.contains(num1) then
            throwError(s"num ${num1} for `${typeName}` is restricted") 
          else num1
        }.getOrElse(
          throwError(s"missing num for `${name}: ${typeName}`")
        )
      FieldInfo(
        name = name
      , num = num
      , sym = s
      , tpe = tpe.matchable
      , getter = (a: Term) => Select.unique(a, name)
      , sizeSym = Symbol.newVal(Symbol.spliceOwner, s"${name}Size", TypeRepr.of[Int], Flags.Mutable, Symbol.noSymbol)
      , prepareSym = Symbol.newVal(Symbol.spliceOwner, s"${name}Prepare", PrepareType, Flags.Mutable, Symbol.noSymbol)
      , prepareOptionSym = Symbol.newVal(Symbol.spliceOwner, s"${name}Prepare", OptionType.appliedTo(PrepareType), Flags.Mutable, Symbol.noSymbol)
      , prepareArraySym = Symbol.newVal(Symbol.spliceOwner, s"${name}Prepare", TypeRepr.of[Array[Prepare]], Flags.Mutable, Symbol.noSymbol)
      , defaultValue = defaultValue
      )
    }
    '{ 
      new MessageCodec[A] {
        def prepare(a: A): Prepare =
          ${ prepareImpl('a, fields) }
        def read(is: CodedInputStream): A =
          ${ readImpl(a_tpe, fields, 'is, constructor=constructor).asExprOf[A] }
      }
    }

  def enumByN[A: Type]: Expr[MessageCodec[A]] =
    val a_tpe = TypeRepr.of[A].matchable
    val a_typeSym = a_tpe.typeSymbol
    val typeName = a_typeSym.fullName
    val xs = a_typeSym.children
    val nums: List[(TypeRepr, Int)] =
      xs.map(x =>
        x.annotations.collect{
          case Apply(Select(New(tpt), _), List(Literal(IntConstant(num))))
            if tpt.tpe.matchable.isNType =>
              x.tpe -> num
        } match
          case List(x) => x
          case Nil =>
            throwError(s"missing ${NTpe.typeSymbol.name} annotation for `${typeName}`")
          case _ =>
            throwError(s"multiple ${NTpe.typeSymbol.name} annotations applied for `${typeName}`")
      )
    sealedTraitCodec(a_tpe, nums)

  def sealedTraitCodecAuto[A: Type]: Expr[MessageCodec[A]] =
    val a_tpe = getSealedTrait[A].matchable
    val aTypeSymbol = a_tpe.typeSymbol
    val typeName = aTypeSymbol.fullName
    val xs = aTypeSymbol.children
    val nums: List[(TypeRepr, Int)] =
      xs.map(x =>
        x.annotations.collect{
          case Apply(Select(New(tpt), _), List(Literal(IntConstant(num))))
            if tpt.tpe.matchable.isNType =>
              x.tpe -> num
        } match
          case List(x) => x
          case Nil =>
            throwError(s"missing ${NTpe.typeSymbol.name} annotation for `${typeName}`")
          case _ =>
            throwError(s"multiple ${NTpe.typeSymbol.name} annotations applied for `${typeName}`")
      )
    sealedTraitCodec(a_tpe, nums)

  def sealedTraitCodecNums[A: Type](numsExpr: Expr[Seq[(String, Int)]]): Expr[MessageCodec[A]] =
    val nums: Seq[(String, Int)] = numsExpr.valueOrError
    val a_tpe = getSealedTrait[A].matchable
    val aTypeSymbol = a_tpe.typeSymbol
    val typeName = aTypeSymbol.fullName
    val xs = aTypeSymbol.children
    val nums1: List[(TypeRepr, Int)] = xs.map{ x =>
      val name = x.name
      x.tpe -> nums.collectFirst{case (n, num) if n == name => num}.getOrElse(throwError(s"missing num for `${name}: ${x.fullName}`"))
    }
    sealedTraitCodec(a_tpe, nums1)

  def sealedTraitCodec[A: Type](
    a_tpe: TypeRepr & Matchable
  , nums: Seq[(TypeRepr, Int)]
  ): Expr[MessageCodec[A]] =
    val aTypeSymbol = a_tpe.typeSymbol
    val typeName = aTypeSymbol.fullName
    val subclasses = aTypeSymbol.children

    if subclasses.size <= 0 then throwError(s"required at least 1 subclass for `${typeName}`")
    if nums.size != subclasses.size then throwError(s"`${typeName}` subclasses ${subclasses.size} count != nums definition ${nums.size}")
    if nums.exists(_._2 < 1) then throwError(s"nums for ${typeName} should be > 0")
    if nums.groupBy(_._2).exists(_._2.size != 1) then throwError(s"nums for ${typeName} should be unique")
    val restrictedNums = a_tpe.restrictedNums

    val fields: List[FieldInfo] = subclasses.map{ s =>
      val tpe = s.tpe
      val num: Int = nums.collectFirst{ case (tpe1, num) if tpe =:= tpe1 => num }.getOrElse(throwError(s"missing num for class `${tpe}` of trait `${a_tpe}`"))
      if restrictedNums.contains(num) then throwError(s"num ${num} is restricted for class `${tpe}` of trait `${a_tpe}`")
    
      FieldInfo(
        name = s.fullName
      , num = num
      , sym = s
      , tpe = tpe.matchable
      , getter = 
          if s.isTerm then (a: Term) => Ref(s)
          else (a: Term) => Select.unique(a, "asInstanceOf").appliedToType(tpe)
      , sizeSym = Symbol.newVal(Symbol.spliceOwner, s"field${num}Size", TypeRepr.of[Int], Flags.Mutable, Symbol.noSymbol)
      , prepareSym = Symbol.newVal(Symbol.spliceOwner, s"field${num}Prepare", PrepareType, Flags.Mutable, Symbol.noSymbol)
      , defaultValue = None
      , isCaseObject = s.isTerm
      )
    }
    '{
      new MessageCodec[A] {
        def prepare(a: A): Prepare =
          ${ prepareTrait('a, fields) }
        def read(is: CodedInputStream): A =
          ${ readImpl(a_tpe, fields, 'is, isTrait=true).asExprOf[A] }
      }
    }

  private def getSealedTrait[A: Type]: TypeRepr =
    val tpe = TypeRepr.of[A]
    if tpe.matchable.isSealedTrait then tpe
    else
      throwError(s"`${tpe.typeSymbol.fullName}` is not a sealed trait. Make sure that you specify codec type explicitly.\nExample:\n implicit val codecName: MessageCodec[SealedTraitTypeHere] = ...\n\n")

  private def getCaseClassType[A: Type]: TypeRepr =
    val tpe = TypeRepr.of[A]
    if tpe.matchable.isCaseType then tpe
    else
      throwError(s"`${tpe.typeSymbol.fullName}` is not a case class")

end Impl
