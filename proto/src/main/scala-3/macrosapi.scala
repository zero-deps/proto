package proto

import com.google.protobuf.{CodedInputStream}
import scala.quoted.*
import compiletime.asMatchable

//todo; optimisation for MessageCodec (add .size/.write and use these in proto.api.encode instead of .prepare)
//todo; optimisation for string (write custom .size/.write for string to prevent double time .size computation)

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
  import report.*

  def caseCodecAuto[A: Type]: Expr[MessageCodec[A]] =
    val a_tpe = getCaseClassType[A]
    val aTypeSymbol = a_tpe.typeSymbol
    val typeName = aTypeSymbol.fullName
    val params: List[Symbol] = aTypeSymbol.constructorParams
    val nums: List[(String, Int)] = params.map(p =>
      p.annotations.collect{
        case Apply(Select(New(tpt), _), List(Literal(IntConstant(num))))
          if tpt.tpe.asMatchable.isNType =>
            p.name -> num 
      } match
        case List(x) => x
        case Nil => errorAndAbort(s"missing ${NTpe.typeSymbol.name} annotation for `${typeName}`")
        case _ => errorAndAbort(s"multiple ${NTpe.typeSymbol.name} annotations applied for `${typeName}`")
    )
    messageCodec(a_tpe.asMatchable, nums, params, restrictDefaults=true)

  def caseCodecNums[A: Type](numsExpr: Expr[Seq[(String, Int)]])(using Quotes): Expr[MessageCodec[A]] =
    val nums: Seq[(String, Int)] = numsExpr.valueOrAbort
    val a_tpe = getCaseClassType[A]
    val aTypeSymbol = a_tpe.typeSymbol
    val params: List[Symbol] = aTypeSymbol.constructorParams
    messageCodec(a_tpe.asMatchable, nums, params, restrictDefaults=false)

  def caseCodecIdx[A: Type]: Expr[MessageCodec[A]] =
    val a_tpe = getCaseClassType[A]
    val aTypeSymbol = a_tpe.typeSymbol
    val params: List[Symbol] = aTypeSymbol.constructorParams
    val nums: List[(String, Int)] = params.zipWithIndex.map{case (p, idx) => (p.name, idx + 1) }
    messageCodec(a_tpe.asMatchable, nums, params, restrictDefaults=false)

  def classCodecAuto[A: Type]: Expr[MessageCodec[A]] =
    val a_tpe = TypeRepr.of[A]
    val aTypeSymbol = a_tpe.typeSymbol
    val typeName = aTypeSymbol.fullName
    val params: List[Symbol] = aTypeSymbol.constructorParams
    val nums: List[(String, Int)] =
      params.map(p =>
        p.annotations.collect{
          case Apply(Select(New(tpt), _), List(Literal(IntConstant(num))))
            if tpt.tpe.asMatchable.isNType =>
              p.name -> num
        } match
          case List(x) => x
          case Nil => errorAndAbort(s"missing ${NTpe.typeSymbol.name} annotation for `${typeName}`")
          case _ => errorAndAbort(s"multiple ${NTpe.typeSymbol.name} annotations applied for `${typeName}`")
      )
    messageCodec(a_tpe.asMatchable, nums, params, restrictDefaults=true)

  def classCodecNums[A: Type](
    numsExpr: Expr[Seq[(String, Int)]]
  )(
    constructor: Expr[Any]
  )(using Quotes): Expr[MessageCodec[A]] =
    val nums: Seq[(String, Int)] = numsExpr.valueOrAbort
    val a_tpe = TypeRepr.of[A]
    val aTypeSymbol = a_tpe.typeSymbol
    val typeName = aTypeSymbol.fullName
    val members: List[Symbol] = aTypeSymbol.fieldMembers ++ aTypeSymbol.methodMembers
    val params: List[Symbol] =
      nums.map{ case (name, num) =>
        members.find(_.name == name) match
          case Some(s) if s.isTerm => s
          case Some(s) => errorAndAbort(s"`${typeName}` field `${name}` is not a term")
          case None => errorAndAbort(s"`${typeName}` has no field `${name}`")
      }.toList
    messageCodec(a_tpe.asMatchable, nums, params, restrictDefaults=false, constructor=Some(constructor.asTerm))

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
    
    if nums.exists(_._2 < 1) then errorAndAbort(s"nums ${nums} should be > 0")
    if nums.size != cParams.size then errorAndAbort(s"nums size ${nums} not equal to `${typeName}` constructor params size ${cParams.size}")
    if nums.groupBy(_._2).exists(_._2.size != 1) then errorAndAbort(s"nums ${nums} should be unique")
    val restrictedNums = a_tpe.restrictedNums
    val typeArgsToReplace: Map[String, TypeRepr] = a_tpe.typeArgsToReplace

    val fields: List[FieldInfo] = cParams.zipWithIndex.map{ case (s, i) =>
      val (name, tpe) = s.tree match  
        case ValDef(v_name, v_tpt, v_rhs) =>
          val tpe1 = v_tpt.tpe.asMatchable
          (v_name, tpe1.replaceTypeArgs(typeArgsToReplace))
        case DefDef(d_name, _, d_tpt, d_rhs) =>
          val tpe1 = d_tpt.tpe.asMatchable
          (d_name, tpe1.replaceTypeArgs(typeArgsToReplace))
        case _ => errorAndAbort(s"wrong param definition of case class `${typeName}`")

      val defaultValue: Option[Term] = 
        if s.flags.is(Flags.HasDefault) then
          aTypeCompanionSym.methodMember(defaultMethodName(i)) match
            case List(x) =>
              if tpe.asMatchable.isOption && restrictDefaults then
                errorAndAbort(s"`${name}: ${tpe.typeSymbol.fullName}`: default value for Option isn't allowed")
              else if tpe.asMatchable.isRepeated && restrictDefaults then
                errorAndAbort(s"`${name}: ${tpe.typeSymbol.fullName}`: default value for collections isn't allowed")
              else
                Some(Select(Ref(aTypeCompanionSym), x))
            case _ => errorAndAbort(s"`${name}: ${tpe.typeSymbol.fullName}`: default value method not found")
        else None
      val num: Int =
        nums.collectFirst{ case (name1, num1) if name1 == name =>
          if restrictedNums.contains(num1) then
            errorAndAbort(s"num ${num1} for `${typeName}` is restricted")
          else num1
        }.getOrElse(
          errorAndAbort(s"missing num for `${name}: ${typeName}`")
        )
      FieldInfo(
        name = name
      , num = num
      , sym = s
      , tpe = tpe.asMatchable
      , getter = (a: Term) => 
          if tpe.isArray then Select.unique(a, name).wrapArrayOps(tpe)
          else Select.unique(a, name)
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

  private def collectNs(a_tpe: TypeRepr): List[(TypeRepr, Int)] =
    val a_typeSym = a_tpe.typeSymbol
    val typeName = a_typeSym.fullName
    a_tpe.asMatchable.knownFinalSubclasses.map(x =>
      x.annotations.collect{
        case Apply(Select(New(tpt), _), List(Literal(IntConstant(num))))
          if tpt.tpe.asMatchable.isNType =>
            x.tpe -> num
      } match
        case List(x) => x
        case Nil =>
          errorAndAbort(s"missing ${NTpe.typeSymbol.name} annotation for `${x.name}`")
        case _ =>
          errorAndAbort(s"multiple ${NTpe.typeSymbol.name} annotations applied for `${typeName}`")
    )

  def enumByN[A: Type]: Expr[MessageCodec[A]] =
    val a_tpe = TypeRepr.of[A]
    val nums = collectNs(a_tpe)
    sealedTraitCodec(a_tpe.asMatchable, nums)

  def sealedTraitCodecAuto[A: Type]: Expr[MessageCodec[A]] =
    val a_tpe = getSealedTrait[A]
    val nums = collectNs(a_tpe)
    sealedTraitCodec(a_tpe.asMatchable, nums)

  def sealedTraitCodecNums[A: Type](numsExpr: Expr[Seq[(String, Int)]]): Expr[MessageCodec[A]] =
    val nums: Seq[(String, Int)] = numsExpr.valueOrAbort
    val a_tpe = getSealedTrait[A]
    val nums1: List[(TypeRepr, Int)] =
      a_tpe.asMatchable.knownFinalSubclasses.map{ x =>
        x.tpe ->
          nums.collectFirst{
            case (n, num) if n == x.name => num
          }.getOrElse{
            errorAndAbort(s"missing num for `${x.name}: ${x.fullName}`")
          }
      }
    sealedTraitCodec(a_tpe.asMatchable, nums1)

  def sealedTraitCodec[A: Type](
    a_tpe: TypeRepr & Matchable
  , nums: Seq[(TypeRepr, Int)]
  ): Expr[MessageCodec[A]] =
    val fields: List[FieldInfo] = childrenWithNum(a_tpe, nums).map(fieldInfoFromSym)
    '{
      new MessageCodec[A] {
        def prepare(a: A): Prepare =
          ${ prepareTrait('a, fields) }
        def read(is: CodedInputStream): A =
          ${ readImpl(a_tpe, fields, 'is, isTrait=true).asExprOf[A] }
      }
    }

  private def childrenWithNum(a_tpe: TypeRepr & Matchable, nums: Seq[(TypeRepr, Int)]): List[(Symbol, Int)] =
    val aTypeSymbol = a_tpe.typeSymbol
    val typeName = aTypeSymbol.fullName
    val subclasses = a_tpe.asMatchable.knownFinalSubclasses

    if subclasses.size <= 0 then
      errorAndAbort(s"required at least 1 subclass for `${typeName}`")
    if nums.size != subclasses.size then
      errorAndAbort(s"`${typeName}` subclasses ${subclasses.size} count != nums definition ${nums.size}")
    if nums.exists(_._2 < 1) then
      errorAndAbort(s"nums for ${typeName} should be > 0")
    if nums.groupBy(_._2).exists(_._2.size != 1) then
      errorAndAbort(s"nums for ${typeName} should be unique")

    val restrictedNums = a_tpe.restrictedNums

    subclasses.map{ s =>
      val tpe = s.tpe
      val num: Int =
        nums.collectFirst{
          case (tpe1, num) if tpe =:= tpe1 => num
        }.getOrElse{
          errorAndAbort(s"missing num for class `${tpe}` of trait `${a_tpe}`")
        }
      if restrictedNums.contains(num) then
        errorAndAbort(s"num ${num} is restricted for class `${tpe}` of trait `${a_tpe}`")
      (s, num)
    }

  private def fieldInfoFromSym(s: Symbol, num: Int): FieldInfo =
    FieldInfo(
      name = s.fullName
    , num = num
    , sym = s
    , tpe = s.tpe.asMatchable
    , getter = 
        if s.isTerm then (a: Term) => Ref(s)
        else (a: Term) => Select.unique(a, "asInstanceOf").appliedToType(s.tpe)
    , sizeSym = Symbol.newVal(Symbol.spliceOwner, s"field${num}Size", TypeRepr.of[Int], Flags.Mutable, Symbol.noSymbol)
    , prepareSym = Symbol.newVal(Symbol.spliceOwner, s"field${num}Prepare", PrepareType, Flags.Mutable, Symbol.noSymbol)
    , defaultValue = None
    , isCaseObject = s.isTerm
    )

  private def getSealedTrait[A: Type]: TypeRepr =
    val tpe = TypeRepr.of[A]
    if tpe.isSealedTrait then tpe
    else
      errorAndAbort(s"`${tpe.typeSymbol.fullName}` is not a sealed trait. Make sure that you specify codec type explicitly.\nExample:\n implicit val codecName: MessageCodec[SealedTraitTypeHere] = ...\n\n")

  private def getCaseClassType[A: Type]: TypeRepr =
    val tpe = TypeRepr.of[A]
    if tpe.asMatchable.isCaseType then tpe
    else
      errorAndAbort(s"`${tpe.typeSymbol.fullName}` is not a case class")

end Impl
