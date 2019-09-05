package zd
package proto

import proto.api.MessageCodec
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

//todo; optimisation for case object (don't create prepare)
//todo; optimisation for MessageCodec (add .size/.write and use these in proto.api.encode instead of .prepare)
//todo; optimisation for string (write custom .size/.write for string to prevent double time .size computation)
//todo; remove .read exception and rewrite all the protobuf methods that throws exceptions

object macrosapi {

  def caseCodecAuto[A]: MessageCodec[A] = macro Impl.caseCodecAuto[A]
  def caseCodecNums[A](): MessageCodec[A] = macro Impl.caseCodecNoArgs[A]
  def caseCodecNums[A](nums: (String, Int)*): MessageCodec[A] = macro Impl.caseCodecString[A]
  def caseCodecIdx[A]: MessageCodec[A] = macro Impl.caseCodecIdx[A]

  def classCodecAuto[A]: MessageCodec[A] = macro Impl.classCodecAuto[A]
  def classCodecNums[A](nums: (String, Int)*)(constructor: Any): MessageCodec[A] = macro Impl.classCodecString[A]

  def sealedTraitCodecAuto[A]: MessageCodec[A] = macro Impl.sealedTraitCodecAuto[A]
  def sealedTraitCodecNums[A](nums: (String, Int)*): MessageCodec[A] = macro Impl.sealedTraitCodecString[A]
}

class Impl(val c: Context) extends BuildCodec {
  import c.universe._

  private def getCaseClassType[A:c.WeakTypeTag]: c.Type = {
    val tpe: c.Type = c.weakTypeOf[A]
    if (isCaseClass(tpe)) tpe else error(s"`${tpe}` is not a final case class")
  }

  def caseCodecAuto[A:c.WeakTypeTag]: c.Tree = {
    val aType: c.Type = getCaseClassType[A]
    val nums: List[(String, Int)] = constructorParams(aType).map(p =>
      p.annotations.filter(_.tree.tpe =:= NType) match {
        case List(a) =>
          a.tree.children.tail match {
            case List(Literal(Constant(n: Int))) => p.name.decodedName.toString -> n
            case _ => error(s"wrong annotation=${a} for `${p.name}: ${p.info}`")
          }
        case Nil => error(s"missing ${NType} annotation for `${p.name}: ${p.info}`")
        case _ => error(s"multiple ${NType} annotations applied for `${p.name}: ${p.info}`")
      }
    )
    messageCodec(aType=aType, nums=nums, cParams=constructorParams(aType))
  }
  def caseCodecNoArgs[A:c.WeakTypeTag](): c.Tree = {
    val aType: c.Type = getCaseClassType[A]
    messageCodec(aType=aType, nums=Seq.empty, cParams=constructorParams(aType))
  }
  def caseCodecString[A:c.WeakTypeTag](nums: c.Expr[(String, Int)]*): c.Tree = {
    val aType: c.Type = getCaseClassType[A]
    messageCodec(aType=aType, nums=nums.map(evalTyped), cParams=constructorParams(aType))
  }
  def caseCodecIdx[A:c.WeakTypeTag]: c.Tree = {
    val aType: c.Type = getCaseClassType[A]
    val cParams: List[TermSymbol] = constructorParams(aType)
    val nums = cParams.zipWithIndex.map{case (p, idx) => (p.name.decodedName.toString, idx + 1)}
    messageCodec(aType=aType, nums=nums, cParams=cParams)
  }

  def classCodecAuto[A:c.WeakTypeTag]: c.Tree = {
    val aType: c.Type = c.weakTypeOf[A]
    val nums: List[(String, Int)] = constructorParams(aType).map(p =>
      p.annotations.filter(_.tree.tpe =:= NType) match {
        case List(a) =>
          a.tree.children.tail match {
            case List(Literal(Constant(n: Int))) => p.name.decodedName.toString -> n
            case _ => error(s"wrong annotation=${a} for `${p.name}: ${p.info}`")
          }
        case Nil => error(s"missing ${NType} annotation for `${p.name}: ${p.info}`")
        case _ => error(s"multiple ${NType} annotations applied for `${p.name}: ${p.info}`")
      }
    )
    messageCodec(aType=aType, nums=nums, cParams=constructorParams(aType))
  }
  def classCodecString[A:c.WeakTypeTag](nums: c.Expr[(String, Int)]*)(constructor: Impl.this.c.Expr[Any]): c.Tree =
    classCodec(aType=c.weakTypeOf[A], nums=nums.map(evalTyped), constructor=constructor.tree)

  def classCodec(aType: c.Type, nums: Seq[(String, Int)], constructor: c.Tree): c.Tree = {
    val cParams: List[TermSymbol] = nums.map{ case (name, num) =>
      val member = aType.member(TermName(name))
      if (member == NoSymbol) error(s"`${aType}` has no field `${name}`")
      if (!member.isTerm) error(s"`${aType}` field `${name}` is not a term")
      member.asTerm
    }.toList
    val res = messageCodec(aType=aType, nums=nums, cParams=cParams, constructor=Some(constructor))
    res
  }

  def messageCodec(aType: c.Type, nums: Seq[(String, Int)], cParams: List[TermSymbol], constructor: Option[c.Tree]=None): c.Tree = {
    if (nums.exists(_._2 < 1)) error(s"nums ${nums} should be > 0")
    if (nums.size != cParams.size) error(s"nums size ${nums} not equal to `${aType}` constructor params size ${cParams.size}")
    if (nums.groupBy(_._2).exists(_._2.size != 1)) error(s"nums ${nums} should be unique")
    val aName = TermName("a")
    val osName = TermName("os")
    val sizeAcc = TermName("sizeAcc")
    val params: List[FieldInfo] = cParams.map{ p =>
      val tpe: c.Type = p.info match {
        case t@NullaryMethodType(_) => t.resultType
        case t => t
      }
      val decodedName: String = p.name.decodedName.toString
      val encodedName: String = p.name.encodedName.toString
      val typeArgs: List[(c.Type, c.Type)] = typeArgsToReplace(aType)
      val withoutTypeArgs: c.Type = if (typeArgs.isEmpty) {
        tpe
      } else {
        tpe.map(tt => typeArgs.collectFirst{case (fromT, toT) if fromT =:= tt => toT}.getOrElse(tt))
      }
      FieldInfo(
        name=p.name
      , sizeName=TermName(s"${encodedName}Size")
      , prepareName=TermName(s"${encodedName}Prepare")
      , readName=TermName(s"${encodedName}Read")
      , getter=q"${aName}.${p.name}"
      , tpe=withoutTypeArgs
      , num=nums.collectFirst{case (name, num) if name == decodedName => num}.getOrElse(error(s"missing num for `${decodedName}: ${tpe}`"))
      )
    }
    val constructorF = constructor.collect{
      case fun1: Function =>
        val initArgs = params.map(field => q"${initArg(field.tpe, field.name, field.readName)}")
        q"${c.untypecheck(fun1.duplicate)}(..${initArgs})"
      case fun1 => error(s"`${showRaw(fun1)}` is not a function")
    }
    val res: c.Tree = q"""new ${messageCodecFor(aType)} {      
      ..${prepare(params, aType, aName, sizeAcc, osName)}
      ${read(params, aType, constructorF)}
    }"""
    res
  }

  private def getSealedTrait[A:c.WeakTypeTag] = {
    val tpe: c.Type = c.weakTypeOf[A]
    if (isTrait(tpe)) tpe else error(s"`${tpe}` is not a sealed trait. Make sure that you specify codec type explicitly.\nExample:\n implicit val codecName: MessageCodec[SealedTraitTypeHere] = ...\n\n")
  }

  def sealedTraitCodecAuto[A:c.WeakTypeTag]: c.Tree = {
    val aType: c.Type = getSealedTrait[A]
    val nums: List[(c.Type, Int)] = knownDirectSubclasses(aType).map { tpe =>
      tpe.typeSymbol.annotations.filter(_.tree.tpe =:= NType) match {
        case List(a) =>
          a.tree.children.tail match {
            case List(Literal(Constant(n: Int))) => tpe -> n
            case _ => error(s"wrong annotation=${a} for `${tpe}`")
          }
        case Nil => error(s"missing ${NType} annotation for `${tpe}`")
        case _ => error(s"multiple ${NType} annotations applied for `${tpe}`")
      }
    }
    sealedTraitCodec(nums)
  }

  def sealedTraitCodecString[A:c.WeakTypeTag](nums: c.Expr[(String, Int)]*): c.Tree = sealedTraitCodec(findTypes(nums.map(evalTyped)))

  def findTypes[A:c.WeakTypeTag](nums: Seq[(String, Int)]): Seq[(c.Type, Int)] = {
    val aType: c.Type = getSealedTrait[A]
    knownDirectSubclasses(aType).map{tpe =>
      val decodedName: String = tpe.typeSymbol.name.decodedName.toString
      tpe -> nums.collectFirst{case (name, num) if name == decodedName => num}.getOrElse(error(s"missing num for `${decodedName}: ${tpe}`"))
    }
  }

  def sealedTraitCodec[A:c.WeakTypeTag](nums: Seq[(c.Type, Int)]): c.Tree = {
    val aType: c.Type = getSealedTrait[A]
    val subclasses = knownDirectSubclasses(aType)
    if (subclasses.size <= 0) error(s"required at least 1 subclass for `${aType}`")
    if (nums.size != subclasses.size) error(s"`${aType}` subclasses ${subclasses.size} count != nums definition ${nums.size}")
    if (nums.exists(_._2 < 1)) error(s"nums ${nums} should be > 0")
    if (nums.groupBy(_._2).exists(_._2.size != 1)) error(s"nums ${nums} should be unique")
    val aName = TermName("a")
    val osName = TermName("os")
    val sizeAcc = TermName("sizeAcc")
    val params: List[FieldInfo] = subclasses
      .map{tpe => 
        val num: Int = nums.collectFirst{case (tpe1, num) if tpe1 =:= tpe => num}.getOrElse(error(s"missing num for class `${tpe}` of trait `${aType}`"))
        FieldInfo(
          name=TermName(s"field${num}")
        , sizeName=TermName(s"field${num}Size")
        , prepareName=TermName(s"field${num}Prepare")
        , readName=TermName(s"readRes")
        , getter=q" ${aName}"
        , tpe=tpe
        , num=num
        )
      }
    val prepareAll = params.map(field => prepare(List(field), field.tpe, aName, sizeAcc, osName)).flatten
    val matchParams = params.map(field => cq"${aName}: ${field.tpe} => prepare(${aName})")
    val res = q"""new ${messageCodecFor(aType)} {
      ..${prepareAll}
      def prepare(${aName}: ${aType}): ${PrepareType} = {
        ${aName} match {
          case ..${matchParams}
        }
      }
      ${read(params, aType)}
    }"""
    res
  }
}
