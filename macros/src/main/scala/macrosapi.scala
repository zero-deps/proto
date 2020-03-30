package zd
package proto

import proto.api.{MessageCodec, Prepare, N}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.quoted._
import scala.quoted.matching._
import scala.internal.quoted.showName
import scala.collection.immutable.ArraySeq
import zd.proto.Bytes

//todo; optimisation for case object (don't create prepare)
//todo; optimisation for MessageCodec (add .size/.write and use these in proto.api.encode instead of .prepare)
//todo; optimisation for string (write custom .size/.write for string to prevent double time .size computation)
//todo; remove .read exception and rewrite all the protobuf methods that throws exceptions

object macrosapi {

  inline def casecodecAuto[A]: MessageCodec[A] = ${Macro.caseCodecAuto[A]}
  inline def caseCodecNums[A](nums: (String, Int)*): MessageCodec[A] = ???
  inline def caseCodecIdx[A]: MessageCodec[A] = ???

  inline def classCodecAuto[A]: MessageCodec[A] = ???
  inline def classCodecNums[A](nums: (String, Int)*)(constructor: Any): MessageCodec[A] = ???

  inline def sealedTraitCodecAuto[A]: MessageCodec[A] = ???
  inline def sealedTraitCodecNums[A](nums: (String, Int)*): MessageCodec[A] = ???
  
  inline def enumByN[A]: MessageCodec[A] = ${Macro.enumByN[A]}
}

object Macro {
  def caseCodecAuto[A: Type](using qctx: QuoteContext): Expr[MessageCodec[A]] = Impl().caseCodecAuto[A]
  def enumByN[A: Type](using qctx: QuoteContext): Expr[MessageCodec[A]] = Impl().enumByN[A]
}

private class Impl(using val qctx: QuoteContext) extends BuildCodec {
  import qctx.tasty.{_, given _}
  import qctx.tasty.defn._

  def caseCodecAuto[A: quoted.Type]: Expr[MessageCodec[A]] = {
    val ctx = summon[Context]
    val t = summon[quoted.Type[A]]
    val aType = t.unseal.tpe
    val aTypeSymbol = aType.typeSymbol
    val typeName = t.unseal.tpe.typeSymbol.name
    val params: List[Symbol] = aTypeSymbol.caseFields
    val nums: List[(String, Int)] = params.map(p =>
      p.annots.collect{ case Apply(Select(New(tpt),_), List(Literal(Constant(num: Int)))) if tpt.tpe.isNType => p.name -> num } match {
        case List(x) => x
        case Nil => qctx.throwError(s"missing ${NTpe.typeSymbol.name} annotation for `${typeName}`")
        case _ => qctx.throwError(s"multiple ${NTpe.typeSymbol.name} annotations applied for `${typeName}`")
      }
    )
    messageCodec(aType, nums, params, restrictDefaults=true)
  }

  def messageCodec[A: quoted.Type](aType: Type, nums: Seq[(String, Int)], cParams: List[Symbol], restrictDefaults: Boolean)(using ctx: Context): Expr[MessageCodec[A]] = {
    val aTypeSym = aType.typeSymbol
    val aTypeCompanionSym = aTypeSym.companionModule
    val typeName = aTypeSym.fullName
    
    if (nums.exists(_._2 < 1)) qctx.throwError(s"nums ${nums} should be > 0")
    if (nums.size != cParams.size) qctx.throwError(s"nums size ${nums} not equal to `${typeName}` constructor params size ${cParams.size}")
    if (nums.groupBy(_._2).exists(_._2.size != 1)) qctx.throwError(s"nums ${nums} should be unique")
    val restrictedNums = aType.restrictedNums

    val fields: List[FieldInfo] = cParams.zipWithIndex.map{ case (s, i) =>
      val (name, tpt) = s.tree match
        case ValDef(vName,vTpt,vRhs) => (vName, vTpt)
        case _ => qctx.throwError(s"wrong param definition of case class `${typeName}`")
      val tpe = tpt.tpe
      val defaultValue: Option[Term] = aTypeCompanionSym.method(defaultMethodName(i)) match {
        case List(x) =>
          if tpe.isOption && restrictDefaults then qctx.throwError(s"`${name}: ${tpe.seal.show}`: default value for Option isn't allowed")
          else if tpe.isIterable && restrictDefaults then qctx.throwError(s"`${name}: ${tpe.seal.show}`: default value for collections isn't allowed")
          else Some(Select(Ref(aTypeCompanionSym), x))
        case _ => None
      }
      val num: Int =
        nums.collectFirst{ case (name1, num1) if name1 == name =>
          if restrictedNums.contains(num1) then qctx.throwError(s"num ${num1} for `${typeName}` is restricted") 
          else num1
        }.getOrElse{
          qctx.throwError(s"missing num for `${name}: ${typeName}`")
        }
      FieldInfo(
        name = name
      , num = num
      , tpe = tpe
      , tpt = tpt
      , getter = aTypeSym.field(name)
      , sizeSym = Symbol.newVal(ctx.owner, s"${name}Size", IntType, Flags.Mutable, Symbol.noSymbol)
      , prepareSym = Symbol.newVal(ctx.owner, s"${name}Prepare", PrepareType, Flags.Mutable, Symbol.noSymbol)
      , prepareOptionSym = Symbol.newVal(ctx.owner, s"${name}Prepare", appliedOptionType(PrepareType), Flags.Mutable, Symbol.noSymbol)
      , prepareArraySym = Symbol.newVal(ctx.owner, s"${name}Prepare", typeOf[Array[Prepare]], Flags.Mutable, Symbol.noSymbol)
      , defaultValue = defaultValue
      )
    }
    '{ 
      new MessageCodec[A] {
        def prepare(a: A): Prepare = ${ prepareImpl('a, fields) }
        def read(is: CodedInputStream): A = ${ readImpl(aType, fields, 'is).cast[A] }
      }
    }
  }

  def enumByN[A: quoted.Type]: Expr[MessageCodec[A]] = {
    val ctx = summon[Context]
    val t = summon[quoted.Type[A]]
    val aType = t.unseal.tpe
    val aTypeSym = aType.typeSymbol
    val typeName = aTypeSym.fullName
    val xs = aTypeSym.children
    val restrictedN: List[Int] = aType.restrictedNums
    xs.map{ x =>
      val num: Int =
        x.annots.collect{
          case Apply(Select(New(tpt),_), List(Literal(Constant(num1: Int)))) if tpt.tpe.isNType => num1
        } match {
          case List(num1) if restrictedN.contains(num1) => qctx.throwError(s"num ${num1} for `${typeName}` is restricted") 
          case List(num1) => num1
          case Nil => qctx.throwError(s"missing ${NTpe.typeSymbol.name} annotation for `${typeName}`")
          case _ => qctx.throwError(s"multiple ${NTpe.typeSymbol.name} annotations applied for `${typeName}`")
        }
      // val field = FieldInfo(
      //   name = s"field${num}"
      // , num = num
      // , tpe = ???
      // , tpt = ???
      // , getter = aTypeSym //?
      // , sizeSym = Symbol.newVal(ctx.owner, s"field${num}Size", IntType, Flags.Mutable, Symbol.noSymbol)
      // , prepareSym = Symbol.newVal(ctx.owner, s"field${num}Prepare", PrepareType, Flags.Mutable, Symbol.noSymbol)
      // , prepareOptionSym = Symbol.newVal(ctx.owner, s"field${num}Prepare", appliedOptionType(PrepareType), Flags.Mutable, Symbol.noSymbol)
      // , prepareArraySym = Symbol.newVal(ctx.owner, s"field${num}Prepare", typeOf[Array[Prepare]], Flags.Mutable, Symbol.noSymbol)
      // , defaultValue = None
      // )
      // prepareImpl(x, List(field))
    }
    '{
      new MessageCodec[A] {
        def prepare(a: A): Prepare = ???
        def read(is: CodedInputStream): A = ???
      }
    }
  }
}