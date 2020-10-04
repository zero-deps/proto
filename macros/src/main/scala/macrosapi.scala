package zd
package proto

import proto.api.{MessageCodec, Prepare, N}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.quoted._, report._
import scala.internal.quoted.showName
import scala.collection.immutable.ArraySeq
import zd.proto.Bytes

//todo; optimisation for case object (don't create prepare)
//todo; optimisation for MessageCodec (add .size/.write and use these in proto.api.encode instead of .prepare)
//todo; optimisation for string (write custom .size/.write for string to prevent double time .size computation)
//todo; remove .read exception and rewrite all the protobuf methods that throws exceptions

object macrosapi {

  inline def caseCodecAuto[A]: MessageCodec[A] = ${Macro.caseCodecAuto[A]}
  inline def caseCodecNums[A](inline nums: (String, Int)*): MessageCodec[A] = ${Macro.caseCodecNums[A]('nums)}
  inline def caseCodecIdx[A]: MessageCodec[A] = ${Macro.caseCodecIdx[A]}

  inline def classCodecAuto[A]: MessageCodec[A] = ???
  inline def classCodecNums[A](nums: (String, Int)*)(constructor: Any): MessageCodec[A] = ???

  inline def sealedTraitCodecAuto[A]: MessageCodec[A] = ${Macro.sealedTraitCodecAuto[A]}
  inline def sealedTraitCodecNums[A](nums: (String, Int)*): MessageCodec[A] = ???
  
  inline def enumByN[A]: MessageCodec[A] = ${Macro.enumByN[A]}
}

object Macro {
  def caseCodecAuto[A: Type](using qctx: QuoteContext): Expr[MessageCodec[A]] = Impl().caseCodecAuto[A]
  def caseCodecNums[A: Type](numsExpr: Expr[Seq[(String, Int)]])(using qctx: QuoteContext): Expr[MessageCodec[A]] = Impl().caseCodecNums[A](numsExpr)
  def caseCodecIdx[A: Type](using qctx: QuoteContext): Expr[MessageCodec[A]] = Impl().caseCodecIdx[A]

  def enumByN[A: Type](using qctx: QuoteContext): Expr[MessageCodec[A]] = Impl().enumByN[A]

  def sealedTraitCodecAuto[A: Type](using qctx: QuoteContext): Expr[MessageCodec[A]] = Impl().sealedTraitCodecAuto[A]

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
    val params: List[Symbol] = aTypeSymbol.caseClassValueParams
    val nums: List[(String, Int)] = params.map(p =>
      p.annots.collect{ case Apply(Select(New(tpt),_), List(Literal(Constant(num: Int)))) if tpt.tpe.isNType => p.name -> num } match {
        case List(x) => x
        case Nil => throwError(s"missing ${NTpe.typeSymbol.name} annotation for `${typeName}`")
        case _ => throwError(s"multiple ${NTpe.typeSymbol.name} annotations applied for `${typeName}`")
      }
    )
    messageCodec(aType, nums, params, restrictDefaults=true)
  }

  def caseCodecNums[A: quoted.Type](numsExpr: Expr[Seq[(String, Int)]]): Expr[MessageCodec[A]] = {
    val nums: Seq[(String, Int)] = numsExpr match {
      case Varargs(argExprs) =>
        argExprs.collect{
          case '{ ($x:String, $y:Int) } => x.unseal -> y.unseal
          case '{ ($x:String) -> ($y:Int) } => x.unseal -> y.unseal
        }.collect{
          case (Literal(Constant(name: String)), Literal(Constant(num: Int))) => name -> num
        }
      case _ => Seq()
    }
    val ctx = summon[Context]
    val t = summon[quoted.Type[A]]
    val aType = t.unseal.tpe
    val aTypeSymbol = aType.typeSymbol
    val typeName = t.unseal.tpe.typeSymbol.name
    val params: List[Symbol] = aTypeSymbol.caseFields
    messageCodec(aType, nums, params, restrictDefaults=true)
  }

  def caseCodecIdx[A: quoted.Type]: Expr[MessageCodec[A]] = {
    val ctx = summon[Context]
    val t = summon[quoted.Type[A]]
    val aType = t.unseal.tpe
    val aTypeSymbol = aType.typeSymbol
    val typeName = t.unseal.tpe.typeSymbol.name
    val params: List[Symbol] = aTypeSymbol.caseFields
    val nums: List[(String, Int)] = params.zipWithIndex.map{case (p, idx) => (p.name, idx + 1) }
    messageCodec(aType, nums, params, restrictDefaults=false)
  }

  def messageCodec[A: quoted.Type](aType: Type, nums: Seq[(String, Int)], cParams: List[Symbol], restrictDefaults: Boolean)(using ctx: Context): Expr[MessageCodec[A]] = {
    val aTypeSym = aType.typeSymbol
    val aTypeCompanionSym = aTypeSym.companionModule
    val typeName = aTypeSym.fullName
    
    if (nums.exists(_._2 < 1)) throwError(s"nums ${nums} should be > 0")
    if (nums.size != cParams.size) throwError(s"nums size ${nums} not equal to `${typeName}` constructor params size ${cParams.size}")
    if (nums.groupBy(_._2).exists(_._2.size != 1)) throwError(s"nums ${nums} should be unique")
    val restrictedNums = aType.restrictedNums
    val typeArgs: Map[String, Type] = aType.typeArgsToReplace

    val fields: List[FieldInfo] = cParams.zipWithIndex.map{ case (s, i) =>
      val (name, tpt, tpe) = s.tree match  
        case ValDef(v_name, v_tpt, v_rhs) => 
          typeArgs.get(v_tpt.tpe.typeSymbol.name) match
            case Some(typeArg) => (v_name, typeArg.typeTree, typeArg)
            case None => (v_name, v_tpt, v_tpt.tpe)
        case _ => throwError(s"wrong param definition of case class `${typeName}`")
      
      val defaultValue: Option[Term] = aTypeCompanionSym.method(defaultMethodName(i)) match {
        case List(x) =>
          if tpe.isOption && restrictDefaults then throwError(s"`${name}: ${tpe.seal.show}`: default value for Option isn't allowed")
          else if tpe.isIterable && restrictDefaults then throwError(s"`${name}: ${tpe.seal.show}`: default value for collections isn't allowed")
          else Some(Select(Ref(aTypeCompanionSym), x))
        case _ => None
      }
      val num: Int =
        nums.collectFirst{ case (name1, num1) if name1 == name =>
          if restrictedNums.contains(num1) then throwError(s"num ${num1} for `${typeName}` is restricted") 
          else num1
        }.getOrElse{
          throwError(s"missing num for `${name}: ${typeName}`")
        }
      FieldInfo(
        name = name
      , num = num
      , tpe = tpe
      , tpt = tpt
      , getter = aTypeSym.field(name)
      , sizeSym = Symbol.newVal(Symbol.currentOwner, s"${name}Size", IntType, Flags.Mutable, Symbol.noSymbol)
      , prepareSym = Symbol.newVal(Symbol.currentOwner, s"${name}Prepare", PrepareType, Flags.Mutable, Symbol.noSymbol)
      , prepareOptionSym = Symbol.newVal(Symbol.currentOwner, s"${name}Prepare", appliedOptionType(PrepareType), Flags.Mutable, Symbol.noSymbol)
      , prepareArraySym = Symbol.newVal(Symbol.currentOwner, s"${name}Prepare", typeOf[Array[Prepare]], Flags.Mutable, Symbol.noSymbol)
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
          case List(num1) if restrictedN.contains(num1) => throwError(s"num ${num1} for `${typeName}` is restricted") 
          case List(num1) => num1
          case Nil => throwError(s"missing ${NTpe.typeSymbol.name} annotation for `${typeName}`")
          case _ => throwError(s"multiple ${NTpe.typeSymbol.name} annotations applied for `${typeName}`")
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

  def sealedTraitCodecAuto[A: quoted.Type]: Expr[MessageCodec[A]] = {
    val t = summon[quoted.Type[A]]
    val aType = t.unseal.tpe
    val aTypeSymbol = aType.typeSymbol
    val xs = aTypeSymbol.children
    '{
      new MessageCodec[A] {
        def prepare(a: A): Prepare = ???
        def read(is: CodedInputStream): A = ???
      }
    }
  }

}