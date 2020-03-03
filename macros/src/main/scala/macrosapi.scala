package zd
package proto

import proto.api.{MessageCodec, Prepare, N}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.quoted._
import scala.quoted.matching._
import scala.internal.quoted.showName
import scala.collection.immutable.ArraySeq
import zd.proto.Bytes

object macrosapi {
  inline def casecodecAuto[A]: MessageCodec[A] = ${Macro.caseCodecAuto[A]}
}

object Macro {
  def caseCodecAuto[A: Type](using qctx: QuoteContext): Expr[MessageCodec[A]] = Impl().caseCodecAuto[A]
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
    val fields: List[FieldInfo] = params.map{ s =>
      val (name, tpt) = s.tree match
        case ValDef(vName,vTpt,vRhs) => (vName, vTpt)
        case _ => qctx.throwError(s"wrong param definition of case class `${typeName}`")

      FieldInfo(
        name = name
      , num = nums.collectFirst{ case (n, num) if n == name => num }.getOrElse(qctx.throwError(s"missing num for `${name}: ${typeName}`"))
      , tpe = tpt.tpe
      , tpt = tpt
      , getter = aTypeSymbol.field(name)
      , sizeSym = Symbol.newVal(ctx.owner, s"${name}Size", IntType, Flags.Mutable, Symbol.noSymbol)
      , prepareSym = Symbol.newVal(ctx.owner, s"${name}Prepare", PrepareType, Flags.Mutable, Symbol.noSymbol)
      , prepareOptionSym = Symbol.newVal(ctx.owner, s"${name}Prepare", appliedOptionType(PrepareType), Flags.Mutable, Symbol.noSymbol)
      , prepareArraySym = Symbol.newVal(ctx.owner, s"${name}Prepare", typeOf[Array[Prepare]], Flags.Mutable, Symbol.noSymbol)
      )
    }
    if (nums.exists(_._2 < 1)) qctx.throwError(s"nums ${nums} should be > 0")
    if (nums.size != fields.size) qctx.throwError(s"nums size ${nums} not equal to `${aType}` constructor params size ${fields.size}")
    if (nums.groupBy(_._2).exists(_._2.size != 1)) qctx.throwError(s"nums ${nums} should be unique")

    val codec = '{ 
      new MessageCodec[A] {
        def prepare(a: A): Prepare = ${ prepareImpl('a, fields) }
        def read(is: CodedInputStream): A = ${ readImpl(t.unseal.tpe, fields, 'is).cast[A] }
      }
    }
    codec
  }

}