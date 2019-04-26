package zd
package proto

import scala.collection.mutable
import scala.reflect.runtime.{universe => u}
import scala.reflect.runtime.universe._

object Purescript {
  def generate[A](implicit ttag: u.TypeTag[A]): List[String] = {
    val tpe = u.typeOf[A]
    val clazz = tpe.typeSymbol.asClass
    val datas = mutable.ListBuffer.empty[String]
    val types = mutable.ListBuffer.empty[String]
    clazz.knownDirectSubclasses.map{ x =>
      x.annotations.filter(_.tree.tpe == u.typeOf[zd.proto.api.N]) match {
        case List(x1) => x1.tree.children.tail match {
          case List(Literal(Constant(n: Int))) =>
            val name = x.name.decodedName.toString
            datas += s"${name} ${name}"
            val t = x.asType.toType
            val constr = t.typeSymbol.asClass.primaryConstructor
            println(constr)
            val fields = constr.asMethod.paramLists.flatten.map{ x =>
              val name = x.asTerm.name.decodedName.toString 
              val tpe = x.asTerm.info
              val value = if (tpe.baseClasses.exists(_.asType.toType.typeConstructor <:< typeOf[scala.collection.TraversableOnce[Unit]].typeConstructor)) {
                s"Array ${tpe.typeArgs.head.typeSymbol.asClass.name.decodedName.toString}"
              } else {
                "?"+tpe.toString
              }
              s"${name}: ${value}"
            }
            // println(params.map{ _.name.decodeName.toString })
            // println(constr.owner.asClass.typeParams.map(_.asType.toType)) //.zip(t.typeArgs))
            types += s"type ${name} = { ${fields.mkString(", ")} }"
          case _ => throw new Exception("bad args in N")
        }
        case Nil => throw new Exception(s"no N on ${x}")
        case _ => throw new Exception(s"multiple N on ${x}")
      }
    }
    val name = clazz.name.encodedName.toString
    List(
      s"data ${name} = ${datas.mkString(" | ")}",
      types.mkString("\n\n")
    )
  }

  val prelude = s"""module Codecs where

import Data.Array (snoc)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Int.Bits (zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Prelude

"""
}
