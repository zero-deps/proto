package proto

import scala.annotation._
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._

case class ChildMeta(name: String, tpe: Type, n: Int, noargs: Boolean, rec: Boolean)

sealed trait Tpe { val tpe: Type; val name: String }
final case class TraitType(tpe: Type, name: String, children: Seq[ChildMeta], firstLevel: Boolean) extends Tpe
final case class RegularType(tpe: Type, name: String) extends Tpe
final case class RecursiveType(tpe: Type, name: String) extends Tpe
final case class NoargsType(tpe: Type, name: String) extends Tpe
final case class TupleType(tpe: Type, name: String, tpe_1: Type, tpe_2: Type) extends Tpe

sealed trait DefVal
sealed trait HasDefFun { val value: String }
sealed trait FillDef   { val value: String }

final case object NoDef extends DefVal

final case object NoneDef extends DefVal with HasDefFun { val value = "Nothing" }
final case object SeqDef  extends DefVal with HasDefFun { val value = "[]"      }

final case class StrDef(v: String) extends DefVal with HasDefFun with FillDef { val value = s""""$v""""  }
final case class OthDef(v: Any)    extends DefVal with HasDefFun with FillDef { val value = v.toString   }

sealed trait IterablePurs
final case class ArrayPurs(tpe: Type) extends IterablePurs
final case class ArrayTuplePurs(tpe1: Type, tpe2: Type) extends IterablePurs

object Ops {
  def knownFinalSubclasses(tpe: Type): Vector[Symbol] = tpe.typeSymbol.asClass.knownDirectSubclasses.toVector.flatMap{
    case t if isTrait(t.asType.toType) => t.asClass.knownDirectSubclasses.toVector
    case t => List(t)
  }

  def findChildren(tpe: Type): Seq[ChildMeta] = {
    knownFinalSubclasses(tpe).map(x => x -> findN(x)).collect{ case (x, Some(n)) => x -> n }.sortBy(_._2).map{
      case (x, n) =>
        val tpe1 = x.asType.toType
        ChildMeta(name=tpe1.typeSymbol.name.encodedName.toString, tpe1, n, noargs=fields(tpe1).isEmpty, rec=isRecursive(tpe1))
    }
  }

  def findN(x: Symbol): Option[Int] = {
    x.annotations.filter(_.tree.tpe =:= typeOf[proto.N]) match {
      case List(x1) => x1.tree.children.tail match {
        case List(Literal(Constant(n: Int))) => Some(n)
        case _ => throw new Exception("bad args in N")
      }
      case Nil => None
      case _ => throw new Exception(s"multiple N on ${x}")
    }
  }

  @tailrec
  def collectTpes(head: Type, tail: Seq[Type], acc: Seq[Tpe], firstLevel: Boolean): Seq[Tpe] = {
    val (tail1, acc1): (Seq[Type], Seq[Tpe]) =
      if (acc.exists(_.tpe =:= head)) {
        (tail, acc)
      } else if (isTrait(head)) {
        val children = findChildren(head)
        (children.map(_.tpe)++tail, acc:+TraitType(head, head.typeSymbol.name.encodedName.toString, children, firstLevel))
      } else if (head.typeConstructor =:= OptionClass.selfType.typeConstructor) {
        val typeArg = head.typeArgs.head.typeSymbol
        val typeArgType = typeArg.asType.toType
        if (complexType(typeArgType)) (typeArgType+:tail, acc)
        else (tail, acc)
      } else if (isIterable(head)) {
        head.typeArgs match {
          case x :: Nil =>
            val typeArg = x.typeSymbol
            val typeArgType = typeArg.asType.toType
            if (complexType(typeArgType)) (typeArgType+:tail, acc)
            else (tail, acc)
          case x :: y :: Nil =>
            val zs = List(x, y).filter(complexType)
            (zs++tail, acc:+TupleType(appliedType(typeOf[Tuple2[Unit, Unit]].typeConstructor, x, y), tupleFunName(x, y), x, y))
          case _ => throw new Exception(s"too many type args for ${head}")
        }
      } else {
        val (ys, z) = type_to_tpe(head)
        (ys++tail, acc:+z)
      }
    tail1 match {
      case h +: t => collectTpes(h, tail=t, acc=acc1, firstLevel=false)
      case _ => acc1
    }
  }

  def collectTpes(tpe: Type): Seq[Tpe] = {
    collectTpes(tpe, tail=Nil, acc=Nil, firstLevel=true)
  }
  
  def fields(tpe: Type): List[(String, Type, Int, DefVal)] = {
    tpe.typeSymbol.asClass.primaryConstructor.asMethod.paramLists.flatten.zipWithIndex.map{ case (x, i) =>
      val term = x.asTerm
      val tpe1 = term.info
      val defval = if (term.isParamWithDefault) {
        val m = currentMirror
        val im = m.reflect((m.reflectModule(tpe.typeSymbol.asClass.companion.asModule)).instance)
        val method = tpe.companion.decl(TermName("apply$default$"+(i+1).toString)).asMethod
        (im.reflectMethod(method)() match {
          case x: String => StrDef(x)
          case x => OthDef(x)
        })
      } else if (tpe1.typeConstructor =:= OptionClass.selfType.typeConstructor) {
        NoneDef
      } else if (isIterable(tpe1)) {
        SeqDef
      } else NoDef
      (term.name.encodedName.toString, tpe1, findN(x), defval)
    }.collect{ case (a, b, Some(n), dv) => (a, b, n, dv) }.sortBy(_._3)
  }

  def isTrait(t: Type): Boolean = {
    t.typeSymbol.isClass && t.typeSymbol.asClass.isTrait && t.typeSymbol.asClass.isSealed
  }

  def isRecursive(base: Type): Boolean = {
    @tailrec def loop(compareTo: List[Type]): Boolean = compareTo match {
      case Nil => false
      case x :: _ if x =:= base => true
      case x :: xs => loop(x.typeArgs.map(_.typeSymbol.asType.toType) ++ xs)
    }
    loop(fields(base).map(_._2).filter(complexType))
  }

  val complexType: Type => Boolean = {
    case tpe if tpe =:= StringClass.selfType => false
    case tpe if tpe =:= IntClass.selfType => false
    case tpe if tpe =:= LongClass.selfType => false
    case tpe if tpe =:= BooleanClass.selfType => false
    case tpe if tpe =:= DoubleClass.selfType => false
    case tpe if tpe =:= typeOf[Array[Byte]] || tpe =:= typeOf[Bytes]  => false
    case _ => true
  }

  def isIterable(tpe: Type): Boolean = {
    tpe.baseClasses.exists(_.asType.toType.typeConstructor <:< typeOf[scala.collection.Iterable[Unit]].typeConstructor)
  }
  
  def tupleFunName(tpe_1: Type, tpe_2: Type): String = {
    (pursType(tpe_1)._1.filter(_.isLetter) + "_" + pursType(tpe_2)._1).filter(_.isLetter)
  }

  def type_to_tpe(head: Type): (Seq[Type], Tpe) = {
    val xs = fields(head).map(_._2)
    val ys = xs.filter(complexType)
    val z =
      if (xs.isEmpty) NoargsType(head, head.typeSymbol.name.encodedName.toString)
      else if (isRecursive(head)) RecursiveType(head, head.typeSymbol.name.encodedName.toString)
      else RegularType(head, head.typeSymbol.name.encodedName.toString)
    (ys, z)
  }
  
  def pursType(tpe: Type): (String, String) = {
    def trim(x: String): String = x.stripPrefix("(").stripSuffix(")")
    val (a, b) = pursTypePars(tpe)
    trim(a) -> trim(b)
  }

  def pursTypePars(tpe: Type): (String, String) = {
    if (tpe =:= StringClass.selfType) {
      "String" -> "(Maybe String)"
    } else if (tpe =:= IntClass.selfType) {
      "Int" -> "(Maybe Int)"
    } else if (tpe =:= LongClass.selfType) {
      "BigInt" -> "(Maybe BigInt)"
    } else if (tpe =:= BooleanClass.selfType) {
      "Boolean" -> "(Maybe Boolean)"
    } else if (tpe =:= DoubleClass.selfType) {
      "Number" -> "(Maybe Number)"
    } else if (tpe =:= typeOf[Array[Byte]] || tpe =:= typeOf[Bytes]) {
      "Uint8Array" -> "(Maybe Uint8Array)"
    } else if (tpe.typeConstructor =:= OptionClass.selfType.typeConstructor) {
      val typeArg = tpe.typeArgs.head
      if (typeArg =:= LongClass.selfType) {
        "(Maybe BigInt)" -> "(Maybe BigInt)"
      } else if (typeArg =:= DoubleClass.selfType) {
        "(Maybe Number)" -> "(Maybe Number)"
      } else {
        val name = typeArg.typeSymbol.name.encodedName.toString
        s"(Maybe $name)" -> s"Maybe $name)"
      }
    } else if (isIterable(tpe)) {
      iterablePurs(tpe) match {
        case ArrayPurs(tpe) =>
          val name = tpe.typeSymbol.asClass.name.encodedName.toString
          s"(Array $name)" -> s"(Array $name)"
        case ArrayTuplePurs(tpe1, tpe2) =>
          val name1 = pursTypePars(tpe1)._1
          val name2 = pursTypePars(tpe2)._1
          s"(Array (Tuple $name1 $name2))" -> s"(Array (Tuple $name1 $name2))"
      }
    } else {
      val name = tpe.typeSymbol.name.encodedName.toString
      name -> s"(Maybe $name)"
    }
  }

  def iterablePurs(tpe: Type): IterablePurs = {
    tpe.typeArgs match {
      case x :: Nil => ArrayPurs(x)
      case x :: y :: Nil => ArrayTuplePurs(x, y)
      case _ => throw new Exception(s"too many type args for $tpe")
    }
  }
}
