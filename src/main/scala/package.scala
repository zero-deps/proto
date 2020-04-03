package zd.proto

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._
import scala.annotation.tailrec
import zd.gs.z._

package object purs {
  def pursTypePars(tpe: Type): (String, String) = {
    if (tpe =:= StringClass.selfType) {
      "String" -> "(Maybe String)"
    } else if (tpe =:= IntClass.selfType) {
      "Int" -> "(Maybe Int)"
    } else if (tpe =:= BooleanClass.selfType) {
      "Boolean" -> "(Maybe Boolean)"
    } else if (tpe =:= DoubleClass.selfType) {
      "Number" -> "(Maybe Number)"
    } else if (tpe =:= typeOf[Array[Byte]]) {
      "Uint8Array" -> "(Maybe Uint8Array)"
    } else if (tpe.typeConstructor =:= OptionClass.selfType.typeConstructor) {
      val typeArg = tpe.typeArgs.head
      if (typeArg =:= DoubleClass.selfType) {
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
  
  def pursType(tpe: Type): (String, String) = {
    def trim(x: String): String = x.stripPrefix("(").stripSuffix(")")
    val (a, b) = pursTypePars(tpe)
    trim(a) -> trim(b)
  }

  def isIterable(tpe: Type): Boolean = {
    tpe.baseClasses.exists(_.asType.toType.typeConstructor <:< typeOf[scala.collection.Iterable[Unit]].typeConstructor)
  }
  
  def tupleFunName(tpe_1: Type, tpe_2: Type): String = {
    (pursType(tpe_1)._1.filter(_.isLetter) + "_" + pursType(tpe_2)._1).filter(_.isLetter)
  }
  
  def nothingValue(name: String, tpe: Type): String = {
    if (isIterable(tpe)) {
      iterablePurs(tpe) match {
        case _: ArrayPurs => s"$name: []"
        case _: ArrayTuplePurs => s"$name: []"
      }
    } else s"$name: Nothing"
  }
  
  def justValue(name: String, tpe: Type): String = {
    if (tpe.typeConstructor =:= OptionClass.selfType.typeConstructor) name
    else if (isIterable(tpe)) name
    else {
      s"$name: Just $name"
    }
  }

  def iterablePurs(tpe: Type): IterablePurs = {
    tpe.typeArgs match {
      case x :: Nil => ArrayPurs(x)
      case x :: y :: Nil => ArrayTuplePurs(x, y)
      case _ => throw new Exception(s"too many type args for $tpe")
    }
  }

  def collectTpes(tpe: Type): Seq[Tpe] = {
    val complexType: Type => Boolean = {
      case tpe if tpe =:= StringClass.selfType => false
      case tpe if tpe =:= IntClass.selfType => false
      case tpe if tpe =:= BooleanClass.selfType => false
      case tpe if tpe =:= DoubleClass.selfType => false
      case tpe if tpe =:= typeOf[Array[Byte]] => false
      case _ => true
    }
    @tailrec def isRecursive(base: Type, compareTo: List[Type]): Boolean = compareTo match {
      case Nil => false
      case x :: _ if x =:= base => true
      case x :: xs => isRecursive(base, x.typeArgs.map(_.typeSymbol.asType.toType) ++ xs)
    }
    def isTrait(t: Type): Boolean = {
      t.typeSymbol.isClass && t.typeSymbol.asClass.isTrait && t.typeSymbol.asClass.isSealed
    }
    def findChildren(tpe: Type): Seq[ChildMeta] = {
      tpe.typeSymbol.asClass.knownDirectSubclasses.toVector.map(x => x -> findN(x)).collect{ case (x, Some(n)) => x -> n }.sortBy(_._2).map{
        case (x, n) =>
          val tpe = x.asType.toType
          ChildMeta(name=tpe.typeSymbol.name.encodedName.toString, tpe, n, noargs=fields(tpe).isEmpty)
      }
    }
    @tailrec def loop(head: Type, tail: Seq[Type], acc: Seq[Tpe], firstLevel: Boolean): Seq[Tpe] = {
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
          val xs = fields(head).map(_._2)
          val ys = xs.filter(complexType)
          val z =
            if (xs.isEmpty) NoargsType(head, head.typeSymbol.name.encodedName.toString)
            else if (isRecursive(head, ys)) RecursiveType(head, head.typeSymbol.name.encodedName.toString)
            else RegularType(head, head.typeSymbol.name.encodedName.toString)
          (ys++tail, acc:+z)
        }
      tail1 match {
        case h +: t => loop(h, t, acc1, false)
        case _ => acc1
      }
    }
    loop(tpe, Nil, Nil, true)
  }

  private[this] def findN(x: Symbol): Option[Int] = {
    x.annotations.filter(_.tree.tpe =:= typeOf[zd.proto.api.N]) match {
      case List(x1) => x1.tree.children.tail match {
        case List(Literal(Constant(n: Int))) => Some(n)
        case _ => throw new Exception("bad args in N")
      }
      case Nil => None
      case _ => throw new Exception(s"multiple N on ${x}")
    }
  }
  
  def fields(tpe: Type): List[(String, Type, Int, Maybe[Any])] = {
    tpe.typeSymbol.asClass.primaryConstructor.asMethod.paramLists.flatten.zipWithIndex.map{ case (x, i) =>
      val term = x.asTerm
      val defval = if (term.isParamWithDefault) {
        val m = currentMirror
        val im = m.reflect((m.reflectModule(tpe.typeSymbol.asClass.companion.asModule)).instance)
        val method = tpe.companion.decl(TermName("apply$default$"+(i+1).toString)).asMethod
        im.reflectMethod(method)().just
      } else Nothing
      (term.name.encodedName.toString, term.info, findN(x), defval)
    }.collect{ case (a, b, Some(n), dv) => (a, b, n, dv) }.sortBy(_._3)
  }

  def makePursTypes(types: Seq[Tpe], genMaybe: Boolean): Seq[PursType] = {
    types.flatMap{
      case TraitType(tpe, name, children, true) =>
        List(PursType(List(
          s"data $name = ${children.map{
            case x if x.noargs => x.name
            case x => s"${x.name} ${x.name}"
          }.mkString(" | ")}"
        ), export=s"$name(..)".just))
      case TraitType(tpe, name, children, false) =>
        List(PursType(List(
          s"data $name = ${children.map{
            case x if x.noargs => x.name
            case x => s"${x.name} ${x.name}"
          }.mkString(" | ")}"
        , s"derive instance eq$name :: Eq $name"
        ), export=s"$name(..)".just))
      case _: TupleType => Nil
      case _: NoargsType => Nil
      case RecursiveType(tpe, name) =>
        val f = fields(tpe)
        val fs = f.map{ case (name1, tpe, _, _) => name1 -> pursType(tpe) }
        val params = fs.map{ case (name1, tpe) => s"$name1 :: ${tpe._1}" }.mkString(", ")
        val x = s"newtype $name = $name { $params }"
        val eq = s"derive instance eq$name :: Eq $name"
        val defaults = f.collect{ case (name1, tpe1, _, Just(v)) => (name1, pursType(tpe1)._1, v) }
        Seq(
          PursType(List(x, eq), s"$name($name)".just).just
        , if (defaults.nonEmpty) {
            val tmpl1 = s"default$name :: { ${defaults.map{ case (name1, tpe1, _) => s"$name1 :: $tpe1" }.mkString(", ")} }"
            val tmpl2 = s"default$name = { ${defaults.map{ case (name1, _, v) => s"$name1: $v" }.mkString(", ")} }"
            PursType(Seq(tmpl1, tmpl2), s"default$name".just).just
          } else Nothing
        , if (genMaybe) {
            val params1 = fs.map{ case (name1, tpe) => s"$name1 :: ${tpe._2}" }.mkString(", ")
            if (params != params1) {
              val x1 = s"newtype $name' = $name' { $params1 }"
              PursType(List(x1), Nothing).just
            } else Nothing
          } else Nothing
        ).flatten
      case RegularType(tpe, name) =>
        val f = fields(tpe)
        val fs = f.map{ case (name1, tpe1, _, _) => name1 -> pursType(tpe1) }
        val params = fs.map{ case (name1, tpe1) => s"$name1 :: ${tpe1._1}" }.mkString(", ")
        val x = if (params.nonEmpty) s"type $name = { $params }" else s"type $name = {}"
        val defaults = f.collect{ case (name1, tpe1, _, Just(v)) => (name1, pursType(tpe1)._1, v) }
        Seq(
          PursType(Seq(x), s"$name".just).just
        , if (defaults.nonEmpty) {
            val tmpl1 = s"default$name :: { ${defaults.map{ case (name1, tpe1, _) => s"$name1 :: $tpe1" }.mkString(", ")} }"
            val tmpl2 = s"default$name = { ${defaults.map{ case (name1, _, v) => s"$name1: $v" }.mkString(", ")} }"
            PursType(Seq(tmpl1, tmpl2), s"default$name".just).just
          } else Nothing
        , if (genMaybe) {
            val params1 = fs.map{ case (name1, tpe1) => s"$name1 :: ${tpe1._2}" }.mkString(", ")
            if (params != params1) {
              val x1 = s"type $name' = { $params1 }"
              PursType(Seq(x1), Nothing).just
            } else Nothing
          } else Nothing
        ).flatten
    }.distinct
  }
}