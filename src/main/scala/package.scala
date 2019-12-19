package zd.proto

import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._
import scala.annotation.tailrec

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
  
  def encodeField(name: String, tpe: Type, n: Int): List[String] = {
    if (tpe =:= StringClass.selfType) {
      List(
        s"""Encode.uint32 ${(n<<3)+2}"""
      , s"""Encode.string msg.$name"""
      )
    } else if (tpe =:= IntClass.selfType) {
      List(
        s"""Encode.uint32 ${(n<<3)+0}"""
      , s"""Encode.uint32 msg.$name"""
      )
    } else if (tpe =:= BooleanClass.selfType) {
      List(
        s"""Encode.uint32 ${(n<<3)+0}"""
      , s"""Encode.boolean msg.$name"""
      )
    } else if (tpe =:= DoubleClass.selfType) {
      List(
        s"""Encode.uint32 ${(n<<3)+1}"""
      , s"""Encode.double msg.$name"""
      )
    } else if (tpe.typeConstructor =:= OptionClass.selfType.typeConstructor) {
      val typeArg = tpe.typeArgs.head.typeSymbol
      val tpe1 = typeArg.asType.toType
      if (tpe1 =:= StringClass.selfType) {
        s"""fromMaybe (fromArray []) $$ map (\\x -> concatAll [ Encode.uint32 ${(n<<3)+2}, Encode.string x ]) msg.$name""" :: Nil
      } else if (tpe1 =:= IntClass.selfType) {
        s"""fromMaybe (fromArray []) $$ map (\\x -> concatAll [ Encode.uint32 ${(n<<3)+0}, Encode.uint32 x ]) msg.$name""" :: Nil
      } else if (tpe1 =:= BooleanClass.selfType) {
        s"""fromMaybe (fromArray []) $$ map (\\x -> concatAll [ Encode.uint32 ${(n<<3)+0}, Encode.boolean x ]) msg.$name""" :: Nil
      } else if (tpe1 =:= DoubleClass.selfType) {
        s"""fromMaybe (fromArray []) $$ map (\\x -> concatAll [ Encode.uint32 ${(n<<3)+1}, Encode.double x ]) msg.$name""" :: Nil
      } else {
        val typeArgName = typeArg.name.encodedName.toString
        s"""fromMaybe (fromArray []) $$ map (\\x -> concatAll [ Encode.uint32 ${(n<<3)+2}, encode$typeArgName x ]) msg.$name""" :: Nil
      }
    } else if (tpe =:= typeOf[Array[Byte]]) {
      List(
        s"""Encode.uint32 ${(n<<3)+2}"""
      , s"""Encode.bytes msg.$name"""
      )
    } else if (isIterable(tpe)) {
      iterablePurs(tpe) match {
        case ArrayPurs(x) =>
          if (x =:= StringClass.selfType) {
            s"""concatAll $$ concatMap (\\x -> [ Encode.uint32 ${(n<<3)+2}, Encode.string x ]) msg.$name""" :: Nil
          } else {
            s"""concatAll $$ concatMap (\\x -> [ Encode.uint32 ${(n<<3)+2}, encode${x.typeSymbol.asClass.name.encodedName.toString} x ]) msg.$name""" :: Nil
          }
        case ArrayTuplePurs(tpe1, tpe2) =>
          s"concatAll $$ concatMap (\\x -> [ Encode.uint32 ${(n<<3)+2}, encode${tupleFunName(tpe1, tpe2)} x ]) msg.$name" :: Nil
      }
    } else {
      val tpeName = tpe.typeSymbol.name.encodedName.toString
      List(
        s"""Encode.uint32 ${(n<<3)+2}"""
      , s"""encode${tpeName} msg.$name"""
      )
    }
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
          (children.map(_.tpe)++tail, acc:+TraitType(head, children, firstLevel))
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
              (zs++tail, acc:+TupleType(appliedType(typeOf[Tuple2[Unit, Unit]].typeConstructor, x, y), x, y))
            case _ => throw new Exception(s"too many type args for ${head}")
          }
        } else {
          val xs = fields(head).map(_._2)
          val ys = xs.filter(complexType)
          val z =
            if (xs.isEmpty) NoargsType(head)
            else if (isRecursive(head, ys)) RecursiveType(head)
            else RegularType(head)
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
    x.annotations.filter(_.tree.tpe =:= typeOf[zd.proto.api.N])  match {
      case List(x1) => x1.tree.children.tail match {
        case List(Literal(Constant(n: Int))) => Some(n)
        case _ => throw new Exception("bad args in N")
      }
      case Nil => None
      case _ => throw new Exception(s"multiple N on ${x}")
    }
  }
  
  def fields(tpe: Type): List[(String, Type, Int)] = {
    tpe.typeSymbol.asClass.primaryConstructor.asMethod.paramLists.flatten.map{ x =>
      val term = x.asTerm
      (term.name.encodedName.toString, term.info, findN(x))
    }.collect{ case (a, b, Some(n)) => (a, b, n) }.sortBy(_._3)
  }
}