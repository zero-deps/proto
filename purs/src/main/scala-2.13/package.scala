import scala.annotation.tailrec
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._
import zero.ext._, option._
import proto.Bytes

package object purs {
  def pursTypePars(tpe: Type): (String, String) = {
    if (tpe =:= StringClass.selfType) {
      "String" -> "(Maybe String)"
    } else if (tpe =:= IntClass.selfType) {
      "Int" -> "(Maybe Int)"
    } else if (tpe =:= LongClass.selfType) {
      "Number" -> "(Maybe Number)"
    } else if (tpe =:= BooleanClass.selfType) {
      "Boolean" -> "(Maybe Boolean)"
    } else if (tpe =:= DoubleClass.selfType) {
      "Number" -> "(Maybe Number)"
    } else if (tpe =:= typeOf[Array[Byte]] || tpe =:= typeOf[Bytes]) {
      "Uint8Array" -> "(Maybe Uint8Array)"
    } else if (tpe.typeConstructor =:= OptionClass.selfType.typeConstructor) {
      val typeArg = tpe.typeArgs.head
      if (typeArg =:= LongClass.selfType) {
        "(Maybe Number)" -> "(Maybe Number)"
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

  def pursTypeParsTex(tpe: Type): String = {
    if (tpe =:= StringClass.selfType) {
      "String"
    } else if (tpe =:= IntClass.selfType) {
      "Int"
    } else if (tpe =:= LongClass.selfType) {
      "Number"
    } else if (tpe =:= BooleanClass.selfType) {
      "Boolean"
    } else if (tpe =:= DoubleClass.selfType) {
      "Number"
    } else if (tpe =:= typeOf[Array[Byte]] || tpe =:= typeOf[Bytes]) {
      "Uint8Array"
    } else if (tpe.typeConstructor =:= OptionClass.selfType.typeConstructor) {
      val typeArg = tpe.typeArgs.head
      if (typeArg =:= LongClass.selfType) {
        "(Maybe Number)"
      } else if (typeArg =:= DoubleClass.selfType) {
        "(Maybe Number)"
      } else {
        val name = typeArg.typeSymbol.name.encodedName.toString
        if (complexType(typeArg)) s"(Maybe \\hyperlink{$name}{$name})"
        else s"(Maybe $name)"
      }
    } else if (isIterable(tpe)) {
      iterablePurs(tpe) match {
        case ArrayPurs(tpe) =>
          val name = tpe.typeSymbol.asClass.name.encodedName.toString
          if (complexType(tpe)) s"[\\hyperlink{$name}{$name}]"
          else s"[$name]"
        case ArrayTuplePurs(tpe1, tpe2) =>
          val name1 = pursTypeParsTex(tpe1)
          val name2 = pursTypeParsTex(tpe2)
          s"[Tuple $name1 $name2]"
      }
    } else {
      val name = tpe.typeSymbol.name.encodedName.toString
      s"\\hyperlink{$name}{$name}"
    }
  }
  
  def pursType(tpe: Type): (String, String) = {
    def trim(x: String): String = x.stripPrefix("(").stripSuffix(")")
    val (a, b) = pursTypePars(tpe)
    trim(a) -> trim(b)
  }
  
  def pursTypeTex(tpe: Type): String = {
    pursTypeParsTex(tpe).stripPrefix("(").stripSuffix(")")
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

  val complexType: Type => Boolean = {
    case tpe if tpe =:= StringClass.selfType => false
    case tpe if tpe =:= IntClass.selfType => false
    case tpe if tpe =:= LongClass.selfType => false
    case tpe if tpe =:= BooleanClass.selfType => false
    case tpe if tpe =:= DoubleClass.selfType => false
    case tpe if tpe =:= typeOf[Array[Byte]] || tpe =:= typeOf[Bytes]  => false
    case _ => true
  }
  def isRecursive(base: Type): Boolean = {
    @tailrec def loop(compareTo: List[Type]): Boolean = compareTo match {
      case Nil => false
      case x :: _ if x =:= base => true
      case x :: xs => loop(x.typeArgs.map(_.typeSymbol.asType.toType) ++ xs)
    }
    loop(fields(base).map(_._2).filter(complexType))
  }
  def isTrait(t: Type): Boolean = {
    t.typeSymbol.isClass && t.typeSymbol.asClass.isTrait && t.typeSymbol.asClass.isSealed
  }

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

  def type_to_tpe(head: Type): (Seq[Type], Tpe) = {
    val xs = fields(head).map(_._2)
    val ys = xs.filter(complexType)
    val z =
      if (xs.isEmpty) NoargsType(head, head.typeSymbol.name.encodedName.toString)
      else if (isRecursive(head)) RecursiveType(head, head.typeSymbol.name.encodedName.toString)
      else RegularType(head, head.typeSymbol.name.encodedName.toString)
    (ys, z)
  }

  def collectTpes(tpe: Type): Seq[Tpe] = {
    collectTpes(tpe, tail=Nil, acc=Nil, firstLevel=true)
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

  def makePursTypes(types: Seq[Tpe], genMaybe: Boolean): Seq[PursType] = {
    types.flatMap{
      case TraitType(tpe, name, children, _) =>
        List(PursType(List(
          s"data $name = ${children.map{
            case x if x.noargs => x.name
            case x if x.rec => s"${x.name}'' ${x.name}"
            case x => s"${x.name} ${x.name}"
          }.mkString(" | ")}"
        , s"derive instance eq$name :: Eq $name"
        ), export=s"$name(..)".some))
      case _: TupleType => Nil
      case _: NoargsType => Nil
      case RecursiveType(tpe, name) =>
        val f = fields(tpe)
        val fs = f.map{ case (name1, tpe, _, _) => name1 -> pursType(tpe) }
        val params = fs.map{ case (name1, tpe) => s"$name1 :: ${tpe._1}" }.mkString(", ")
        val x = s"newtype $name = $name { $params }"
        val eq = s"derive instance eq$name :: Eq $name"
        val defaults = f.collect{ case (name1, tpe1, _, v: HasDefFun) => (name1, pursType(tpe1)._1, v) }
        Seq(
          PursType(List(x, eq), s"$name($name)".some).some
        , if (defaults.nonEmpty) {
            val tmpl1 = s"default$name :: { ${defaults.map{ case (name1, tpe1, _) => s"$name1 :: $tpe1" }.mkString(", ")} }"
            val tmpl2 = s"default$name = { ${defaults.map{ case (name1, _, v) => s"$name1: ${v.value}" }.mkString(", ")} }"
            PursType(Seq(tmpl1, tmpl2), s"default$name".some).some
          } else None
        , if (genMaybe) {
            val params1 = fs.map{ case (name1, tpe) => s"$name1 :: ${tpe._2}" }.mkString(", ")
            if (params != params1) {
              val x1 = s"type $name' = { $params1 }"
              PursType(List(x1), None).some
            } else None
          } else None
        ).flatten
      case RegularType(tpe, name) =>
        val f = fields(tpe)
        val fs = f.map{ case (name1, tpe1, _, _) => name1 -> pursType(tpe1) }
        val params = fs.map{ case (name1, tpe1) => s"$name1 :: ${tpe1._1}" }.mkString(", ")
        val x = if (params.nonEmpty) s"type $name = { $params }" else s"type $name = {}"
        val defaults = f.collect{ case (name1, tpe1, _, v: HasDefFun) => (name1, pursType(tpe1)._1, v) }
        Seq(
          PursType(Seq(x), s"$name".some).some
        , if (defaults.nonEmpty) {
            val tmpl1 = s"default$name :: { ${defaults.map{ case (name1, tpe1, _) => s"$name1 :: $tpe1" }.mkString(", ")} }"
            val tmpl2 = s"default$name = { ${defaults.map{ case (name1, _, v) => s"$name1: ${v.value}" }.mkString(", ")} }"
            PursType(Seq(tmpl1, tmpl2), s"default$name".some).some
          } else None
        , if (genMaybe) {
            val params1 = fs.map{ case (name1, tpe1) => s"$name1 :: ${tpe1._2}" }.mkString(", ")
            if (params != params1) {
              val x1 = s"type $name' = { $params1 }"
              PursType(Seq(x1), None).some
            } else None
          } else None
        ).flatten
    }.distinct
  }
}