package zd.proto

import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._

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
        case SetPurs(tpe) =>
          val name = tpe.typeSymbol.asClass.name.encodedName.toString
          s"(Set $name)" -> s"(Set $name)"
        case ArrayPurs(tpe) =>
          val name = tpe.typeSymbol.asClass.name.encodedName.toString
          s"(Array $name)" -> s"(Array $name)"
        case MapPurs(tpe1, tpe2) =>
          val name1 = pursTypePars(tpe1)._1
          val name2 = pursTypePars(tpe2)._1
          s"(Map $name1 $name2)" -> s"(Map $name1 $name2)"
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
  
  def decodeField(recursive: Option[String])(name: String, tpe: Type, n: Int): List[String] = {
    def tmpl(n: Int, fun: String, mod: String): List[String] = {
      List(
        s"${n} ->"
      , s"  case ${fun} _xs_ pos2 of"
      , s"    Left x -> Left x"
      , s"    Right { pos: pos3, val } ->"
      , s"      decode end (${if (recursive.isDefined) s"${recursive.get}' $$ " else ""}acc { ${mod} }) pos3"
      )
    }
    if (tpe =:= StringClass.selfType) {
      tmpl(n, "Decode.string", s"$name = Just val")
    } else if (tpe =:= IntClass.selfType) {
      tmpl(n, "Decode.int32", s"$name = Just val")
    } else if (tpe =:= BooleanClass.selfType) {
      tmpl(n, "Decode.boolean", s"$name = Just val")
    } else if (tpe =:= DoubleClass.selfType) {
      tmpl(n, "Decode.double", s"$name = Just val")
    } else if (tpe.typeConstructor =:= OptionClass.selfType.typeConstructor) {
      val typeArg = tpe.typeArgs.head.typeSymbol
      val tpe1 = typeArg.asType.toType
      if (tpe1 =:= StringClass.selfType) {
        tmpl(n, "Decode.string", s"$name = Just val")
      } else if (tpe1 =:= IntClass.selfType) {
        tmpl(n, "Decode.int32", s"$name = Just val")
      } else if (tpe1 =:= BooleanClass.selfType) {
        tmpl(n, "Decode.boolean", s"$name = Just val")
      } else if (tpe1 =:= DoubleClass.selfType) {
        tmpl(n, "Decode.double", s"$name = Just val")
      } else {
        val typeArgName = typeArg.name.encodedName.toString
        tmpl(n, s"decode$typeArgName", s"$name = Just val")
      }
    } else if (isIterable(tpe)) {
      iterablePurs(tpe) match {
        case SetPurs(tpe) =>
          if (tpe =:= StringClass.selfType) {
            tmpl(n, "Decode.string", s"$name = Set.insert val acc.$name")
          } else {
            throw new Exception("unexpected case")
          }
        case ArrayPurs(tpe) =>
          if (tpe =:= StringClass.selfType) {
            tmpl(n, "Decode.string", s"$name = snoc acc.$name val")
          } else {
            val tpeName = tpe.typeSymbol.asClass.name.encodedName.toString
            tmpl(n, s"decode$tpeName", s"$name = snoc acc.$name val")
          }
        case MapPurs(tpe1, tpe2) =>
          tmpl(n, s"decode${tupleFunName(tpe1, tpe2)}", s"$name = Map.insert (fst val) (snd val) acc.$name")
        case ArrayTuplePurs(tpe1, tpe2) =>
          tmpl(n, s"decode${tupleFunName(tpe1, tpe2)}", s"$name = snoc acc.$name val")
      }
    } else {
      val name1 = tpe.typeSymbol.name.encodedName.toString
      tmpl(n, s"decode${name1}", s"$name = Just val")
    }
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
        case SetPurs(x) =>
          if (x =:= StringClass.selfType) {
            s"""concatAll $$ concatMap (\\x -> [ Encode.uint32 ${(n<<3)+2}, Encode.string x ]) $$ Set.toUnfoldable msg.$name""" :: Nil
          } else {
            s"""concatAll $$ concatMap (\\x -> [ Encode.uint32 ${(n<<3)+2}, encode${x.typeSymbol.asClass.name.encodedName.toString} x ]) $$ Set.toUnfoldable msg.$name""" :: Nil
          }
        case ArrayPurs(x) =>
          if (x =:= StringClass.selfType) {
            s"""concatAll $$ concatMap (\\x -> [ Encode.uint32 ${(n<<3)+2}, Encode.string x ]) msg.$name""" :: Nil
          } else {
            s"""concatAll $$ concatMap (\\x -> [ Encode.uint32 ${(n<<3)+2}, encode${x.typeSymbol.asClass.name.encodedName.toString} x ]) msg.$name""" :: Nil
          }
        case MapPurs(tpe1, tpe2) =>
          s"concatAll $$ concatMap (\\x -> [ Encode.uint32 ${(n<<3)+2}, encode${tupleFunName(tpe1, tpe2)} x ]) $$ Map.toUnfoldableUnordered msg.$name" :: Nil
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
        case _: SetPurs => s"$name: Set.empty"
        case _: ArrayPurs => s"$name: []"
        case _: MapPurs => s"$name: Map.empty"
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

  private[this] def iterablePurs(tpe: Type): IterablePurs = {
    tpe.typeArgs match {
      case x :: Nil =>
        if (tpe.baseClasses.exists(_.asType.toType.typeConstructor <:< typeOf[scala.collection.Set[Unit]].typeConstructor) &&
            x =:= StringClass.selfType)
          SetPurs(x)
        else
          ArrayPurs(x)
      case x :: y :: Nil if x =:= StringClass.selfType || x =:= IntClass.selfType ||  x =:= DoubleClass.selfType =>
        MapPurs(x, y)
      case x :: y :: Nil =>
        ArrayTuplePurs(x, y)
      case _ => throw new Exception(s"too many type args for $tpe")
    }
  }
}