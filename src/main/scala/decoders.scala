package zd.proto.purs

import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._
import zd.proto.api.MessageCodec
import zd.gs.z._

object Decoders {
  def from(types: Seq[Tpe], codecs: List[MessageCodec[_]]): Seq[Coder] = {
    types.map{
      case TraitType(tpe, children, true) =>
        val cases = children.map{ case ChildMeta(name, tpe, n, noargs) =>
          if (noargs)
            List(
              s"$n -> do"
            , s"  { pos: pos2 } <- decode$name _xs_ pos1"
            , s"  pure { pos: pos2, val: $name }"
            )
          else
            List(
              s"$n -> do"
            , s"  { pos: pos2, val } <- decode$name _xs_ pos1"
            , s"  pure { pos: pos2, val: $name val }"
            )
        }
        val name = tpe.typeSymbol.name.encodedName.toString
        val tmpl =
          s"""|decode$name :: Uint8Array -> Decode.Result $name
              |decode$name _xs_ = do
              |  { pos: pos1, val: tag } <- Decode.uint32 _xs_ 0
              |  case tag `zshr` 3 of${cases.map(_.map("\n    "+_).mkString("")).mkString("")}
              |    i ->
              |      Left $$ Decode.BadType i""".stripMargin
        Coder(tmpl, s"decode$name".just)
      case TraitType(tpe, children, false) =>
        val name = tpe.typeSymbol.name.encodedName.toString
        val cases = children.flatMap{ case ChildMeta(name, tpe, n, noargs) =>
          if (noargs)
            List(
              s"$n -> do"
            , s"  { pos: pos3 } <- decode$name _xs_ pos2"
            , s"  pure $$ Loop { a: end, b: Just $name, c: pos3 }"
            )
          else
            List(
              s"$n -> do"
            , s"  { pos: pos3, val } <- decode$name _xs_ pos2"
            , s"  pure $$ Loop { a: end, b: Just $$ $name val, c: pos3 }"
            )
        }
        val tmpl =
          s"""|decode$name :: Uint8Array -> Int -> Decode.Result $name
              |decode$name _xs_ pos0 = do
              |  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
              |  let end = pos + msglen
              |  tailRecM3 decode end Nothing pos
              |    where
              |    decode :: Int -> Maybe $name -> Int -> Decode.Result' (Step { a :: Int, b :: Maybe $name, c :: Int } { pos :: Int, val :: $name })
              |    decode end acc pos1 | pos1 < end = do
              |      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
              |      case tag `zshr` 3 of${cases.map("\n        "+_).mkString}
              |        _ -> do
              |          { pos: pos3 } <- Decode.skipType _xs_ pos2 $$ tag .&. 7
              |          pure $$ Loop { a: end, b: acc, c: pos3 }
              |    decode end (Just acc) pos1 = pure $$ Done { pos: pos1, val: acc }
              |    decode end acc@Nothing pos1 = Left $$ Decode.MissingFields "$name"""".stripMargin
        Coder(tmpl, Nothing)
      case TupleType(tpe, tpe_1, tpe_2) =>
        val xs = codecs.find(_.aType == tpe.toString).map(_.nums).getOrElse(throw new Exception(s"codec is missing for ${tpe.toString}"))
        val fun = "decode" + tupleFunName(tpe_1, tpe_2)
        val cases = List(("first", tpe_1, xs("_1")), ("second", tpe_2, xs("_2"))).flatMap((decodeField(None) _).tupled)
        val tmpl =
          s"""|$fun :: Uint8Array -> Int -> Decode.Result (Tuple ${pursTypePars(tpe_1)._1} ${pursTypePars(tpe_2)._1})
              |$fun _xs_ pos0 = do
              |  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
              |  let end = pos + msglen
              |  { pos: pos1, val } <- tailRecM3 decode end { ${nothingValue("first", tpe_1)}, ${nothingValue("second", tpe_2)} } pos
              |  case val of
              |    { ${justValue("first", tpe_1)}, ${justValue("second", tpe_2)} } -> pure { pos: pos1, val: Tuple first second }
              |    _ -> Left $$ Decode.MissingFields "$fun"
              |    where
              |    decode :: Int -> { first :: ${pursType(tpe_1)._2}, second :: ${pursType(tpe_2)._2} } -> Int -> Decode.Result' (Step { a :: Int, b :: { first :: ${pursType(tpe_1)._2}, second :: ${pursType(tpe_2)._2} }, c :: Int } { pos :: Int, val :: { first :: ${pursType(tpe_1)._2}, second :: ${pursType(tpe_2)._2} } })
              |    decode end acc pos1 | pos1 < end = do
              |      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
              |      case tag `zshr` 3 of${cases.map("\n        "+_).mkString("")}
              |        _ -> do
              |          { pos: pos3 } <- Decode.skipType _xs_ pos2 $$ tag .&. 7
              |          pure $$ Loop { a: end, b: acc, c: pos3 }
              |    decode end acc pos1 = pure $$ Done { pos: pos1, val: acc }""".stripMargin
        Coder(tmpl, Nothing)
      case NoargsType(tpe) =>
        val name = tpe.typeSymbol.name.encodedName.toString
        val tmpl =
          s"""|decode$name :: Uint8Array -> Int -> Decode.Result Unit
              |decode$name _xs_ pos0 = do
              |  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
              |  let end = pos + msglen
              |  pure { pos: end, val: unit }""".stripMargin
        Coder(tmpl, Nothing)
      case RecursiveType(tpe) =>
        val recursive = true
        val fs = fields(tpe)
        val defObj: String = fs.map{ case (name, tpe, _) => nothingValue(name, tpe) }.mkString("{ ", ", ", " }")
        val justObj: String = fs.map{ case (name, tpe, _) => justValue(name, tpe) }.mkString("{ ", ", ", " }")
        val unObj: String = fs.map{
          case (name,_,_) => name
        }.mkString("{ ", ", ", " }")
        val name = tpe.typeSymbol.name.encodedName.toString
        val cases = fields(tpe).flatMap((decodeField(Some(name).filter(_ => recursive)) _).tupled)
        val tmpl =
          if (justObj == unObj)
            s"""|decode$name :: Uint8Array -> Int -> Decode.Result $name
                |decode$name _xs_ pos0 = do
                |  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
                |  let end = pos + msglen
                |  tailRecM3 decode end ($name $defObj) pos
                |    where
                |    decode :: Int -> $name -> Int -> Decode.Result' (Step { a :: Int, b :: $name, c :: Int } { pos :: Int, val :: $name })
                |    decode end ($name acc) pos1 | pos1 < end = do
                |      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
                |      case tag `zshr` 3 of${cases.map("\n        "+_).mkString("")}
                |        _ -> do
                |          { pos: pos3 } <- Decode.skipType _xs_ pos2 $$ tag .&. 7
                |          pure $$ Loop { a: end, b: ($name' acc), c: pos3 }
                |    decode end acc pos1 = pure $$ Done { pos: pos1, val: acc }""".stripMargin
          else
            s"""|decode$name :: Uint8Array -> Int -> Decode.Result $name
                |decode$name _xs_ pos0 = do
                |  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
                |  let end = pos + msglen
                |  { pos: pos1, val } <- tailRecM3 decode end ($name' $defObj) pos
                |  case val of
                |    $name' $justObj -> pure { pos: pos1, val: $name $unObj }
                |    _ -> Left $$ Decode.MissingFields "$name"
                |    where
                |    decode :: Int -> $name' -> Int -> Decode.Result' (Step { a :: Int, b :: $name', c :: Int } { pos :: Int, val :: $name' })
                |    decode end ($name' acc) pos1 | pos1 < end = do
                |      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
                |      case tag `zshr` 3 of${cases.map("\n        "+_).mkString("")}
                |        _ -> do
                |          { pos: pos3 } <- Decode.skipType _xs_ pos2 $$ tag .&. 7
                |          pure $$ Loop { a: end, b: ($name' acc), c: pos3 }
                |    decode end acc pos1 = pure $$ Done { pos: pos1, val: acc }""".stripMargin
        Coder(tmpl, Nothing)
      case RegularType(tpe) =>
        val recursive = false
        val fs = fields(tpe)
        val defObj: String = fs.map{ case (name, tpe, _) => nothingValue(name, tpe) }.mkString("{ ", ", ", " }")
        val justObj: String = fs.map{ case (name, tpe, _) => justValue(name, tpe) }.mkString("{ ", ", ", " }")
        val unObj: String = fs.map{
          case (name,_,_) => name
        }.mkString("{ ", ", ", " }")
        val name = tpe.typeSymbol.name.encodedName.toString
        val cases = fields(tpe).flatMap((decodeField(Some(name).filter(_ => recursive)) _).tupled)
        val tmpl =
          if (justObj == unObj)
            s"""|decode$name :: Uint8Array -> Int -> Decode.Result $name
                |decode$name _xs_ pos0 = do
                |  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
                |  let end = pos + msglen
                |  tailRecM3 decode end $defObj pos
                |    where
                |    decode :: Int -> $name -> Int -> Decode.Result' (Step { a :: Int, b :: $name, c :: Int } { pos :: Int, val :: $name })
                |    decode end acc pos1 | pos1 < end = do
                |      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
                |      case tag `zshr` 3 of${cases.map("\n        "+_).mkString("")}
                |        _ -> do
                |          { pos: pos3 } <- Decode.skipType _xs_ pos2 $$ tag .&. 7
                |          pure $$ Loop { a: end, b: acc, c: pos3 }
                |    decode end acc pos1 = pure $$ Done { pos: pos1, val: acc }""".stripMargin
          else
            s"""|decode$name :: Uint8Array -> Int -> Decode.Result $name
                |decode$name _xs_ pos0 = do
                |  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
                |  let end = pos + msglen
                |  { pos: pos1, val } <- tailRecM3 decode end $defObj pos
                |  case val of
                |    $justObj -> pure { pos: pos1, val: $unObj }
                |    _ -> Left $$ Decode.MissingFields "$name"
                |    where
                |    decode :: Int -> $name' -> Int -> Decode.Result' (Step { a :: Int, b :: $name', c :: Int } { pos :: Int, val :: $name' })
                |    decode end acc pos1 | pos1 < end = do
                |      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
                |      case tag `zshr` 3 of${cases.map("\n        "+_).mkString("")}
                |        _ -> do
                |          { pos: pos3 } <- Decode.skipType _xs_ pos2 $$ tag .&. 7
                |          pure $$ Loop { a: end, b: acc, c: pos3 }
                |    decode end acc pos1 = pure $$ Done { pos: pos1, val: acc }""".stripMargin
        Coder(tmpl, Nothing)
    }.distinct
  }
  
  private[this] def decodeField(recursive: Option[String])(name: String, tpe: Type, n: Int): List[String] = {
    def tmpl(n: Int, fun: String, mod: String): List[String] = {
      List(
        s"${n} -> do"
      , s"  { pos: pos3, val } <- $fun _xs_ pos2"
      , s"  pure $$ Loop { a: end, b: ${if (recursive.isDefined) s"${recursive.get}' $$ " else ""}acc { $mod }, c: pos3 }"
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
        case ArrayPurs(tpe) =>
          if (tpe =:= StringClass.selfType) {
            tmpl(n, "Decode.string", s"$name = snoc acc.$name val")
          } else {
            val tpeName = tpe.typeSymbol.asClass.name.encodedName.toString
            tmpl(n, s"decode$tpeName", s"$name = snoc acc.$name val")
          }
        case ArrayTuplePurs(tpe1, tpe2) =>
          tmpl(n, s"decode${tupleFunName(tpe1, tpe2)}", s"$name = snoc acc.$name val")
      }
    } else {
      val name1 = tpe.typeSymbol.name.encodedName.toString
      tmpl(n, s"decode${name1}", s"$name = Just val")
    }
  }
}