package zd.proto.purs

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
              s"$n ->"
            , s"  case decode$name _xs_ pos2 of"
            , s"    Left x -> Left x"
            , s"    Right { pos: pos3 } -> decode end (Just $name) pos3"
            )
          else
            List(
              s"$n ->"
            , s"  case decode$name _xs_ pos2 of"
            , s"    Left x -> Left x"
            , s"    Right { pos: pos3, val } ->"
            , s"      decode end (Just $$ $name val) pos3"
            )
        }
        val tmpl =
          s"""|decode$name :: Uint8Array -> Int -> Decode.Result $name
              |decode$name _xs_ pos0 = do
              |  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
              |  let end = pos + msglen
              |  decode end Nothing pos
              |    where
              |    decode :: Int -> Maybe $name -> Int -> Decode.Result $name
              |    decode end acc pos1 | pos1 < end =
              |      case Decode.uint32 _xs_ pos1 of
              |        Left x -> Left x
              |        Right { pos: pos2, val: tag } ->
              |          case tag `zshr` 3 of${cases.map("\n            "+_).mkString}
              |            _ ->
              |              case Decode.skipType _xs_ pos2 $$ tag .&. 7 of
              |                Left x -> Left x
              |                Right { pos: pos3 } ->
              |                  decode end acc pos3
              |    decode end (Just acc) pos1 = pure { pos: pos1, val: acc }
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
              |  { pos: pos1, val } <- decode end { ${nothingValue("first", tpe_1)}, ${nothingValue("second", tpe_2)} } pos
              |  case val of
              |    { ${justValue("first", tpe_1)}, ${justValue("second", tpe_2)} } -> pure { pos: pos1, val: Tuple first second }
              |    _ -> Left $$ Decode.MissingFields "$fun"
              |    where
              |    decode :: Int -> { first :: ${pursType(tpe_1)._2}, second :: ${pursType(tpe_2)._2} } -> Int -> Decode.Result { first :: ${pursType(tpe_1)._2}, second :: ${pursType(tpe_2)._2} }
              |    decode end acc pos1 =
              |      if pos1 < end then
              |        case Decode.uint32 _xs_ pos1 of
              |          Left x -> Left x
              |          Right { pos: pos2, val: tag } ->
              |            case tag `zshr` 3 of${cases.map("\n              "+_).mkString("")}
              |              _ ->
              |                case Decode.skipType _xs_ pos2 $$ tag .&. 7 of
              |                  Left x -> Left x
              |                  Right { pos: pos3 } ->
              |                    decode end acc pos3
              |      else pure { pos: pos1, val: acc }""".stripMargin
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
          s"""|decode$name :: Uint8Array -> Int -> Decode.Result $name
              |decode$name _xs_ pos0 = do
              |  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
              |  let end = pos + msglen
              |  { pos: pos1, ${if (recursive) s"val: $name' val" else "val"} } <- decode end ${if (recursive) s"($name' ${defObj})" else s"${defObj}"} pos
              |  case val of
              |    ${justObj} -> pure { pos: pos1, val: ${if (recursive) s"$name " else ""}${unObj} }${if (justObj==unObj) "" else s"""\n    _ -> Left $$ Decode.MissingFields "$name""""}
              |    where
              |    decode :: Int -> $name' -> Int -> Decode.Result $name'
              |    decode end ${if (recursive) s"($name' acc)" else "acc"} pos1 =
              |      if pos1 < end then
              |        case Decode.uint32 _xs_ pos1 of
              |          Left x -> Left x
              |          Right { pos: pos2, val: tag } ->
              |            case tag `zshr` 3 of${cases.map("\n              "+_).mkString("")}
              |              _ ->
              |                case Decode.skipType _xs_ pos2 $$ tag .&. 7 of
              |                  Left x -> Left x
              |                  Right { pos: pos3 } ->
              |                    decode end ${if (recursive) s"($name' acc)" else "acc"} pos3
              |      else pure { pos: pos1, val: ${if (recursive) s"$name' acc" else "acc"} }""".stripMargin
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
          s"""|decode$name :: Uint8Array -> Int -> Decode.Result $name
              |decode$name _xs_ pos0 = do
              |  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
              |  let end = pos + msglen
              |  { pos: pos1, ${if (recursive) s"val: $name' val" else "val"} } <- decode end ${if (recursive) s"($name' ${defObj})" else s"${defObj}"} pos
              |  case val of
              |    ${justObj} -> pure { pos: pos1, val: ${if (recursive) s"$name " else ""}${unObj} }${if (justObj==unObj) "" else s"""\n    _ -> Left $$ Decode.MissingFields "$name""""}
              |    where
              |    decode :: Int -> $name' -> Int -> Decode.Result $name'
              |    decode end ${if (recursive) s"($name' acc)" else "acc"} pos1 =
              |      if pos1 < end then
              |        case Decode.uint32 _xs_ pos1 of
              |          Left x -> Left x
              |          Right { pos: pos2, val: tag } ->
              |            case tag `zshr` 3 of${cases.map("\n              "+_).mkString("")}
              |              _ ->
              |                case Decode.skipType _xs_ pos2 $$ tag .&. 7 of
              |                  Left x -> Left x
              |                  Right { pos: pos3 } ->
              |                    decode end ${if (recursive) s"($name' acc)" else "acc"} pos3
              |      else pure { pos: pos1, val: ${if (recursive) s"$name' acc" else "acc"} }""".stripMargin
        Coder(tmpl, Nothing)
    }.distinct
  }
}