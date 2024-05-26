package proto
package purs

import scala.annotation.unused
import scala.quoted.*

trait Decoders extends Ops with Purs:
  implicit val qctx: Quotes
  import qctx.reflect.*

  def decodersFrom(types: Seq[Tpe]): Seq[Coder] =
    types.map{
      case TraitType(tpe, name, children, true) =>
        val cases = children.map{ case ChildMeta(name1, _, n, noargs, rec) =>
          if (noargs)   s"$n -> decode (decode$name1 _xs_ pos1) \\_ -> $name1"
          else if (rec) s"$n -> decode (decode$name1 _xs_ pos1) $name1''"
          else          s"$n -> decode (decode$name1 _xs_ pos1) $name1"
        }
        val tmpl =
          s"""|decode$name :: Uint8Array -> Decode.Result $name
              |decode$name _xs_ = do
              |  { pos: pos1, val: tag } <- Decode.unsignedVarint32 _xs_ 0
              |  case tag `zshr` 3 of${cases.map("\n    "+_).mkString("")}
              |    i -> Left $$ Decode.BadType i
              |  where
              |  decode :: forall a. Decode.Result a -> (a -> $name) -> Decode.Result $name
              |  decode res f = map (\\{ pos, val } -> { pos, val: f val }) res""".stripMargin
        Coder(tmpl, Some(s"decode$name"))
      case TraitType(tpe, name, children, false) =>
        val cases = children.flatMap{ case ChildMeta(name1, _, n, noargs, rec) =>
          if (noargs)   s"$n -> decodeFieldLoop end (decode$name1 _xs_ pos2) \\_ -> Just $name1" :: Nil
          else if (rec) s"$n -> decodeFieldLoop end (decode$name1 _xs_ pos2) (Just <<< $name1'')" :: Nil
          else          s"$n -> decodeFieldLoop end (decode$name1 _xs_ pos2) (Just <<< $name1)" :: Nil
        }
        val tmpl =
          s"""|decode$name :: Uint8Array -> Int -> Decode.Result $name
              |decode$name _xs_ pos0 = do
              |  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
              |  tailRecM3 decode (pos + msglen) Nothing pos
              |    where
              |    decode :: Int -> Maybe $name -> Int -> Decode.Result' (Step { a :: Int, b :: Maybe $name, c :: Int } { pos :: Int, val :: $name })
              |    decode end acc pos1 | pos1 < end = do
              |      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
              |      case tag `zshr` 3 of${cases.map("\n        "+_).mkString}
              |        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $$ tag .&. 7) \\_ -> acc
              |    decode end (Just acc) pos1 = pure $$ Done { pos: pos1, val: acc }
              |    decode end acc@Nothing pos1 = Left $$ Decode.MissingFields "$name"""".stripMargin
        Coder(tmpl, None)
      case TupleType(tpe, tupleName, tpe_1, tpe_2) =>
        val fun = "decode" + tupleName
        val cases = List(("first", tpe_1, 1, NoDef), ("second", tpe_2, 2, NoDef)).flatMap(decodeFieldLoop.tupled)
        val tmpl =
          s"""|$fun :: Uint8Array -> Int -> Decode.Result (Tuple ${pursTypePars(tpe_1)._1} ${pursTypePars(tpe_2)._1})
              |$fun _xs_ pos0 = do
              |  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
              |  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { ${nothingValue("first", tpe_1)}, ${nothingValue("second", tpe_2)} } pos
              |  case val of
              |    { ${justValue("first", tpe_1)}, ${justValue("second", tpe_2)} } -> pure { pos: pos1, val: Tuple first second }
              |    _ -> Left $$ Decode.MissingFields "$fun"
              |    where
              |    decode :: Int -> { first :: ${pursType(tpe_1)._2}, second :: ${pursType(tpe_2)._2} } -> Int -> Decode.Result' (Step { a :: Int, b :: { first :: ${pursType(tpe_1)._2}, second :: ${pursType(tpe_2)._2} }, c :: Int } { pos :: Int, val :: { first :: ${pursType(tpe_1)._2}, second :: ${pursType(tpe_2)._2} } })
              |    decode end acc pos1 | pos1 < end = do
              |      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
              |      case tag `zshr` 3 of${cases.map("\n        "+_).mkString("")}
              |        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $$ tag .&. 7) (\\_ -> acc)
              |    decode end acc pos1 = pure $$ Done { pos: pos1, val: acc }""".stripMargin
        Coder(tmpl, None)
      case NoargsType(tpe, name) =>
        val tmpl =
          s"""|decode$name :: Uint8Array -> Int -> Decode.Result Unit
              |decode$name _xs_ pos0 = do
              |  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
              |  pure { pos: pos + msglen, val: unit }""".stripMargin
        Coder(tmpl, None)
      case RecursiveType(tpe, name) =>
        val fs = fields(tpe)
        val defObj: String = fs.map{ case (name, tpe, _, _) => nothingValue(name, tpe) }.mkString("{ ", ", ", " }")
        val justObj: String = fs.map{ case (name, tpe, _, _) => justValue(name, tpe) }.mkString("{ ", ", ", " }")
        val unObj: String = fs.map{
          case (name,_,_,_) => name
        }.mkString("{ ", ", ", " }")
        val simplify = fs.forall{ case (name, tpe, _, defval) => defval.isInstanceOf[HasDefFun] }
        val hasDefval = fs.exists(_._4.isInstanceOf[FillDef])
        val tmpl =
          if (simplify) {
            if (hasDefval) {
              val defs = fs.map{
                case (name, _, _, defval: FillDef) => s"$name: fromMaybe ${defval.value} $name"
                case (name, _, _, _) => name
              }.mkString(", ")
              val cases = fs.flatMap(decodeFieldLoop.tupled)
              s"""|decode$name :: Uint8Array -> Int -> Decode.Result $name
                  |decode$name _xs_ pos0 = do
                  |  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
                  |  { pos: pos1, val: $unObj } <- tailRecM3 decode (pos + msglen) $defObj pos
                  |  pure { pos: pos1, val: $name { $defs }}
                  |    where
                  |    decode :: Int -> $name' -> Int -> Decode.Result' (Step { a :: Int, b :: $name', c :: Int } { pos :: Int, val :: $name' })
                  |    decode end acc pos1 | pos1 < end = do
                  |      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
                  |      case tag `zshr` 3 of${cases.map("\n        "+_).mkString("")}
                  |        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $$ tag .&. 7) \\_ -> acc
                  |    decode end acc pos1 = pure $$ Done { pos: pos1, val: acc }""".stripMargin
            } else {
              val cases = fs.flatMap(decodeFieldLoopNewtype(name).tupled)
              s"""|decode$name :: Uint8Array -> Int -> Decode.Result $name
                  |decode$name _xs_ pos0 = do
                  |  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
                  |  tailRecM3 decode (pos + msglen) ($name $defObj) pos
                  |    where
                  |    decode :: Int -> $name -> Int -> Decode.Result' (Step { a :: Int, b :: $name, c :: Int } { pos :: Int, val :: $name })
                  |    decode end acc'@($name acc) pos1 | pos1 < end = do
                  |      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
                  |      case tag `zshr` 3 of${cases.map("\n        "+_).mkString("")}
                  |        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $$ tag .&. 7) \\_ -> acc'
                  |    decode end acc pos1 = pure $$ Done { pos: pos1, val: acc }""".stripMargin
            }
          } else {
            if (hasDefval) {
              val name = tpe.typeSymbol.name
              val name1 = tpe.typeSymbol.name + "'"
              val cases = fs.flatMap(decodeFieldLoop.tupled)
              val defs = fs.map{
                case (name, _, _, defval: FillDef) => s"$name: fromMaybe ${defval.value} $name"
                case (name, _, _, _) => name
              }.mkString(", ")
              val case1 = fs.map{
                case (name, tpe, _, _: FillDef) => name
                case (name, tpe, _, _) => justValue(name, tpe)
              }.mkString(", ")
              s"""|decode$name :: Uint8Array -> Int -> Decode.Result $name
                  |decode$name _xs_ pos0 = do
                  |  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
                  |  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) $defObj pos
                  |  case val of
                  |    { $case1 } -> pure { pos: pos1, val: $name { $defs }}
                  |    _ -> Left $$ Decode.MissingFields "$name"
                  |    where
                  |    decode :: Int -> $name1 -> Int -> Decode.Result' (Step { a :: Int, b :: $name1, c :: Int } { pos :: Int, val :: $name1 })
                  |    decode end acc pos1 | pos1 < end = do
                  |      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
                  |      case tag `zshr` 3 of${cases.map("\n        "+_).mkString("")}
                  |        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $$ tag .&. 7) \\_ -> acc
                  |    decode end acc pos1 = pure $$ Done { pos: pos1, val: acc }""".stripMargin
            } else {
              val name = tpe.typeSymbol.name
              val name1 = tpe.typeSymbol.name + "'"
              val cases = fs.flatMap(decodeFieldLoop.tupled)
              s"""|decode$name :: Uint8Array -> Int -> Decode.Result $name
                  |decode$name _xs_ pos0 = do
                  |  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
                  |  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) $defObj pos
                  |  case val of
                  |    $justObj -> pure { pos: pos1, val: $name $unObj }
                  |    _ -> Left $$ Decode.MissingFields "$name"
                  |    where
                  |    decode :: Int -> $name1 -> Int -> Decode.Result' (Step { a :: Int, b :: $name1, c :: Int } { pos :: Int, val :: $name1 })
                  |    decode end acc pos1 | pos1 < end = do
                  |      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
                  |      case tag `zshr` 3 of${cases.map("\n        "+_).mkString("")}
                  |        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $$ tag .&. 7) \\_ -> acc
                  |    decode end acc pos1 = pure $$ Done { pos: pos1, val: acc }""".stripMargin
            }
          }
        Coder(tmpl, None)
      case RegularType(tpe, name) =>
        val fs = fields(tpe)
        val defObj: String = fs.map{ case (name, tpe, _, _) => nothingValue(name, tpe) }.mkString("{ ", ", ", " }")
        val justObj: String = fs.map{ case (name, tpe, _, _) => justValue(name, tpe) }.mkString("{ ", ", ", " }")
        val unObj: String = fs.map{
          case (name,_,_,_) => name
        }.mkString("{ ", ", ", " }")
        val cases = fs.flatMap(decodeFieldLoop.tupled)
        val simplify = fs.forall{ case (name, tpe, _, defval) => defval.isInstanceOf[HasDefFun] }
        val hasDefval = fs.exists(_._4.isInstanceOf[FillDef])
        val tmpl =
          if (simplify) {
            if (hasDefval) {
              val defs = fs.map{
                case (name, _, _, defval: FillDef) => s"$name: fromMaybe ${defval.value} $name"
                case (name, _, _, _) => name
              }.mkString(", ")
              s"""|decode$name :: Uint8Array -> Int -> Decode.Result $name
                  |decode$name _xs_ pos0 = do
                  |  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
                  |  { pos: pos1, val: $unObj } <- tailRecM3 decode (pos + msglen) $defObj pos
                  |  pure { pos: pos1, val: { ${defs} }}
                  |    where
                  |    decode :: Int -> $name' -> Int -> Decode.Result' (Step { a :: Int, b :: $name', c :: Int } { pos :: Int, val :: $name' })
                  |    decode end acc pos1 | pos1 < end = do
                  |      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
                  |      case tag `zshr` 3 of${cases.map("\n        "+_).mkString("")}
                  |        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $$ tag .&. 7) \\_ -> acc
                  |    decode end acc pos1 = pure $$ Done { pos: pos1, val: acc }""".stripMargin
            } else
              s"""|decode$name :: Uint8Array -> Int -> Decode.Result $name
                  |decode$name _xs_ pos0 = do
                  |  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
                  |  tailRecM3 decode (pos + msglen) $defObj pos
                  |    where
                  |    decode :: Int -> $name -> Int -> Decode.Result' (Step { a :: Int, b :: $name, c :: Int } { pos :: Int, val :: $name })
                  |    decode end acc pos1 | pos1 < end = do
                  |      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
                  |      case tag `zshr` 3 of${cases.map("\n        "+_).mkString("")}
                  |        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $$ tag .&. 7) \\_ -> acc
                  |    decode end acc pos1 = pure $$ Done { pos: pos1, val: acc }""".stripMargin
          } else {
            if (hasDefval) {
              val defs = fs.map{
                case (name, _, _, defval: FillDef) => s"$name: fromMaybe ${defval.value} $name"
                case (name, _, _, _) => name
              }.mkString(", ")
              val case1 = fs.map{
                case (name, tpe, _, _: FillDef) => name
                case (name, tpe, _, _) => justValue(name, tpe)
              }.mkString(", ")
              s"""|decode$name :: Uint8Array -> Int -> Decode.Result $name
                  |decode$name _xs_ pos0 = do
                  |  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
                  |  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) $defObj pos
                  |  case val of
                  |    { $case1 } -> pure { pos: pos1, val: { $defs }}
                  |    _ -> Left $$ Decode.MissingFields "$name"
                  |    where
                  |    decode :: Int -> $name' -> Int -> Decode.Result' (Step { a :: Int, b :: $name', c :: Int } { pos :: Int, val :: $name' })
                  |    decode end acc pos1 | pos1 < end = do
                  |      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
                  |      case tag `zshr` 3 of${cases.map("\n        "+_).mkString("")}
                  |        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $$ tag .&. 7) \\_ -> acc
                  |    decode end acc pos1 = pure $$ Done { pos: pos1, val: acc }""".stripMargin
            } else {
              s"""|decode$name :: Uint8Array -> Int -> Decode.Result $name
                  |decode$name _xs_ pos0 = do
                  |  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
                  |  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) $defObj pos
                  |  case val of
                  |    $justObj -> pure { pos: pos1, val: $unObj }
                  |    _ -> Left $$ Decode.MissingFields "$name"
                  |    where
                  |    decode :: Int -> $name' -> Int -> Decode.Result' (Step { a :: Int, b :: $name', c :: Int } { pos :: Int, val :: $name' })
                  |    decode end acc pos1 | pos1 < end = do
                  |      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
                  |      case tag `zshr` 3 of${cases.map("\n        "+_).mkString("")}
                  |        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $$ tag .&. 7) \\_ -> acc
                  |    decode end acc pos1 = pure $$ Done { pos: pos1, val: acc }""".stripMargin
            }
          }
        Coder(tmpl, None)
    }.distinct

  private def decodeFieldLoop(name: String, tpe: TypeRepr, n: Int, defval: DefVal): List[String] = {
    decodeFieldLoopTmpl{ case (n: Int, fun: String, mod: String) =>
      s"$n -> decodeFieldLoop end ($fun _xs_ pos2) \\val -> acc { $mod }" :: Nil
    }(name, tpe, n, defval)
  }

  private def decodeFieldLoopNewtype(newtype: String)(name: String, tpe: TypeRepr, n: Int, defval: DefVal): List[String] = {
    decodeFieldLoopTmpl{ case (n: Int, fun: String, mod: String) =>
      s"$n -> decodeFieldLoop end ($fun _xs_ pos2) \\val -> $newtype $$ acc { $mod }" :: Nil
    }(name, tpe, n, defval)
  }

  private def decodeFieldLoopTmpl(tmpl: (Int, String, String) => List[String])(name: String, tpe: TypeRepr, n: Int, @unused defval: DefVal): List[String] = {
    if (tpe.isString) {
      tmpl(n, "Decode.string", s"$name = Just val")
    } else if (tpe.isInt) {
      tmpl(n, "Decode.signedVarint32", s"$name = Just val")
    } else if (tpe.isLong) {
      tmpl(n, "Decode.bigInt", s"$name = Just val")
    } else if (tpe.isBoolean) {
      tmpl(n, "Decode.boolean", s"$name = Just val")
    } else if (tpe.isDouble) {
      tmpl(n, "Decode.double", s"$name = Just val")
    } else if (tpe.isOption) {
      val tpe1 = tpe.optionArgument
      if (tpe1.isString) {
        tmpl(n, "Decode.string", s"$name = Just val")
      } else if (tpe1.isInt) {
        tmpl(n, "Decode.signedVarint32", s"$name = Just val")
      } else if (tpe1.isLong) {
        tmpl(n, "Decode.bigInt", s"$name = Just val")
      } else if (tpe1.isBoolean) {
        tmpl(n, "Decode.boolean", s"$name = Just val")
      } else if (tpe1.isDouble) {
        tmpl(n, "Decode.double", s"$name = Just val")
      } else {
        val typeArgName = tpe1.typeSymbol.name
        tmpl(n, s"decode$typeArgName", s"$name = Just val")
      }
    } else if (tpe.isArrayByte) {
       tmpl(n, "Decode.bytes", s"$name = Just val")
    } else if (tpe.isRepeated) {
      iterablePurs(tpe) match {
        case ArrayPurs(tpe1) =>
          if (tpe1.isString) {
            tmpl(n, "Decode.string", s"$name = snoc acc.$name val")
          } else {
            val tpeName = tpe1.typeSymbol.name
            tmpl(n, s"decode$tpeName", s"$name = snoc acc.$name val")
          }
        case ArrayTuplePurs(tpe1, tpe2) =>
          tmpl(n, s"decode${tupleFunName(tpe1, tpe2)}", s"$name = snoc acc.$name val")
      }
    } else {
      val name1 = tpe.typeSymbol.name
      tmpl(n, s"decode${name1}", s"$name = Just val")
    }
  }