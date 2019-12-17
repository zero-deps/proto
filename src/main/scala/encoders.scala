package zd.proto.purs

import zd.proto.api.MessageCodec

object Encoders {
  def from(types: Seq[Tpe], codecs: List[MessageCodec[_]]): Seq[String] = {
    types.map{
      case TraitType(tpe, children, true) =>
        val name = tpe.typeSymbol.name.encodedName.toString
        val cases = children.map{ case ChildMeta(name1, tpe, n, noargs) =>
          if (noargs)
            List(
              s"encode$name $name1 = concatAll [ Encode.uint32 ${(n << 3) + 2}, encode$name1 ]"
            )
          else
            List(
              s"encode$name ($name1 x) = concatAll [ Encode.uint32 ${(n << 3) + 2}, encode$name1 x ]"
            )
        }
        s"""|encode$name :: $name -> Uint8Array
            |${cases.map(_.mkString("\n")).mkString("\n")}""".stripMargin
      case TraitType(tpe, children,false) =>
        val name = tpe.typeSymbol.name.encodedName.toString
        val cases = children.map{ case ChildMeta(name1, tpe, n, noargs) =>
          if (noargs)
            List(
              List(
                s"encode$name $name1 = do"
              , s"  let xs = concatAll [ Encode.uint32 ${(n << 3) + 2}, encode$name1 ]"
              , s"  let len = length xs"
              , s"  concatAll [ Encode.uint32 len, xs ]"
              ).mkString("\n")
            )
          else
            List(
              List(
                s"encode$name ($name1 x) = do"
              , s"  let xs = concatAll [ Encode.uint32 ${(n << 3) + 2}, encode$name1 x ]"
              , s"  let len = length xs"
              , s"  concatAll [ Encode.uint32 len, xs ]"
              ).mkString("\n")
            )
        }
        s"""|encode$name :: $name -> Uint8Array
            |${cases.map(_.mkString("\n")).mkString("\n")}""".stripMargin
      case TupleType(tpe, tpe_1, tpe_2) =>
        val xs = codecs.find(_.aType == tpe.toString).map(_.nums).getOrElse(throw new Exception(s"codec is missing for ${tpe.toString}"))
        val fun = "encode" + tupleFunName(tpe_1, tpe_2)
        s"""|$fun :: Tuple ${pursTypePars(tpe_1)._1} ${pursTypePars(tpe_2)._1} -> Uint8Array
            |$fun (Tuple _1 _2) = do
            |  let msg = { _1, _2 }
            |  let xs = concatAll
            |  ${List(("_1", tpe_1, xs("_1")), ("_2", tpe_2, xs("_2"))).flatMap((encodeField _).tupled).mkString("      [ ",            "\n        , ", "\n        ]")}
            |  let len = length xs
            |  concatAll [ Encode.uint32 len, xs ]""".stripMargin
      case NoargsType(tpe) =>
        val name = tpe.typeSymbol.name.encodedName.toString
        s"""|encode$name :: Uint8Array
            |encode$name = Encode.uint32 0""".stripMargin
      case RecursiveType(tpe) =>
        val recursive = true
        val encodeFields = fields(tpe).flatMap((encodeField _).tupled)
        val name = tpe.typeSymbol.name.encodedName.toString
        if (encodeFields.nonEmpty) {
          s"""|encode$name :: $name -> Uint8Array
              |encode$name ${if (recursive) s"($name msg)" else "msg"} = do
              |  let xs = concatAll
              |  ${encodeFields.mkString("      [ ", "\n        , ", "\n        ]")}
              |  let len = length xs
              |  concatAll [ Encode.uint32 len, xs ]""".stripMargin
        } else {
          s"""|encode$name :: $name -> Uint8Array
              |encode$name _ = Encode.uint32 0""".stripMargin
        }
      case RegularType(tpe) =>
        val recursive = false
        val encodeFields = fields(tpe).flatMap((encodeField _).tupled)
        val name = tpe.typeSymbol.name.encodedName.toString
        if (encodeFields.nonEmpty) {
          s"""|encode$name :: $name -> Uint8Array
              |encode$name ${if (recursive) s"($name msg)" else "msg"} = do
              |  let xs = concatAll
              |  ${encodeFields.mkString("      [ ", "\n        , ", "\n        ]")}
              |  let len = length xs
              |  concatAll [ Encode.uint32 len, xs ]""".stripMargin
        } else {
          s"""|encode$name :: $name -> Uint8Array
              |encode$name _ = Encode.uint32 0""".stripMargin
        }
    }.distinct
  }
}