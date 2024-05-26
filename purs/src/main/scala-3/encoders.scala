package proto
package purs

import scala.quoted.*
import scala.annotation.unused

trait Encoders extends Ops:
  implicit val qctx: Quotes
  import qctx.reflect.*

  def encodersFrom(types: Seq[Tpe]): Seq[Coder] =
    types.map{
      case TraitType(tpe, name, children, true) =>
        val cases = children.map{ case ChildMeta(name1, _, n, noargs, rec) =>
          if (noargs)
            s"encode$name $name1 = concatAll [ Encode.unsignedVarint32 ${(n << 3) + 2}, encode$name1 ]" :: Nil
          else {
            val pursTypeName = if (rec) s"$name1''" else name1
            s"encode$name ($pursTypeName x) = concatAll [ Encode.unsignedVarint32 ${(n << 3) + 2}, encode$name1 x ]" :: Nil
          }
        }
        val tmpl =
          s"""|encode$name :: $name -> Uint8Array
              |${cases.map(_.mkString("\n")).mkString("\n")}""".stripMargin
        Coder(tmpl, Some(s"encode$name"))
      case TraitType(tpe, name, children, false) =>
        val cases = children.map{ case ChildMeta(name1, _, n, noargs, rec) =>
          if (noargs)
            List(
              s"encode$name $name1 = do"
            , s"  let xs = concatAll [ Encode.unsignedVarint32 ${(n << 3) + 2}, encode$name1 ]"
            , s"  concatAll [ Encode.unsignedVarint32 $$ length xs, xs ]"
            ).mkString("\n") :: Nil
          else {
            val pursTypeName = if (rec) s"$name1''" else name1
            List(
              s"encode$name ($pursTypeName x) = do"
            , s"  let xs = concatAll [ Encode.unsignedVarint32 ${(n << 3) + 2}, encode$name1 x ]"
            , s"  concatAll [ Encode.unsignedVarint32 $$ length xs, xs ]"
            ).mkString("\n") :: Nil
          }
        }
        val tmpl =
          s"""|encode$name :: $name -> Uint8Array
              |${cases.map(_.mkString("\n")).mkString("\n")}""".stripMargin
        Coder(tmpl, None)
      case TupleType(tpe, tupleName, tpe_1, tpe_2) =>
        val fun = "encode" + tupleName
        val tmpl =
          s"""|$fun :: Tuple ${pursTypePars(tpe_1)._1} ${pursTypePars(tpe_2)._1} -> Uint8Array
              |$fun (Tuple _1 _2) = do
              |  let msg = { _1, _2 }
              |  let xs = concatAll
              |  ${List(("_1", tpe_1, 1, NoDef), ("_2", tpe_2, 2, NoDef)).flatMap(encodeField.tupled).mkString("      [ ",            "\n        , ", "\n        ]")}
              |  concatAll [ Encode.unsignedVarint32 $$ length xs, xs ]""".stripMargin
        Coder(tmpl, None)
      case NoargsType(tpe, name) =>
        val tmpl =
          s"""|encode$name :: Uint8Array
              |encode$name = Encode.unsignedVarint32 0""".stripMargin
        Coder(tmpl, None)
      case RecursiveType(tpe, name) =>
        val encodeFields = fields(tpe).flatMap(encodeField.tupled)
        val tmpl =
          if (encodeFields.nonEmpty) {
            s"""|encode$name :: $name -> Uint8Array
                |encode$name ($name msg) = do
                |  let xs = concatAll
                |  ${encodeFields.mkString("      [ ", "\n        , ", "\n        ]")}
                |  concatAll [ Encode.unsignedVarint32 $$ length xs, xs ]""".stripMargin
          } else {
            s"""|encode$name :: $name -> Uint8Array
                |encode$name _ = Encode.unsignedVarint32 0""".stripMargin
          }
        Coder(tmpl, None)
      case RegularType(tpe, name) =>
        val encodeFields = fields(tpe).flatMap(encodeField.tupled)
        val tmpl =
          if (encodeFields.nonEmpty) {
            s"""|encode$name :: $name -> Uint8Array
                |encode$name msg = do
                |  let xs = concatAll
                |  ${encodeFields.mkString("      [ ", "\n        , ", "\n        ]")}
                |  concatAll [ Encode.unsignedVarint32 $$ length xs, xs ]""".stripMargin
          } else {
            s"""|encode$name :: $name -> Uint8Array
                |encode$name _ = Encode.unsignedVarint32 0""".stripMargin
          }
        Coder(tmpl, None)
    }.distinct

  def encodeField(name: String, tpe: TypeRepr, n: Int, @unused defval: DefVal): List[String] =
    if tpe.isString then
      List(
        s"""Encode.unsignedVarint32 ${(n<<3)+2}"""
      , s"""Encode.string msg.$name"""
      )
    else if tpe.isInt then
      List(
        s"""Encode.unsignedVarint32 ${(n<<3)+0}"""
      , s"""Encode.signedVarint32 msg.$name"""
      )
    else if tpe.isLong then
      List(
        s"""Encode.unsignedVarint32 ${(n<<3)+0}"""
      , s"""Encode.bigInt msg.$name"""
      )
    else if tpe.isBoolean then
      List(
        s"""Encode.unsignedVarint32 ${(n<<3)+0}"""
      , s"""Encode.boolean msg.$name"""
      )
    else if tpe.isDouble then
      List(
        s"""Encode.unsignedVarint32 ${(n<<3)+1}"""
      , s"""Encode.double msg.$name"""
      )
    else if tpe.isOption then
      val tpe1 = tpe.optionArgument
      if tpe1.isString then
        s"""fromMaybe (fromArray []) $$ map (\\x -> concatAll [ Encode.unsignedVarint32 ${(n<<3)+2}, Encode.string x ]) msg.$name""" :: Nil
      else if tpe1.isInt then
        s"""fromMaybe (fromArray []) $$ map (\\x -> concatAll [ Encode.unsignedVarint32 ${(n<<3)+0}, Encode.signedVarint32 x ]) msg.$name""" :: Nil
      else if tpe1.isLong then
        s"""fromMaybe (fromArray []) $$ map (\\x -> concatAll [ Encode.unsignedVarint32 ${(n<<3)+0}, Encode.bigInt x ]) msg.$name""" :: Nil
      else if tpe1.isBoolean then
        s"""fromMaybe (fromArray []) $$ map (\\x -> concatAll [ Encode.unsignedVarint32 ${(n<<3)+0}, Encode.boolean x ]) msg.$name""" :: Nil
      else if tpe1.isDouble then
        s"""fromMaybe (fromArray []) $$ map (\\x -> concatAll [ Encode.unsignedVarint32 ${(n<<3)+1}, Encode.double x ]) msg.$name""" :: Nil
      else
        val typeArgName = tpe1.typeSymbol.name
        s"""fromMaybe (fromArray []) $$ map (\\x -> concatAll [ Encode.unsignedVarint32 ${(n<<3)+2}, encode$typeArgName x ]) msg.$name""" :: Nil
    else if tpe.isArrayByte then
      List(
        s"""Encode.unsignedVarint32 ${(n<<3)+2}"""
      , s"""Encode.bytes msg.$name"""
      )
    else if tpe.isRepeated then
      iterablePurs(tpe) match {
        case ArrayPurs(x) =>
          if (x.isString) {
            s"""concatAll $$ concatMap (\\x -> [ Encode.unsignedVarint32 ${(n<<3)+2}, Encode.string x ]) msg.$name""" :: Nil
          } else {
            s"""concatAll $$ concatMap (\\x -> [ Encode.unsignedVarint32 ${(n<<3)+2}, encode${x.typeSymbol.name} x ]) msg.$name""" :: Nil
          }
        case ArrayTuplePurs(tpe1, tpe2) =>
          s"concatAll $$ concatMap (\\x -> [ Encode.unsignedVarint32 ${(n<<3)+2}, encode${tupleFunName(tpe1, tpe2)} x ]) msg.$name" :: Nil
      }
    else
      val tpeName = tpe.typeSymbol.name
      List(
        s"""Encode.unsignedVarint32 ${(n<<3)+2}"""
      , s"""encode${tpeName} msg.$name"""
      )
