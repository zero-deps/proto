package proto
package ts

import scala.quoted.*
import scala.annotation.unused

trait Encoders extends Ops:
  import qctx.reflect.*

  /** protobuf wire type number for a (non-repeated, non-option) element type. */
  private def wireTypeNum(tpe: TypeRepr): Int =
    if tpe.isInt || tpe.isLong || tpe.isBoolean then 0
    else if tpe.isDouble then 1
    else if tpe.isFloat then 5
    else 2

  /** Encode expression for a single value bound to `v` (no tag). */
  private def elemEncode(tpe: TypeRepr, v: String): String =
    if tpe.isString then s"Encode.string($v)"
    else if tpe.isInt then s"Encode.signedVarint32($v)"
    else if tpe.isLong then s"Encode.bigInt($v)"
    else if tpe.isBoolean then s"Encode.boolean($v)"
    else if tpe.isDouble then s"Encode.double($v)"
    else if tpe.isFloat then s"Encode.float($v)"
    else if tpe.isArrayByte then s"Encode.bytes($v)"
    else s"encode${tpe.typeSymbol.name}($v)"

  private def message(name: String, paramTpe: String, entries: List[String]): String =
    if entries.isEmpty then
      s"""|export function encode$name(_msg: $paramTpe): Uint8Array {
          |  return Encode.unsignedVarint32(0)
          |}""".stripMargin
    else
      s"""|export function encode$name(msg: $paramTpe): Uint8Array {
          |  const xs = Encode.concatAll([
          |${entries.map("    " + _ + ",").mkString("\n")}
          |  ])
          |  return Encode.concatAll([Encode.unsignedVarint32(xs.length), xs])
          |}""".stripMargin

  def encodersFrom(types: Seq[Tpe]): Seq[Coder] =
    types.map{
      case TraitType(_, name, children, true) =>
        val cases = children.map{ case ChildMeta(name1, _, n, _, _) =>
          s"  if (x instanceof $name1) return Encode.concatAll([Encode.unsignedVarint32(${(n << 3) + 2}), encode$name1(x)])"
        }
        val tmpl =
          s"""|export function encode$name(x: $name): Uint8Array {
              |${cases.mkString("\n")}
              |  throw new Error("encode$name: unknown variant")
              |}""".stripMargin
        Coder(tmpl, Some(s"encode$name"))
      case TraitType(_, name, children, false) =>
        val cases = children.map{ case ChildMeta(name1, _, n, _, _) =>
          s"  if (x instanceof $name1) { const xs = Encode.concatAll([Encode.unsignedVarint32(${(n << 3) + 2}), encode$name1(x)]); return Encode.concatAll([Encode.unsignedVarint32(xs.length), xs]) }"
        }
        val tmpl =
          s"""|export function encode$name(x: $name): Uint8Array {
              |${cases.mkString("\n")}
              |  throw new Error("encode$name: unknown variant")
              |}""".stripMargin
        Coder(tmpl, None)
      case TupleType(_, tupleName, tpe_1, tpe_2) =>
        val entries = List(("_1", tpe_1, 1, NoDef), ("_2", tpe_2, 2, NoDef)).flatMap(encodeField.tupled)
        val tmpl =
          s"""|export function encode$tupleName(x: [${tsType(tpe_1)}, ${tsType(tpe_2)}]): Uint8Array {
              |  const msg = { _1: x[0], _2: x[1] }
              |  const xs = Encode.concatAll([
              |${entries.map("    " + _ + ",").mkString("\n")}
              |  ])
              |  return Encode.concatAll([Encode.unsignedVarint32(xs.length), xs])
              |}""".stripMargin
        Coder(tmpl, None)
      case NoargsType(_, name) =>
        Coder(message(name, name, Nil), None)
      case RecursiveType(tpe, name) =>
        Coder(message(name, name, fields(tpe).flatMap(encodeField.tupled)), None)
      case RegularType(tpe, name) =>
        Coder(message(name, name, fields(tpe).flatMap(encodeField.tupled)), None)
    }.distinct

  def encodeField(name: String, tpe: TypeRepr, n: Int, @unused defval: DefVal): List[String] =
    if tpe.isString then
      List(s"Encode.unsignedVarint32(${(n<<3)+2})", s"Encode.string(msg.$name)")
    else if tpe.isInt then
      List(s"Encode.unsignedVarint32(${(n<<3)+0})", s"Encode.signedVarint32(msg.$name)")
    else if tpe.isLong then
      List(s"Encode.unsignedVarint32(${(n<<3)+0})", s"Encode.bigInt(msg.$name)")
    else if tpe.isBoolean then
      List(s"Encode.unsignedVarint32(${(n<<3)+0})", s"Encode.boolean(msg.$name)")
    else if tpe.isDouble then
      List(s"Encode.unsignedVarint32(${(n<<3)+1})", s"Encode.double(msg.$name)")
    else if tpe.isFloat then
      List(s"Encode.unsignedVarint32(${(n<<3)+5})", s"Encode.float(msg.$name)")
    else if tpe.isOption then
      val tpe1 = tpe.optionArgument
      val tag = (n<<3) + wireTypeNum(tpe1)
      List(s"(msg.$name === undefined ? new Uint8Array(0) : Encode.concatAll([Encode.unsignedVarint32($tag), ${elemEncode(tpe1, s"msg.$name")}]))")
    else if tpe.isArrayByte then
      List(s"Encode.unsignedVarint32(${(n<<3)+2})", s"Encode.bytes(msg.$name)")
    else if tpe.isRepeated then
      iterableTs(tpe) match
        case ArrayTs(x) =>
          val tag = (n<<3) + wireTypeNum(x)
          List(s"Encode.concatAll(msg.$name.flatMap(x => [Encode.unsignedVarint32($tag), ${elemEncode(x, "x")}]))")
        case ArrayTupleTs(t1, t2) =>
          List(s"Encode.concatAll(msg.$name.flatMap(x => [Encode.unsignedVarint32(${(n<<3)+2}), encode${tupleFunName(t1, t2)}(x)]))")
    else
      List(s"Encode.unsignedVarint32(${(n<<3)+2})", s"encode${tpe.typeSymbol.name}(msg.$name)")
