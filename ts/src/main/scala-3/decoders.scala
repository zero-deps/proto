package proto
package ts

import scala.quoted.*

trait Decoders extends Ops:
  import qctx.reflect.*

  private def scalarDecoder(tpe: TypeRepr): String =
    if tpe.isString then "Decode.string"
    else if tpe.isInt then "Decode.signedVarint32"
    else if tpe.isLong then "Decode.bigInt"
    else if tpe.isBoolean then "Decode.boolean"
    else if tpe.isDouble then "Decode.double"
    else if tpe.isFloat then "Decode.float"
    else if tpe.isArrayByte then "Decode.bytes"
    else s"decode${tpe.typeSymbol.name}"

  /** Decoder function for a field plus whether the field is repeated (push vs assign). */
  private def decoderFor(tpe: TypeRepr): (String, Boolean) =
    if tpe.isOption then (scalarDecoder(tpe.optionArgument), false)
    else if tpe.isRepeated then
      iterableTs(tpe) match
        case ArrayTs(x) => (scalarDecoder(x), true)
        case ArrayTupleTs(t1, t2) => (s"decode${tupleFunName(t1, t2)}", true)
    else (scalarDecoder(tpe), false)

  private def accDecl(name: String, tpe: TypeRepr, defval: DefVal): String =
    defval match
      case SeqDef     => s"  let $name: ${tsType(tpe)} = []"
      case d: FillDef => s"  let $name: ${tsType(tpe)} = ${d.value}"
      case _          => s"  let $name: ${tsType(tpe)} | undefined = undefined"

  private def decodeCase(name: String, tpe: TypeRepr, n: Int): String =
    val (fn, repeated) = decoderFor(tpe)
    val assign = if repeated then s"$name.push(__r.val)" else s"$name = __r.val"
    s"      case $n: { const __r = $fn(__xs, __pos); $assign; __pos = __r.pos; break }"

  private def requiredGuard(name: String, fs: List[(String, TypeRepr, Int, DefVal)]): String =
    val required = fs.collect{ case (n, _, _, NoDef) => n }
    if required.isEmpty then ""
    else s"""  if (${required.map(n => s"$n === undefined").mkString(" || ")}) throw new Decode.MissingFields("$name")\n"""

  private def construct(name: String, fs: List[(String, TypeRepr, Int, DefVal)], isVariant: Boolean): String =
    if isVariant then
      val (required, rest) = fs.partition{ case (_, _, _, dv) => dv == NoDef }
      val args = (required ++ rest).map(_._1)
      s"new $name(${args.mkString(", ")})"
    else
      s"{ ${fs.map(_._1).mkString(", ")} }"

  private def messageDecoder(name: String, retTpe: String, fs: List[(String, TypeRepr, Int, DefVal)], constructExpr: String): String =
    if fs.isEmpty then
      s"""|export function decode$name(__xs: Uint8Array, __pos0: number): { pos: number, val: $retTpe } {
          |  const __m = Decode.unsignedVarint32(__xs, __pos0)
          |  return { pos: __m.pos + __m.val, val: $constructExpr }
          |}""".stripMargin
    else
      val accs = fs.map{ case (n, t, _, dv) => accDecl(n, t, dv) }.mkString("\n")
      val cases = fs.map{ case (n, t, num, _) => decodeCase(n, t, num) }.mkString("\n")
      s"""|export function decode$name(__xs: Uint8Array, __pos0: number): { pos: number, val: $retTpe } {
          |  const __m = Decode.unsignedVarint32(__xs, __pos0)
          |  let __pos = __m.pos
          |  const __end = __pos + __m.val
          |$accs
          |  while (__pos < __end) {
          |    const __t = Decode.unsignedVarint32(__xs, __pos); __pos = __t.pos
          |    switch (__t.val >>> 3) {
          |$cases
          |      default: { __pos = Decode.skipType(__xs, __pos, __t.val & 7); break }
          |    }
          |  }
          |${requiredGuard(name, fs)}  return { pos: __pos, val: $constructExpr }
          |}""".stripMargin

  def decodersFrom(types: Seq[Tpe], variants: Set[String]): Seq[Coder] =
    types.map{
      case TraitType(_, name, children, true) =>
        val cases = children.map{ case ChildMeta(name1, _, n, _, _) =>
          s"    case $n: { const __r = decode$name1(__xs, __pos1); return __r.val }"
        }
        val tmpl =
          s"""|export function decode$name(__xs: Uint8Array): $name {
              |  const __t = Decode.unsignedVarint32(__xs, 0)
              |  const __pos1 = __t.pos
              |  switch (__t.val >>> 3) {
              |${cases.mkString("\n")}
              |    default: throw new Decode.BadType(__t.val >>> 3)
              |  }
              |}""".stripMargin
        Coder(tmpl, Some(s"decode$name"))
      case TraitType(_, name, children, false) =>
        val cases = children.map{ case ChildMeta(name1, _, n, _, _) =>
          s"      case $n: { const __r = decode$name1(__xs, __pos); __acc = __r.val; __pos = __r.pos; break }"
        }
        val tmpl =
          s"""|export function decode$name(__xs: Uint8Array, __pos0: number): { pos: number, val: $name } {
              |  const __m = Decode.unsignedVarint32(__xs, __pos0)
              |  let __pos = __m.pos
              |  const __end = __pos + __m.val
              |  let __acc: $name | undefined = undefined
              |  while (__pos < __end) {
              |    const __t = Decode.unsignedVarint32(__xs, __pos); __pos = __t.pos
              |    switch (__t.val >>> 3) {
              |${cases.mkString("\n")}
              |      default: { __pos = Decode.skipType(__xs, __pos, __t.val & 7); break }
              |    }
              |  }
              |  if (__acc === undefined) throw new Decode.MissingFields("$name")
              |  return { pos: __pos, val: __acc }
              |}""".stripMargin
        Coder(tmpl, None)
      case TupleType(_, tupleName, tpe_1, tpe_2) =>
        val fs = List(("_1", tpe_1, 1, NoDef), ("_2", tpe_2, 2, NoDef))
        val retTpe = s"[${tsType(tpe_1)}, ${tsType(tpe_2)}]"
        Coder(messageDecoder(tupleName, retTpe, fs, "[_1, _2]"), None)
      case NoargsType(_, name) =>
        val constructExpr = if variants.contains(name) then s"new $name()" else "{}"
        Coder(messageDecoder(name, name, Nil, constructExpr), None)
      case RecursiveType(tpe, name) =>
        val fs = fields(tpe)
        Coder(messageDecoder(name, name, fs, construct(name, fs, variants.contains(name))), None)
      case RegularType(tpe, name) =>
        val fs = fields(tpe)
        Coder(messageDecoder(name, name, fs, construct(name, fs, variants.contains(name))), None)
    }.distinct
