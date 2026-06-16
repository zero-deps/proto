package proto

import scala.quoted.*
import scala.annotation.*

trait Ops extends CommonOps:
  import qctx.reflect.{*, given}
  import report.*

  private[proto] case class ChildMeta(name: String, tpe: TypeRepr, n: Int, noargs: Boolean, rec: Boolean)

  sealed trait Tpe { val tpe: TypeRepr; val name: String }
  final case class TraitType(tpe: TypeRepr, name: String, children: Seq[ChildMeta], firstLevel: Boolean) extends Tpe
  final case class RegularType(tpe: TypeRepr, name: String) extends Tpe
  final case class RecursiveType(tpe: TypeRepr, name: String) extends Tpe
  final case class NoargsType(tpe: TypeRepr, name: String) extends Tpe
  final case class TupleType(tpe: TypeRepr, name: String, tpe_1: TypeRepr, tpe_2: TypeRepr) extends Tpe

  sealed trait DefVal
  sealed trait HasDefFun { val value: String }
  sealed trait FillDef   { val value: String }

  case object NoDef extends DefVal

  case object NoneDef extends DefVal with HasDefFun { val value = "undefined" }
  case object SeqDef  extends DefVal with HasDefFun { val value = "[]"        }

  final case class StrDef(v: String) extends DefVal with HasDefFun with FillDef { val value = s""""$v""""  }
  final case class OthDef(v: Any)    extends DefVal with HasDefFun with FillDef { val value = v.toString   }

  sealed trait IterableTs
  final case class ArrayTs(tpe: TypeRepr) extends IterableTs
  final case class ArrayTupleTs(tpe1: TypeRepr, tpe2: TypeRepr) extends IterableTs

  def collectDefExpressions(tpe: TypeRepr): Seq[(Expr[String], Expr[Any])] = {
    val types = collectTypeRepr(tpe, tail=Nil, acc=Nil)
    types.flatMap{ tpe =>
      val tpe_sym = tpe.typeSymbol

      tpe.typeSymbol.constructorParams.zipWithIndex.map{ case (sym, i) =>
          if sym.flags.is(Flags.HasDefault) then
            val compMod = tpe_sym.companionModule
            val id = s"`${tpe_sym.name}_${sym.name}_default`"
            compMod.methodMember(defaultMethodName(i)) match
              case List(methodSym) =>
                val y = methodSym.tree.asInstanceOf[DefDef]
                val returnTpe = y.returnTpt.tpe
                if !returnTpe.isCommonType then {
                  val xs = returnTpe.typeSymbol.caseFields
                  val methodCall = Ref(compMod).select(methodSym)
                  val fieldExprs = xs.map { field =>
                    val nameExpr = Expr(field.name)
                    val valueExpr = methodCall.select(field).asExpr
                    val fieldValueExprStr =
                      if field.tpe.isString then
                        '{ s""""${$valueExpr}"""" }
                      else
                        valueExpr
                    List(nameExpr, Expr(": "), fieldValueExprStr, Expr(", "))
                  }
                  val resultExpr = Expr.ofList(Expr("{ ") +: fieldExprs.flatten.init :+ Expr("}"))
                  Some(Expr(id) -> resultExpr)
                }  else {
                  Some(Expr(id) -> Ref(compMod).select(methodSym).asExpr)
                }
              case _ => errorAndAbort(s"${sym.fullName}`: default value method not found")
          else None
      }
    }.flatten
  }

  def collectTypeRepr(head: TypeRepr, tail: Seq[TypeRepr], acc: Seq[TypeRepr]): Seq[TypeRepr] = {
    val (tail1, acc1): (Seq[TypeRepr], Seq[TypeRepr]) =
      if acc.exists(_ =:= head) then
        (tail, acc)
      else if head.isSealedTrait then
        val children = head.knownFinalSubclasses.map(_.tpe)
        (children++tail, acc)
      else if head.isOption then
        val typeArgType = head.optionArgument
        if !typeArgType.isCommonType then (typeArgType+:tail, acc)
        else (tail, acc)
      else if head.isRepeated then
        head.typeArgs match
          case x :: Nil =>
            val typeArgType = x
            if !typeArgType.isCommonType then (typeArgType+:tail, acc)
            else (tail, acc)
          case x :: y :: Nil =>
            val zs = List(x, y).filter(!_.isCommonType)
            (zs++tail, acc)
          case _ => throw new Exception(s"too many type args for ${head}")
      else
        val ys = head.typeSymbol.constructorParams.map(_.tpe)
        (ys++tail, acc:+head)

    tail1 match {
      case h +: t => collectTypeRepr(h, tail=t, acc=acc1)
      case _ => acc1
    }
  }

  def collectTpes(tpe: TypeRepr): Seq[Tpe] = {
    collectTpes(tpe, tail=Nil, acc=Nil, firstLevel=true)
  }

  def collectTpes(head: TypeRepr, tail: Seq[TypeRepr], acc: Seq[Tpe], firstLevel: Boolean): Seq[Tpe] = {
    val (tail1, acc1): (Seq[TypeRepr], Seq[Tpe]) =
      if acc.exists(_.tpe =:= head) then
        (tail, acc)
      else if head.isSealedTrait then
        val children = findChildren(head)
        (children.map(_.tpe)++tail, acc:+TraitType(head, head.typeSymbol.name, children, firstLevel))
      else if head.isOption then
        val typeArgType = head.optionArgument
        if !typeArgType.isCommonType then (typeArgType+:tail, acc)
        else (tail, acc)
      else if head.isRepeated then
        head.typeArgs match
          case x :: Nil =>
            val typeArgType = x
            if !typeArgType.isCommonType then (typeArgType+:tail, acc)
            else (tail, acc)
          case x :: y :: Nil =>
            val zs = List(x, y).filter(!_.isCommonType)
            (zs++tail, acc:+TupleType(TypeRepr.of[Tuple2[Unit, Unit]].appliedTo(x::y::Nil), tupleFunName(x, y), x, y))
          case _ => throw new Exception(s"too many type args for ${head}")
      else
        val (ys, z) = type_to_tpe(head)
        (ys++tail, acc:+z)

    tail1 match {
      case h +: t => collectTpes(h, tail=t, acc=acc1, firstLevel=false)
      case _ => acc1
    }
  }

  def fields(tpe: TypeRepr): List[(String, TypeRepr, Int, DefVal)] = {
    val tpe_Sym = tpe.typeSymbol
    val tpe_CompanionSym = tpe_Sym.companionModule

    tpe.typeSymbol.constructorParams.zipWithIndex.map{ case (sym, i) =>
      val tpe1 = sym.tpe
      val defval =
        if sym.flags.is(Flags.HasDefault) then
          val id = s"`${tpe_Sym.name}_${sym.name}_default`"
          tpe_CompanionSym.methodMember(defaultMethodName(i)) match
            case List(x) if x.isDefDef =>
              val y = x.tree.asInstanceOf[DefDef]
              if y.returnTpt.tpe.isString then StrDef(id)
              else OthDef(id)
            case _ => errorAndAbort(s"${sym.fullName}`: default value method not found")
        else if tpe1.isOption then
          NoneDef
        else if tpe1.isRepeated then
          SeqDef
        else
          NoDef
      (sym.name, tpe1, findN(sym), defval)

    }.collect{ case (a, b, Some(n), dv) => (a, b, n, dv) }.sortBy(_._3)
  }

  def findChildren(tpe: TypeRepr): Seq[ChildMeta] =
    tpe.knownFinalSubclasses.map(x => x -> findN(x)).collect{ case (x, Some(n)) => x -> n }.sortBy(_._2).map{
      case (x: Symbol, n) =>
        val tpe1: TypeRepr = x.tpe
        ChildMeta(name=x.name, tpe1, n, noargs=fields(tpe1).isEmpty, rec=isRecursive(tpe1))
    }

  def isRecursive(base: TypeRepr): Boolean = {
    @tailrec def loop(compareTo: List[TypeRepr]): Boolean = compareTo match
      case Nil => false
      case x :: _ if x =:= base => true
      case x :: xs => loop(x.typeArgs ++ xs)
    loop(fields(base).map(_._2).filter(tpe => !tpe.isCommonType))
  }

  def type_to_tpe(head: TypeRepr): (Seq[TypeRepr], Tpe) = {
    val xs = fields(head).map(_._2)
    val ys = xs.filter(x => !x.isCommonType)
    val z =
      if xs.isEmpty then NoargsType(head, head.typeSymbol.name.stripSuffix("$")) //strip $ for case objects
      else if isRecursive(head) then RecursiveType(head, head.typeSymbol.name)
      else RegularType(head, head.typeSymbol.name)
    (ys, z)
  }

  /** Stable identifier for a type, based on its Scala name (so Int vs Double do not collide
    * the way their TS `number` mapping would). Used to name tuple/map codec functions. */
  def scalaTypeId(tpe: TypeRepr): String =
    if tpe.isString then "String"
    else if tpe.isInt then "Int"
    else if tpe.isLong then "Long"
    else if tpe.isBoolean then "Boolean"
    else if tpe.isDouble then "Double"
    else if tpe.isFloat then "Float"
    else if tpe.isArrayByte then "Bytes"
    else tpe.typeSymbol.name

  def tupleFunName(tpe_1: TypeRepr, tpe_2: TypeRepr): String =
    scalaTypeId(tpe_1) + scalaTypeId(tpe_2)

  /** TypeScript type expression for a field type. Option is unwrapped here; optionality is
    * expressed on the field declaration (a trailing `?`) by [[isOptionalField]]. */
  def tsType(tpe: TypeRepr): String =
    if tpe.isString then "string"
    else if tpe.isInt then "number"
    else if tpe.isLong then "bigint"
    else if tpe.isBoolean then "boolean"
    else if tpe.isDouble then "number"
    else if tpe.isFloat then "number"
    else if tpe.isArrayByte then "Uint8Array"
    else if tpe.isOption then tsType(tpe.optionArgument)
    else if tpe.isRepeated then
      iterableTs(tpe) match
        case ArrayTs(x) => s"Array<${tsType(x)}>"
        case ArrayTupleTs(t1, t2) => s"Array<[${tsType(t1)}, ${tsType(t2)}]>"
    else tpe.typeSymbol.name

  def isOptionalField(tpe: TypeRepr): Boolean = tpe.isOption

  def iterableTs(tpe: TypeRepr): IterableTs =
    tpe.typeArgs match
      case x :: Nil => ArrayTs(x)
      case x :: y :: Nil => ArrayTupleTs(x, y)
      case _ => throw new Exception(s"too many type args for $tpe")

  /** Names of all case classes / objects that are variants of some sealed trait in `types`.
    * Those are emitted as TS classes (so encoders can dispatch with `instanceof` and the
    * union is runtime-discriminable); everything else is emitted as an interface. */
  def traitChildNames(types: Seq[Tpe]): Set[String] =
    types.collect{ case TraitType(_, _, children, _) => children.map(_.name) }.flatten.toSet
