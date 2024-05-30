package proto

import scala.quoted.*
import scala.annotation.*

trait Ops extends CommonOps:
  implicit val qctx: Quotes
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

  case object NoneDef extends DefVal with HasDefFun { val value = "Nothing" }
  case object SeqDef  extends DefVal with HasDefFun { val value = "[]"      }

  final case class StrDef(v: String) extends DefVal with HasDefFun with FillDef { val value = s""""$v""""  }
  final case class OthDef(v: Any)    extends DefVal with HasDefFun with FillDef { val value = v.toString   }

  sealed trait IterablePurs
  final case class ArrayPurs(tpe: TypeRepr) extends IterablePurs
  final case class ArrayTuplePurs(tpe1: TypeRepr, tpe2: TypeRepr) extends IterablePurs

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
                Some(Expr(id) -> Ref(compMod).select(methodSym).asExpr)
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

  // @tailrec
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
            (zs++tail, acc:+TupleType(TypeRepr.of[Tuple2[Unit, Unit]].appliedTo(x::y::Nil), tupleFunName(x, y), x, y)) //todo check
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
    val compClass = tpe_Sym.companionClass
    val compModRef = Ref(tpe_CompanionSym)

    tpe.typeSymbol.constructorParams.zipWithIndex.map{ case (sym, i) =>
      val tpe1 = sym.tpe
      val defval =
        if sym.flags.is(Flags.HasDefault) then
          val id = s"`${tpe_Sym.name}_${sym.name}_default`"
          tpe_CompanionSym.methodMember(defaultMethodName(i)) match
            case List(x) if x.isDefDef =>
              val y = x.tree.asInstanceOf[DefDef]              
              if (y.returnTpt.tpe.isString) StrDef(id)
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

  def tupleFunName(tpe_1: TypeRepr, tpe_2: TypeRepr): String = {
    (pursType(tpe_1)._1.filter(_.isLetter) + "_" + pursType(tpe_2)._1).filter(_.isLetter)
  }

  def type_to_tpe(head: TypeRepr): (Seq[TypeRepr], Tpe) = {
    val xs = fields(head).map(_._2)
    val ys = xs.filter(x => !x.isCommonType)
    val z =
      if (xs.isEmpty) NoargsType(head, head.typeSymbol.name.stripSuffix("$")) //strip $ for case objects
      else if (isRecursive(head)) RecursiveType(head, head.typeSymbol.name)
      else RegularType(head, head.typeSymbol.name)
    (ys, z)
  }

  def pursType(tpe: TypeRepr): (String, String) = {
    def trim(x: String): String = x.stripPrefix("(").stripSuffix(")")
    val (a, b) = pursTypePars(tpe)
    trim(a) -> trim(b)
  }

  def pursTypePars(tpe: TypeRepr): (String, String) = {
    if tpe.isString then
      "String" -> "(Maybe String)"
    else if tpe.isInt then
      "Int" -> "(Maybe Int)"
    else if tpe.isLong then
      "BigInt" -> "(Maybe BigInt)"
    else if tpe.isBoolean then
      "Boolean" -> "(Maybe Boolean)"
    else if tpe.isDouble then
      "Number" -> "(Maybe Number)"
    else if tpe.isArrayByte then
      "Uint8Array" -> "(Maybe Uint8Array)"
    else if tpe.isOption then
      val typeArg = tpe.optionArgument
      if typeArg.isLong then
        "(Maybe BigInt)" -> "(Maybe BigInt)"
      else if typeArg.isDouble then
        "(Maybe Number)" -> "(Maybe Number)"
      else
        val name = typeArg.typeSymbol.name
        s"(Maybe $name)" -> s"Maybe $name)"
    else if tpe.isRepeated then
      iterablePurs(tpe) match
        case ArrayPurs(tpe) =>
          val name = tpe.typeSymbol.name
          s"(Array $name)" -> s"(Array $name)"
        case ArrayTuplePurs(tpe1, tpe2) =>
          val name1 = pursTypePars(tpe1)._1
          val name2 = pursTypePars(tpe2)._1
          s"(Array (Tuple $name1 $name2))" -> s"(Array (Tuple $name1 $name2))"
    else
      val name = tpe.typeSymbol.name
      name -> s"(Maybe $name)"
  }

  def iterablePurs(tpe: TypeRepr): IterablePurs =
    tpe.typeArgs match
      case x :: Nil => ArrayPurs(x)
      case x :: y :: Nil => ArrayTuplePurs(x, y)
      case _ => throw new Exception(s"too many type args for $tpe")
