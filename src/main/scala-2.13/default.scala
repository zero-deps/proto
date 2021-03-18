package zero.protopurs

sealed trait DefVal
sealed trait HasDefFun { val value: String }
sealed trait FillDef   { val value: String }

final case object NoDef extends DefVal

final case object NoneDef extends DefVal with HasDefFun { val value = "Nothing" }
final case object SeqDef  extends DefVal with HasDefFun { val value = "[]"      }

final case class StrDef(v: String) extends DefVal with HasDefFun with FillDef { val value = s""""$v""""  }
final case class OthDef(v: Any)    extends DefVal with HasDefFun with FillDef { val value = v.toString   }