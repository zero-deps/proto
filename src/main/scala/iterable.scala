package zd.proto.purs

import scala.reflect.runtime.universe._

sealed trait IterablePurs
final case class ArrayPurs(tpe: Type) extends IterablePurs
final case class ArrayTuplePurs(tpe1: Type, tpe2: Type) extends IterablePurs