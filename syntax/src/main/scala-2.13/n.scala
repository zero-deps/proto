package proto

import scala.annotation._

final case class N(n: Int) extends StaticAnnotation
final case class RestrictedN(nums: Int*) extends StaticAnnotation
