package proto

import scala.annotation.StaticAnnotation

final class N(n: Int) extends StaticAnnotation
final class RestrictedN(nums: Int*) extends StaticAnnotation
