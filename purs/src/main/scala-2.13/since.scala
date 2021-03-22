package zero.protopurs

import scala.annotation.StaticAnnotation

final case class Since(version: String, change: String) extends StaticAnnotation