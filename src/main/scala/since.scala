package zd.proto.purs

import scala.annotation.StaticAnnotation

final case class Since(version: String, change: String) extends StaticAnnotation