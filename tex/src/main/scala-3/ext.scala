package proto.tex

extension [A](x: Option[A])
  def cata[B](f: A => B, b: => B): B =
    x match
      case Some(a) => f(a)
      case None => b
