package proto

final class Bytes private (a: Array[Byte]) {
  lazy val length: Int = a.length
  lazy val isEmpty: Boolean = a.isEmpty
  lazy val nonEmpty: Boolean = a.nonEmpty
  lazy val mkString: String = new String(a, "utf8")
  val unsafeArray: Array[Byte] = a
  override def equals(other: Any): Boolean = {
    if (!other.isInstanceOf[Bytes]) false
    else java.util.Arrays.equals(a, other.asInstanceOf[Bytes].unsafeArray)
  }
  override def hashCode(): Int = java.util.Arrays.hashCode(a)
}

object Bytes {
  val empty = new Bytes(Array.emptyByteArray)
  def unsafeWrap(a: Array[Byte]): Bytes = new Bytes(a)
}
