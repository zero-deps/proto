package zd
package proto

final class Bytes private (a: Array[Byte]) {
  val unsafeArray: Array[Byte] = a
  override def equals(other: Any): Boolean = {
    if (!other.isInstanceOf[Bytes]) false
    else java.util.Arrays.equals(a, other.asInstanceOf[Bytes].unsafeArray)
  }
  override def hashCode(): Int = java.util.Arrays.hashCode(a)
}

object Bytes {
  def unsafeWrap(a: Array[Byte]): Bytes = {
    new Bytes(a)
  }
}
