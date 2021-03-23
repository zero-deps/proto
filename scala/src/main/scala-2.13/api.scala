import com.google.protobuf.{CodedOutputStream, CodedInputStream}

package object proto {
  def encode[A](a: A)(implicit c: MessageCodec[A]) = {
    val p = c.prepare(a)
    val bytes = new Array[Byte](p.size)
    val os = CodedOutputStream.newInstance(bytes)
    p.write(os)
    bytes
  }

  def encodeToBytes[A](a: A)(implicit c: MessageCodec[A]) =
    Bytes.unsafeWrap(encode[A](a))

  def decode[A](xs: Array[Byte])(implicit c: MessageCodec[A]): A =
    decode[A](xs, offset=0)

  def decode[A](xs: Array[Byte], offset: Int)(implicit c: MessageCodec[A]): A = {
    val is =
      if (offset > 0) CodedInputStream.newInstance(xs, offset, xs.length-offset)
      else CodedInputStream.newInstance(xs)
    c.read(is)
  }

  def decode[A](bs: Bytes)(implicit c: MessageCodec[A]): A =
    decode[A](bs.unsafeArray)

  def decode[A](bs: Bytes, offset: Int)(implicit c: MessageCodec[A]): A =
    decode[A](bs.unsafeArray, offset)
}
