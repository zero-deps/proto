package zd
package proto

import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import zd.proto.Bytes

object api {
  trait Prepare {
    val size: Int
    def write(os: CodedOutputStream): Unit
  }

  trait MessageCodec[A] {
    def prepare(a: A): Prepare
    def read(is: CodedInputStream): A
    val nums: Map[String, Int]
    val aType: String
  }

  def encodeToBytes[A](a: A)(implicit c: MessageCodec[A]) = {
    Bytes.unsafeWrap(encode[A](a))
  }

  def encode[A](a: A)(implicit c: MessageCodec[A]) = {
    val p = c.prepare(a)
    val bytes = new Array[Byte](p.size)
    val os = CodedOutputStream.newInstance(bytes)
    p.write(os)
    bytes
  }

  def decode[A](xs: Bytes)(implicit c: MessageCodec[A]): A = {
    decode[A](xs.unsafeArray, offset=0)
  }

  def decode[A](xs: Bytes, offset: Int)(implicit c: MessageCodec[A]): A = {
    decode[A](xs.unsafeArray, offset)
  }

  def decode[A](xs: Array[Byte], offset: Int=0)(implicit c: MessageCodec[A]): A = {
    val is =
      if (offset > 0) CodedInputStream.newInstance(xs, offset, xs.length-offset)
      else CodedInputStream.newInstance(xs)
    c.read(is)
  }

  final case class N(n: Int) extends scala.annotation.StaticAnnotation
  final case class RestrictedN(nums: Int*) extends scala.annotation.StaticAnnotation
}
