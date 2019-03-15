package zd
package proto

import com.google.protobuf.{CodedOutputStream, CodedInputStream}

object api {
  trait Prepare {
    val size: Int
    def write(os: CodedOutputStream): Unit
  }

  trait MessageCodec[A] {
    def prepare(a: A): Prepare
    def read(is: CodedInputStream): A
  }

  def encode[A](a: A)(implicit c: MessageCodec[A]) = {
    val p = c.prepare(a)
    val bytes = new Array[Byte](p.size)
    val os = CodedOutputStream.newInstance(bytes)
    p.write(os)
    bytes
  }

  def decode[A](x: Array[Byte])(implicit c: MessageCodec[A]): A = {
    val is = CodedInputStream.newInstance(x)
    c.read(is)
  }

  final case class N(n: Int) extends scala.annotation.StaticAnnotation
}
