package zd
package proto
package api

import com.google.protobuf.{CodedOutputStream, CodedInputStream}

trait Prepare:
  val size: Int
  def write(os: CodedOutputStream): Unit

trait MessageCodec[A]:
  def prepare(a: A): Prepare
  def read(is: CodedInputStream): A
  val nums: Map[String, Int]
  val aType: String

def encode[A](a: A)(implicit c: MessageCodec[A]): Array[Byte] =
  val p = c.prepare(a)
  val bytes = new Array[Byte](p.size)
  val os = CodedOutputStream.newInstance(bytes)
  p.write(os)
  bytes

def encodeI[A](a: A)(implicit c: MessageCodec[A]): IArray[Byte] =
  IArray.unsafeFromArray[Byte](encode[A](a))

def decode[A](xs: Array[Byte])(implicit c: MessageCodec[A]): A =
  decode[A](xs, offset=0)

def decode[A](xs: Array[Byte], offset: Int)(implicit c: MessageCodec[A]): A =
  val is =
    if offset > 0 then CodedInputStream.newInstance(xs, offset, xs.length-offset)
    else CodedInputStream.newInstance(xs)
  c.read(is)

def decodeI[A](bs: IArray[Byte])(implicit c: MessageCodec[A]): A =
  decode[A](bs.toArray)

def decodeI[A](bs: IArray[Byte], offset: Int)(implicit c: MessageCodec[A]): A =
  decode[A](bs.toArray, offset)

final case class N(n: Int) extends annotation.StaticAnnotation
final case class RestrictedN(nums: Int*) extends annotation.StaticAnnotation
