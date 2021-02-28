package proto
package api

import java.io.{OutputStream, InputStream}
import scala.annotation.*
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

def encodeS[A](a: A, s: OutputStream)(implicit c: MessageCodec[A]): OutputStream =
  val p = c.prepare(a)
  val os = CodedOutputStream.newInstance(s)
  p.write(os)
  s

def decode[A](xs: Array[Byte])(implicit c: MessageCodec[A]): A =
  c.read(CodedInputStream.newInstance(xs))

def decode[A](xs: Array[Byte], offset: Int)(implicit c: MessageCodec[A]): A =
  c.read(CodedInputStream.newInstance(xs, offset, xs.length-offset))

def decodeI[A](bs: IArray[Byte])(implicit c: MessageCodec[A]): A =
  decode[A](bs.toArray)

def decodeI[A](bs: IArray[Byte], offset: Int)(implicit c: MessageCodec[A]): A =
  decode[A](bs.toArray, offset)

def decodeS[A](s: InputStream)(implicit c: MessageCodec[A]): A =
  c.read(CodedInputStream.newInstance(s))

final class N(n: Int) extends StaticAnnotation
final class RestrictedN(nums: Int*) extends StaticAnnotation
