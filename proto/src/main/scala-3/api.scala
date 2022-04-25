package proto

import java.io.{OutputStream, InputStream}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}

def encode[A](a: A)(implicit c: MessageCodec[A]): Array[Byte] =
  val p = c.prepare(a)
  val bytes = new Array[Byte](p.size)
  val os = CodedOutputStream.newInstance(bytes).nn
  p.write(os)
  bytes

def encodeS[A](a: A, s: OutputStream)(implicit c: MessageCodec[A]): OutputStream =
  val p = c.prepare(a)
  val os = CodedOutputStream.newInstance(s).nn
  p.write(os)
  s

def decode[A](xs: Array[Byte])(implicit c: MessageCodec[A]): A =
  c.read(CodedInputStream.newInstance(xs).nn)

def decode[A](xs: Array[Byte], offset: Int)(implicit c: MessageCodec[A]): A =
  c.read(CodedInputStream.newInstance(xs, offset, xs.length-offset).nn)

def decodeS[A](s: InputStream)(implicit c: MessageCodec[A]): A =
  c.read(CodedInputStream.newInstance(s).nn)