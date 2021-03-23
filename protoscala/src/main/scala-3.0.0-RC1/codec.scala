package proto

import com.google.protobuf.{CodedOutputStream, CodedInputStream}

trait Prepare:
  val size: Int
  def write(os: CodedOutputStream): Unit

trait MessageCodec[A]:
  def prepare(a: A): Prepare
  def read(is: CodedInputStream): A
