package zd
package proto

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.infra.Blackhole

final case class Vc(a: String, b: Long)
final case class Data(key: Array[Byte], value: Array[Byte], modified: Long, vc: Vector[Vc])

final case class DataScodec(key: Array[Byte], value: Array[Byte], modified: Long, vc: List[(String, Long)])

object States {

  @State(Scope.Benchmark)
  class JavaState {
    val key = (1 to 20).mkString.getBytes("UTF-8")
    val value = Array.fill(1000)(1).map(_.toByte)
    val modified: Long = 1552661477L
    val vc = Vector.fill(2)(Vc(a="169.0.0.1:4400", b=2000L))
    
    val data = Data(key=key, value=value, modified=modified, vc=vc)
    val bytes: Array[Byte] = {
      import java.io._
      val bos = new ByteArrayOutputStream
      val out = new ObjectOutputStream(bos)
      out.writeObject(data)
      out.close()
      bos.toByteArray
    }
  }

  @State(Scope.Benchmark)
  class ScodecState {
    val key = (1 to 20).mkString.getBytes("UTF-8")
    val value = Array.fill(1000)(1).map(_.toByte)
    val modified: Long = 1552661477L
    val vc = List.fill(2)("169.0.0.1:4400" -> 2000L)

    val data = DataScodec(key=key, value=value, modified=modified, vc=vc)
    import scodec.codecs._
    import scodec.{Attempt, DecodeResult, Codec, SizeBound}
    import scodec.bits.BitVector
    val abytes = new Codec[Array[Byte]] {
      def sizeBound = SizeBound.unknown
      def encode(a: Array[Byte]) = Attempt.successful(BitVector.view(a))
      def decode(bytes: BitVector) = Attempt.successful(DecodeResult(bytes.toByteArray, BitVector.empty))
      override def toString = "byteArray"
    }
    val codec = abytes ~ abytes ~ vlong ~ list(utf8 ~ vlong)
    val bytes: Array[Byte] = codec.encode(data.key ~ data.value ~ data.modified ~ data.vc).toOption.get.toByteArray
  }

  @State(Scope.Benchmark)
  class JacksonState {
    val key = (1 to 20).mkString.getBytes("UTF-8")
    val value = Array.fill(1000)(1).map(_.toByte)
    val modified: Long = 1552661477L
    val vc = Vector.fill(2)(Vc(a="169.0.0.1:4400", b=2000L))

    val data = Data(key=key, value=value, modified=modified, vc=vc)
    import com.fasterxml.jackson.module.scala.DefaultScalaModule
    import com.fasterxml.jackson.databind.ObjectMapper
    val m = new ObjectMapper()
    m.registerModule(DefaultScalaModule)
    val bytes: Array[Byte] = m.writeValueAsBytes(data)
  }

  @State(Scope.Benchmark)
  class JsoniterState {
    val key = (1 to 20).mkString.getBytes("UTF-8")
    val value = Array.fill(1000)(1).map(_.toByte)
    val modified: Long = 1552661477L
    val vc = Vector.fill(2)(Vc(a="169.0.0.1:4400", b=2000L))

    val data = Data(key=key, value=value, modified=modified, vc=vc)

    import com.github.plokhotnyuk.jsoniter_scala.macros._
    import com.github.plokhotnyuk.jsoniter_scala.core._
    val codec: JsonValueCodec[Data] = JsonCodecMaker.make[Data](CodecMakerConfig())
    val bytes: Array[Byte] = writeToArray(data)(codec)
  }

  @State(Scope.Benchmark)
  class ProtobufState {
    import com.google.protobuf.ByteString
    val key = ByteString.copyFrom((1 to 20).mkString.getBytes("UTF-8"))
    val value = ByteString.copyFrom(Array.fill(1000)(1).map(_.toByte))
    val modified: Long = 1552661477L
    val vc = Vector.fill(2)(binarymodel.Vc(a="169.0.0.1:4400", b=2000L))

    val data = binarymodel.Data(key=key, value=value, modified=modified, vc=vc)
    val bytes: Array[Byte] = data.toByteArray
  }

  @State(Scope.Benchmark)
  class MacrosState {
    val key = (1 to 20).mkString.getBytes("UTF-8")
    val value = Array.fill(1000)(1).map(_.toByte)
    val modified: Long = 1552661477L
    val vc = Vector.fill(2)(Vc(a="169.0.0.1:4400", b=2000L))

    val data = Data(key=key, value=value, modified=modified, vc=vc)
    import zd.proto.api.encode
    import zd.proto.macrosapi.caseCodecIdx
    implicit val vcCodec = caseCodecIdx[Vc]
    val codec = caseCodecIdx[Data]
    val bytes: Array[Byte] = encode(data)(codec)
  }
}

class Encode {
  @Benchmark
  def java_(state: States.JavaState, bh: Blackhole): Unit = {
    val bos = new java.io.ByteArrayOutputStream
    val out = new java.io.ObjectOutputStream(bos)
    out.writeObject(state.data)
    out.close()
    bh.consume(bos.toByteArray)
  }

  @Benchmark
  def scodec_(state: States.ScodecState, bh: Blackhole): Unit = {
    import scodec.codecs._
    bh.consume(state.codec.encode(state.data.key ~ state.data.value ~ state.data.modified ~ state.data.vc).toOption.get.toByteArray)
  }

  @Benchmark
  def jackson(state: States.JacksonState, bh: Blackhole): Unit = {
    bh.consume(state.m.writeValueAsBytes(state.data))
  }

  @Benchmark
  def jsoniter_scala(state: States.JsoniterState, bh: Blackhole): Unit = {
    import com.github.plokhotnyuk.jsoniter_scala.core._
    bh.consume(writeToArray(state.data)(state.codec))
  }

  @Benchmark
  def scalapb(state: States.ProtobufState, bh: Blackhole): Unit = {
    bh.consume(state.data.toByteArray)
  }

  @Benchmark
  def protobuf_scala_macros(state: States.MacrosState, bh: Blackhole): Unit = {
    bh.consume(proto.api.encode(state.data)(state.codec))
  }
}

class Decode {
  @Benchmark
  def java_(state: States.JavaState, bh: Blackhole): Unit = {
    val in = new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(state.bytes))
    val obj = in.readObject
    in.close()
    bh.consume(obj.asInstanceOf[Data])
  }

  @Benchmark
  def scodec_(state: States.ScodecState, bh: Blackhole): Unit = {
    import scodec.bits.BitVector
    val y = state.codec.decode(BitVector.view(state.bytes)).toOption.get.value
    bh.consume(DataScodec(y._1._1._1, y._1._1._2, y._1._2, y._2))
  }

  @Benchmark
  def jackson(state: States.JacksonState, bh: Blackhole): Unit = {
    bh.consume(state.m.readValue(state.bytes, classOf[Data]): Data)
  }

  @Benchmark
  def jsoniter_scala(state: States.JsoniterState, bh: Blackhole): Unit = {
    import com.github.plokhotnyuk.jsoniter_scala.core._
    bh.consume(readFromArray(state.bytes)(state.codec): Data)
  }

  @Benchmark
  def scalapb(state: States.ProtobufState, bh: Blackhole): Unit = {
    bh.consume(binarymodel.Data.parseFrom(state.bytes): binarymodel.Data)
  }

  @Benchmark
  def protobuf_scala_macros(state: States.MacrosState, bh: Blackhole): Unit = {
    bh.consume(proto.api.decode(state.bytes)(state.codec))
  }
}
