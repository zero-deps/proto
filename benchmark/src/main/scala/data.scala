package zd
package proto
package data

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.infra.Blackhole

final case class Data(lastModified: Long, vc: VectorClock, value: Array[Byte])
final case class VectorClock(versions: Vector[Version])
final case class Version(node: String, timestamp: Long)

object States {
  @State(Scope.Benchmark)
  class JavaState {
    val value = Array.fill(1000)(1).map(_.toByte)
    val lastModified: Long = 1552661477L
    val vc = VectorClock(Vector.fill(2)(Version(node="169.0.0.1:4400", timestamp=2000L)))
    
    val data = Data(value=value, lastModified=lastModified, vc=vc)
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
  class JacksonState {
    val value = Array.fill(1000)(1).map(_.toByte)
    val lastModified: Long = 1552661477L
    val vc = VectorClock(Vector.fill(2)(Version(node="169.0.0.1:4400", timestamp=2000L)))

    val data = Data(value=value, lastModified=lastModified, vc=vc)
    import com.fasterxml.jackson.module.scala.DefaultScalaModule
    import com.fasterxml.jackson.databind.ObjectMapper
    val m = new ObjectMapper()
    m.registerModule(DefaultScalaModule)
    val bytes: Array[Byte] = m.writeValueAsBytes(data)
  }

  @State(Scope.Benchmark)
  class JsoniterState {
    val value = Array.fill(1000)(1).map(_.toByte)
    val lastModified: Long = 1552661477L
    val vc = VectorClock(Vector.fill(2)(Version(node="169.0.0.1:4400", timestamp=2000L)))

    val data = Data(value=value, lastModified=lastModified, vc=vc)

    import com.github.plokhotnyuk.jsoniter_scala.macros._
    import com.github.plokhotnyuk.jsoniter_scala.core._
    val codec: JsonValueCodec[Data] = JsonCodecMaker.make[Data](CodecMakerConfig())
    val bytes: Array[Byte] = writeToArray(data)(codec)
  }

  @State(Scope.Benchmark)
  class ProtobufState {
    import com.google.protobuf.ByteString
    val value = ByteString.copyFrom(Array.fill(1000)(1).map(_.toByte))
    val lastModified: Long = 1552661477L
    val vc = binarymodel.VectorClock(Vector.fill(2)(binarymodel.Version(node="169.0.0.1:4400", timestamp=2000L)))

    val data = binarymodel.Data(value=value, lastModified=lastModified, vc=Some(vc))
    val bytes: Array[Byte] = data.toByteArray
  }

  @State(Scope.Benchmark)
  class MacrosState {
    val value = Array.fill(1000)(1).map(_.toByte)
    val lastModified: Long = 1552661477L
    val vc = VectorClock(Vector.fill(2)(Version(node="169.0.0.1:4400", timestamp=2000L)))

    val data = Data(value=value, lastModified=lastModified, vc=vc)
    import zd.proto.api.encode
    import zd.proto.macrosapi.caseCodecIdx
    implicit val vCodec = caseCodecIdx[Version]
    implicit val vcCodec = caseCodecIdx[VectorClock]
    val codec = caseCodecIdx[Data]
    val bytes: Array[Byte] = encode(data)(codec)
  }

  @State(Scope.Benchmark)
  class BoopickleState {
    val value = Array.fill(1000)(1).map(_.toByte)
    val lastModified: Long = 1552661477L
    val vc = VectorClock(Vector.fill(2)(Version(node="169.0.0.1:4400", timestamp=2000L)))

    val data = Data(value=value, lastModified=lastModified, vc=vc)
    import boopickle.Default._
    val bb = Pickle.intoBytes(data)
    val bytes: Array[Byte] = new Array[Byte](bb.remaining)
    bb.get(bytes)
  }

  @State(Scope.Benchmark)
  class KryoMacrosState {
    val value = Array.fill(1000)(1).map(_.toByte)
    val lastModified: Long = 1552661477L
    val vc = VectorClock(Vector.fill(2)(Version(node="169.0.0.1:4400", timestamp=2000L)))

    val data = Data(value=value, lastModified=lastModified, vc=vc)
    import com.evolutiongaming.kryo.Serializer
    val serializer = Serializer.make[Data]
    import com.esotericsoftware.kryo.Kryo
    val kryo = new Kryo
    kryo.register(classOf[Data])
    import com.esotericsoftware.kryo.io.Output
    val output = new Output(1024, -1)
    kryo.writeObject(output, data)
    val bytes: Array[Byte] = output.getBuffer
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

  @Benchmark
  def boopickle_(state: States.BoopickleState, bh: Blackhole): Unit = {
    import boopickle.Default._
    val bb = Pickle.intoBytes(state.data)
    val bytes = new Array[Byte](bb.remaining)
    bb.get(bytes)
    bh.consume(bytes)
  }

  @Benchmark
  def kryo_macros(state: States.KryoMacrosState, bh: Blackhole): Unit = {
    import com.esotericsoftware.kryo.io.Output
    val output = new Output(1024, -1)
    state.kryo.writeObject(output, state.data)
    val bytes: Array[Byte] = output.getBuffer
    bh.consume(bytes)
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

  @Benchmark
  def boopickle_(state: States.BoopickleState, bh: Blackhole): Unit = {
    import boopickle.Default._
    val bb = java.nio.ByteBuffer.wrap(state.bytes)
    bh.consume(Unpickle[Data].fromBytes(bb))
  }

  // @Benchmark
  // def kryo_macros(state: States.KryoMacrosState, bh: Blackhole): Unit = {
  //   import com.esotericsoftware.kryo.io.Input
  //   val input = new Input(state.bytes)
  //   bh.consume(state.kryo.readObject(input, classOf[Data]))
  // }
}
