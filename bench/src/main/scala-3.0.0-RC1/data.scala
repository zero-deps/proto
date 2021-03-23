package proto
package data

import org.openjdk.jmh.annotations.*
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
    given MessageCodec[Version] = caseCodecIdx
    given MessageCodec[VectorClock] = caseCodecIdx
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
  def jackson(state: States.JacksonState, bh: Blackhole): Unit = {
    bh.consume(state.m.writeValueAsBytes(state.data))
  }

  @Benchmark
  def scalapb(state: States.ProtobufState, bh: Blackhole): Unit = {
    bh.consume(state.data.toByteArray)
  }

  @Benchmark
  def proto(state: States.MacrosState, bh: Blackhole): Unit = {
    bh.consume(encode(state.data)(state.codec))
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
  def scalapb(state: States.ProtobufState, bh: Blackhole): Unit = {
    bh.consume(binarymodel.Data.parseFrom(state.bytes): binarymodel.Data)
  }

  @Benchmark
  def proto(state: States.MacrosState, bh: Blackhole): Unit = {
    bh.consume(decode(state.bytes)(state.codec))
  }
}
