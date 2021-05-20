package proto
package msg

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

final case class Msg(stat: Stat, meta: StatMeta)
final case class Stat(name: String, value: String)
final case class StatMeta(time: String, addr: String)

object States {
  // @State(Scope.Benchmark)
  // class ProtobufState {
  //   val msg = binarymodel.Msg(Some(binarymodel.Stat("cpu", "0.45")), Some(binarymodel.StatMeta("1571067208996", "127.0.0.1:8080")))
  //   val bytes: Array[Byte] = msg.toByteArray
  // }

  @State(Scope.Benchmark)
  class MacrosState {
    val msg = Msg(Stat("cpu", "0.45"), StatMeta("1571067208996", "127.0.0.1:8080"))
    given MessageCodec[Stat] = caseCodecIdx
    given MessageCodec[StatMeta] = caseCodecIdx
    val codec = caseCodecIdx[Msg]
    val bytes: Array[Byte] = encode(msg)(codec)
  }
}

class Encode {
  // @Benchmark
  // def scalapb(state: States.ProtobufState, bh: Blackhole): Unit = {
  //   bh.consume(state.msg.toByteArray)
  // }

  @Benchmark
  def proto(state: States.MacrosState, bh: Blackhole): Unit = {
    bh.consume(encode(state.msg)(state.codec))
  }
}

class Decode {
  // @Benchmark
  // def scalapb(state: States.ProtobufState, bh: Blackhole): Unit = {
  //   bh.consume(binarymodel.Msg.parseFrom(state.bytes): binarymodel.Msg)
  // }

  @Benchmark
  def proto(state: States.MacrosState, bh: Blackhole): Unit = {
    bh.consume(decode(state.bytes)(state.codec))
  }
}
