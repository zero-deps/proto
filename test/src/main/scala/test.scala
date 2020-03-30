package zd.proto

import zd.proto.api.{N, encode, MessageCodec}
import zd.proto.macrosapi.enumByN

enum Push {
  @N(1) case Pong
  @N(2) case Msg(@N(1) txt: String)
}

object Tests extends App {
  // assert(1 == 2, "test assert")
  given MessageCodec[Push] = enumByN
  encode[Push](Push.Pong)
}