package zd.proto.test

import org.junit.Test
import org.junit.Assert._
import java.util.Arrays

import zd.proto.macrosapi.casecodecAuto
import zd.proto.api.{MessageCodec, N, decode, encode, Prepare}
import zd.proto.Bytes
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.collection.immutable.ArraySeq

object models {
  final case class Basic(
    @N(21) int: Int
  , @N(22) long: Long
  , @N(23) bool: Boolean
  , @N(24) double: Double
  , @N(25) float: Float
  , @N(26) str: String
  , @N(50) bytes: Array[Byte]
  )

  final case class OptionBasic( 
    @N(21) int: Option[Int]
  , @N(22) long: Option[Long]
  , @N(23) bool: Option[Boolean]
  , @N(24) double: Option[Double]
  , @N(25) float: Option[Float]
  , @N(26) str: Option[String]
  , @N(50) bytes: Option[Array[Byte]]
  )

  final case class ClassWithArray(@N(1) x: Array[Byte])
  final case class ClassWithArraySeq(@N(1) y: ArraySeq[Byte])
  final case class ClassWithBytes(@N(1) z: Bytes)

  final case class Collections(
    @N(21) int: List[Int]
  , @N(22) long: List[Long]
  , @N(23) bool: List[Boolean]
  , @N(24) double: List[Double]
  , @N(25) float: List[Float]
  , @N(26) str: List[String]
  , @N(27) bytes: List[Array[Byte]]
  , @N(28) message: List[Message]
  )

  final case class Message(@N(2) int: Int, @N(4) str: String, @N(6) set: Set[String], @N(8) msg1: Option[Message1])
  final case class Message1(@N(1) name: String, @N(2) value: Double)

  given MessageCodec[Basic] = casecodecAuto
  given MessageCodec[OptionBasic] = casecodecAuto
  given MessageCodec[ClassWithArray] = casecodecAuto
  given MessageCodec[ClassWithArraySeq] = casecodecAuto
  given MessageCodec[ClassWithBytes] = casecodecAuto
  given MessageCodec[Message1] = casecodecAuto
  given MessageCodec[Message] = casecodecAuto
  given MessageCodec[Collections] = casecodecAuto
}

class Testing {
  import models._
  
  @Test def encodeDecode(): Unit = {
    (for {
      int <- List(Int.MinValue, -2, -1, 0, 1, 2, Int.MaxValue)
      long <- List(Long.MinValue, -2L, -1L, 0L, 1L, 2L, Long.MaxValue)
      bool <- List(false, true)
      double <- List(Double.MinValue, -2.0D, -1.0D, 0.0D, 1.0D, 2.0D, Double.MaxValue)
      float <- List(Float.MinValue, -2.0F, -1.0F, 0.0F, 1.0F, 2.0F, Float.MaxValue)
      str <- List("", "str")
      bytes <- List(Array.empty[Byte], Array(0.toByte), Array(1.toByte), Array(2.toByte), Array(255.toByte))
    } yield Basic(int = int, long = long, bool = bool, double = double, float = float, str = str, bytes = bytes)).foreach{ data =>
      val decoded: Basic = decode(encode(data))
      assert(decoded.int == data.int)
      assert(decoded.long == data.long)
      assert(decoded.bool == data.bool)
      assert(decoded.double == data.double)
      assert(decoded.float == data.float)
      assert(decoded.str == data.str)
      assert(Arrays.equals(decoded.bytes, data.bytes))
    }
  }

  //ArraySeq[Byte] is compatible with Array[Byte]
  @Test def test1(): Unit = {
    import java.util.Arrays
    assert(Arrays.equals(decode[ClassWithArraySeq](encode[ClassWithArray](ClassWithArray(x=Array[Byte](1,2,3)))).y.unsafeArray.asInstanceOf[Array[Byte]], Array[Byte](1,2,3)))
    assert(Arrays.equals(decode[ClassWithArray](encode[ClassWithArraySeq](ClassWithArraySeq(y=ArraySeq.unsafeWrapArray[Byte](Array[Byte](1,2,3))))).x, Array[Byte](1,2,3)))
  }

  //"Bytes is compatible with Array[Byte]"
  @Test def test2(): Unit = {
    import java.util.Arrays
    assert(Arrays.equals(decode[ClassWithBytes](encode[ClassWithArray](ClassWithArray(x=Array[Byte](1,2,3)))).z.unsafeArray, Array[Byte](1,2,3)))
    assert(Arrays.equals(decode[ClassWithArray](encode[ClassWithBytes](ClassWithBytes(z=Bytes.unsafeWrap(Array[Byte](1,2,3))))).x, Array[Byte](1,2,3)))
  }

  //array byte wrapper encode
  @Test def test3(): Unit = {
    val data = ClassWithArray(Array(6, 7, 8, 9, 0))
    val encoded: Array[Byte] = encode(data)
    assert(Arrays.equals(Array[Byte](10,5, 6,7,8,9,0), encoded))
  }

  // "empty bytearray <-> all fields none"
  @Test def test4(): Unit = {
    import java.util.Arrays
    val data = OptionBasic(int = None, long = None, bool = None, double = None, float = None, str = None, bytes = None)
    assert(Arrays.equals(encode(data), Array.empty[Byte]))
    val decoded = decode[OptionBasic](Array.empty[Byte])
    assert(decoded == data)
  }

  // "bytearray <-> all fields"
  @Test def test5(): Unit = {
    import java.util.Arrays

    val data = OptionBasic(int = Some(1), long = Some(2L), bool = Some(true), double = Some(3.0D), float = Some(4.0F), str = Some("5"), bytes = Some(Array(6, 7, 8, 9, 0)))

    val bytes = Array[Byte](168.toByte,1,1, 176.toByte,1,2, 184.toByte,1,1, 193.toByte,1,0,0,0,0,0,0,8,64, 205.toByte,1,0,0,128.toByte,64, 210.toByte,1,1,53, 146.toByte,3,5,6,7,8,9,0)
    assert(Arrays.equals(encode(data), bytes))

    val decoded = decode[OptionBasic](bytes)
    assert(decoded.int.get == data.int.get)
    assert(decoded.long.get == data.long.get)
    assert(decoded.bool.get == data.bool.get)
    assert(decoded.double.get == data.double.get)
    assert(decoded.float.get == data.float.get)
    assert(decoded.str.get == data.str.get)
    assert(Arrays.equals(decoded.bytes.get, data.bytes.get))
  }

  @Test def test6(): Unit = {
    import java.util.Arrays

    val int = List(Int.MinValue, -2, -1, 0, 1, 2, Int.MaxValue)
    val long = List(Long.MinValue, -2L, -1L, 0L, 1L, 2L, Long.MaxValue)
    val bool = List(false, true)
    val double = List(Double.MinValue, -2.0D, -1.0D, 0.0D, 1.0D, 2.0D, Double.MaxValue)
    val float = List(Float.MinValue, -2.0F, -1.0F, 0.0F, 1.0F, 2.0F, Float.MaxValue)
    val str = List("", "str")
    val bytes = List(Array.empty[Byte], Array(0.toByte), Array(1.toByte), Array(2.toByte), Array(255.toByte))
    val message = List(
        Message(0, "0", Set("1", "2", "3"), Some(Message1("msg1", 3.0)))
      , Message(1, "2", Set("4", "5", "6"), None)
      )

    val data = Collections(
        int = int
      , long = long
      , bool = bool
      , double = double
      , float = float
      , str = str
      , bytes = bytes
      , message = message
    )
    val encoded = encode[Collections](data)
    val decoded = decode[Collections](encoded)
    assert(decoded.int == data.int)
    assert(decoded.long == data.long)
    assert(decoded.bool == data.bool)
    assert(decoded.double == data.double)
    assert(decoded.float == data.float)
    assert(decoded.str == data.str)
    assert(decoded.message == data.message)
    val _ = assert(decoded.bytes.zip(data.bytes).forall{ case (decodedBytes, dataBytes) => Arrays.equals(decodedBytes, dataBytes) })
  }
}