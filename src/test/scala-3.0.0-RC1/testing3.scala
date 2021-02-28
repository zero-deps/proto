package test3

import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import java.util.Arrays
import org.junit.Assert.*
import org.junit.Test
import scala.collection.immutable.ArraySeq
import proto.api.*, proto.macros.*

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
  final case class ClassWithBytes(@N(1) z: IArray[Byte])

  final case class Collections(
    @N(21) int: List[Int]
  , @N(22) long: List[Long]
  , @N(23) bool: List[Boolean]
  , @N(24) double: List[Double]
  , @N(25) float: List[Float]
  , @N(26) str: List[String]
  , @N(27) bytes: List[Array[Byte]]
  , @N(28) message: List[Message]
  , @N(29) basicBasic: Map[Int, Int]
  )

  final case class Message(@N(2) int: Int, @N(4) str: String, @N(6) set: Set[String], @N(8) msg1: Option[Message1])
  final case class Message1(@N(1) name: String, @N(2) value: Double)

  final case class DefaultValuesClass(
    @N(1) int: Int = 10
  , @N(2) bool: Boolean = true
  , @N(3) float: Float
  , @N(4) str: String = "NonEmptyString"
  , @N(5) msg: Message = Message(int=321, "string", Set("1","3","5","7","11"), None)
  )

  final case class DefaultValuesClass1(@N(3) float: Float)

  final case class ClassWithTypeParams[A,B,C](@N(1) a: A, @N(2) b: B, @N(3) c: C)

  enum Push {
    @N(1) case Pong
    @N(2) case Msg(@N(1) txt: String, @N(2) id: Int)
  }

  sealed trait Color
  @N(1) final case class Black(
      @N(1) name: String
    , @N(2) value: Int
    , @N(3) msg: Message
    ) extends Color
  @N(2) final case class White(@N(1) value: Int) extends Color
  @N(3) case object Yellow extends Color
  @N(4) case object Red extends Color

  final class Teleport(@N(21) val id: String, @N(22) val n: Int) {
    override def toString(): String = s"Teleport(id=$id, n=$n)"
  }

  object Teleport {
    def apply(id: String, n: Int): Teleport = {
      new Teleport(id, n)
    }
  }

  abstract class SimpleClass[A] {
    def id: Int
    def id2: List[A]
    override def toString(): String = s"(id=$id, id2=$id2)"
  }
  
  object SimpleClass{
    def init(p1: Int): SimpleClass[String] = new SimpleClass[String] {
      override def id: Int = p1
      override def id2: List[String] = List("0")
    }
    def init2(p1: Int, p2: List[String]): SimpleClass[String] = new SimpleClass[String] {
      override def id: Int = p1
      override def id2: List[String] = p2
    }
    def init3[A](p1: Int, p2: List[A]): SimpleClass[A] = new SimpleClass[A] {
      override def id: Int = p1
      override def id2: List[A] = p2
    }
  }

  given MessageCodec[Tuple2[Int, Int]] = caseCodecIdx
  given MessageCodec[Basic] = caseCodecAuto
  given MessageCodec[OptionBasic] = caseCodecAuto
  given MessageCodec[ClassWithArray] = caseCodecAuto
  given MessageCodec[ClassWithArraySeq] = caseCodecAuto
  given MessageCodec[ClassWithBytes] = caseCodecAuto
  given MessageCodec[Message1] = caseCodecAuto
  given MessageCodec[Message] = caseCodecAuto
  given MessageCodec[Collections] = caseCodecAuto
  given MessageCodec[DefaultValuesClass] = caseCodecAuto
  given MessageCodec[DefaultValuesClass1] = caseCodecAuto
}

class Testing {
  import models.*
  
  @Test def encodeDecodeCaseCodecAuto(): Unit = {
    val c: MessageCodec[Basic] = caseCodecAuto
    encodeDecode(c)
  }

  @Test def encodeDecodeCaseCodecNums(): Unit = {
    val c: MessageCodec[Basic] = caseCodecNums("int"->21,"long"->22,"bool"->23,"double"->24,"float"->25,"str"->26,"bytes"->50)
    encodeDecode(c)
  }

  @Test def encodeDecodeCaseCodecIdx(): Unit = {
    val c: MessageCodec[Basic] = caseCodecIdx
    encodeDecode(c)
  }

  @Test def encodeDecodeClassCodecAuto(): Unit = {
    val c: MessageCodec[Basic] = classCodecAuto
    encodeDecode(c)
  }

  def encodeDecode(codec: MessageCodec[Basic]): Unit = {
    (for {
      int <- List(Int.MinValue, -2, -1, 0, 1, 2, Int.MaxValue)
      long <- List(Long.MinValue, -2L, -1L, 0L, 1L, 2L, Long.MaxValue)
      bool <- List(false, true)
      double <- List(Double.MinValue, -2.0D, -1.0D, 0.0D, 1.0D, 2.0D, Double.MaxValue)
      float <- List(Float.MinValue, -2.0F, -1.0F, 0.0F, 1.0F, 2.0F, Float.MaxValue)
      str <- List("", "str")
      bytes <- List(Array.empty[Byte], Array(0.toByte), Array(1.toByte), Array(2.toByte), Array(255.toByte))
    } yield Basic(int = int, long = long, bool = bool, double = double, float = float, str = str, bytes = bytes)).foreach{ data =>
      val decoded: Basic = decode(encode(data)(codec))(codec)
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
    assert(Arrays.equals(decode[ClassWithArray](encode[ClassWithBytes](ClassWithBytes(z=IArray.unsafeFromArray(Array[Byte](1,2,3))))).x, Array[Byte](1,2,3)))
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
    val basicBasic: Map[Int, Int] = Map(1->2, 2->3, 4->5)

    val data = Collections(
        int = int
      , long = long
      , bool = bool
      , double = double
      , float = float
      , str = str
      , bytes = bytes
      , message = message
      , basicBasic = basicBasic
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

  //All params passed
  @Test def testDefaultValues1(): Unit = {
    val data = DefaultValuesClass(
      int=23
    , bool=false
    , float=33
    , str="hello"
    , msg=Message(int=123, "hello", Set("1","3","5","7","11","17"), Some(Message1("msg1", 10.0)))
    )
    val decoded = decode[DefaultValuesClass](encode(data))
    val _ = assert(decoded == data)
  }

  //None params passed
  @Test def testDefaultValues2(): Unit = {
    val data = DefaultValuesClass(
      float=33
    )
    val decoded = decode[DefaultValuesClass](encode(data))
    val _ = assert(decoded == data)
  }

  //new fields with default values
  @Test def testDefaultValues3(): Unit = {
    val data = DefaultValuesClass1(float=123)
    val expected = DefaultValuesClass(float=123)
    val decoded: DefaultValuesClass = decode[DefaultValuesClass](encode[DefaultValuesClass1](data))
    val _ = assert(decoded == expected)
  }

  @Test def testClassWithTypeParams1(): Unit = {
    implicit val c = caseCodecAuto[ClassWithTypeParams[String, Int, Float]]
    val data = ClassWithTypeParams[String, Int, Float]("test", 111, 3.14f)
    val decoded = decode[ClassWithTypeParams[String, Int, Float]](encode(data))
    val _ = assert(decoded == data)
  }

  @Test def testClassWithTypeParams2(): Unit = {
    implicit val c: MessageCodec[ClassWithTypeParams[String, Int, List[Int]]] = caseCodecAuto[ClassWithTypeParams[String, Int, List[Int]]]
    val data = ClassWithTypeParams[String, Int, List[Int]]("test", 111, List(1,2,3,4,5))
    val decoded = decode[ClassWithTypeParams[String, Int, List[Int]]](encode(data))
    val _ = assert(decoded == data)
  }

  def test[A:MessageCodec](data: A) = {
    val bytes = encode[A](data)
    val decoded: A = decode(bytes)
    val _ = assert(decoded == data)
  }

  @Test def classCodecNumsTest1(): Unit = {
    implicit val c: MessageCodec[Teleport] = classCodecNums[Teleport]("id"->2, "n"->3)(Teleport.apply(_,_))
    val data = Teleport(id="ID", n=101)
    val decoded: Teleport = decode(encode(data))
    val _ = assert(decoded.id == data.id)
    val _ = assert(decoded.n == data.n)
  }

  @Test def classCodecNumsTest2(): Unit = {
    val c = classCodecNums[SimpleClass[String]]("id"->1)(SimpleClass.init(_))
    val data: SimpleClass[String] = SimpleClass.init(111)
    val decoded: SimpleClass[String] = decode(encode(data)(c))(c)
    val _ = assert(decoded.id == data.id)
    val _ = assert(decoded.id2 == data.id2)
  }

  @Test def classCodecNumsTest3(): Unit = {
    val c = classCodecNums[SimpleClass[String]]("id"->1, "id2"->2)(SimpleClass.init2(_, _))
    val data: SimpleClass[String] = SimpleClass.init2(111, List("a", "b"))
    val decoded: SimpleClass[String] = decode(encode(data)(c))(c)
    val _ = assert(decoded.id == data.id)
    val _ = assert(decoded.id2 == data.id2)
  }

  @Test def classCodecNumsTest4(): Unit = {
    def codecSynthetic[A:MessageCodec] = classCodecNums[SimpleClass[A]]("id"->1, "id2"->2)(SimpleClass.init3[A](_, _))
    val c0: MessageCodec[Message1] = caseCodecAuto[Message1]
    val c: MessageCodec[SimpleClass[Message1]] = codecSynthetic[Message1]
    val data: SimpleClass[Message1] = SimpleClass.init3(101, List(Message1("name", 10d)))
    val decoded: SimpleClass[Message1] = decode(encode(data)(c))(c)
    val _ = assert(decoded.id == data.id)
    val _ = assert(decoded.id2 == data.id2)
  }

  @Test def sealedTraitCodecAutoTest(): Unit = {
    implicit val c1: MessageCodec[Black] = caseCodecAuto[Black]
    implicit val c2: MessageCodec[White] = caseCodecAuto[White]
    implicit val codec: MessageCodec[Color] = sealedTraitCodecAuto[Color]
    test[Color](data=White(value=100))
    test[Color](data=Black(name="black color111", value=33, msg=Message(int=1, str="str", set=Set("1","2"), msg1=None)))
    test[Color](data=Yellow)
    test[Color](data=Red)
  }

  @Test def enumByNameTest(): Unit = {
    implicit val enumCodec1: MessageCodec[Push.Msg] = caseCodecAuto[Push.Msg]
    implicit val enumCodec: MessageCodec[Push] = enumByN[Push]
    def test[A:MessageCodec](data: A) = {
      val bytes = encode[A](data)
      val decoded: A = decode(bytes)
      val _ = assert(decoded == data)
    }
    test[Push](data=Push.Pong)
    test[Push](data=Push.Msg(txt="binary message", id=1001))
  }

}