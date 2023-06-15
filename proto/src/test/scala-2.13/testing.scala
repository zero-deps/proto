package proto

import org.scalatest.freespec.AnyFreeSpec
import proto.macrosapi.{caseCodecIdx, caseCodecNums, caseCodecAuto, sealedTraitCodecAuto, sealedTraitCodecNums, classCodecNums, classCodecAuto}
import scala.collection.immutable.ArraySeq

object models {
  case class Basic(
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

  final case class Collections(
    @N(21) int: List[Int]
  , @N(22) long: List[Long]
  , @N(23) bool: List[Boolean]
  , @N(24) double: List[Double]
  , @N(25) float: List[Float]
  , @N(26) str: List[String]
  , @N(27) bytes: List[Array[Byte]]
  , @N(28) message: List[Vehicle]
  , @N(29) basicBasic: Map[Int, Int]
  , @N(30) basisMessage: Map[String, Vehicle]
  , @N(31) messageMessage: Map[Vehicle, Vehicle]
  , @N(32) messageBasic: Map[Vehicle, String]
  , @N(33) setBasic: Set[Double]
  , @N(50) setMessage: Set[Vehicle]
  , @N(60) array: Array[Int]
  , @N(70) arrayVehicle: Array[Vehicle]
  )

  sealed trait Vehicle
  @N(21) final case class Car(@N(21) id: String) extends Vehicle
  @N(22) final case class Bus(@N(21) id: String) extends Vehicle
  @N(23) final class Teleport(@N(21) val id: String) extends Vehicle
  @N(50) final case object Unknown extends Vehicle

  object Teleport {
    def apply(id: String): Teleport = {
      new Teleport(id)
    }
  }

  final case class Parking(@N(21) place1: Option[Vehicle], @N(22) place2: Option[Vehicle], @N(23) other: List[Vehicle], @N(50) reserved: Vehicle)

  final case class ShoppingMall(@N(21) parking1: Option[Parking], @N(50) parking2: Option[Parking])

  abstract class SimpleClass[A] {
    def id: Int
    def id2: List[A]
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

  final case class ClassWithArray(@N(1) x: Array[Byte])
  final case class ClassWithArraySeq(@N(1) y: ArraySeq[Byte])
  final case class ClassWithBytes(@N(1) z: Bytes)

  final case class DefaultValuesClass(
    @N(1) int: Int = 10
  , @N(2) bool: Boolean = true
  , @N(3) float: Float
  , @N(4) str: String = "NonEmptyString"
  , @N(5) vehicle: Vehicle = Car(id="321")
  )

  final case class DefaultValuesClass1(@N(3) float: Float)

  case class Simple(
    @N(1) id: Int
  , @N(3) name: String
  , @N(4) int_list: List[Int]
  , @N(5) double_list: List[Double]
  )

  case class SimpleWithArray(
    @N(1) id: Int
  , @N(3) name: String
  , @N(4) int_list: Array[Int]
  , @N(5) double_list: Array[Double]
  )
}

class testing extends AnyFreeSpec {
  import models._

  "ArraySeq[Byte] is compatible with Array[Byte]" in {
    import java.util.Arrays
    implicit val ac: MessageCodec[ClassWithArray] = caseCodecAuto[ClassWithArray]
    implicit val bc: MessageCodec[ClassWithArraySeq] = caseCodecAuto[ClassWithArraySeq]
    assert(Arrays.equals(decode[ClassWithArraySeq](encode[ClassWithArray](ClassWithArray(x=Array[Byte](1,2,3)))).y.unsafeArray.asInstanceOf[Array[Byte]], Array[Byte](1,2,3)))
    assert(Arrays.equals(decode[ClassWithArray](encode[ClassWithArraySeq](ClassWithArraySeq(y=ArraySeq.unsafeWrapArray[Byte](Array[Byte](1,2,3))))).x, Array[Byte](1,2,3)))
  }

  "Bytes is compatible with Array[Byte]" in {
    import java.util.Arrays
    implicit val ac: MessageCodec[ClassWithArray] = caseCodecAuto[ClassWithArray]
    implicit val bc: MessageCodec[ClassWithBytes] = caseCodecAuto[ClassWithBytes]
    assert(Arrays.equals(decode[ClassWithBytes](encode[ClassWithArray](ClassWithArray(x=Array[Byte](1,2,3)))).z.unsafeArray, Array[Byte](1,2,3)))
    assert(Arrays.equals(decode[ClassWithArray](encode[ClassWithBytes](ClassWithBytes(z=Bytes.unsafeWrap(Array[Byte](1,2,3))))).x, Array[Byte](1,2,3)))
  }

  "array byte wrapper" - {
    "encode" in {
      implicit val arrayByteWrapperCodec = caseCodecAuto[ClassWithArray]
      val data = ClassWithArray(Array(6, 7, 8, 9, 0))
      val encoded: Array[Byte] = encode(data)
      assert(Array[Byte](10,5, 6,7,8,9,0) === encoded)
    }
  }

  "basic" - {
    def test(implicit codec: MessageCodec[Basic]): Unit = {
      (for {
        int <- List(Int.MinValue, -2, -1, 0, 1, 2, Int.MaxValue)
        long <- List(Long.MinValue, -2L, -1L, 0L, 1L, 2L, Long.MaxValue)
        bool <- List(false, true)
        double <- List(Double.MinValue, -2.0D, -1.0D, 0.0D, 1.0D, 2.0D, Double.MaxValue)
        float <- List(Float.MinValue, -2.0F, -1.0F, 0.0F, 1.0F, 2.0F, Float.MaxValue)
        str <- List("", "str")
        bytes <- List(Array.empty[Byte], Array(0.toByte), Array(1.toByte), Array(2.toByte), Array(255.toByte))
      } yield Basic(int = int, long = long, bool = bool, double = double, float = float, str = str, bytes = bytes)).foreach{ data =>
        val decoded = decode(encode(data))
        assert(decoded.int === data.int)
        assert(decoded.long === data.long)
        assert(decoded.bool === data.bool)
        assert(decoded.double === data.double)
        assert(decoded.float === data.float)
        assert(decoded.str === data.str)
        assert(decoded.bytes === data.bytes)
      }
    }
    "encode <-> decode" - {
      "auto codec" in { implicit val codec = caseCodecAuto[Basic]; test }
      "nums codec" in { implicit val codec = caseCodecNums[Basic]("int"->6, "long"->7, "bool"->8, "double"->9, "float"->10, "str"->20, "bytes"->21); test }
      "idx codec" in { implicit val codec = caseCodecIdx[Basic]; test }
    }
  }

  "option basic" - {
    object autocodec { implicit val codec: MessageCodec[OptionBasic] = caseCodecAuto }
    object numscodec { implicit val codec: MessageCodec[OptionBasic] = caseCodecNums("int"->6, "long"->7, "bool"->8, "double"->9, "float"->10, "str"->20, "bytes"->21) }
    object idxcodec { implicit val codec: MessageCodec[OptionBasic] = caseCodecIdx }

    "empty bytearray <-> all fields none" - {
      val data = OptionBasic(int = None, long = None, bool = None, double = None, float = None, str = None, bytes = None)
      def test(implicit codec: MessageCodec[OptionBasic]): Unit = {
        assert(encode(data) === Array.empty[Byte])
        val decoded = decode[OptionBasic](Array.empty[Byte])
        val _ = assert(decoded === data)
      }
      "auto codec" in { import autocodec._; test }
      "nums codec" in { import numscodec._; test }
      "idx codec" in { import idxcodec._; test }
    }

    "bytearray <-> all fields" - {
      val data = OptionBasic(int = Some(1), long = Some(2L), bool = Some(true), double = Some(3.0D), float = Some(4.0F), str = Some("5"), bytes = Some(Array(6, 7, 8, 9, 0)))
      "auto codec" in {
        import autocodec._

        val bytes = Array[Byte](168.toByte,1,1, 176.toByte,1,2, 184.toByte,1,1, 193.toByte,1,0,0,0,0,0,0,8,64, 205.toByte,1,0,0,128.toByte,64, 210.toByte,1,1,53, 146.toByte,3,5,6,7,8,9,0)
        assert(encode(data) === bytes)
        val decoded = decode[OptionBasic](bytes)
        assert(decoded.int.get === data.int.get)
        assert(decoded.long.get === data.long.get)
        assert(decoded.bool.get === data.bool.get)
        assert(decoded.double.get === data.double.get)
        assert(decoded.float.get === data.float.get)
        assert(decoded.str.get === data.str.get)
        assert(decoded.bytes.get === data.bytes.get)
      }
      "nums codec" in { 
        import numscodec._
        val bytes = Array[Byte](48,1, 56,2, 64,1, 73,0,0,0,0,0,0,8,64, 85,0,0,128.toByte,64, 162.toByte,1,1,53, 170.toByte,1,5,6,7,8,9,0)
        assert(encode(data) === bytes)
        val decoded = decode[OptionBasic](bytes)
        assert(decoded.int.get === data.int.get)
        assert(decoded.long.get === data.long.get)
        assert(decoded.bool.get === data.bool.get)
        assert(decoded.double.get === data.double.get)
        assert(decoded.float.get === data.float.get)
        assert(decoded.str.get === data.str.get)
        assert(decoded.bytes.get === data.bytes.get)
      }
      "idx codec" in {
        import idxcodec._
        val bytes = Array[Byte](8,1, 16,2, 24,1, 33,0,0,0,0,0,0,8,64, 45,0,0,128.toByte,64, 50,1,53, 58,5,6,7,8,9,0)
        assert(encode(data) === bytes)
        val decoded = decode[OptionBasic](bytes)
        assert(decoded.int.get === data.int.get)
        assert(decoded.long.get === data.long.get)
        assert(decoded.bool.get === data.bool.get)
        assert(decoded.double.get === data.double.get)
        assert(decoded.float.get === data.float.get)
        assert(decoded.str.get === data.str.get)
        assert(decoded.bytes.get === data.bytes.get)
      }
    }
  }

  object messages {
    object autocodec {
      implicit val vehicleCodec: MessageCodec[Vehicle] = {
        implicit val carCodec = caseCodecAuto[Car]
        implicit val busCodec = caseCodecAuto[Bus]
        implicit val teleportCodec: MessageCodec[Teleport] = classCodecAuto
        implicit val unknownCodec: MessageCodec[Unknown.type] = caseCodecAuto
        sealedTraitCodecAuto[Vehicle]
      }
      implicit val parkingCodec: MessageCodec[Parking] = caseCodecAuto
      implicit val shoppingMallCodec: MessageCodec[ShoppingMall] = caseCodecAuto
    }
    object numscodec {
      implicit val vehicleCodec: MessageCodec[Vehicle] = {
        implicit val carCodec = caseCodecNums[Car]("id"->2)
        implicit val busCodec = caseCodecNums[Bus]("id"->2)
        implicit val teleportCodec = classCodecNums[Teleport]("id"->2)(Teleport.apply(_))
        implicit val unknownCodec = caseCodecNums[Unknown.type]()
        sealedTraitCodecNums[Vehicle]("Car"->10, "Bus"->22, "Teleport"->23, "Unknown"->51)
      }
      implicit val parkingCodec: MessageCodec[Parking] = caseCodecNums[Parking]("place1"->1, "place2"->2, "other"->3, "reserved"->4)
      implicit val shoppingMallCodec: MessageCodec[ShoppingMall] = caseCodecNums("parking1"->1, "parking2"->2)
    }
    object idxcodec {
      implicit val vehicleCodec: MessageCodec[Vehicle] = {
        implicit val carCodec = caseCodecIdx[Car]
        implicit val busCodec = caseCodecIdx[Bus]
        implicit val teleportCodec = classCodecNums[Teleport]("id"->2)(Teleport.apply(_))
        implicit val unknownCodec = caseCodecIdx[Unknown.type]
        sealedTraitCodecNums[Vehicle]("Car"->10, "Bus"->22, "Teleport"->23, "Unknown"->51)
      }
      implicit val parkingCodec: MessageCodec[Parking] = caseCodecIdx
      implicit val shoppingMallCodec: MessageCodec[ShoppingMall] = caseCodecIdx
    }
  }

  object collections {
    object autocodec {
      import messages.autocodec._
      implicit val tuple2IntInt: MessageCodec[(Int, Int)] = caseCodecIdx[Tuple2[Int, Int]]
      implicit def tuple2StringA[A:MessageCodec]: MessageCodec[(String, A)] = caseCodecIdx[Tuple2[String, A]]
      implicit def tuple2AString[A:MessageCodec]: MessageCodec[(A, String)] = caseCodecIdx[Tuple2[A, String]]
      implicit def tuple2AB[A:MessageCodec, B:MessageCodec]: MessageCodec[(A, B)] = caseCodecIdx[Tuple2[A, B]]
      implicit val collectionCodec: MessageCodec[Collections] = caseCodecAuto[Collections]
    }
    object numscodec {
      import messages.numscodec._
      implicit val tuple2IntInt: MessageCodec[(Int, Int)] = caseCodecNums[Tuple2[Int, Int]]("_1"->1, "_2"->2)
      implicit def tuple2StringA[A:MessageCodec]: MessageCodec[(String, A)] = caseCodecNums[Tuple2[String, A]]("_1"->1, "_2"->2)
      implicit def tuple2AString[A:MessageCodec]: MessageCodec[(A, String)] = caseCodecNums[Tuple2[A, String]]("_1"->1, "_2"->2)
      implicit def tuple2AB[A:MessageCodec, B:MessageCodec]: MessageCodec[(A, B)] = caseCodecNums[Tuple2[A, B]]("_1"->1, "_2"->2)
      implicit val collectionCodec: MessageCodec[Collections] = caseCodecNums[Collections](
        "int"->7
      , "long"->8
      , "bool"->9
      , "double"->10
      , "float"->11
      , "str"->12
      , "bytes"->13
      , "message"->14
      , "basicBasic"->15
      , "basisMessage"->16
      , "messageMessage"->17
      , "messageBasic"->18
      , "setBasic"->19
      , "setMessage"->20
      , "array"->21
      , "arrayVehicle"->22
      )
    }
    object idxcodec {
      import messages.idxcodec._
      implicit val tuple2IntInt: MessageCodec[Tuple2[Int, Int]] = caseCodecIdx
      implicit def tuple2StringA[A:MessageCodec]: MessageCodec[(String, A)] = caseCodecIdx[Tuple2[String, A]]
      implicit def tuple2AString[A:MessageCodec]: MessageCodec[(A, String)] = caseCodecIdx[Tuple2[A, String]]
      implicit def tuple2AB[A:MessageCodec, B:MessageCodec]: MessageCodec[(A, B)] = caseCodecIdx[Tuple2[A, B]]
      implicit val collectionCodec: MessageCodec[Collections] = caseCodecIdx[Collections]
    }
  }

  "collections" - {
    def test(implicit codec: MessageCodec[Collections]): Unit = {
      import java.util.Arrays
      val int = List(Int.MinValue, -2, -1, 0, 1, 2, Int.MaxValue)
      val long = List(Long.MinValue, -2L, -1L, 0L, 1L, 2L, Long.MaxValue)
      val bool = List(false, true)
      val double = List(Double.MinValue, -2.0D, -1.0D, 0.0D, 1.0D, 2.0D, Double.MaxValue)
      val float = List(Float.MinValue, -2.0F, -1.0F, 0.0F, 1.0F, 2.0F, Float.MaxValue)
      val str = List("", "str")
      val bytes = List(Array.empty[Byte], Array(0.toByte), Array(1.toByte), Array(2.toByte), Array(255.toByte))
      val message = List(Car(id = "123"), Bus(id = "456"), Unknown, Car(id = "789"))
      val basicBasic: Map[Int, Int] = Map(1->2, 2->3, 4->5)
      val basisMessage: Map[String, Vehicle] = Map("1"->Car(id="1"), "2"->Bus(id="2"), "3"->Unknown)
      val messageMessage: Map[Vehicle, Vehicle] = Map(Bus(id="1")->Unknown, Bus(id="2")->Bus(id="3"))
      val messageBasic: Map[Vehicle, String] = Map(Bus(id="1")->"123", Car("2")->"456")
      val setBasic: Set[Double] = Set(Double.MinValue, -2.0D, -1.1D, 0.0D, 1.1D, 2.0D, Double.MaxValue)
      val setMessage: Set[Vehicle] = Set(Car(id="1"), Bus(id="2"), Unknown)
      val array: Array[Int] = Array(1, 3, 5, 7, 9, 12345)
      val arrayVehicle: Array[Vehicle] = Array(Car(id="1"), Bus(id="2"), Unknown)
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
      , basisMessage = basisMessage
      , messageMessage = messageMessage
      , messageBasic = messageBasic
      , setBasic = setBasic
      , setMessage = setMessage
      , array = array
      , arrayVehicle = arrayVehicle
      )
      val encoded = encode(data)
      val decoded = decode(encoded)
      assert(decoded.int === data.int)
      assert(decoded.long === data.long)
      assert(decoded.bool === data.bool)
      assert(decoded.double === data.double)
      assert(decoded.float === data.float)
      assert(decoded.str === data.str)
      assert(decoded.message === data.message)
      assert(decoded.basicBasic === data.basicBasic)
      assert(decoded.messageMessage === data.messageMessage)
      assert(decoded.messageBasic === data.messageBasic)
      assert(decoded.setBasic === data.setBasic)
      assert(decoded.setMessage === data.setMessage)
      assert(Arrays.equals(decoded.array, data.array))
      assert(decoded.arrayVehicle.sameElements(data.arrayVehicle))
      val _ = assert(decoded.bytes.zip(data.bytes).forall{ case (decodedBytes, dataBytes) => decodedBytes === dataBytes })
    }
    "encode <-> decode" - {
      "auto codec" in { import collections.autocodec._; test }
      "nums codec" in { import collections.numscodec._; test }
      "idx codec" in { import collections.idxcodec._; test }
    }
  }

  "message" - {
    "encode <-> decode" - {
      def test(implicit codec: MessageCodec[ShoppingMall]): Unit = {
        val parking = Parking(place1=None, place2=Some(Car(id="123")), other=List(Car(id="456"), Bus(id="789"), Unknown), reserved=Car(id="0"))
        val data = ShoppingMall(parking1=None, parking2=Some(parking))
        val decoded = decode[ShoppingMall](encode(data))
        val _ = assert(decoded === data)
      }
      "auto codec" in { import messages.autocodec._; test }
      "nums codec" in { import messages.numscodec._; test }
      "idx codec" in { import messages.idxcodec._; test }
    }
  }

  object custom {
    val codecSynthetic1 = classCodecNums[SimpleClass[String]]("id"->1)(SimpleClass.init(_))
    val codecSynthetic2 = classCodecNums[SimpleClass[String]]("id"->1, "id2"->2)(SimpleClass.init2(_, _))
    def codecSynthetic3[A:MessageCodec] = classCodecNums[SimpleClass[A]]("id"->1, "id2"->2)(SimpleClass.init3[A](_, _))

    val codecAnonymous1 = classCodecNums[SimpleClass[String]]("id"->1)((id: Int) => SimpleClass.init(id))
    val codecAnonymous2 = classCodecNums[SimpleClass[String]]("id"->1, "id2"->2)((id: Int, id2: List[String]) => SimpleClass.init2(id, id2))
    def codecAnonymous3[A:MessageCodec] = classCodecNums[SimpleClass[A]]("id"->1, "id2"->2)((id: Int, id2: List[A]) => SimpleClass.init3(id, id2))
  }

  "custom class" - {
    "synthetic function 1 arg" in {
      implicit val codec = custom.codecSynthetic1
      val data = SimpleClass.init(123456789)
      val decoded = decode(encode(data))
      assert(data.id === decoded.id)  
      val _ = assert(data.id2 === decoded.id2)  
    }

    "synthetic function 2 args" in {
      implicit val codec = custom.codecSynthetic2
      val data = SimpleClass.init2(123456789, List("987654321"))
      val decoded = decode(encode(data))
      assert(data.id === decoded.id)  
      val _ = assert(data.id2 === decoded.id2)
    }

    "synthetic function 2 with generic" in {
      import messages.numscodec._
      implicit def codec[A:MessageCodec]: MessageCodec[SimpleClass[A]] = custom.codecSynthetic3[A]
      val data: SimpleClass[Vehicle] = SimpleClass.init3(987654321, List(Bus(id="123"), Unknown))
      val decoded: SimpleClass[Vehicle] = decode[SimpleClass[Vehicle]](encode(data))
      assert(data.id === decoded.id)
      val _ = assert(data.id2 === decoded.id2)
    }

    "anonymous function 1 arg" in {
      implicit val codec = custom.codecAnonymous1
      val data = SimpleClass.init(987654321)
      val decoded = decode(encode(data))
      assert(data.id === decoded.id)
      val _ = assert(data.id2 === decoded.id2)
    }

    "anonymous function 2 args" in {
      implicit val codec = custom.codecAnonymous2
      val data = SimpleClass.init2(987654321, List("123456789"))
      val decoded = decode(encode(data))
      assert(data.id === decoded.id)
      val _ = assert(data.id2 === decoded.id2)
    }

    "anonymous function 2 with generic" in {
      import messages.numscodec._
      implicit def codec[A:MessageCodec]: MessageCodec[SimpleClass[A]] = custom.codecAnonymous3[A]
      val data: SimpleClass[Vehicle] = SimpleClass.init3(987654321, List(Bus(id="123"), Unknown))
      val decoded: SimpleClass[Vehicle] = decode[SimpleClass[Vehicle]](encode(data))
      assert(data.id === decoded.id)
      val _ = assert(data.id2 === decoded.id2)
    }
  }

  object defaultValues {
    object autocodec {
      import messages.autocodec.vehicleCodec
      implicit val DefaultValuesClassCodec: MessageCodec[DefaultValuesClass]  = caseCodecAuto
      implicit val DefaultValuesClass1Codec: MessageCodec[DefaultValuesClass1] = caseCodecAuto
    }
    object numscodec {
      import messages.numscodec.vehicleCodec
      implicit val DefaultValuesClassCodec: MessageCodec[DefaultValuesClass] = caseCodecNums("int"->2,"bool"->3,"float"->4,"str"->5,"vehicle"->6)
    }
    object idxcodec {
      import messages.idxcodec.vehicleCodec
      implicit val DefaultValuesClassCodec: MessageCodec[DefaultValuesClass] = caseCodecIdx
    }
  }

  "default values" - {
    "encode <-> decode" - {
      def testAllParamsPassed(implicit codec: MessageCodec[DefaultValuesClass]): Unit = {
        val data = DefaultValuesClass(
            int=23
          , bool=false
          , float=33
          , str="hello"
          , vehicle=Bus("33")
        )
        val decoded = decode[DefaultValuesClass](encode(data))
        val _ = assert(decoded === data)
      }
       def testNoneParamsPassed(implicit codec: MessageCodec[DefaultValuesClass]): Unit = {
        val data = DefaultValuesClass(
            float=33
        )
        val decoded = decode[DefaultValuesClass](encode(data))
        val _ = assert(decoded === data)
      }
      "auto codec (all params passed)" in { import defaultValues.autocodec._; testAllParamsPassed }
      "auto codec (none params passed)" in { import defaultValues.autocodec._; testNoneParamsPassed }
      "numscodec (all params passed)" in { import defaultValues.numscodec._; testAllParamsPassed }
      "numscodec (none params passed)" in { import defaultValues.numscodec._; testNoneParamsPassed }
      "idxcodec (all params passed)" in { import defaultValues.idxcodec._; testAllParamsPassed }
      "idxcodec (none params passed)" in { import defaultValues.idxcodec._; testNoneParamsPassed }
    }
    "new fields with default values" in {
      import defaultValues.autocodec._
      val data = DefaultValuesClass1(float=123)
      val expected = DefaultValuesClass(float=123)
      val decoded: DefaultValuesClass = decode[DefaultValuesClass](encode[DefaultValuesClass1](data))
      val _ = assert(decoded === expected)
    }
  }

  "decode non-packed format of repeated primitives" in {
    import java.util.Arrays
    implicit val codec1: MessageCodec[Simple] = caseCodecAuto[Simple]
    implicit val codec2: MessageCodec[SimpleWithArray] = caseCodecAuto[SimpleWithArray]

    val reference_list = Simple(42,"simple",List(1, 2, 3, 4, 5, 6),List(10.0, 20.0, 30.0))
    val reference_array = SimpleWithArray(42,"simple",Array(1, 2, 3, 4, 5, 6),Array(10.0, 20.0, 30.0))
    val nonPackedBytes: Array[Byte] = Array(8, 42, 26, 6, 115, 105, 109, 112, 108, 101, 32, 1, 32, 2, 32, 3, 32, 4, 32, 5, 32, 6, 41, 0, 0, 0, 0, 0, 0, 36, 64, 41, 0, 0, 0, 0, 0, 0, 52, 64, 41, 0, 0, 0, 0, 0, 0, 62, 64)
    val packedBytes: Array[Byte] = Array(8, 42, 26, 6, 115, 105, 109, 112, 108, 101, 34, 6, 1, 2, 3, 4, 5, 6, 42, 24, 0, 0, 0, 0, 0, 0, 36, 64, 0, 0, 0, 0, 0, 0, 52, 64, 0, 0, 0, 0, 0, 0, 62, 64)

    val message1: Simple = decode[Simple](nonPackedBytes)
    val message2: Simple = decode[Simple](packedBytes)

    val message3: SimpleWithArray = decode[SimpleWithArray](nonPackedBytes)
    val message4: SimpleWithArray = decode[SimpleWithArray](packedBytes)

    assert(message1 == reference_list)
    assert(message2 == reference_list)

    assert(message3.int_list.sameElements(reference_array.int_list))
    assert(message4.double_list.sameElements(reference_array.double_list))


    val encoded: Array[Byte] = encode(reference_list)
    assert(Arrays.equals(packedBytes, encoded))

    val encodedArrays: Array[Byte] = encode(reference_array)
    assert(Arrays.equals(packedBytes, encodedArrays))
  }
}
