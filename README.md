# protobuf-scala-macros

Lightweight, high performance, fast serialization library for scala based on Protocol Buffers (protobuf) without proto files

# Motivation

Serialization library for Scala that can be used either for long term store models and short term models.
With easy to migrate possibility.

- lightweight
- fast
- Protocol Buffers compatible
- no proto files
- no model convertation
- typesafe
- serialization/deserialization without changes in models
- possibility to use specific types in model

# Benchmark

JMH framework used for benchmark.

Encode binary model:

Library                 | Mode  | Cnt |           Score |   Error (±) | Units
----------------------- | ----- | --- | ---------------:| -----------:| -----
Chill                   | thrpt | 10  | `1'430'215.034` | `25633.918` | ops/s
Jackson                 | thrpt | 10  |   `386'583.021` |  `8171.466` | ops/s
Java                    | thrpt | 10  |   `309'812.676` |  `3264.095` | ops/s
Jsoniter                | thrpt | 10  |   `344'249.777` |  `2115.862` | ops/s
_Protobuf Scala Macros_ | thrpt | 10  | `3'513'769.379` | `87134.627` | ops/s
Scalapb                 | thrpt | 10  | `3'365'452.452` | `91954.323` | ops/s
Scodec                  | thrpt | 10  |   `538'439.124` |  `4053.778` | ops/s

Decode binary model:

Library                 | Mode  | Cnt |           Score |   Error (±) | Units
----------------------- | ----- | --- | ---------------:| -----------:| -----
Chill                   | thrpt | 10  | `1'811'975.200` | `24817.175` | ops/s
Jackson                 | thrpt | 10  |   `258'545.215` |  `1915.428` | ops/s
Java                    | thrpt | 10  |    `60'502.636` |  `2054.997` | ops/s
Jsoniter                | thrpt | 10  |   `181'508.082` |   `798.176` | ops/s
_Protobuf Scala Macros_ | thrpt | 10  | `3'344'461.999` | `27551.737` | ops/s
Scalapb                 | thrpt | 10  | `3'565'243.532` | `39629.749` | ops/s
Scodec                  | thrpt | 10  |   `538'439.124` |  `4053.778` | ops/s

to run benchmark:
```
> project benchmark
> jmh:run -i 10 -wi 10 -f1 -t1
```

# Install

Add dependency:
```
resolvers += Resolver.jcenterRepo
libraryDependencies += "io.github.zero-deps" %% "proto-macros" % 1.1.3 % Compile
libraryDependencies += "io.github.zero-deps" %% "proto-runtime" % 1.1.3
```

# Usage

You can pick one of the way how to difine field number:
- with annotation `@zd.proto.api.N` and use `caseCodecAuto`
- explicitly specify nums `caseCodecNums('field1->1, 'field2->2)`
- field numbers by index `caseCodecIdx`

```scala
import scala.collection.immutable.TreeMap
import zd.proto.api.{encode, decode, N}
import zd.proto.macrosapi.{caseCodecIdx, caseCodecNums, caseCodecAuto}

final case class VectorClock(versions: TreeMap[String, Long])
final case class Equipment(@N(1) id: String, @N(2) tpe: String)
final case class Car(id: String, color: Int, equipment: List[Equipment], vc: VectorClock)

implicit val tuple2Codec = caseCodecIdx[Tuple2[String, Long]] //codec for TreeMap[String, Long]

implicit val vectorClockCodec = caseCodecIdx[VectorClock]
implicit val equipmentCodec = caseCodecAuto[Equipment]
implicit val carCodec = caseCodecNums[Car]('id->1, 'color->4, 'equipment->2, 'vc->3)

val vc = VectorClock(versions=TreeMap.empty)
val equipment = List(Equipment(id="1", tpe="123"), Equipment(id="2", tpe="456"))
val car = Car(id="1", color=16416882, equipment=equipment, vc=vc)
//encode
val bytes: Array[Byte] = encode(car)
//decode
val car2: Car = decode[Car](bytes)
```

More examples in testing.scala
