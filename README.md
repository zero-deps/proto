# protobuf-scala-macros

Lightweight, high performance, fast serialization library for scala based on Protocol Buffers (protobuf) without proto files.

[![Bintray](https://img.shields.io/bintray/v/zero-deps/maven/proto-macros.svg?label=macros)](https://bintray.com/zero-deps/maven/proto-macros/_latestVersion)
[![Bintray](https://img.shields.io/bintray/v/zero-deps/maven/proto-runtime.svg?label=runtime)](https://bintray.com/zero-deps/maven/proto-runtime/_latestVersion)
[![Bintray](https://img.shields.io/bintray/v/zero-deps/maven/proto-purs.svg?label=purs)](https://bintray.com/zero-deps/maven/proto-purs/_latestVersion)

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
libraryDependencies += "io.github.zero-deps" %% "proto-macros" % "latest.integration" % Compile
libraryDependencies += "io.github.zero-deps" %% "proto-runtime" % "latest.integration"
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

More examples in [testing.scala](src/test/scala/testing.scala)

## Purescript

There is an option to generate Purescript code to decode/encode protobuf bytes. Code is generated based on same Scala models. Essentially Scala code replaces .proto files. But because Scala code is part of sources Purescript should be generated at moment of runtime using reflection. Macros is not suitable because their purpose is to manipulate with AST and not to generate anything but code tree.

1. Add dependency `libraryDependencies += "io.github.zero-deps" %% "proto-purs" % "latest.integration"`
1. Add resolver `resolvers += Resolver.jcenterRepo`
1. Add "purescript-protobuf": "https://github.com/zero-deps/purescript-protobuf.git" to "dependencies" section in bower.json
1. Run method `zd.proto.Purescript.generate[D, E](moduleName="N")` where `D`/`E` are types of your base trait for decode/encode and `N` is name to put at beginning of file. Method return string which you can save to file or print.

### Test

```bash
sbt 'project purs' test
cd purs/test
npm i
npm run dep
npm run com
npm run tes
```

