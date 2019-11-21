# protobuf-scala-macros

Lightweight, high performance, fast serialization library for scala based on Protocol Buffers (protobuf) without proto files.

## Latest Version

[proto-macros](https://bintray.com/zero-deps/maven/proto-macros/_latestVersion)
[proto-runtime](https://bintray.com/zero-deps/maven/proto-runtime/_latestVersion)

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

msg  |        | library                | score
---- | ------ | ---------------------- | -------------:
data | decode | java_                  | `   19073.590`
data | decode | jackson                | `  104793.164`
data | decode | jsoniter_scala         | `  250291.760`
data | decode | boopickle_             | ` 1511503.588`
data | decode | protobuf_scala_macros  | ` 1841584.774`
data | decode | scalapb                | ` 2191344.254`

msg  |        | library                | score
---- | ------ | ---------------------- | -------------:
data | encode | java_                  | `  115404.671`
data | encode | jackson                | `  239388.147`
data | encode | kryo_macros            | `  398249.642`
data | encode | jsoniter_scala         | `  564752.678`
data | encode | boopickle_             | `  672294.937`
data | encode | protobuf_scala_macros  | ` 1760090.157`
data | encode | scalapb                | ` 2022918.710`

msg  |        | library                | score
---- | ------ | ---------------------- | -------------:
msg  | decode | jsoniter_scala         | ` 2442577.755`
msg  | decode | protobuf_scala_macros  | ` 2786830.341`
msg  | decode | scalapb                | ` 2914400.904`

msg  |        | library                | score
---- | ------ | ---------------------- | -------------:s
msg  | encode | jsoniter_scala         | ` 3601656.904`
msg  | encode | protobuf_scala_macros  | ` 3736006.117`
msg  | encode | scalapb                | ` 4464953.270`

## Environment

1.1 GHz Dual-Core Intel Core m3 \
8 GB 1867 MHz LPDDR3 \
JDK 13, OpenJDK 64-Bit Server VM, 13+33

## Run Benchmark

```bash
sbt
project benchmark
jmh:run -i 2 -wi 1 -f1 -t1
```

# Install

Add dependency:
```
resolvers += Resolver.jcenterRepo
libraryDependencies += "io.github.zero-deps" %% "proto-macros" % "latest.integration" % Provided
libraryDependencies += "io.github.zero-deps" %% "proto-runtime" % "latest.integration"
```

# Usage

You can pick one of the way how to define field number:
- with annotation `@zd.proto.api.N` and use `caseCodecAuto`
- explicitly specify nums `caseCodecNums('field1->1, 'field2->2)`
- field numbers by index `caseCodecIdx`

You can use annotation `@zd.proto.api.RestrictedN` to restrict usage of specified field numbers. Can be used with classes or traits.

```scala
import scala.collection.immutable.TreeMap
import zd.proto.api.{encode, decode, N}
import zd.proto.macrosapi.{caseCodecIdx, caseCodecNums, caseCodecAuto}

final case class VectorClock(versions: TreeMap[String, Long])
@RestrictedN(3,4)
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
