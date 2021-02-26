# proto

Lightweight and fast serialization library for Scala 2/3 based on Protocol Buffers with macros magic.

# Motivation

Serialization library for Scala that can be used either for long term store models and short term models.
With easy to migrate possibility.

- Lightweight
- Fast
- Protocol Buffers compatible
- No .proto files
- No model convertation
- Typesafe
- Serialization/deserialization without changes in models
- Possibility to use specific types in model

# Benchmark

data |        | library        | score
---- | ------ |:-------------- | -------------:
data | decode | java           | `   44079.139`
data | decode | jackson        | `  176941.468`
data | decode | jsoniter-scala | `  483788.001`
data | decode | boopickle      | ` 2885610.648`
data | decode | scalapb        | ` 3270691.564`
data | decode | proto          | ` 3383845.458`

data |        | library        | score
---- | ------ |:-------------- | -------------:
data | encode | java           | `  220444.268`
data | encode | jackson        | `  431318.803`
data | encode | kryo-macros    | `  991944.758`
data | encode | jsoniter-scala | ` 1054650.233`
data | encode | boopickle      | ` 1520834.519`
data | encode | proto          | ` 3186951.441`
data | encode | scalapb        | ` 3628779.864`

data |        | library        | score
---- | ------ |:-------------- | -------------:
msg  | decode | jsoniter-scala | ` 3486552.303`
msg  | decode | scalapb        | ` 4898257.671`
msg  | decode | proto          | ` 5825174.170`

data |        | library        | score
---- | ------ |:-------------- | -------------:
msg  | encode | jsoniter-scala | ` 6372602.760`
msg  | encode | proto          | ` 6487748.959`
msg  | encode | scalapb        | ` 9202135.451`

## Environment

2.8 GHz Quadro-Core Intel Core i7\
16 GB 2133 MHz LPDDR3\
Java 15.0.1\
Scala 2.13.5

## Run Benchmark

```bash
sbt
project benchmark
jmh:run -i 2 -wi 1 -f1 -t1
```

# Install

Add dependency:
```
libraryDependencies += "io.github.zero-deps" %% "proto-macros" % "latest.integration" % Provided
libraryDependencies += "io.github.zero-deps" %% "proto-runtime" % "latest.integration"
```

Dependency as a git-submodule is also supported.

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
