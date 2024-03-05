# proto

[![Scaladex](https://index.scala-lang.org/zero-deps/proto/proto/latest-by-scala-version.svg)](https://index.scala-lang.org/zero-deps/proto/proto)

Lightweight and fast serialization library for Scala 2/3 based on Protocol Buffers with macros magic.

## Motivation

Serialization library for Scala that can be used either for long term store models and short term models.
With easy to migrate possibility.

- Lightweight
- Fast
- Protocol Buffers compatible
- No .proto files
- No model convertation
- Type safe
- Serialization/deserialization without changes in models
- Possibility to use specific types in model

## Install

Add dependency:
```
libraryDependencies += "io.github.zero-deps" %% "proto" % "latest.integration"
```

Dependency as a git-submodule is also supported.

## Benchmark #1

data |        | library        | scala-2        | scala-3
---- | ------ |:-------------- | -------------: | ------------:
data | decode | java           | `   44079.139` | `  38979.697`
data | decode | jackson        | `  176941.468` | ` 188555.562`
data | decode | jsoniter-scala | `  483788.001` | no support for Scala 3
data | decode | boopickle      | ` 2885610.648` | no support for Scala 3
data | decode | proto          | ` 3383845.458` | `3776688.591`
data | decode | scalapb        | ` 3270691.564` | `3893847.420`

data |        | library        | scala-2        | scala-3
---- | ------ |:-------------- | -------------: | ------------:
data | encode | java           | `  220444.268` | ` 217484.396`
data | encode | jackson        | `  431318.803` | ` 384863.249`
data | encode | jsoniter-scala | ` 1054650.233` | no support for Scala 3
data | encode | boopickle      | ` 1520834.519` | no support for Scala 3
data | encode | proto          | ` 3186951.441` | `2965427.382`
data | encode | scalapb        | ` 3628779.864` | `3972905.402`

data |        | library        | scala-2        | scala-3
---- | ------ |:-------------- | -------------: | ------------:
msg  | decode | jsoniter-scala | ` 3486552.303` | no support for Scala 3
msg  | decode | proto          | ` 5825174.170` | `6395557.251`
msg  | decode | scalapb        | ` 4898257.671` | `6902064.854`

data |        | library        | scala-2        | scala-3
---- | ------ |:-------------- | -------------: | ------------:
msg  | encode | jsoniter-scala | ` 6372602.760` | no support for Scala 3
msg  | encode | proto          | ` 6487748.959` | `6745673.393`
msg  | encode | scalapb        | ` 9202135.451` | `9056962.541`

### environment

Intel(R) Core(TM) i7-7700HQ CPU @ 2.80GHz\
16 GB 2133 MHz LPDDR3\
Java 15

### run benchmark

```bash
sbt
project bench
++ 3.0.0
jmh:run -i 2 -wi 1 -f1 -t1
```

## Benchmark #2

data |        | library        | scala-3
---- | ------ |:-------------- | -------------:
data | decode | java           | `   92130,460`
data | decode | jackson        | `  517036,354`
data | decode | proto          | ` 6716619,956`

data |        | library        | scala-3
---- | ------ |:-------------- | -------------:
data | encode | java           | `  537462,511`
data | encode | jackson        | `  882065,311`
data | encode | proto          | ` 9380874,587`

data |        | library        | scala-3
---- | ------ |:-------------- | -------------:
msg  | decode | proto          | `11733555,275`

data |        | library        | scala-3
---- | ------ |:-------------- | -------------:
msg  | encode | proto          | `18486833,582`

### environment

Apple M1\
16 GB\
Java 21

### run benchmark

```bash
sbt
project bench
++ 3.0.0
jmh:run -i 2 -wi 1 -f1 -t1
```

## Usage

You can pick one of the way how to define field number:
- with annotation `@proto.N` and use `caseCodecAuto`
- explicitly specify nums `caseCodecNums(Symbol("field1")->1, Symbol("field2")->2)`
- field numbers by index `caseCodecIdx`

You can use annotation `@proto.RestrictedN` to restrict usage of specified field numbers. Can be used with classes or traits.

```scala
import scala.collection.immutable.TreeMap
import proto.{encode, decode, N}
import proto.{caseCodecIdx, caseCodecNums, caseCodecAuto}

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

## Publishing

```
sbt +publishSigned
open https://oss.sonatype.org/#stagingRepositories
```
