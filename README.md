# protobuf-scala-macros

Lightweight, high performance, fast serialization library for scala based on Protocol Buffers (protobuf) without proto files.

## Latest Version

[proto-macros](https://bintray.com/zero-deps/maven/proto-macros/_latestVersion)
[proto-runtime](https://bintray.com/zero-deps/maven/proto-runtime/_latestVersion)
[proto-purs](https://bintray.com/zero-deps/maven/proto-purs/_latestVersion)

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

## Data Access Object (Data)

### Encode

Library                 | Cnt |       Score | Error (±) | Units
----------------------- | --- | -----------:| ---------:| -----
**JSON**                                                |
Jackson                 | 2/5 |   `501'953` |    `8368` | ops/s
Jsoniter                | 2/5 |   `621'852` |   `23367` | ops/s
**Binary**                                              |
Java                    | 2/5 |   `250'822` |    `6650` | ops/s
Boopickle               | 2/5 | `1'292'133` |   `60921` | ops/s
Kryo Macros             | 2/5 |   `823'570` |   `26575` | ops/s
Scalapb                 | 2/5 | `3'696'083` |  `124840` | ops/s
_Protobuf Scala Macros_ | 2/5 | `3'393'026` |  `234068` | ops/s

### Decode

Library                 | Cnt |       Score | Error (±) | Units
----------------------- | --- | -----------:| ---------:| -----
**JSON**                                                |
Jackson                 | 2/5 |   `201'021` |    `3191` | ops/s
Jsoniter                | 2/5 |   `428'333` |   `74834` | ops/s
**Binary**                                              |
Java                    | 2/5 |    `39'634` |    `3758` | ops/s
Boopickle               | 2/5 | `2'801'239` |  `125618` | ops/s
Kryo Macros             | 2/5 |      failed |    failed | ops/s
Scalapb                 | 2/5 | `4'034'554` |  `113449` | ops/s
_Protobuf Scala Macros_ | 2/5 | `3'816'776` |  `429360` | ops/s

## Data Transfer Object (Msg)

### Encode

Library                 | Cnt |       Score | Error (±) | Units
----------------------- | --- | -----------:| ---------:| -----
**JSON**                                                |
Jsoniter                | 2/5 | `8'341'638` |  `172897` | ops/s
**Binary**                                              |
Scalapb                 | 2/5 | `6'822'814` |  `213144` | ops/s
_Protobuf Scala Macros_ | 2/5 | `7'688'959` |   `80381` | ops/s

### Decode

Library                 | Cnt |       Score | Error (±) | Units
----------------------- | --- | -----------:| ---------:| -----
**JSON**                                                |
Jsoniter                | 2/5 | `4'246'303` |  `195804` | ops/s
**Binary**                                              |
Scalapb                 | 2/5 | `6'556'764` |  `891429` | ops/s
_Protobuf Scala Macros_ | 2/5 | `6'190'147` |   `97743` | ops/s

## Environment

2.8 GHz Quad-Core Intel Core i7 \
JDK 13, OpenJDK 64-Bit Server VM, 13+33

## Run Benchmark

```bash
sbt
project benchmark
jmh:run -i 5 -wi 2 -f1 -t1
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
