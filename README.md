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
Jackson                 | 2/5 |   `284'961` |   `17967` | ops/s
Jsoniter                | 2/5 |   `336'422` |    `9952` | ops/s
**Binary**                                              |
Java                    | 2/5 |   `138'784` |   `14398` | ops/s
Boopickle               | 2/5 |   `814'452` |  `239226` | ops/s
Kryo Macros             | 2/5 |   `499'809` |   `55629` | ops/s
Scalapb                 | 2/5 | `2'442'147` |  `307072` | ops/s
_Protobuf Scala Macros_ | 2/5 | `2'188'548` |  `731245` | ops/s

### Decode

Library                 | Cnt |       Score | Error (±) | Units
----------------------- | --- | -----------:| ---------:| -----
**JSON**                                                |
Jackson                 | 2/5 |   `126'717` |    `1146` | ops/s
Jsoniter                | 2/5 |   `128'983` |    `8813` | ops/s
**Binary**                                              |
Java                    | 2/5 |    `26'035` |    `2259` | ops/s
Boopickle               | 2/5 | `1'908'439` |   `96786` | ops/s
Kryo Macros             | 2/5 |      failed |    failed | ops/s
Scalapb                 | 2/5 | `2'417'988` | `1561369` | ops/s
_Protobuf Scala Macros_ | 2/5 | `2'399'207` |  `142312` | ops/s

## Data Transfer Object (Msg)

### Encode

Library                 | Cnt |       Score | Error (±) | Units
----------------------- | --- | -----------:| ---------:| -----
**JSON**                                                |
Jsoniter                | 2/5 | `4'064'611` | `2194896` | ops/s
**Binary**                                              |
Scalapb                 | 2/5 | `4'719'379` | `1534250` | ops/s
_Protobuf Scala Macros_ | 2/5 | `3'262'720` |  `945078` | ops/s

### Decode

Library                 | Cnt |       Score | Error (±) | Units
----------------------- | --- | -----------:| ---------:| -----
**JSON**                                                |
Jsoniter                | 2/5 | `2'511'272` |  `579622` | ops/s
**Binary**                                              |
Scalapb                 | 2/5 | `3'621'142` |  `750982` | ops/s
_Protobuf Scala Macros_ | 2/5 | `3'118'082` | `1186979` | ops/s

## Environment

1.1 GHz Dual-Core Intel Core m3 \
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
libraryDependencies += "io.github.zero-deps" %% "proto-macros" % "latest.integration" % Compile
libraryDependencies += "io.github.zero-deps" %% "proto-runtime" % "latest.integration"
```

# Usage

You can pick one of the way how to define field number:
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

