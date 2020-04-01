# protobuf-scala-purs

* [latest](https://bintray.com/zero-deps/maven/proto-purs/_latestVersion) version

Library generates Purescript code to decode/encode protobuf bytes. Code is generated based on same Scala models. Essentially Scala code replaces .proto files. But because Scala code is part of sources Purescript should be generated at moment of runtime using reflection. Macros is not suitable because their purpose is to manipulate with AST and not to generate anything but code tree.

1. Add dependency `libraryDependencies += "io.github.zero-deps" %% "proto-purs" % "latest.integration"`
1. Add one resolver of:
    * `resolvers += Resolver.jcenterRepo` or 
    * `resolvers += Resolver.bintrayRepo("zero-deps", "maven")`
1. Add `"purescript-protobuf": "https://github.com/zero-deps/purescript-protobuf.git"` to `"dependencies"` section in `bower.json`
1. Run method `Purescript.generate[Push, Pull](moduleEncodeName="Pull", moduleDecodeName="Push", "Common", codecs=Nil)`
    * Method return string which you can save to file

## Types

Scala type        | Purescript type
----------------- | ---------------
`String         ` | `String`
`Int            ` | `Int`
`Float`           | Not supported
`Double         ` | `Number`
`Option[_] `      | `Maybe _`
`Array[Byte]    ` | `Uint8Array`
`Set[_]    `      | `Array _`
`Map[_, _]      ` | `Array (Tuple _ _)`
`Iterable[_]    ` | `Array _`

## Test

```bash
sbt test
cd test
./dep
./com
./tes
```