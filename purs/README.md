# protobuf-scala-purs

[![test](https://img.shields.io/github/workflow/status/zero-deps/proto/test?label=tests)](https://github.com/zero-deps/proto/actions/workflows/test.yml)

Library generates Purescript code to decode/encode protobuf bytes. Code is generated based on same Scala models. Essentially Scala code replaces .proto files.

## Install

1. Add dependency `libraryDependencies += "io.github.zero-deps" %% "proto-purs" % "latest.integration"` (or as a git-submodule)
1. Add `"protobuf"` to `dependencies` as in `test/spago.dhall`
1. Add `protobuf` to `upstream` as in `test/packages/dhall`
1. Run method `proto.purs.io.writeToFile("web/src/api.purs", proto.purs.enumByN[Push, Pull])`

## Types

Scala type        | Purescript type
----------------- | ---------------
`String         ` | `String`
`Int            ` | `Int`
`Long`            | `Number`
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
bin/com
bin/tes
```
