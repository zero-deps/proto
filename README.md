# protobuf-scala-purs

## Latest Version

[proto-purs](https://bintray.com/zero-deps/maven/proto-purs/_latestVersion)

This library generates Purescript code to decode/encode protobuf bytes. Code is generated based on same Scala models. Essentially Scala code replaces .proto files. But because Scala code is part of sources Purescript should be generated at moment of runtime using reflection. Macros is not suitable because their purpose is to manipulate with AST and not to generate anything but code tree.

1. Add dependency `libraryDependencies += "io.github.zero-deps" %% "proto-purs" % "latest.integration"`
1. Add resolver `resolvers += Resolver.jcenterRepo`
1. Add "purescript-protobuf": "https://github.com/zero-deps/purescript-protobuf.git" to "dependencies" section in bower.json
1. Run method `zd.proto.Purescript.generate[D, E](moduleName="N")` where `D`/`E` are types of your base trait for decode/encode and `N` is name to put at beginning of file. Method return string which you can save to file or print.

### Test

```bash
sbt test
cd test
npm i
npm run dep
npm run com
npm run tes
```
