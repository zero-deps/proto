{ name = "test"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "either"
  , "integers"
  , "maybe"
  , "node-process"
  , "prelude"
  , "protobuf"
  , "tailrec"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
