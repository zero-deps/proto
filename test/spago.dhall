{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "test"
, dependencies =
  [ "console", "effect", "node-process", "protobuf", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
