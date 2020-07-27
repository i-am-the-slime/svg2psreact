{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "node-fs"
  , "node-fs-aff"
  , "node-process"
  , "psci-support"
  , "strings-extra"
  , "stringutils"
  , "svg-parser"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
