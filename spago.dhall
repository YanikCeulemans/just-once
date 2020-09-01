{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "bytestrings"
  , "console"
  , "crypto"
  , "datetime"
  , "debug"
  , "effect"
  , "generics-rep"
  , "integers"
  , "maybe"
  , "now"
  , "psci-support"
  , "result"
  , "spec"
  , "string-parsers"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
