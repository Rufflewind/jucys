{ name = "jucys-tools"
, dependencies =
  [ "argonaut"
  , "console"
  , "debug"
  , "effect"
  , "foreign-object"
  , "group"
  , "halogen"
  , "math"
  , "ordered-collections"
  , "psci-support"
  , "quickcheck"
  , "refs"
  , "string-parsers"
  , "web-html"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "tools/src/**/*.purs", "tools/test/**/*.purs" ]
}
