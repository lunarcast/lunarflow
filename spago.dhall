{ name = "lunarflow"
, dependencies =
  [ "console"
  , "data-default"
  , "debug"
  , "debugged"
  , "effect"
  , "free"
  , "generics-rep"
  , "ordered-collections"
  , "parsing"
  , "profunctor-lenses"
  , "psci-support"
  , "record"
  , "strings"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "typelevel-prelude"
  , "undefined"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
