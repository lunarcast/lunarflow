{ name = "lunarflow-core"
, dependencies =
  [ "data-default"
  , "debug"
  , "debugged"
  , "generics-rep"
  , "ordered-collections"
  , "parsing"
  , "profunctor-lenses"
  , "psci-support"
  , "record"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "undefined"
  , "fixed-points"
  , "matryoshka"
  , "lunarflow-utils"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
