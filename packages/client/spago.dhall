{ name = "lunarflow-client"
, dependencies =
  [ "data-default"
  , "debug"
  , "debugged"
  , "generics-rep"
  , "ordered-collections"
  , "profunctor-lenses"
  , "psci-support"
  , "record"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "undefined"
  , "effect"
  , "console"
  , "canvas"
  , "run"
  , "fixed-points"
  , "matryoshka"
  , "lunarflow-utils"
  , "lunarflow-core"
  , "lunarflow-geometry"
  ]
, packages = ../../packages.dhall
, sources = [ "./src/**/*.purs" ]
}
