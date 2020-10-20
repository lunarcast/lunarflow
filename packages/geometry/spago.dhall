{ name = "lunarflow-geometry"
, dependencies =
  [ "debug"
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
  , "sized-vectors"
  , "undefined-is-not-a-problem"
  , "lunarflow-utils"
  , "lunarflow-core"
  ]
, packages = ../../packages.dhall
, sources = [ "./src/**/*.purs" ]
}
