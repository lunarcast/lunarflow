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
  , "lunarflow-utils"
  , "lunarflow-core"
  ]
, packages = ../../packages.dhall
, sources = [ "./src/**/*.purs" ]
}
