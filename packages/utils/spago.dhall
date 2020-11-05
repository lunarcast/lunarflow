{ name = "lunarflow-utils"
, dependencies =
  [ "debugged"
  , "prelude"
  , "ordered-collections"
  , "psci-support"
  , "tuples"
  , "effect"
  , "console"
  , "transformers"
  , "fixed-points"
  , "matryoshka"
  , "stringutils"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
