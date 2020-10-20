{ name = "lunarflow-utils"
, dependencies =
  [ "debugged"
  , "ordered-collections"
  , "psci-support"
  , "tuples"
  , "effect"
  , "console"
  , "transformers"
  , "fixed-points"
  , "matryoshka"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
