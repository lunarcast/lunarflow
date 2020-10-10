{ name = "lunarflow-utils"
, dependencies =
  [ "debugged"
  , "ordered-collections"
  , "psci-support"
  , "tuples"
  , "effect"
  , "console"
  , "transformers"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs" ]
}
