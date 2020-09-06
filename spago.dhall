{ name = "lunarflow"
, dependencies =
  [ "console"
  , "effect"
  , "free"
  , "generics-rep"
  , "ordered-collections"
  , "parsing"
  , "psci-support"
  , "record"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
