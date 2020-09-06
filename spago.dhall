{ name = "lunarflow"
, dependencies =
  [ "console"
  , "effect"
  , "free"
  , "generics-rep"
  , "ordered-collections"
  , "parsing"
  , "profunctor-lenses"
  , "psci-support"
  , "record"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "undefined"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
