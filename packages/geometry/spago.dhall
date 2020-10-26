{ name = "lunarflow-geometry"
, dependencies =
  [ "psci-support"
  , "prelude"
  , "unsafe-coerce"
  , "effect"
  , "canvas"
  , "foldable-traversable"
  , "typelevel-prelude"
  , "typelevel"
  , "sized-vectors"
  , "undefined-is-not-a-problem"
  , "lunarflow-utils"
  ]
, packages = ../../packages.dhall
, sources = [ "./src/**/*.purs" ]
}
