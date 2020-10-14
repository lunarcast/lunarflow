let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201007/packages.dhall sha256:35633f6f591b94d216392c9e0500207bb1fec42dd355f4fecdfd186956567b6b

let overrides = {=}

let additions =
      { debugged =
        { dependencies =
          [ "prelude"
          , "console"
          , "ordered-collections"
          , "either"
          , "tuples"
          , "lists"
          , "strings"
          , "arrays"
          , "bifunctors"
          , "record"
          , "effect"
          , "generics-rep"
          , "datetime"
          , "enums"
          ]
        , repo = "https://github.com/hdgarrood/purescript-debugged"
        , version = "744498226da3c9b5b37c69771cc0378a65cc8189"
        }
      }

let packages =
      upstream
      with lunarflow-utils = ./packages/utils/spago.dhall as Location
      with lunarflow-core = ./packages/core/spago.dhall as Location
      with lunarflow-geometry = ./packages/geometry/spago.dhall as Location

in  packages // overrides // additions
