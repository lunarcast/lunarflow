let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200831/packages.dhall sha256:cdb3529cac2cd8dd780f07c80fd907d5faceae7decfcaa11a12037df68812c83

let overrides = {=}

let additions = 
      { debugged =
            { dependencies = 
                  [ "prelude"
                  , "console"
                  , "strings"
                  , "ordered-collections"
                  , "either"
                  , "tuples"
                  , "lists"
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

in  upstream // overrides // additions
