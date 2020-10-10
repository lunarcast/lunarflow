let client = ./packages/client/spago.dhall

let overrides = { sources = [ "packages/client/src/**/*.purs" ] }

in  client ⫽ overrides
