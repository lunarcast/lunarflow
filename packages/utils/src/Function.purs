-- | Don't kill me please
module Lunarflow.Function where

import Data.Function (applyFlipped)

-- infixr 0 apply as <|
infixl 1 applyFlipped as |>

-- | fp-ts had this and it's pretty handy
type Endomorphism a
  = a -> a
