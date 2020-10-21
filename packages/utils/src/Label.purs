module Lunarflow.Label where

import Prim.TypeError (class Warn, Above, Beside, QuoteLabel, Text)

-- TODO: Think more about how good / bad of an idea this is.
-- WARNING: Don't use this for actual docs, just for conveying 
-- a small amoung of info to the programmer which actually calls the function.
-- | Dummy typeclass used for giving extra info to the programmer.
class Label (a :: Symbol)

instance label ::
  Warn
      ( Above
          (Text "TODO: create a newtype instead of using the label ")
          (Beside (Text "    \"") (Beside (QuoteLabel a) (Text "\"")))
      ) =>
  Label a
