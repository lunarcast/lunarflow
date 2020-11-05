module Lunarflow.String where

import Prelude
import Data.String (joinWith)
import Data.String.Utils (lines, unsafeRepeat)

-- | Indent a string by a number of spaces
indent :: Int -> String -> String
indent spaces = joinWith "\n" <<< map (space <> _) <<< lines
  where
  space = unsafeRepeat spaces " "
