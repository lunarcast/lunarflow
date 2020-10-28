module Lunarflow.Vector where

import Data.Typelevel.Num (D2)
import Data.Vec (Vec)
import Math (Radians)

-- | Shorthand for vec 2s of ints
type Vec2
  = Vec D2 Int

foreign import rotate :: Vec2 -> Radians -> Vec2

foreign import add :: Vec2 -> Vec2 -> Vec2

foreign import negate :: Vec2 -> Vec2

foreign import sub :: Vec2 -> Vec2 -> Vec2
