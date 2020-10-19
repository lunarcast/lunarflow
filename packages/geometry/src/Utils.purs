module Lunarflow.Geometry.Utils where

import Prelude
import Lunarflow.Geometry.Types (Bounds)

-- | Add uniform padding to some bounds.
withPadding :: Int -> Bounds -> Bounds
withPadding amount { x, y, width, height } = { x, y, width: width + amount, height: height + amount }
