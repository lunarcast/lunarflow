module Lunarflow.Renderer.Constants where

import Prelude
import Data.List.Lazy as LazyList
import Math (Radians, pi)
import Math as Math

lineHeight :: Int
lineHeight = 50

unitHeight :: Int
unitHeight = 100

linePadding :: Int
linePadding = (unitHeight - lineHeight) / 2

lineWidth :: Int
lineWidth = 200

colors :: LazyList.List String
colors =
  LazyList.cycle
    $ LazyList.fromFoldable
        [ "#BBB684"
        , "#F37878"
        , "#21BEE0"
        , "#AA59AB"
        , "#38F461"
        ]

callAngle :: Radians
callAngle = pi / 3.0

callAngleTangent :: Number
callAngleTangent = Math.tan callAngle

callAngleSinus :: Number
callAngleSinus = Math.sin callAngle

callAngleCosinus :: Number
callAngleCosinus = Math.cos callAngle
