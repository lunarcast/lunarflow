-- TODO: docs
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

lineTipWidth :: Int
lineTipWidth = 100

colors :: LazyList.List String
colors =
  LazyList.cycle
    $ LazyList.fromFoldable
        [ "#fe444e"
        , "#62c89d"
        , "#ff7edb"
        , "#ee9d28"
        , "#1e88e5"
        , "#6af1b8"
        , "#abaaaf"
        ]

callAngle :: Radians
callAngle = pi / 3.0

callAngleTangent :: Number
callAngleTangent = Math.tan callAngle

callAngleSinus :: Number
callAngleSinus = Math.sin callAngle

callAngleCosinus :: Number
callAngleCosinus = Math.cos callAngle
