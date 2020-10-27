module Lunarflow.Renderer.Constants where

import Prelude
import Data.List.Lazy as LazyList

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
