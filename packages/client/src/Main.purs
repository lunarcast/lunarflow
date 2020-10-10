module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as Console
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Lunarflow.Geometry.Foreign (Geometry, fromShape, renderGeometry)
import Lunarflow.Geometry.Types (PolygonAttribs(..), circle, group, polygon, rect)

geometry :: Geometry
geometry =
  fromShape
    $ group { stroke: "red" }
        [ polygon { fill: "brown" }
            $ PolygonAttribs
                [ { x: 100, y: 110 }
                , { x: 140, y: 70 }
                , { x: 200, y: 190 }
                , { x: 350, y: 170 }
                , { x: 190, y: 400 }
                , { x: 100, y: 370 }
                ]
        , rect { fill: "yellow" } { x: 200, y: 40 } 70 160
        , circle { fill: "green", stroke: "transparent" } { x: 300, y: 300 } 70
        ]

main :: Effect Unit
main = do
  canvas <- getCanvasElementById "canvas"
  case canvas of
    Nothing -> Console.log "No canvas found"
    Just canvas' -> do
      ctx <- getContext2D canvas'
      renderGeometry geometry ctx
