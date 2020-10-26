module Main where

import Prelude
import Data.List as List
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Lunarbox.Render (render, runRenderM)
import Lunarflow.Ast (withDebrujinIndices)
import Lunarflow.Ast.Grouped (groupExpression)
import Lunarflow.Geometry.Foreign (Geometry, fromShape, renderGeometry)
import Lunarflow.Layout (addIndices, fromScoped, runLayoutM)
import Lunarflow.Parser (unsafeParseLambdaCalculus)
import Lunarflow.Profile (profileApplication)
import Lunarflow.Renderer.WithHeight (withHeights)
import Partial.Unsafe (unsafePartial)

geometryBenchmarks :: Effect Geometry
geometryBenchmarks =
  profileApplication "Converting shape to geometry" fromShape
    $ profileApplication "Rendering layout" (runRenderM <<< render)
    $ map fst
    $ profileApplication "Adding height data" withHeights
    $ profileApplication "Unscoping layout" (unsafePartial $ fromJust <<< fromScoped)
    $ profileApplication "Picking the layout to render"
        ( unsafePartial
            $ fromJust
            <<< flip List.index 0
        )
    $ map
        ( \layouts ->
            unsafePerformEffect do
              Console.log
                $ "Created "
                <> show (List.length layouts)
                <> " layouts."
              pure layouts
        )
    $ profileApplication "Adding vertical indices" (runLayoutM <<< addIndices)
    $ profileApplication "Grouping expression" groupExpression
    $ profileApplication "Adding de-brujin indices" withDebrujinIndices
    $ profileApplication "Parsing" unsafeParseLambdaCalculus """\s z. s (s (s (s (s z)))))"""

-- $ unsafeParseLambdaCalculus """\f x y. f y x \x -> y x"""
main :: Effect Unit
main = do
  canvas <- getCanvasElementById "canvas"
  case canvas of
    Nothing -> Console.log "No canvas found"
    Just canvas' -> do
      geometry <- geometryBenchmarks
      ctx <- getContext2D canvas'
      renderGeometry geometry ctx
