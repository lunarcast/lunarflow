module Main where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Console as Console
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Lunarbox.Render (render, runRenderM)
import Lunarflow.Ast (withDebrujinIndices)
import Lunarflow.Ast.Grouped (groupExpression)
import Lunarflow.Geometry.Foreign (Geometry, fromShape, renderGeometry)
import Lunarflow.Layout (addIndices, runLayoutM, unscopeLayout)
import Lunarflow.Parser (unsafeParseLambdaCalculus)
import Lunarflow.Profile (profileApplication)
import Lunarflow.Renderer.WithHeight (withHeights)
import Partial.Unsafe (unsafeCrashWith)

geometryBenchmarks :: Effect Geometry
geometryBenchmarks =
  profileApplication "Converting shape to geometry" fromShape
    $ profileApplication "Rendering layout" (spy "hmmm" <<< runRenderM <<< render)
    $ map fst
    $ profileApplication "Adding height data" withHeights
    $ map
        ( case _ of
            Left err -> unsafeCrashWith $ show err
            Right a -> a
        )
    $ profileApplication "Creating layout"
        ( runLayoutM
            -- <<< map (spy "unscoped")
            
            <<< unscopeLayout
            -- <<< map debugSpy
            
            <<< addIndices
        )
    $ profileApplication "Grouping expression" groupExpression
    $ profileApplication "Adding de-brujin indices" withDebrujinIndices
    $ profileApplication "Parsing" unsafeParseLambdaCalculus """\f .(\x. x x)  (\x. f (x x))"""

main :: Effect Unit
main = do
  canvas <- getCanvasElementById "canvas"
  case canvas of
    Nothing -> Console.log "No canvas found"
    Just canvas' -> do
      geometry <- geometryBenchmarks
      ctx <- getContext2D canvas'
      renderGeometry geometry ctx
