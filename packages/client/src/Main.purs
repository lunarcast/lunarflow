module Main where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Console as Console
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Lunarflow.Ast (withDebrujinIndices)
import Lunarflow.Ast.Grouped (groupExpression)
import Lunarflow.Debug (debugSpy)
import Lunarflow.Geometry.Foreign (Geometry, fromShape, renderGeometry)
import Lunarflow.Layout (addIndices, runLayoutM, unscopeLayout)
import Lunarflow.Parser (unsafeParseLambdaCalculus)
import Lunarflow.Profile (profileApplication)
import Lunarflow.Render (render, runRenderM)
import Lunarflow.Renderer.WithHeight (withHeights)
import Partial.Unsafe (unsafeCrashWith)

geometryBenchmarks :: Effect Geometry
geometryBenchmarks =
  profileApplication "Converting shape to geometry" fromShape
    $ profileApplication "Rendering layout" (spy "hmmm" <<< runRenderM <<< render)
    $ map debugSpy
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
    -- $ profileApplication "Parsing" unsafeParseLambdaCalculus """\f .(\x. x x)  (\x. f (x x))"""
    
    -- $ profileApplication "Parsing" unsafeParseLambdaCalculus """\n s z. n (\g h. h (g s)) (\u. z) (\u. u)"""
    
    $ profileApplication "Parsing" unsafeParseLambdaCalculus """\f a b. f b a (\u. f) (\u. u) """

main :: Effect Unit
main = do
  canvas <- getCanvasElementById "canvas"
  case canvas of
    Nothing -> Console.log "No canvas found"
    Just canvas' -> do
      geometry <- geometryBenchmarks
      ctx <- getContext2D canvas'
      renderGeometry geometry ctx
