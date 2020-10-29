module Main where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Debug.Trace (trace)
import Effect (Effect)
import Effect.Console as Console
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Lunarflow.Ast (call, printDeBrujin, withDebrujinIndices)
import Lunarflow.Ast.Grouped (groupExpression)
import Lunarflow.Debug (debugSpy)
import Lunarflow.Examples (exp, mult, n, plus', zero')
import Lunarflow.Geometry.Foreign (Geometry, fromShape, renderGeometry)
import Lunarflow.Layout (addIndices, runLayoutM, unscopeLayout)
import Lunarflow.Profile (profileApplication)
import Lunarflow.Render (render, runRenderM)
import Lunarflow.Renderer.WithHeight (withHeights)
import Partial.Unsafe (unsafeCrashWith)

geometryBenchmarks :: Effect Geometry
geometryBenchmarks =
  profileApplication "Converting shape to geometry" fromShape
    $ profileApplication "Rendering layout" (runRenderM <<< render)
    $ map debugSpy
    $ profileApplication "Adding height data" withHeights
    $ map
        ( case _ of
            Left err -> unsafeCrashWith $ show err
            Right a -> a
        )
    $ profileApplication "Creating layout"
        ( runLayoutM
            <<< unscopeLayout
            <<< addIndices
        )
    $ profileApplication "Grouping expression" groupExpression
    $ map
        ( \a ->
            trace (printDeBrujin a) \_ ->
              a
        )
    $ profileApplication "Adding de-brujin indices" withDebrujinIndices
    -- $ profileApplication "Parsing" unsafeParseLambdaCalculus """\f .(\x. x x)  (\x. f (x x))"""
    
    -- $ profileApplication "Parsing" unsafeParseLambdaCalculus """\n s z. n (\g h. h (g s)) (\u. z) (\u. u)"""
    
    $ call unit (call unit plus' (n 3))
        ( call unit (call unit mult (n 7))
            ( call unit (call unit exp (n 2))
                (zero')
            )
        )

-- $ call
--     unit
--     (call unit exp (n 2))
--     zero'
-- $ profileApplication "Parsing" unsafeParseLambdaCalculus """\f a b. f b a (\u. f) (\u. u) """
main :: Effect Unit
main = do
  canvas <- getCanvasElementById "canvas"
  case canvas of
    Nothing -> Console.log "No canvas found"
    Just canvas' -> do
      geometry <- geometryBenchmarks
      ctx <- getContext2D canvas'
      renderGeometry geometry ctx
