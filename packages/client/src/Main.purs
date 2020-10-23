module Main where

import Prelude
import Data.List as List
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (fst)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Console as Console
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Lunarbox.Render (render, runRenderM)
import Lunarflow.Ast (withDebrujinIndices)
import Lunarflow.Ast.Grouped (groupExpression)
import Lunarflow.Geometry.Foreign (Geometry, fromShape, renderGeometry)
import Lunarflow.Layout (addIndices, fromScoped, runLayoutM)
import Lunarflow.Parser (unsafeParseLambdaCalculus)
import Lunarflow.Renderer.WithHeight (withHeights)
import Partial.Unsafe (unsafePartial)

geometry :: Geometry
geometry =
  fromShape $ spy "shape" $ runRenderM
    $ render
    $ fst
    $ withHeights
    $ unsafePartial
    $ fromJust
    $ flip List.index 0
    $ List.catMaybes
    $ map fromScoped
    $ runLayoutM
    $ addIndices
    $ groupExpression
    $ withDebrujinIndices
    -- $ debugSpy
    
    $ unsafeParseLambdaCalculus """\f. (\x. f (x x)) (\y. f (y y))"""

-- $ unsafeParseLambdaCalculus """\f x y. f y x \x -> y x"""
main :: Effect Unit
main = do
  canvas <- getCanvasElementById "canvas"
  case canvas of
    Nothing -> Console.log "No canvas found"
    Just canvas' -> do
      ctx <- getContext2D canvas'
      renderGeometry geometry ctx
