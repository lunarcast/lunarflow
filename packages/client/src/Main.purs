module Main where

import Prelude
import Data.List as List
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (fst)
import Debug.Trace (traceM)
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
  fromShape $ runRenderM
    $ render
    $ fst
    $ withHeights
    $ unsafePartial
    $ fromJust
    -- TODO: fix bug with application creating a line in the same place as the lambda used as argument.
    
    $ flip List.index 1
    $ List.catMaybes
    $ map fromScoped
    $ runLayoutM
    $ addIndices
    $ groupExpression
    $ withDebrujinIndices
    $ unsafeParseLambdaCalculus """\f a b -> f b a \x -> x"""

main :: Effect Unit
main = do
  canvas <- getCanvasElementById "canvas"
  case canvas of
    Nothing -> Console.log "No canvas found"
    Just canvas' -> do
      ctx <- getContext2D canvas'
      traceM geometry
      renderGeometry geometry ctx
