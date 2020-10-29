module Main where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Console as Console
import Effect.Ref as Ref
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getContext2D)
import Lunarflow.Ast (call, withDebrujinIndices)
import Lunarflow.Ast.Grouped (groupExpression)
import Lunarflow.Examples (exp, mult, n, plus', zero')
import Lunarflow.Geometry.Foreign (Geometry, boundsImpl, fromShape, renderGeometry)
import Lunarflow.Layout (addIndices, runLayoutM, unscopeLayout)
import Lunarflow.Profile (profileApplication)
import Lunarflow.Render (render, runRenderM)
import Lunarflow.Renderer.Canvas (fillScreen, fitIntoBounds, onResize)
import Lunarflow.Renderer.WithHeight (withHeights)
import Partial.Unsafe (unsafeCrashWith)

geometryBenchmarks :: Effect Geometry
geometryBenchmarks =
  profileApplication "Converting shape to geometry" fromShape
    $ profileApplication "Rendering layout" (runRenderM <<< render)
    -- $ map debugSpy
    
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
    -- $ map
    
    --     ( \a ->
    
    --         trace (printDeBrujin a) \_ ->
    
    --           a
    
    --     )
    
    $ profileApplication "Adding de-brujin indices" withDebrujinIndices
    -- $ profileApplication "Parsing" unsafeParseLambdaCalculus """\f .(\x. x x)  (\x. f (x x))"""
    
    -- $ profileApplication "Parsing" unsafeParseLambdaCalculus """\n s z. n (\g h. h (g s)) (\u. z) (\u. u)"""
    
    $ call unit (call unit plus' (n 3))
        ( call unit (call unit mult (n 7))
            ( call unit (call unit exp (n 2))
                (zero')
            )
        )

app :: Ref.Ref Geometry -> CanvasElement -> Context2D -> Effect Unit
app ref canvas ctx = do
  fillScreen canvas
  geometry <- Ref.read ref
  case Nullable.toMaybe (boundsImpl geometry) of
    Just bounds -> fitIntoBounds bounds ctx
    Nothing -> pure unit
  renderGeometry geometry ctx

main :: Effect Unit
main = do
  maybeCanvas <- getCanvasElementById "canvas"
  case maybeCanvas of
    Nothing -> Console.log "No canvas found"
    Just canvas -> do
      ctx <- getContext2D canvas
      geometry <- geometryBenchmarks
      ref <- Ref.new geometry
      let
        runApp = app ref canvas ctx
      onResize runApp
      runApp
