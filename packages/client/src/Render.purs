module Lunarbox.Render where

import Prelude
import Data.FoldableWithIndex (foldrWithIndex)
import Data.List as List
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Debug.Trace (traceM)
import Lunarflow.Ast (AstF(..), isVar)
import Lunarflow.Geometry.Foreign (getRightBound)
import Lunarflow.Geometry.Foreign as ForeignShape
import Lunarflow.Geometry.Types as Shape
import Lunarflow.Geometry.Utils (withPadding)
import Lunarflow.Renderer.WithHeight (YLayout, YLayoutF, YMapSlice, getPosition)
import Matryoshka (GAlgebra, para)
import Run (Run, extract)
import Run.Reader (READER, ask, local, runReader)

type RenderContext
  = { doNotRender :: Set.Set Int
    , start :: Int
    , end :: Int
    , slice :: YMapSlice
    }

type RenderM r
  = Run ( reader :: READER RenderContext | r )

-- | Prepare stuff for rendering inside a lambda.
shiftContext :: Int -> RenderContext -> RenderContext
shiftContext by ctx = ctx { doNotRender = ((+) by) `Set.map` ctx.doNotRender }

lineHeight :: Int
lineHeight = 50

lineWidth :: Int
lineWidth = 100

{-- 
So:
- When we encounter a lambda, we draw the body and then the box around it
- When we encouner a var, we check where it is in scope and draw until here
--}
render :: forall r. YLayout -> RenderM r Shape.Shape
render = para algebra
  where
  algebra :: GAlgebra (Tuple YLayout) YLayoutF (RenderM r Shape.Shape)
  algebra (Lambda { args, heights } (Tuple _ body)) = do
    bodyShape <- local (_ { slice = heights } <<< (shiftContext $ List.length args)) body
    let
      bounds = ForeignShape.bounds bodyShape
    pure
      $ Shape.group {}
          [ Shape.rect { fill: "black", stroke: "red" } $ withPadding 20 bounds
          , bodyShape
          ]

  algebra (Var { position, index }) = do
    { end, start, slice, doNotRender } <- ask
    if (Set.member index doNotRender) then
      pure mempty
    else do
      pure
        $ Shape.rect { fill: "yellow", stroke: "black" }
            { x: 0
            , y: getY position slice
            , height: lineHeight
            , width: max lineWidth start
            }

  algebra (Call position mkFunc@(Tuple functionLayout _) mkArg@(Tuple argumentLayout _)) = do
    function <- renderFn 0 mkFunc
    slice <- ask <#> _.slice
    traceM ("Call position " <> show position)
    let
      functionEnd = if isVar functionLayout then 0 else getRightBound function

      functionPosition = getPosition functionLayout
    argument <- renderFn functionEnd mkArg
    let
      argumentEnd = getRightBound argument
    pure
      $ Shape.group {}
          [ function
          , argument
          , Shape.rect
              { fill: "green"
              , stroke: "black"
              }
              { x: functionEnd
              , width: argumentEnd - functionEnd
              , height: lineHeight
              , y: getY functionPosition slice
              }
          , Shape.rect
              { fill: "blue"
              , stroke: "black"
              }
              { x: argumentEnd
              , width: lineWidth
              , height: lineHeight
              , y: getY position slice
              }
          ]
    where
    renderFn start (Tuple ast m) = local (_ { start = start, end = start + lineWidth }) m

  getY :: Int -> YMapSlice -> Int
  getY position slice =
    lineHeight
      * foldrWithIndex
          (\index height result -> if position < index then result else result + height)
          0
          slice

-- | Run a computation in the Render monad
runRenderM :: forall a. RenderM () a -> a
runRenderM m = extract $ runReader { doNotRender: Set.empty, start: 0, end: 0, slice: mempty } m
