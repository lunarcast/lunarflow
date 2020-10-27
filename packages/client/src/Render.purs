-- TODO: Document this
module Lunarbox.Render where

import Prelude
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.List as List
import Data.Maybe (fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (replicate)
import Data.Vec (vec2)
import Debug.Trace (spy)
import Lunarflow.Ast (AstF(..), isVar)
import Lunarflow.Geometry.Foreign (getRightBound)
import Lunarflow.Geometry.Foreign as ForeignShape
import Lunarflow.Geometry.Types (Bounds)
import Lunarflow.Geometry.Types as Shape
import Lunarflow.Label (class Label)
import Lunarflow.Renderer.WithHeight (YLayout, YLayoutF, YMapSlice)
import Matryoshka (GAlgebra, para, project)
import Run (Run, extract)
import Run.Reader (READER, ask, local, runReader)

type RenderContext
  = { doNotRender :: Set.Set Int
    , start :: Int
    , end :: Int
    , slices :: List.List YMapSlice
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

type ScopedShape
  = Tuple Int Shape.Shape

type RenderList
  = NonEmptyArray.NonEmptyArray ScopedShape

inScope ::
  Int ->
  RenderList ->
  { no :: Array ScopedShape
  , yes :: Array ScopedShape
  }
inScope max = NonEmptyArray.partition (fst >>> (_ < max))

shiftScope :: Int -> ScopedShape -> ScopedShape
shiftScope amount = lmap (_ - amount)

-- | Renders a layout using the Render monad.
render :: forall r. YLayout -> RenderM r Shape.Shape
render = para algebra >>> map (map snd >>> Shape.fromFoldable)
  where
  algebra :: GAlgebra (Tuple YLayout) YLayoutF (RenderM r RenderList)
  algebra (Lambda { args, heights, position } (Tuple _ body)) = do
    let
      argCount = List.length args
    bodyShapes <- inScope argCount <$> local (updateContext argCount) body
    slices <- ask <#> _.slices
    let
      shapesInScope = snd <$> spy "yes" (bodyShapes.yes)

      bounds :: Bounds
      bounds = ForeignShape.bounds $ Shape.fromFoldable shapesInScope

      result :: ScopedShape
      result =
        Tuple 0
          $ Shape.group { translate: vec2 0 (getY 0 position slices) }
          $ Array.cons (Shape.rect { fill: "rgba(128,128,128,0.5)", stroke: "red" } bounds)
              shapesInScope
    pure $ NonEmptyArray.cons' result $ shiftScope argCount <$> bodyShapes.no
    where
    updateContext argCount = shiftContext argCount >>> \a -> a { slices = (replicate argCount heights) <> a.slices }

  algebra (Var { position, index }) = do
    { end, start, slices, doNotRender } <- ask
    pure
      $ NonEmptyArray.singleton
          if (Set.member index doNotRender) then
            Tuple 0 Shape.Null
          else
            Tuple index
              $ Shape.rect
                  { fill: "yellow"
                  , stroke: "black"
                  }
                  { x: 0
                  , y: getY index position slices
                  , height: lineHeight
                  , width: max lineWidth start
                  }

  algebra (Call position mkFunc@(Tuple functionLayout _) mkArg@(Tuple argumentLayout _)) = do
    function <- renderFn 0 mkFunc
    slices <- ask <#> _.slices
    let
      -- TODO: maybe make the call to getRightBound only include stuff in scope or something
      functionEnd = if isVar functionLayout then 0 else getRightBound $ Shape.fromFoldable $ snd <$> function
    argument <- renderFn functionEnd mkArg
    let
      argumentEnd = getRightBound $ Shape.fromFoldable $ snd <$> argument

      functionContinuation =
        Shape.rect
          { fill: "green"
          , stroke: "black"
          }
          { x: functionEnd
          , width: argumentEnd - functionEnd
          , height: lineHeight
          , y: getLayoutY functionLayout slices
          }

      functionShapes :: RenderList
      functionShapes =
        NonEmptyArray.cons
          ( Tuple (fst $ NonEmptyArray.head function)
              functionContinuation
          )
          function

      resultShape =
        Tuple 0
          $ Shape.rect
              { fill: "blue"
              , stroke: "black"
              }
              { x: argumentEnd
              , width: lineWidth
              , height: lineHeight
              , y: getY 0 position slices
              }
    pure $ NonEmptyArray.cons resultShape $ functionShapes <> argument
    where
    renderFn start (Tuple ast m) =
      local
        ( _
            { start = start
            , end = start + lineWidth
            }
        )
        m

-- | Get the y position based on an index and a (relative) position.
getY :: (Label "index" => Int) -> (Label "position" => Int) -> List.List YMapSlice -> Int
getY index position slices = lineHeight * units
  where
  units =
    foldrWithIndex
      (\index' height result -> if position <= index' then result else result + height)
      0
      $ fromMaybe []
      $ List.index slices index

-- | Get the y position from a YLayout by inferring the correct scope. 
getLayoutY :: YLayout -> List.List YMapSlice -> Int
getLayoutY layout slices = case project layout of
  Var { index, position } -> getY index position slices
  Call position _ _ -> getY 0 position slices
  Lambda { position } _ -> getY 0 position slices

-- | Run a computation in the render monad.
runRenderM :: forall a. RenderM () a -> a
runRenderM m = extract $ runReader { doNotRender: Set.empty, start: 0, end: 0, slices: List.singleton mempty } m
