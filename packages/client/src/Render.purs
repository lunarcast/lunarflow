-- TODO: Document this
module Lunarbox.Render where

import Prelude
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.FoldableWithIndex (foldrWithIndex)
import Data.List as List
import Data.List.Lazy as LazyList
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicate)
import Data.Vec (vec2)
import Debug.Trace (spy, traceM)
import Lunarflow.Ast (AstF(..), isVar)
import Lunarflow.Geometry.Foreign (getRightBound)
import Lunarflow.Geometry.Foreign as ForeignShape
import Lunarflow.Geometry.Types (Bounds)
import Lunarflow.Geometry.Types as Shape
import Lunarflow.Label (class Label)
import Lunarflow.Pipe ((|>))
import Lunarflow.Renderer.Constants (colors, lineHeight, linePadding, lineWidth, unitHeight)
import Lunarflow.Renderer.WithHeight (YLayout, YLayoutF, YMapSlice)
import Matryoshka (GAlgebra, para, project)
import Run (Run, extract)
import Run.Reader (READER, ask, local, runReader)
import Run.State (STATE, evalState, get, put)

type RenderContext
  = { doNotRender :: Set.Set Int
    , start :: Int
    , end :: Int
    , slices :: List.List YMapSlice
    , colors :: List.List String
    }

type RenderState
  = { colors :: LazyList.List String
    }

type RenderM r
  = Run
      ( reader :: READER RenderContext
      , state :: STATE RenderState
      | r
      )

-- | Prepare stuff for rendering inside a lambda.
shiftContext :: Int -> RenderContext -> RenderContext
shiftContext by ctx = ctx { doNotRender = ((+) by) `Set.map` ctx.doNotRender }

type ScopedShape
  = { scope :: Int
    , shape :: Shape.Shape
    , color :: String
    }

type RenderList
  = NonEmptyArray.NonEmptyArray ScopedShape

inScope ::
  Int ->
  RenderList ->
  { no :: Array ScopedShape
  , yes :: Array ScopedShape
  }
inScope max = NonEmptyArray.partition (_.scope >>> (_ < max))

shiftScope :: Int -> ScopedShape -> ScopedShape
shiftScope amount shape =
  shape
    { scope = shape.scope - amount
    }

-- | Renders a layout using the Render monad.
render :: forall r. YLayout -> RenderM r Shape.Shape
render = para algebra >>> map (map _.shape >>> Shape.fromFoldable)
  where
  algebra :: GAlgebra (Tuple YLayout) YLayoutF (RenderM r RenderList)
  algebra (Lambda { args, heights, position } (Tuple _ body)) = do
    let
      argCount = List.length args
    newColors <- sequence $ replicate argCount freshColor
    bodyShapes <- inScope argCount <$> local (updateContext argCount newColors) body
    color <- maybe freshColor (_.color >>> pure) (Array.head bodyShapes.yes)
    slices <- ask <#> _.slices
    let
      shapesInScope = _.shape <$> bodyShapes.yes

      bounds :: Bounds
      bounds = ForeignShape.bounds $ Shape.fromFoldable shapesInScope

      result :: ScopedShape
      result =
        { shape:
          Shape.group {}
            $ map (Shape.Translate $ vec2 0 $ lineHeight / 2 + getY 0 position slices)
            $ Array.cons
                ( Shape.rect
                    { fill: "black"
                    , stroke: color
                    }
                    $ bounds
                        { y = bounds.y - linePadding
                        , height = bounds.height + 2 * linePadding
                        }
                )
                shapesInScope
        , scope: 0
        , color
        }
    pure $ NonEmptyArray.cons' result $ shiftScope argCount <$> bodyShapes.no
    where
    updateContext argCount newColors ctx =
      ctx
        { slices = (replicate argCount heights) <> ctx.slices
        , doNotRender = ((+) argCount) `Set.map` ctx.doNotRender
        , colors = newColors <> ctx.colors
        }

  algebra (Var { position, index }) = do
    { end, start, slices, doNotRender, colors } <- ask
    -- TODO: don't do stupid stuff like this
    let
      color = fromMaybe "black" (List.index colors index)
    pure
      $ NonEmptyArray.singleton
          if (Set.member index doNotRender) then
            { scope: 0
            , color
            , shape: Shape.Null
            }
          else
            { scope: index
            , color
            , shape:
              Shape.rect
                { fill: color
                , stroke: "black"
                }
                { x: 0
                , y: linePadding + getY index position slices
                , height: lineHeight
                , width: max lineWidth start
                }
            }

  algebra (Call position mkFunc@(Tuple functionLayout _) mkArg@(Tuple argumentLayout _)) = do
    function <- renderFn 0 mkFunc
    slices <- ask <#> _.slices
    let
      -- TODO: maybe make the call to getRightBound only include stuff in scope or something
      functionEnd =
        if isVar functionLayout then
          0
        else
          function
            |> map _.shape
            |> Shape.fromFoldable
            |> getRightBound
    argument <- renderFn functionEnd mkArg
    let
      argumentEnd =
        argument
          |> map _.shape
          |> Shape.fromFoldable
          |> getRightBound

      functionContinuation =
        Shape.rect
          { fill: "green"
          , stroke: "black"
          }
          { x: functionEnd
          , width: argumentEnd - functionEnd
          , height: lineHeight
          , y: linePadding + getLayoutY functionLayout slices
          }

      functionHead :: ScopedShape
      functionHead = NonEmptyArray.head function

      functionShapes :: RenderList
      functionShapes =
        NonEmptyArray.cons
          { scope: functionHead.scope
          , shape: functionContinuation
          , color: functionHead.color
          }
          function

      argumentHead :: ScopedShape
      argumentHead = NonEmptyArray.head argument

      resultShape :: ScopedShape
      resultShape =
        { scope: 0
        , shape:
          Shape.rect
            { fill: argumentHead.color
            , stroke: "black"
            }
            { x: argumentEnd
            , width: lineWidth
            , height: lineHeight
            , y: linePadding + getY 0 position slices
            }
        , color: argumentHead.color
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

freshColor :: forall r. Run ( state :: STATE RenderState | r ) String
freshColor = do
  state <- get
  case LazyList.uncons state.colors of
    Just { head, tail } -> do
      put { colors: tail }
      pure $ spy "Generated" head
    -- TODO: don't do stupid stuff like this
    Nothing -> pure "black"

-- | Get the y position based on an index and a (relative) position.
getY :: (Label "index" => Int) -> (Label "position" => Int) -> List.List YMapSlice -> Int
getY index position slices = unitHeight * units
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
runRenderM = evalState state >>> runReader ctx >>> extract
  where
  ctx :: RenderContext
  ctx =
    { doNotRender: Set.empty
    , start: 0
    , end: 0
    , slices: List.singleton mempty
    , colors: List.Nil
    }

  state :: RenderState
  state =
    { colors: colors
    }
