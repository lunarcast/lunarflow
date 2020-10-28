-- TODO: Document this
module Lunarflow.Render where

import Prelude
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Int (floor, toNumber)
import Data.List as List
import Data.List.Lazy as LazyList
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Ord (abs)
import Data.Set as Set
import Data.Traversable (sequence, sum)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (d0, d1)
import Data.Unfoldable (replicate)
import Data.Vec (vec2, (!!))
import Debug.Trace (trace, traceM)
import Lunarflow.Ast (AstF(..), isVar, lambda)
import Lunarflow.Geometry.Foreign (getRightBound)
import Lunarflow.Geometry.Foreign as ForeignShape
import Lunarflow.Geometry.Types (Bounds)
import Lunarflow.Geometry.Types as Shape
import Lunarflow.Label (class Label)
import Lunarflow.Pipe ((|>))
import Lunarflow.Renderer.Constants (callAngle, callAngleCosinus, callAngleSinus, callAngleTangent, colors, lineHeight, linePadding, lineWidth, unitHeight)
import Lunarflow.Renderer.WithHeight (YLayout, YLayoutF, YMeasures)
import Lunarflow.Vector as Vector
import Matryoshka (GAlgebra, para, project)
import Run (Run, extract)
import Run.Reader (READER, ask, local, runReader)
import Run.State (STATE, evalState, get, put)

type RenderContext
  = { doNotRender :: Set.Set Int
    , start :: Int
    , end :: Int
    , slices :: List.List YMeasures
    , colors :: List.List String
    , yOffsets :: List.List Int
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
    , y :: Int
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
render :: forall r. Tuple YLayout YMeasures -> RenderM r Shape.Shape
render (Tuple layout rootMeasures) =
  layout
    |> para algebra
    |> local (_ { slices = List.singleton rootMeasures })
    |> map (map _.shape >>> Shape.fromFoldable)
    |> map (Shape.Translate (vec2 5 10))
  where
  algebra :: GAlgebra (Tuple YLayout) YLayoutF (RenderM r RenderList)
  algebra (Lambda data'@{ args, heights, position } (Tuple bodyLayout body)) = do
    slices <- ask <#> _.slices
    yOffset <- getYOffset 0
    --
    traceM $ "Function  w " <> show (List.length args) <> " arguments"
    let
      argCount = List.length args

      updatedYOffset = yOffset + getLayoutY (lambda data' bodyLayout) slices
    --
    newColors <- sequence $ replicate argCount freshColor
    rawBodyShapes <-
      local
        ( updateContext
            { argCount
            , newColors
            , yOffset: updatedYOffset
            }
        )
        body
    let
      bodyShapes = inScope argCount rawBodyShapes

      bodyHead = Array.head bodyShapes.yes
    --
    color <- maybe freshColor (_.color >>> pure) bodyHead
    --
    let
      shapesInScope :: Array Shape.Shape
      shapesInScope = _.shape <$> bodyShapes.yes

      maybeBounds :: Maybe Bounds
      maybeBounds = ForeignShape.bounds $ Shape.fromFoldable shapesInScope

      y :: Int
      y =
        maybe
          ( updatedYOffset + ((argCount - 1) * unitHeight + linePadding)
              / 2
          )
          _.y
          bodyHead

      -- y = maybe 30 _.y bodyHead
      functionShape = case maybeBounds of
        Just bounds ->
          Shape.rect
            { stroke: color
            , weight: 5.0
            }
            $ bounds
                { y = updatedYOffset
                , height = bounds.height + 2 * linePadding
                }
        Nothing ->
          Shape.rect
            { stroke: color
            , weight: 5.0
            }
            $ { x: 0
              , y: updatedYOffset
              , height: argCount * unitHeight
              , width:
                rawBodyShapes
                  |> map _.shape
                  |> Shape.fromFoldable
                  |> getRightBound
                  |> fromMaybe lineWidth
              }

      result :: ScopedShape
      result =
        { shape:
          Shape.group {}
            $ Array.cons functionShape shapesInScope
        , scope: 0
        , color
        , y
        }
    pure $ NonEmptyArray.cons' result $ shiftScope argCount <$> bodyShapes.no
    where
    updateContext { argCount, newColors, yOffset } ctx =
      ctx
        { slices = (replicate argCount heights) <> ctx.slices
        , doNotRender = ((+) argCount) `Set.map` ctx.doNotRender
        , colors = newColors <> ctx.colors
        , yOffsets = replicate argCount yOffset <> ctx.yOffsets
        }

  algebra (Var { position, index }) = do
    { end, start, slices, doNotRender, colors } <- ask
    yOffset <- getYOffset index
    -- TODO: don't do stupid stuff like this
    let
      color = fromMaybe "black" (List.index colors index)

      y = yOffset + linePadding + getY index position 1 slices
    pure
      $ NonEmptyArray.singleton
          if (Set.member index doNotRender) then
            { scope: 0
            , color
            , shape: Shape.Null
            , y
            }
          else
            { scope: index
            , color
            , y
            , shape:
              Shape.rect
                { fill: color
                }
                { x: 0
                , y
                , height: lineHeight
                , width: max lineWidth start
                }
            }

  algebra (Call position mkFunc@(Tuple functionLayout _) mkArg@(Tuple argumentLayout _)) = do
    function <- renderFn 0 mkFunc
    slices <- ask <#> _.slices
    yOffset <- getYOffset 0
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
            |> fromMaybe 0
    argument <- renderFn functionEnd mkArg
    let
      argumentEnd :: Int
      argumentEnd =
        argument
          |> map _.shape
          |> Shape.fromFoldable
          |> getRightBound
          |> fromMaybe 0

      functionHead :: ScopedShape
      functionHead = NonEmptyArray.head function

      argumentHead :: ScopedShape
      argumentHead = NonEmptyArray.head argument

      y :: Int
      y = yOffset + linePadding + getY 0 position 1 slices

      sameDirection :: Boolean
      sameDirection = compare argumentHead.y functionHead.y == compare functionHead.y y

      diagonalHeight :: Int
      diagonalHeight = floor $ toNumber lineHeight * callAngleCosinus

      diagonalWidth :: Int
      diagonalWidth = floor $ toNumber lineHeight * callAngleSinus

      middleY :: Int
      middleY = functionHead.y + (lineHeight - diagonalHeight) / 2

      up :: Boolean
      up = functionHead.y > argumentHead.y

      diagonal =
        mkDiagonal
          { tanAngle: callAngleTangent
          , diagonalWidth: lineHeight
          , x: argumentEnd
          , y0: argumentHead.y
          , y1:
            if not sameDirection then
              functionHead.y
            else
              if up then middleY - diagonalHeight else middleY
          }

      continuationWidth :: Int
      continuationWidth = diagonal.x1 + diagonalWidth / 2 - functionEnd

      functionContinuation :: Shape.Shape
      functionContinuation =
        Shape.rect
          { fill: functionHead.color
          }
          { x: functionEnd
          , width: continuationWidth
          , height: lineHeight
          , y: functionHead.y
          }

      callCircle :: ScopedShape
      callCircle =
        { scope: functionHead.scope
        , color: functionHead.color
        , y: functionHead.y
        , shape:
          Shape.circle { fill: "green" }
            { x: functionEnd + continuationWidth
            , y: functionHead.y + lineHeight / 2
            , radius: floor $ toNumber lineHeight * 0.7
            }
        }

      functionShapes :: RenderList
      functionShapes =
        NonEmptyArray.cons
          { scope: functionHead.scope
          , shape: functionContinuation
          , color: functionHead.color
          , y: functionHead.y
          }
          function

      diagonal' =
        mkDiagonal
          { tanAngle: callAngleTangent
          , diagonalWidth: lineHeight
          , x: diagonal.x1 + diagonal.delta !! d0
          , y0:
            if not sameDirection then
              functionHead.y
            else
              if up then middleY else middleY - diagonalHeight
          , y1: y
          }

      callDiagonal :: ScopedShape
      callDiagonal =
        { scope: max argumentHead.scope functionHead.scope
        , y
        , color: functionHead.color
        , shape:
          Shape.group {}
            $ [ Shape.polygon { fill: argumentHead.color }
                  diagonal.points
              ]
            <> if argumentHead.y == functionHead.y then
                []
              else
                [ Shape.polygon
                    { fill: functionHead.color
                    }
                    diagonal'.points
                ]
        }

      resultShape :: ScopedShape
      resultShape =
        { scope: 0
        , shape:
          Shape.rect
            { fill: functionHead.color
            }
            { x: diagonal'.x1
            , width: lineWidth
            , height: lineHeight
            , y
            }
        , color: functionHead.color
        , y
        }
    pure $ flip NonEmptyArray.snoc callCircle
      $ NonEmptyArray.cons' callDiagonal [ resultShape ]
      <> argument
      <> functionShapes
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
      pure head
    -- TODO: don't do stupid stuff like this
    Nothing -> pure "black"

-- | Given a point on a call diagonal, calculate the position on the other side
callDiagonalOpposite :: Boolean -> Vector.Vec2 -> Vector.Vec2
callDiagonalOpposite up = Vector.add offset
  where
  offset = Vector.rotate vertical angle

  vertical = vec2 0 if up then lineHeight else -lineHeight

  angle = direction * callAngle

  direction = if up then 1.0 else -1.0

-- | Get the y position based on an index and a (relative) position.
getY ::
  (Label "index" => Int) ->
  (Label "position" => Int) ->
  (Label "height" => Int) ->
  List.List YMeasures -> Int
getY index position height slices =
  trace
    { units
    , left
    , index
    , position
    , slices
    , height
    } \_ -> unitHeight * units + unitHeight / 2 * (left - height)
  where
  (Tuple units left) =
    foldrWithIndex
      ( \index' height' result@(Tuple heightResult availibleResult) -> case compare position index' of
          LT -> result
          EQ -> Tuple heightResult height'
          GT -> Tuple (heightResult + height') availibleResult
      )
      (Tuple 0 1)
      $ fromMaybe []
      $ List.index slices index

mkDiagonal ::
  { y0 :: Int
  , y1 :: Int
  , tanAngle :: Number
  , diagonalWidth :: Int
  , x :: Int
  } ->
  { points :: Array Vector.Vec2
  , x1 :: Int
  , offset :: Vector.Vec2
  , delta :: Vector.Vec2
  }
mkDiagonal { y0, y1, x, tanAngle, diagonalWidth } = { points, x1: end' !! d0, offset, delta }
  where
  up :: Boolean
  up = y1 > y0

  start :: Vector.Vec2
  start = vec2 x (y0 + if up then 0 else diagonalWidth)

  start' :: Vector.Vec2
  start' = callDiagonalOpposite up start

  delta :: Vector.Vec2
  delta = Vector.sub (vec2 x $ y0 + if up then lineHeight else 0) start'

  offset :: Vector.Vec2
  offset = vec2 offsetX offsetY
    where
    offsetX = floor $ toNumber (abs offsetY) / tanAngle

    offsetY = (delta !! d1) + y1 - y0

  end' :: Vector.Vec2
  end' = Vector.add start' offset

  points :: Array Vector.Vec2
  points =
    [ start
    , start'
    , end'
    , Vector.add start offset
    ]

-- | Retrive an arbitrary y offset from the current environment
getYOffset :: forall r. Int -> RenderM r Int
getYOffset at = ask <#> (_.yOffsets >>> flip List.index at >>> fromMaybe 0)

-- | Get the y position from a YLayout by inferring the correct scope. 
getLayoutY :: YLayout -> List.List YMeasures -> Int
getLayoutY layout slices = case project layout of
  Var { index, position } -> getY index position 1 slices
  Call position _ _ -> getY 0 position 1 slices
  Lambda { position, heights } _ -> getY 0 position (sum heights) slices

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
    , yOffsets: List.Nil
    }

  state :: RenderState
  state =
    { colors: colors
    }
