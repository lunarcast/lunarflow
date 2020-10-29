-- TODO: Document this
module Lunarflow.Render where

import Prelude
import Control.MonadZero (guard)
import Data.Array as Arary
import Data.Array as Array
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Int (floor, toNumber)
import Data.List as List
import Data.List.Lazy as LazyList
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.Traversable (sequence, sum)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (d0, d1)
import Data.Vec (vec2, (!!))
import Lunarflow.Ast (AstF(..))
import Lunarflow.Geometry.Types as Shape
import Lunarflow.Label (class Label)
import Lunarflow.Pipe ((|>))
import Lunarflow.Renderer.Constants (callAngle, callAngleCosinus, callAngleSinus, callAngleTangent, colors, lineHeight, linePadding, lineTipWidth, lineWidth, unitHeight)
import Lunarflow.Renderer.WithHeight (YLayout, YLayoutF, YMeasures)
import Lunarflow.Vector as Vector
import Matryoshka (Algebra, cata)
import Run (Run, extract)
import Run.Reader (READER, ask, local, runReader)
import Run.State (STATE, evalState, get, put)

type RenderContext
  = { starts :: Array Int
    , xStart :: Int
    , slices :: Array YMeasures
    , colors :: Array String
    , yOffsets :: Array Int
    }

type RenderState
  = { colors :: LazyList.List String
    }

type RenderM
  = Run
      ( reader :: READER RenderContext
      , state :: STATE RenderState
      )

type RenderList
  = { shapes :: Array Shape.Shape
    , overlays :: Array Shape.Shape
    , color :: String
    , lineY :: Int
    , maxX :: Int
    }

-- TODO: error handling
-- | Renders a layout using the Render monad.
render :: Tuple YLayout YMeasures -> RenderM Shape.Shape
render (Tuple layout rootMeasures) =
  layout
    |> cata algebra
    |> local (_ { slices = [ rootMeasures ] })
    |> map ((\{ shapes, overlays } -> shapes <> overlays) >>> Shape.fromFoldable)
    |> map (Shape.Translate (vec2 5 10))
  where
  algebra :: Algebra YLayoutF (RenderM RenderList)
  algebra (Lambda data'@{ args, heights, position } body) = do
    xStart <- ask <#> _.xStart
    slices <- ask <#> _.slices
    yOffset <- getYOffset 0
    --
    let
      argCount = List.length args

      updatedYOffset = yOffset + getY 0 position (sum heights) slices
    --
    newColors <- sequence $ Array.replicate argCount freshColor
    bodyRenderList <-
      local
        ( updateContext
            { argCount
            , newColors
            , yOffset:
              updatedYOffset
            , start: xStart
            }
        )
        body
    --
    let
      height :: Int
      height = (sum heights) * unitHeight

      lineY :: Int
      lineY
        | bodyRenderList.lineY > updatedYOffset + height
            || bodyRenderList.lineY
            < updatedYOffset =
          updatedYOffset + ((argCount - 1) * unitHeight + linePadding)
            / 2
        | otherwise = bodyRenderList.lineY

      width = max (bodyRenderList.maxX - xStart) lineWidth

      lambdaShape :: Shape.Shape
      lambdaShape =
        Shape.rect
          { stroke: bodyRenderList.color
          , weight: 5.0
          }
          { y: updatedYOffset
          , height
          , x: xStart
          , width
          }

      renderList :: RenderList
      renderList =
        { shapes: bodyRenderList.shapes
        , overlays: [ lambdaShape ] <> bodyRenderList.overlays
        , color: bodyRenderList.color
        , lineY
        , maxX: xStart + width
        }
    pure renderList
    where
    updateContext { argCount, newColors, yOffset, start } ctx =
      ctx
        { slices = (Arary.replicate argCount heights) <> ctx.slices
        , colors = newColors <> ctx.colors
        , yOffsets = Array.replicate argCount yOffset <> ctx.yOffsets
        , starts = Array.replicate argCount start <> ctx.starts
        }

  algebra (Var { position, index }) = do
    { xStart, slices, colors } <- ask
    yOffset <- getYOffset index
    start <- getStart index
    -- TODO: don't do stupid stuff like this
    let
      color = fromMaybe "black" (Array.index colors index)

      y = yOffset + linePadding + getY index position 1 slices

      width = max lineWidth (xStart - start)
    pure
      { color
      , lineY: y
      , maxX: start + width
      , overlays: []
      , shapes:
        [ Shape.rect
            { fill: color
            }
            { x: start
            , y
            , height: lineHeight
            , width: width
            }
        ]
      }

  algebra (Call position mkFunc mkArg) = do
    -- Get stuff from the environment
    slices <- ask <#> _.slices
    yOffset <- getYOffset 0
    --
    function <- mkFunc
    argument <- local _ { xStart = function.maxX } mkArg
    let
      lineY :: Int
      lineY = yOffset + linePadding + getY 0 position 1 slices

      sameDirection :: Boolean
      sameDirection = compare argument.lineY function.lineY == compare function.lineY lineY

      diagonalHeight :: Int
      diagonalHeight = floor $ toNumber lineHeight * callAngleCosinus

      diagonalWidth :: Int
      diagonalWidth = floor $ toNumber lineHeight * callAngleSinus

      middleY :: Int
      middleY = function.lineY + (lineHeight - diagonalHeight) / 2

      up :: Boolean
      up = function.lineY > argument.lineY

      diagonal =
        mkDiagonal
          { tanAngle: callAngleTangent
          , diagonalWidth: lineHeight
          , x: argument.maxX
          , y0: argument.lineY
          , y1:
            if not sameDirection then
              function.lineY
            else
              if up then middleY - diagonalHeight else middleY
          }

      continuationWidth :: Int
      continuationWidth = diagonal.x1 + diagonalWidth / 2 - function.maxX

      functionContinuation :: Shape.Shape
      functionContinuation =
        Shape.rect
          { fill: function.color
          }
          { x: function.maxX
          , width: continuationWidth
          , height: lineHeight
          , y: function.lineY
          }

      callCircle :: Shape.Shape
      callCircle =
        Shape.circle { fill: "green" }
          { x: function.maxX + continuationWidth
          , y: function.lineY + lineHeight / 2
          , radius: floor $ toNumber lineHeight * 0.7
          }

      functionShapes :: Array Shape.Shape
      functionShapes = Array.cons functionContinuation function.shapes

      diagonal' =
        mkDiagonal
          { tanAngle: callAngleTangent
          , diagonalWidth: lineHeight
          , x: diagonal.x1 + diagonal.delta !! d0
          , y0:
            if not sameDirection then
              function.lineY
            else
              if up then middleY else middleY - diagonalHeight
          , y1: lineY
          }

      argumentDiagonal :: Array Shape.Shape
      argumentDiagonal = do
        guard $ argument.lineY /= function.lineY
        pure
          $ Shape.polygon
              { fill: argument.color
              }
              diagonal.points

      callDiagonal :: Shape.Shape
      callDiagonal =
        Shape.group {}
          $ argumentDiagonal
          <> [ Shape.polygon { fill: function.color }
                diagonal'.points
            ]

      resultShape :: Shape.Shape
      resultShape =
        Shape.rect
          { fill: function.color
          }
          { x: diagonal'.x1
          , width: lineTipWidth
          , height: lineHeight
          , y: lineY
          }

      renderList :: RenderList
      renderList =
        { color: function.color
        , lineY
        , maxX: diagonal'.x1 + lineTipWidth
        , shapes: argument.shapes <> [ callDiagonal, resultShape ] <> functionShapes
        , overlays: [ callCircle ] <> argument.overlays <> function.overlays
        }
    pure renderList

freshColor :: forall r. Run ( state :: STATE RenderState | r ) String
freshColor = do
  state <- get
  case LazyList.uncons state.colors of
    Just { head, tail } -> do
      put state { colors = tail }
      pure head
    -- TODO: don't do stupid stuff like this
    Nothing -> pure "black"

getStart :: forall r. Int -> Run ( reader :: READER RenderContext | r ) Int
getStart x = ado
  starts <- ask <#> _.starts
  in case Array.index starts x of
    Just r -> r
    Nothing -> 0

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
  Array YMeasures -> Int
getY index position height slices = unitHeight * units + unitHeight / 2 * (left - height)
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
      $ Array.index slices index

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
getYOffset :: Int -> RenderM Int
getYOffset at = ask <#> (_.yOffsets >>> flip Array.index at >>> fromMaybe 0)

-- | Run a computation in the render monad.
runRenderM :: forall a. RenderM a -> a
runRenderM = evalState state >>> runReader ctx >>> extract
  where
  ctx :: RenderContext
  ctx =
    { slices: []
    , colors: []
    , yOffsets: []
    , starts: []
    , xStart: 0
    }

  state :: RenderState
  state =
    { colors: colors
    }
