-- TODO: Document this
module Lunarflow.Render where

import Lunarlude
import Data.Array as Array
import Data.HashMap as HashMap
import Data.List as List
import Data.Typelevel.Num (d0, d1)
import Data.Vec (vec2, (!!))
import Debug.Trace (traceM)
import Lunarflow.Ast (AstF(..), Name)
import Lunarflow.Geometry.Types as Shape
import Lunarflow.Label (class Label)
import Lunarflow.LayoutM (ScopeId(..))
import Lunarflow.Renderer.Constants (callAngle, callAngleCosinus, callAngleSinus, callAngleTangent, callCircleColor, lineHeight, linePadding, lineTipWidth, lineWidth, unitHeight)
import Lunarflow.Renderer.WithHeight (YLayoutLike, YLayoutLikeF, YMeasures, totalHeight)
import Lunarflow.Vector as Vector
import NameMap as NameMap
import Run (Run, extract)
import Run.Except (EXCEPT, note, runExcept)
import Run.Reader (READER, ask, local, runReader)

data RenderingError
  = NotInScope Name
  | MissingScope ScopeId

type RenderContext
  = { xStart :: Int
    , starts :: NameMap.NameMap Int
    , scopes :: HashMap.HashMap ScopeId { measures :: YMeasures, y :: Int }
    }

type RenderM
  = Run
      ( reader :: READER RenderContext
      , except :: EXCEPT RenderingError
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
render :: forall v c a l. Tuple YMeasures (YLayoutLike v c a l) -> RenderM Shape.Shape
render (Tuple rootMeasures layout) =
  layout
    |> cata algebra
    |> local (_ { scopes = HashMap.singleton Root { measures: rootMeasures, y: 0 } })
    |> map ((\{ shapes, overlays } -> shapes <> overlays) >>> Shape.fromFoldable)
  where
  algebra :: Algebra (YLayoutLikeF v c a l) (RenderM RenderList)
  algebra (Var { position, inScope, name, color, freeTerms }) =
    ask
      >>= \{ xStart } ->
          withFreeVars xStart freeTerms do
            yOffset <- getYOffset inScope
            start <- getStart name
            y <- getY inScope position 1 <#> \y -> y + yOffset + linePadding
            traceM { position, name, y }
            let
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

  algebra (Lambda data'@{ args, heights, position, isRoot, inScope, scope } body) = do
    xStart <- ask <#> _.xStart
    yOffset <- getYOffset inScope
    updatedYOffset <- getY inScope position (totalHeight heights) <#> \y -> y + yOffset
    --
    bodyRenderList <-
      local
        ( updateContext
            { argCount
            , yOffset: updatedYOffset
            , start: xStart
            }
        )
        body
    --
    let
      height :: Int
      height = (totalHeight heights) * unitHeight

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
          { fill: "rgb(196,196,196, 0.12)"
          }
          { y: updatedYOffset
          , height
          , x: xStart
          , width
          }

      renderList :: RenderList
      renderList =
        { shapes: (if isRoot then [] else [ lambdaShape ]) <> bodyRenderList.shapes
        , overlays: bodyRenderList.overlays
        , color: bodyRenderList.color
        , lineY
        , maxX: xStart + width
        }
    pure renderList
    where
    argCount :: Int
    argCount = List.length args

    updateContext :: _ -> Endomorphism RenderContext
    updateContext { yOffset, start } ctx =
      ctx
        { scopes = HashMap.insert scope { measures: heights, y: yOffset } ctx.scopes
        , starts = NameMap.fromBound (Array.replicate argCount start) <> ctx.starts
        }

  algebra (Call { position, inScope } mkFunc mkArg) = do
    -- Get stuff from the environment
    yOffset <- getYOffset inScope
    --
    function <- mkFunc
    argument <- local _ { xStart = function.maxX } mkArg
    lineY <- getY inScope position 1 <#> \y -> y + yOffset + linePadding
    let
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
        Shape.circle { fill: callCircleColor }
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
  ScopeId ->
  (Label "position" => Int) ->
  (Label "height" => Int) -> RenderM Int
getY scope position height = do
  heights <- ask <#> _.scopes
  { measures } <- note (MissingScope scope) $ HashMap.lookup scope heights
  let
    (Tuple units left) =
      foldrWithIndex
        ( \index' height' result@(Tuple heightResult availibleResult) -> case compare position index' of
            LT -> result
            EQ -> Tuple heightResult height'
            GT -> Tuple (heightResult + height') availibleResult
        )
        (Tuple 0 1)
        $ unwrap measures
  pure $ unitHeight * units + unitHeight / 2 * (left - height)

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
getYOffset :: ScopeId -> RenderM Int
getYOffset scope = ask >>= (_.scopes >>> HashMap.lookup scope >>> note (MissingScope scope) >>> map _.y)

getStart :: Name -> RenderM Int
getStart name = ask >>= (_.starts >>> NameMap.lookup name >>> note (NotInScope name))

-- | Introduce a bunch of free variables into scope
withFreeVars :: forall a. Int -> HashMap.HashMap String a -> RenderM ~> RenderM
withFreeVars start vars m =
  flip local m \ctx ->
    ctx
      { starts = ctx.starts <> NameMap.fromFree (start <$ vars)
      }

-- | Run a computation in the render monad.
runRenderM :: forall a. RenderM a -> Either RenderingError a
runRenderM = runReader ctx >>> runExcept >>> extract
  where
  ctx :: RenderContext
  ctx =
    { xStart: 0
    , starts: mempty
    , scopes: mempty
    }

--------- Typeclass instances
instance showRenderingError :: Show RenderingError where
  show (NotInScope name) = show name <> " not in scope"
  show (MissingScope scope) = "Cannot find data related to scope " <> show scope
