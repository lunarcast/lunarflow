module Lunarflow.Geometry.Foreign
  ( Geometry
  , fromShape
  , renderGeometry
  ) where

import Prelude
import Effect (Effect)
import Graphics.Canvas (Context2D)
import Lunarflow.Geometry.Types (CommonAttribs, PolygonAttribs, Position, Shape(..))

-- | @thi.ng/geom Geometry representation.
foreign import data Geometry :: Type

-- | Cast a purescript shape to a js geometry.
fromShape :: Shape -> Geometry
fromShape = case _ of
  Rect attribs position width height -> mkRect attribs position width height
  Circle attribs position radius -> mkCircle attribs position radius
  Polygon attribs points -> mkPolygon attribs points
  Group attribs shapes -> mkGroup attribs (fromShape <$> shapes)

foreign import mkRect :: { | CommonAttribs () } -> { | Position () } -> Int -> Int -> Geometry

foreign import mkCircle :: { | CommonAttribs () } -> { | Position () } -> Int -> Geometry

foreign import mkPolygon :: { | CommonAttribs () } -> PolygonAttribs -> Geometry

foreign import mkGroup :: { | CommonAttribs () } -> Array Geometry -> Geometry

foreign import renderGeometry :: Geometry -> Context2D -> Effect Unit
