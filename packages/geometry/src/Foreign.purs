module Lunarflow.Geometry.Foreign
  ( Geometry
  , fromShape
  , renderGeometry
  , bounds
  , getRightBound
  ) where

import Prelude
import Effect (Effect)
import Graphics.Canvas (Context2D)
import Lunarflow.Geometry.Types (CommonAttribs', PolygonAttribs, Position, Shape(..), Bounds)

-- | @thi.ng/geom Geometry representation.
foreign import data Geometry :: Type

-- | Cast a purescript shape to a js geometry.
fromShape :: Shape -> Geometry
fromShape = case _ of
  Rect attribs bounds' -> mkRect attribs bounds'
  Circle attribs position radius -> mkCircle attribs position radius
  Polygon attribs points -> mkPolygon attribs points
  Group attribs shapes -> mkGroup attribs (fromShape <$> shapes)

-- | Find the smallest rect some shapes fit in.
bounds :: Shape -> Bounds
bounds = boundsImpl <<< fromShape

-- TODO: more efficient way
-- | Get the rightmost point in a shape 
getRightBound :: Shape -> Int
getRightBound = (\{ width, x } -> x + width) <<< bounds

foreign import mkRect :: CommonAttribs' -> Bounds -> Geometry

foreign import mkCircle :: CommonAttribs' -> { | Position () } -> Int -> Geometry

foreign import mkPolygon :: CommonAttribs' -> PolygonAttribs -> Geometry

foreign import mkGroup :: CommonAttribs' -> Array Geometry -> Geometry

foreign import renderGeometry :: Geometry -> Context2D -> Effect Unit

foreign import boundsImpl :: Geometry -> Bounds
 -- foreign import geometryToRectImpl :: Partial => Geometry -> CommonAttribs' -> Position' -> Int -> Int -> Shape