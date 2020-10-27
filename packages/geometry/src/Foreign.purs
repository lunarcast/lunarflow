module Lunarflow.Geometry.Foreign
  ( Geometry
  , fromShape
  , renderGeometry
  , bounds
  , getRightBound
  ) where

import Prelude
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Graphics.Canvas (Context2D)
import Lunarflow.Geometry.Types (Bounds, CircleAttribs, CommonAttribs, PolygonAttribs, Shape(..), Vec2)

-- | @thi.ng/geom Geometry representation.
foreign import data Geometry :: Type

-- | Cast a purescript shape to a js geometry.
fromShape :: Shape -> Geometry
fromShape = case _ of
  Rect attribs bounds' -> mkRect attribs bounds'
  Circle attribs data' -> mkCircle attribs data'
  Polygon attribs points -> mkPolygon attribs points
  Group attribs shapes -> mkGroup attribs $ fromShape <$> shapes
  Translate amount shape -> runFn2 translate (fromShape shape) amount
  Null -> nullGeometry

-- | Find the smallest rect some shapes fit in.
bounds :: Shape -> Bounds
bounds = fromShape >>> boundsImpl

-- TODO: more efficient way
-- | Get the rightmost point in a shape 
getRightBound :: Shape -> Int
getRightBound = (\{ width, x } -> x + width) <<< bounds

foreign import mkRect :: CommonAttribs -> Bounds -> Geometry

foreign import mkCircle :: CommonAttribs -> CircleAttribs -> Geometry

foreign import mkPolygon :: CommonAttribs -> PolygonAttribs -> Geometry

foreign import mkGroup :: CommonAttribs -> Array Geometry -> Geometry

foreign import nullGeometry :: Geometry

foreign import renderGeometry :: Geometry -> Context2D -> Effect Unit

foreign import translate :: Fn2 Geometry Vec2 Geometry

foreign import boundsImpl :: Geometry -> Bounds
 -- foreign import geometryToRectImpl :: Partial => Geometry -> CommonAttribs -> Position' -> Int -> Int -> Shape