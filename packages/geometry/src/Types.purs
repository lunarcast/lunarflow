-- | Types and constructors for the @thi.ng/geom bindings.
module Lunarflow.Geometry.Types where

import Prelude
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Typelevel.Num (D6)
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Closed as Closed
import Data.Vec (Vec)
import Lunarflow.Vector (Vec2)
import Math (Radians)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

-- | Transform matrices are just vectors with 6 elements
type TransformMatrix
  = Vec D6 Int

-- | Attributes all the shapes can accept
type CommonAttribs
  = { fill :: Opt String
    , stroke :: Opt String
    , transform :: Opt TransformMatrix
    , setTransform :: Opt TransformMatrix
    , translate :: Opt Vec2
    , scale :: Opt Vec2
    , rotate :: Opt Radians
    , alpha :: Opt Number
    }

type Position r
  = ( x :: Int, y :: Int
    | r
    )

type Position'
  = { | Position () }

type ShapeConstructor a
  = forall r. Closed.Coerce r CommonAttribs => r -> a

-- | Rect-like shape data.
type Bounds
  = { x :: Int, y :: Int, height :: Int, width :: Int }

type PolygonAttribs
  = Array Vec2

type CircleAttribs
  = Record (Position + ( radius :: Int ))

type LineAttribs
  = { from :: Vec2
    , to :: Vec2
    }

-- | The base functor for Shape.
data Shape
  = Rect CommonAttribs Bounds
  | Polygon CommonAttribs PolygonAttribs
  | Circle CommonAttribs CircleAttribs
  | Group CommonAttribs (Array Shape)
  | Line CommonAttribs LineAttribs
  -- NOTE: this is here to circumvent a bug in thi.ng/geom
  -- which causes the bounds function to ignore transforms on groups.
  | Translate Vec2 Shape
  | Null

-- Constructors
rect :: ShapeConstructor (Bounds -> Shape)
rect attribs = Rect (Closed.coerce attribs)

polygon :: ShapeConstructor (PolygonAttribs -> Shape)
polygon attribs = Polygon (Closed.coerce attribs)

circle :: ShapeConstructor (CircleAttribs -> Shape)
circle attribs = Circle (Closed.coerce attribs)

line :: ShapeConstructor (LineAttribs -> Shape)
line attribs = Line (Closed.coerce attribs)

group :: ShapeConstructor (Array Shape -> Shape)
group attribs = Group (Closed.coerce attribs)

defaultAttribs :: CommonAttribs
defaultAttribs = Closed.coerce {}

fromFoldable :: forall f. Foldable f => f (Shape) -> Shape
fromFoldable = Array.fromFoldable >>> go
  where
  go [] = Null

  go [ v ] = v

  go more = Group (unsafeCoerce {}) more
