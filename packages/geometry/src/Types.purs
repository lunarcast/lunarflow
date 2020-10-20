-- | Types and constructors for the @thi.ng/geom bindings.
module Lunarflow.Geometry.Types where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Typelevel.Num (D2, D6)
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Closed as Closed
import Data.Vec (Vec)
import Math (Radians)
import Type.Row (type (+))

-- | Shorthand for vec 2s of ints
type Vec2
  = Vec D2 Int

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
    , alpha :: Opt Int
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
  = Array (Record (Position + ()))

type CircleAttribs
  = Record (Position + ( radius :: Int ))

data Shape
  = Rect CommonAttribs Bounds
  | Polygon CommonAttribs PolygonAttribs
  | Circle CommonAttribs CircleAttribs
  | Group CommonAttribs (Array Shape)

-- Constructors
rect :: ShapeConstructor (Bounds -> Shape)
rect attribs = Rect (Closed.coerce attribs)

polygon :: ShapeConstructor (PolygonAttribs -> Shape)
polygon attribs = Polygon (Closed.coerce attribs)

circle :: ShapeConstructor (CircleAttribs -> Shape)
circle attribs = Circle (Closed.coerce attribs)

group :: ShapeConstructor (Array Shape -> Shape)
group attribs = Group (Closed.coerce attribs)

defaultAttribs :: CommonAttribs
defaultAttribs = Closed.coerce {}

-- Typeclass instances:
-- NOTE: in case we want to go back to the weirder but more efficient 
-- version we can look back to the 0aa7dde39324816a8a92e89d368ca9278f593d45 commit.
instance shapeSemigroup :: Semigroup Shape where
  append a b = Group defaultAttribs [ a, b ]

instance shapeMonoid :: Monoid Shape where
  mempty = Group defaultAttribs []

derive instance genericShape :: Generic Shape _
