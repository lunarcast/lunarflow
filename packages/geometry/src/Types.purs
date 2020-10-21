-- | Types and constructors for the @thi.ng/geom bindings.
module Lunarflow.Geometry.Types where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Typelevel.Num (D2, D6)
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Closed as Closed
import Data.Vec (Vec)
import Lunarflow.Mu (Mu)
import Math (Radians)
import Matryoshka (class Corecursive, embed)
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

-- | The base functor for Shape.
data ShapeF a
  = Rect CommonAttribs Bounds
  | Polygon CommonAttribs PolygonAttribs
  | Circle CommonAttribs CircleAttribs
  | Group CommonAttribs (Array a)

-- | Type for stuff we can render using thi.ng/hiccup-canvas.
type Shape
  = Mu ShapeF

-- Constructors
rect :: forall t. Corecursive t ShapeF => ShapeConstructor (Bounds -> t)
rect attribs = embed <<< Rect (Closed.coerce attribs)

polygon :: forall t. Corecursive t ShapeF => ShapeConstructor (PolygonAttribs -> t)
polygon attribs = embed <<< Polygon (Closed.coerce attribs)

circle :: forall t. Corecursive t ShapeF => ShapeConstructor (CircleAttribs -> t)
circle attribs = embed <<< Circle (Closed.coerce attribs)

group :: forall t. Corecursive t ShapeF => ShapeConstructor (Array t -> t)
group attribs = embed <<< Group (Closed.coerce attribs)

defaultAttribs :: CommonAttribs
defaultAttribs = Closed.coerce {}

derive instance genericShape :: Generic (ShapeF a) _

derive instance functorShape :: Functor ShapeF
