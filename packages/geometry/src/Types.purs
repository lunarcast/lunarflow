module Lunarflow.Geometry.Types where

import Prelude
import Data.Debug (class Debug, genericDebug)
import Data.Generic.Rep (class Generic)
import Lunarflow.Row (class PartialRow, withDefaults)
import Type.Row (type (+))

type CommonAttribs r
  = ( fill :: String
    , stroke :: String
    | r
    )

type Position r
  = ( x :: Int, y :: Int
    | r
    )

type ShapeConstructor a
  = forall r. PartialRow (CommonAttribs ()) r => Record r -> a

newtype PolygonAttribs
  = PolygonAttribs (Array (Record (Position + ())))

derive instance genericPolygonAttribs :: Generic PolygonAttribs _

instance debugPolygonAttribs :: Debug PolygonAttribs where
  debug = genericDebug

data Shape
  = Rect { | CommonAttribs () } { | Position () } Int Int
  | Polygon { | CommonAttribs () } PolygonAttribs
  | Circle { | CommonAttribs () } { | Position () } Int
  | Group { | CommonAttribs () } (Array Shape)

mergeAttribs :: { | CommonAttribs () } -> { | CommonAttribs () } -> { | CommonAttribs () }
mergeAttribs a b = { fill: b.fill, stroke: b.stroke }

instance shapeSemigroup :: Semigroup Shape where
  append (Group attribs shapes) (Group attribs' shapes') = Group (mergeAttribs attribs attribs') (shapes <> shapes')
  append (Group attribs shapes) shape = Group attribs (shapes <> [ shape ])
  append shape other@(Group _ _) = append other shape
  append a b = Group defaultAttribs [ a, b ]

instance monoidSemigroup :: Monoid Shape where
  mempty = Group defaultAttribs []

derive instance genericShape :: Generic Shape _

instance debugSemigruoup :: Debug Shape where
  debug a = genericDebug a

rect :: ShapeConstructor ({ | Position () } -> Int -> Int -> Shape)
rect attribs = Rect (withDefaults defaultAttribs attribs)

polygon :: ShapeConstructor (PolygonAttribs -> Shape)
polygon attribs = Polygon (withDefaults defaultAttribs attribs)

circle :: ShapeConstructor ({ | Position () } -> Int -> Shape)
circle attribs = Circle (withDefaults defaultAttribs attribs)

group :: ShapeConstructor (Array Shape -> Shape)
group attribs = Group (withDefaults defaultAttribs attribs)

defaultAttribs :: { | CommonAttribs () }
defaultAttribs = { fill: "blue", stroke: "black" }
