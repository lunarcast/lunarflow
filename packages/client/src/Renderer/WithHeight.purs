module Lunarflow.Renderer.WithHeight
  ( YLayoutLikeF
  , YLayoutLike
  , YMeasures
  , withHeights
  , totalHeight
  ) where

import Lunarlude
import Data.Array as Array
import Data.HashMap as HashMap
import Lunarflow.Array (maxZip)
import Lunarflow.Ast (AstF(..), call, lambda, var)
import Lunarflow.Layout (LayoutLike, LayoutLikeF)
import Lunarflow.LayoutM (Binder)
import Prim.Row as Row
import Record as Record

-- TODO: clean this up
-- | The base functor for YLayouts.
type YLayoutLikeF v c a l
  = LayoutLikeF Int v c a ( isRoot :: Boolean, heights :: YMeasures | l )

-- | YLayouts are layouts which keep track of the maximum height of each line
type YLayoutLike v c a l
  = Mu (YLayoutLikeF v c a l)

-- | An array which holds the max size of each row.
-- | 
-- | Eg: [1, 2, 3] means we have 
-- |    a row of height 1, 
-- |    above a row of height 2, 
-- |    above a row of height 3 
-- | 
-- | The reason we made a newtype for this is the custom Semigroup instance.
newtype YMeasures
  = YMeasures (Array Int)

-- | Add height data to a layout.
withHeights ::
  forall v c a l.
  Row.Lacks "heights" l =>
  LayoutLike Int v c a ( isRoot :: Boolean | l ) ->
  Tuple YMeasures (YLayoutLike v c a l)
withHeights =
  cata case _ of
    -- The heights of vars will be handled inside the lambda which binds them
    Var varData -> Tuple (fromFreeTerms varData.freeTerms) $ var varData
    -- To measure calls we just merge the measures of the function and argument and add one more row for the result.
    Call callData (Tuple functionMeasures function) (Tuple argumentMeasures argument) ->
      Tuple measures
        $ call callData function argument
      where
      measures =
        createMeasure { position: callData.position, height: 1 } -- The row for the result
          <> functionMeasures
          <> argumentMeasures
          <> fromFreeTerms callData.freeTerms
    Lambda lambdaData (Tuple nonVarMeasures body) ->
      Tuple measures $ flip lambda body
        $ Record.insert _heights bodyMeasures lambdaData
      where
      argumentMeasures =
        lambdaData.args
          <#> \{ position } -> createMeasure { position, height: 1 }

      bodyMeasures = nonVarMeasures <> fold argumentMeasures

      measures =
        createMeasure
          { position: lambdaData.position
          , height: totalHeight bodyMeasures + 2
          }

-- | Make a value of type YMeasures which holds a measure of an arbitrary height at an arbitrary position
createMeasure :: { position :: Int, height :: Int } -> YMeasures
createMeasure { position, height } = YMeasures $ Array.snoc (Array.replicate position 0) height

-- | Measure the total height a bunch of rows will occupy
totalHeight :: YMeasures -> Int
totalHeight = unwrap >>> sum

-- | Maeasure a map of free variables
fromFreeTerms :: HashMap.HashMap String (Binder Int) -> YMeasures
fromFreeTerms =
  foldrWithIndex
    (\index { position } previous -> createMeasure { position, height: 1 } <> previous)
    mempty

---------- Typeclass instances
derive instance newtypeYMeasures :: Newtype YMeasures _

instance semigroupYMeasures :: Semigroup YMeasures where
  append (YMeasures arr) (YMeasures arr') = YMeasures $ maxZip 0 0 arr arr' <#> uncurry max

instance monoidYMeasures :: Monoid YMeasures where
  mempty = YMeasures []

derive instance genericYMeasures :: Generic YMeasures _

instance debugYMeasures :: Debug YMeasures where
  debug = genericDebug

---------- SProxies
_heights :: SProxy "heights"
_heights = SProxy
