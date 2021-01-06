module Lunarflow.Renderer.WithHeight
  ( YLayoutLikeF
  , YLayoutLike
  , YMeasures
  , withHeights
  , totalHeight
  ) where

import Lunarlude
import Data.Array as Array
import Lunarflow.Array (maxZip)
import Lunarflow.Ast (AstF(..), call, lambda, var)
import Lunarflow.Layout (LayoutLike, LayoutLikeF)
import Lunarflow.LayoutM (ScopeId)
import Prim.Row as Row
import Record as Record

-- | The base functor for YLayouts.
type YLayoutLikeF v c a l
  = LayoutLikeF Int v c a ( isRoot :: Boolean, scope :: ScopeId, heights :: YMeasures | l )

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
  forall v l a c.
  Row.Lacks "heights" l =>
  -- TODO: clean this up
  LayoutLike Int v c a ( isRoot :: Boolean, scope :: ScopeId | l ) ->
  Tuple YMeasures (YLayoutLike v c a l)
withHeights =
  cata case _ of
    -- The heights of vars will be handled inside the lambda which binds them
    Var varData -> Tuple mempty $ var varData
    -- To measure calls we just merge the measures of the function and argument and add one more row for the result.
    Call data' (Tuple functionMeasures function) (Tuple argumentMeasures argument) -> Tuple measures yCall
      where
      yCall = call data' function argument

      measures =
        createMeasure { position: data'.position, height: 1 } -- The row for the result
          <> functionMeasures
          <> argumentMeasures
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
          , height: totalHeight bodyMeasures + 1
          }

-- | Make a value of type YMeasures which holds a measure of an arbitrary height at an arbitrary position
createMeasure :: { position :: Int, height :: Int } -> YMeasures
createMeasure { position, height } = YMeasures $ Array.snoc (Array.replicate position 0) height

-- | Measure the total height a bunch of rows will occupy
totalHeight :: YMeasures -> Int
totalHeight = unwrap >>> sum

---------- Typeclass instances
derive instance newtypeYMeasures :: Newtype YMeasures _

instance semigroupYMeasures :: Semigroup YMeasures where
  append (YMeasures arr) (YMeasures arr') = YMeasures $ maxZip 0 0 arr arr' <#> uncurry max

derive newtype instance monoidYMeasures :: Monoid YMeasures

---------- SProxies
_heights :: SProxy "heights"
_heights = SProxy
