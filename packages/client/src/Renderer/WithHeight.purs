module Lunarflow.Renderer.WithHeight
  ( YLayoutF
  , YLayout
  , YMeasures
  , withHeights
  , getPosition
  ) where

import Prelude
import Data.Array (foldr)
import Data.Array as Array
import Data.Foldable (sum)
import Data.List (List) as List
import Data.Tuple (Tuple(..), uncurry)
import Debug.Trace (spy)
import Lunarflow.Array (maxZip)
import Lunarflow.Ast (AstF(..), call, lambda, var)
import Lunarflow.Label (class Label)
import Lunarflow.Layout (Layout, LayoutF)
import Lunarflow.Mu (Mu)
import Matryoshka (Algebra, cata, project)

-- | The base functor for YLayouts.
type YLayoutF
  = AstF { position :: Int, index :: Int } Int { position :: Int, args :: List.List Int, heights :: YMeasures }

-- | YLayouts are layouts which keep track of the maximum height of each line
type YLayout
  = Mu YLayoutF

-- TODO: make this a newtype with semigruop (& monoid?) instances
-- | A slice is just an array where each element 
-- | represents the height of the line at the same (local) y index
type YMeasures
  = Array Int

-- | Add height data to a layout.
withHeights ::
  Layout ->
  Tuple YLayout YMeasures
withHeights = cata algebra
  where
  algebra :: Algebra LayoutF (Tuple YLayout YMeasures)
  algebra = case _ of
    Lambda { position, args } (Tuple body rawHeights) -> Tuple yLambda measures
      where
      heights =
        mergeArrays (spy "slice" rawHeights)
          $ foldr mergeArrays []
          $ args
          <#> \x -> createArray x 1 []

      measures = createArray position (sum heights + 1) []

      yLambda =
        lambda
          { position
          , args
          , heights
          }
          body
    Var { index, position } -> Tuple yVar yMap
      where
      yVar = var { index, position }

      yMap = []
    Call position (Tuple function functionMeasures) (Tuple argument argumentMeasures) -> Tuple yCall measures
      where
      yCall = call position function argument

      measures = createArray position 1 $ mergeArrays functionMeasures argumentMeasures

-- | Update an ymap at an arbitrary index, position and value.
createArray :: (Label "position" => Int) -> (Label "value" => Int) -> YMeasures -> YMeasures
createArray position value = mergeArrays $ Array.snoc (Array.replicate position 1) value

-- | Pretty much a semigroup implementation for YMeasures
mergeArrays :: Array Int -> Array Int -> Array Int
mergeArrays arr arr' = maxZip 1 1 arr arr' <#> uncurry max

-- | Get the position an YLayout is at.
getPosition :: YLayout -> Int
getPosition =
  project
    >>> case _ of
        Var { position } -> position
        Call position _ _ -> position
        Lambda { position } _ -> position
