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
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), uncurry)
import Lunarflow.Array (maxZip)
import Lunarflow.Ast (AstF(..), call, lambda, var)
import Lunarflow.Label (class Label)
import Lunarflow.Layout (Layout, LayoutF, LayoutLikeF, ScopeId)
import Lunarflow.Mu (Mu)
import Matryoshka (Algebra, cata, project)
import Record as Record

-- | The base functor for YLayouts.
type YLayoutF
  = LayoutLikeF Int () () () ( heights :: YMeasures, scope :: ScopeId )

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
    Lambda data' (Tuple body rawHeights) -> Tuple yLambda measures
      where
      heights =
        mergeArrays rawHeights
          $ foldr mergeArrays []
          $ data'.args
          <#> \x -> createArray x.position 1 []

      measures = createArray data'.position (sum heights + 1) []

      yLambda =
        lambda
          (Record.insert (SProxy :: _ "heights") heights data')
          body
    Var { index, position, color } -> Tuple yVar yMap
      where
      yVar = var { index, color, position }

      yMap = []
    Call data' (Tuple function functionMeasures) (Tuple argument argumentMeasures) -> Tuple yCall measures
      where
      yCall = call data' function argument

      measures = createArray data'.position 1 $ mergeArrays functionMeasures argumentMeasures

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
        Call { position } _ _ -> position
        Lambda { position } _ -> position
