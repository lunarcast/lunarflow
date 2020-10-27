module Lunarflow.Renderer.WithHeight
  ( YLayoutF
  , YLayout
  , YMapSlice
  , withHeights
  , getPosition
  ) where

import Prelude
import Data.Array (foldr)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Foldable (sum)
import Lunarflow.Mu (Mu)
import Data.List (List, length) as List
import Data.Map as Map
import Data.Tuple (Tuple(..), snd, uncurry)
import Lunarflow.Array (maxZip)
import Lunarflow.Ast (AstF(..), call, lambda, var)
import Lunarflow.Label (class Label)
import Lunarflow.Layout (Layout, LayoutF)
import Matryoshka (Algebra, cata, project)

-- | The base functor for YLayouts.
type YLayoutF
  = AstF { position :: Int, index :: Int } Int { position :: Int, args :: List.List Int, heights :: YMapSlice }

-- | YLayouts are layouts which keep track of the maximum height of each line
type YLayout
  = Mu YLayoutF

-- TODO: make this a newtype with semigruop (& monoid?) instances
-- | A slice is just an array where each element 
-- | represents the height of the line at the same (local) y index
type YMapSlice
  = Array Int

-- TODO: make this a newtype with semigruop & monoid instances.
-- | Map from de brujin indices to slices
type YMap
  = Map.Map Int YMapSlice

-- | Add height data to a layout.
withHeights ::
  Layout ->
  Tuple YLayout YMap
withHeights = cata algebra
  where
  algebra :: Algebra LayoutF (Tuple YLayout YMap)
  algebra = case _ of
    Lambda { position, args } (Tuple body bodyMeasures) -> Tuple yLambda measures
      where
      (Tuple slice remaining) = splitMap (List.length args) bodyMeasures

      measures = updateMap 0 position (sum slice + 1) remaining

      yLambda = lambda { position, args, heights: slice } body
    Var { index, position } -> Tuple yVar yMap
      where
      yVar = var { index, position }

      yMap = updateMap index position 1 Map.empty
    Call position (Tuple function functionMeasures) (Tuple argument argumentMeasures) -> Tuple yCall yMap
      where
      yCall = call position function argument

      yMap = updateMap 0 position 1 $ mergeYMaps functionMeasures argumentMeasures

-- | Update an ymap at an arbitrary index, position and value.
updateMap :: (Label "index" => Int) -> (Label "position" => Int) -> (Label "value" => Int) -> YMap -> YMap
updateMap index position value = mergeYMaps $ Map.singleton index $ Array.snoc (Array.replicate position 1) value

-- | Pretty much a semigroup implementation for YMaps
mergeYMaps :: YMap -> YMap -> YMap
mergeYMaps = Map.unionWith mergeArrays

-- | Pretty much a semigroup implementation for YMapSlice
mergeArrays :: Array Int -> Array Int -> Array Int
mergeArrays arr arr' = maxZip 1 1 arr arr' <#> uncurry max

splitMap :: Int -> YMap -> Tuple YMapSlice YMap
splitMap shiftBy yMap = Tuple slice remaining
  where
  slice = foldr mergeArrays [] $ snd <$> split.yes

  remaining = Map.fromFoldable $ lmap (_ - shiftBy) <$> split.no

  split = flip Array.partition arr \(Tuple x _) -> x < shiftBy

  arr :: Array _
  arr = Map.toUnfoldable yMap

-- | Get the position an YLayout is at.
getPosition :: YLayout -> Int
getPosition =
  project
    >>> case _ of
        Var { position } -> position
        Call position _ _ -> position
        Lambda { position } _ -> position
