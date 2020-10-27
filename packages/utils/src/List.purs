module Lunarflow.List where

import Prelude
import Data.List (List(..), length, mapWithIndex, (!!), (..), (:))
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))

-- | Merge 2 lists element by element.
merge :: forall a. (a -> a -> a) -> List a -> List a -> List a
merge f = case _, _ of
  Nil, list -> list
  list, Nil -> list
  (x : xs), (y : ys) -> f x y : merge f xs ys

-- | Kinda like List.insert but adds an arbitrary empty value as much
-- | as needed until the value we want to add can actually be inserted safely.
with :: forall a. Int -> a -> a -> List a -> List a
with index empty value list =
  0 .. maxLength
    <#> \index' ->
        if inputLength > index then
          fromMaybe empty (list !! index')
        else
          if index == index' then value else empty
  where
  maxLength = max index (inputLength - 1)

  inputLength = length list

-- | Basically mapping with indices and saving them.
indexed :: forall a. List a -> List (Tuple Int a)
indexed = mapWithIndex Tuple
