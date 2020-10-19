module Lunarflow.Array where

import Prelude
import Data.Array (length, zip, (!!), (..))
import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple)

-- | Kinda lika Array.insert but adds an arbitrary empty value as much
-- | as needed until the value we want to add can actually be inserted safely.
with :: forall a. Int -> a -> a -> Array a -> Array a
with index empty value array =
  0 .. length
    <#> \index' ->
        if inputLength > index then
          fromMaybe empty (array !! index')
        else
          if index == index' then value else empty
  where
  length = max index (inputLength - 1)

  inputLength = Array.length array

-- | Resize an array to a certain size filling all the empty spots with an arbitrary element.
resize :: forall a. Int -> a -> Array a -> Array a
resize size empty array = 0 .. (size - 1) <#> \index -> fromMaybe empty (array !! index)

-- | Zip but without discarding anything.
maxZip :: forall a b. a -> b -> Array a -> Array b -> Array (Tuple a b)
maxZip empty empty' array array' = result
  where
  result = case compare lengthArray lengthArray' of
    GT -> zip array (resize lengthArray empty' array')
    LT -> zip (resize lengthArray' empty array) array'
    EQ -> zip array array'

  lengthArray = length array

  lengthArray' = length array'
