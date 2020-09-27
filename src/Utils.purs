module Lunarflow.Utils where

import Prelude
import Control.Monad.State (State, modify)
import Data.List as List
import Data.Tuple (Tuple(..))

-- | We generate incremental ids using the state monad quite often, so I made a general hepler which does that
increase :: State Int Int
increase = modify ((+) 1)

indexed :: forall a. List.List a -> List.List (Tuple Int a)
indexed = go 0
  where
  go _ List.Nil = List.Nil

  go x (List.Cons head tail) = List.Cons (Tuple x head) $ go (x + 1) tail
