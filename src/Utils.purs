module Lunarflow.Utils where

import Prelude
import Control.Monad.State (State, modify)
import Data.Debug (class Debug, debug, prettyPrintWith)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console

-- | We generate incremental ids using the state monad quite often, so I made a general hepler which does that
increase :: State Int Int
increase = modify ((+) 1)

indexed :: forall a. List.List a -> List.List (Tuple Int a)
indexed = go 0
  where
  go _ List.Nil = List.Nil

  go x (List.Cons head tail) = List.Cons (Tuple x head) $ go (x + 1) tail

-- | A debug instance which shows enough context to be useful for zippers.
-- | At repl, call `:print Lunarflow.Utils.myDebug`
myDebug :: forall d. Debug d => d -> Effect Unit
myDebug = Console.log <<< prettyPrintWith { compactThreshold: 6, maxDepth: Just 1000 } <<< debug
