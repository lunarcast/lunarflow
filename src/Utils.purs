module Lunarflow.Utils where

import Prelude
import Control.Monad.State (State, modify)

-- | We generate incremental ids using the state monad quite often, so I made a general hepler which does that
increase :: State Int Int
increase = modify ((+) 1)
