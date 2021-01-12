module NameMap where

import Lunarlude
import Data.Array as Array
import Data.HashMap as HashMap
import Lunarflow.Ast (Name(..))

-- | Map of Name-value pairs
type NameMap v
  = { free :: HashMap.HashMap String v
    , bound :: Array v
    }

-- | Get a value by key
lookup :: forall v. Name -> NameMap v -> Maybe v
lookup name nameMap = case name of
  Free free -> HashMap.lookup free nameMap.free
  Bound bound -> Array.index nameMap.bound bound

-- | Create a name-map from bound values
fromBound :: forall v. Array v -> NameMap v
fromBound = { free: HashMap.empty, bound: _ }

-- | Create a name-map from free values
fromFree :: forall v. HashMap.HashMap String v -> NameMap v
fromFree = { bound: [], free: _ }
