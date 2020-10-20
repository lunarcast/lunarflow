-- TODO: Copy the actual typeclass implementations for performance.
-- | Debuggable fixpoint type.
module Lunarflow.Mu where

import Prelude
import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Debug (class Debug, Repr, debug)
import Data.Eq (class Eq1)
import Data.Functor.Mu as Mu
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (class Ord1)
import Data.TacitString as TS
import Matryoshka (class Corecursive, class Recursive)

-- | Wrapper around the original Mu having an additional Debug instance.
newtype Mu f
  = Mu (Mu.Mu f)

-- | Same as TacitString but not for debug representations.
newtype TacitRepr
  = TacitRepr Repr

instance debugRepr :: Debug TacitRepr where
  debug (TacitRepr a) = a

derive newtype instance eqMu :: Eq1 f => Eq (Mu f)

derive newtype instance ordMu :: (Eq1 f, Ord1 f) => Ord (Mu f)

derive instance newtypeMu :: Newtype (Mu f) _

derive newtype instance showMu :: (Show (f TS.TacitString), Functor f) => Show (Mu f)

derive newtype instance semigroupMu :: Alt f => Semigroup (Mu f)

derive newtype instance monoidMu :: Plus f => Monoid (Mu f)

instance recursiveMu :: Functor f => Recursive (Mu f) f where
  project (Mu (Mu.In x)) = Mu <$> x

instance corecursiveMu :: Functor f => Corecursive (Mu f) f where
  embed a = Mu $ Mu.In $ unwrap <$> a

instance debugMu :: (Debug (f TacitRepr), Functor f) => Debug (Mu f) where
  debug (Mu (Mu.In x)) = debug $ x <#> (Mu >>> debug >>> TacitRepr)
