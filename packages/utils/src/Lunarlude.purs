-- | My own prelude
-- |
-- | Featuring: 
-- |    - Recursion schemes
-- |    - Tuples and Maybe
-- |    - Newtypes
-- |    - Foldable & Traversable
module Lunarlude
  ( module Prelude
  , module Lunarflow.Function
  , module Lunarflow.Mu
  , module Data.Symbol
  , module Data.Tuple
  , module Data.Foldable
  , module Data.Newtype
  , module Matryoshka
  , module Control.MonadZero
  , module Data.Bifunctor
  , module Data.Function
  , module Data.Maybe
  , module Data.Ord
  , module Data.Traversable
  , module Data.TraversableWithIndex
  , module Data.Debug
  , module Data.Generic.Rep
  , module Data.Generic.Rep.Show
  , module Data.Unfoldable
  , module Data.Either
  , module Data.FoldableWithIndex
  , module Data.Int
  , module Type.Data.Row
  , module Control.Plus
  , module Data.Lens
  , module Data.Lens.Record
  , module Effect.Aff
  , module Effect.Aff.Class
  , module Effect.Class
  , module Data.Lens.At
  , module Data.Lens.Index
  , module Data.Lens.Iso.Newtype
  , module Data.Functor
  , module Effect
  , module Type.Row
  ) where

import Prelude

import Control.MonadZero (guard)
import Control.Plus ((<|>), empty)
import Data.Bifunctor (lmap, rmap)
import Data.Debug (class Debug, genericDebug)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, fold, foldl, foldr, product, sum)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndex, foldrWithIndex)
import Data.Function (on)
import Data.Functor (voidLeft, voidRight, void)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (toNumber, floor)
import Data.Lens (Lens', Prism', Iso', _1, _2, _Just, _Nothing, _Left, _Right, set, lens, prism, view, preview, over)
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Ord (abs)
import Data.Symbol (SProxy(..))
import Data.Traversable (class Traversable, for, for_, traverse, traverse_, sequence, sequence_, minimumBy)
import Data.TraversableWithIndex (class TraversableWithIndex, forWithIndex)
import Data.Tuple (Tuple(..), fst, snd, uncurry, curry)
import Data.Unfoldable (class Unfoldable, replicate)
import Effect.Aff (Milliseconds(..), delay, launchAff_, launchAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Lunarflow.Function (Endomorphism, (|>))
import Lunarflow.Mu (Mu(..), TacitRepr)
import Matryoshka (class Corecursive, class Recursive, Algebra, AlgebraM, Coalgebra, CoalgebraM, GAlgebra, GAlgebraM, ana, anaM, cata, cataM, para, embed, project)
import Type.Data.Row (RProxy(..))
import Effect (Effect)
import Type.Row (type (+))