module Lunarflow.Row (class PartialRow, withDefaults) where

import Prelude
import Prim.Row (class Nub, class Union)
import Record as Record

-- | The PartialRow typeclass is the equivalent of Partial<T> in typescript.
class PartialRow (input :: #Type) (output :: #Type) | input -> output, output -> input where
  withDefaults :: Record input -> Record output -> Record input

instance partialRowUnion ::
  ( Union output rest input
  , Union output input input'
  , Nub input' input
  ) =>
  PartialRow input output where
  withDefaults = flip Record.merge
