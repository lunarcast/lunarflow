-- | We aren't really doing actual benchmarks, more like seeing how much each part of the program took.
module Lunarflow.Benchmark
  ( class Benchmarkable
  , benchApplication
  ) where

import Prelude
import Effect (Effect)
import Effect.Console as Console

-- | Class allowing us to have polymorphic benchmarking.
class Benchmarkable a t | t -> a where
  -- | Measures the time a function application takes.
  benchApplication :: forall c. String -> (a -> c) -> t -> Effect c

instance benchmarkableResult :: Benchmarkable a (Effect a) where
  benchApplication name f input = do
    previous <- input
    benchApplication name f previous
else instance benchmarkableIdentity :: Benchmarkable a a where
  benchApplication name f i = do
    Console.time name
    let
      value = f i
    Console.timeEnd name
    pure value
