-- | We aren't really doing actual benchmarks, more like seeing how much each part of the program took.
module Lunarflow.Profile
  ( class Profile
  , profileApplication
  , unsafeProfileApplication
  ) where

import Prelude
import Effect (Effect)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)

-- | Class allowing us to have polymorphic benchmarking.
class Profile a t | t -> a where
  -- | Measures the time a function application takes.
  profileApplication :: forall c. String -> (a -> c) -> t -> Effect c

instance profileResult :: Profile a (Effect a) where
  profileApplication name f input = do
    previous <- input
    profileApplication name f previous
else instance profileIdentity :: Profile a a where
  profileApplication name f i = do
    Console.time name
    let
      value = f i
    Console.timeEnd name
    pure value

-- | Measure some time without tracking the effect in the type system.
unsafeProfileApplication :: forall a t c. Profile a t => String -> (a -> c) -> t -> c
unsafeProfileApplication name f input = unsafePerformEffect $ profileApplication name f input
