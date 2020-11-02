module Lunarflow.Debug where

import Prelude
import Data.Debug (class Debug, debug, prettyPrintWith)
import Data.Maybe (Maybe(..))
import Debug.Trace (class DebugWarning)
import Effect (Effect)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)

-- | Like show but for stuff with debug instances.
showPretty :: forall d. Debug d => d -> String
showPretty = prettyPrintWith { compactThreshold: 6, maxDepth: Just 1000 } <<< debug

-- | A debug instance which shows more context.
-- | At repl, call `:print Lunarflow.Debug.myDebug`
myDebug :: forall d. Debug d => d -> Effect Unit
myDebug = Console.log <<< showPretty

-- | Similar to spy but requires a debug instance.
debugSpy :: forall a. Debug a => DebugWarning => a -> a
debugSpy a = unsafePerformEffect (a <$ myDebug a)
