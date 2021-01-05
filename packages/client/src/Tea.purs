module Lunarflow.Tea where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Run (EFFECT, Run, runBaseEffect)
import Run.State (STATE, execState)

type TeaM s
  = Run
      ( effect :: EFFECT
      , state :: STATE s
      )

-- | The Elm Architecture
tea ::
  forall a s m.
  MonadEffect m =>
  s -> -- Initial state
  (s -> m a) -> -- Render function
  (a -> TeaM s Unit) -> -- Action handler
  forall x. m x
tea initialState render update = go initialState
  where
  go :: forall x. s -> m x
  go state = render state >>= (update >>> execTeaM state >>> liftEffect >=> go)

execTeaM :: forall s a. s -> TeaM s a -> Effect s
execTeaM state = execState state >>> runBaseEffect
