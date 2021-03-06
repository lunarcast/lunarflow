module Lunarflow.Tea where

import Prelude
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Run (AFF, EFFECT, Run, runBaseAff')
import Run.State (STATE, execState)

type TeaM s
  = Run
      ( aff :: AFF
      , effect :: EFFECT
      , state :: STATE s
      )

-- | The Elm Architecture
tea ::
  forall a s m.
  MonadAff m =>
  s -> -- Initial state
  (s -> m a) -> -- Render function
  (a -> TeaM s Unit) -> -- Action handler
  forall x. m x
tea initialState render update = go initialState
  where
  go :: forall x. s -> m x
  go state = render state >>= (update >>> execTeaM state >>> liftAff >=> go)

execTeaM :: forall s a. s -> TeaM s a -> Aff s
execTeaM state = execState state >>> runBaseAff'
