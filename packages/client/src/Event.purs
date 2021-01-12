module Lunarflow.Event where

import Lunarlude

import Effect.Aff.Bus as Bus
import Web.Event.Event (Event, EventType)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)

-- | Create an async bus for dom events
eventBus :: EventType -> Boolean -> EventTarget -> Effect (Bus.BusR Event)
eventBus eventType capture target = do
  Tuple writeBus readBus <- Bus.make <#> Bus.split
  listener <- eventListener \event -> launchAff_ $ Bus.write event readBus
  addEventListener eventType listener capture target  
  pure writeBus