module Lunarflow.Renderer.Canvas where

import Prelude
import Effect (Effect)
import Graphics.Canvas (CanvasElement, Context2D)
import Lunarflow.Geometry.Types (Bounds)

foreign import fitIntoBounds :: Bounds -> Context2D -> Effect Unit

foreign import fillScreen :: CanvasElement -> Effect Unit

foreign import onResize :: Effect Unit -> Effect Unit
