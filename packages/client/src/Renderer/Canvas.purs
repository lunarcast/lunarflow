module Lunarflow.Renderer.Canvas where

import Lunarlude
import Graphics.Canvas (CanvasElement, Context2D)
import Lunarflow.Geometry.Types (Bounds)

foreign import fitIntoBounds :: Bounds -> Context2D -> Effect Unit

foreign import fillScreen :: CanvasElement -> Effect Unit

foreign import onResize :: Effect Unit -> Effect Unit

foreign import stretch :: Context2D -> Effect Unit

foreign import clear :: String -> Context2D -> Effect Unit
