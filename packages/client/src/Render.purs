module Lunarbox.Render where

import Prelude
import Data.List as List
import Lunarflow.Ast (AstF(..))
import Lunarflow.Geometry.Foreign (fitIntoBounds)
import Lunarflow.Geometry.Types as Shape
import Lunarflow.Layout (Layout, LayoutF)
import Matryoshka (Algebra, cata)
import Run (Run)
import Run.Reader (READER, local)

type RenderContext
  = { doNotRender :: List.List Int
    }

type RenderM r
  = Run ( reader :: READER RenderContext | r )

-- | Prepare stuff for rendering inside a lambda.
shiftContext :: Int -> RenderContext -> RenderContext
shiftContext by ctx = ctx { doNotRender = ((+) by) <$> ctx.doNotRender }

{-- 
So:
- When we encounter a lambda, we draw the body and then the box around it
- When we encouner a var, we check where it is in scope and draw until here
--}
render :: forall r. Layout -> RenderM r Shape.Shape
render = cata algebra
  where
  algebra :: Algebra LayoutF (RenderM r Shape.Shape)
  algebra (Lambda { args } body) = do
    bodyShape <- local (shiftContext $ List.length args) body
    let
      bounds = fitIntoBounds bodyShape
    pure
      $ Shape.group {}
          [ Shape.fromBounds { fill: "transparent", stroke: "red" } bounds
          , bodyShape
          ]

  algebra _ = pure mempty
