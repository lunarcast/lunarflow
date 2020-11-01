module Main where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Concur.React.Widgets (textInputWithButton)
import Control.MultiAlternative (orr)
import Control.Plus (empty)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getContext2D, restore, save)
import Lunarflow.Ast (withDebrujinIndices)
import Lunarflow.Ast.Grouped (groupExpression)
import Lunarflow.Geometry.Foreign (Geometry, boundsImpl, fromShape, renderGeometry)
import Lunarflow.Layout (LayoutError, addIndices, markRoot, runLayoutM, unscopeLayout)
import Lunarflow.Parser (parseLambdaCalculus)
import Lunarflow.Pipe ((|>))
import Lunarflow.Render (render, runRenderM)
import Lunarflow.Renderer.Canvas (fillScreen, fitIntoBounds, onResize)
import Lunarflow.Renderer.WithHeight (withHeights)
import Partial.Unsafe (unsafeCrashWith)
import Prelude (class Show, Unit, bind, discard, identity, pure, show, unit, ($), (>>>))
import Text.Parsing.Parser (ParseError)

-- | The state which the concur widget keeps track of.
type State
  = { error :: Maybe String
    , value :: String
    }

-- | Initial config we pass to the concur widget.
type Input
  = { ref :: Ref.Ref Geometry
    , render :: Effect Unit
    }

-- | Possible errors which can occur while generating the geometry.
data Error
  = ParsingError ParseError
  | LayoutCreationError LayoutError

-- | This takes a lambda calulus expression and tries to build a geometry out of it.
mkGeometry :: String -> Either Error Geometry
mkGeometry text = do
  rawExpression <- lmap ParsingError $ parseLambdaCalculus text
  layout <-
    rawExpression
      |> withDebrujinIndices
      |> groupExpression
      |> addIndices
      |> markRoot
      |> unscopeLayout
      |> runLayoutM
      |> lmap LayoutCreationError
  layout
    |> withHeights
    |> render
    |> runRenderM
    |> fromShape
    |> pure

-- | This function renders the current geometry to the canvas
app :: Ref.Ref Geometry -> CanvasElement -> Context2D -> Effect Unit
app ref canvas ctx = do
  fillScreen canvas
  save ctx
  geometry <- Ref.read ref
  case Nullable.toMaybe (boundsImpl geometry) of
    Just bounds -> fitIntoBounds bounds ctx
    Nothing -> pure unit
  renderGeometry geometry ctx
  restore ctx

-- | This is the concur widget for the button thingy in the corner.
textBox :: forall a. Input -> State -> Widget HTML a
textBox input { value, error } = do
  text <-
    orr
      [ textInputWithButton value "Render" [ P._id "input" ] []
      , case error of
          Just error' -> D.p [ P._id "error" ] [ D.text error' ]
          Nothing -> empty
      ]
  let
    geometry = mkGeometry text
  state <- case geometry of
    Left error' -> pure { error: Just $ show error', value: text }
    Right geometry' -> do
      liftEffect $ Ref.write geometry' input.ref
      liftEffect input.render
      pure { error: Nothing, value: text }
  textBox input state

-- | The expression the website loads by default.
initialExpression :: String
initialExpression = """\f a b. f b a"""

main :: Effect Unit
main = do
  maybeCanvas <- getCanvasElementById "canvas"
  case maybeCanvas of
    Nothing -> Console.log "No canvas found"
    Just canvas -> do
      ctx <- getContext2D canvas
      let
        geometry =
          mkGeometry initialExpression
            |> either (show >>> unsafeCrashWith) identity
      ref <- Ref.new geometry
      let
        runApp = app ref canvas ctx
      onResize runApp
      runApp
      runWidgetInDom "root"
        $ textBox { ref, render: runApp }
            { value: initialExpression
            , error: Nothing
            }

---------- Tyeclass instances
derive instance genericError :: Generic Error _

instance showError :: Show Error where
  show = genericShow
