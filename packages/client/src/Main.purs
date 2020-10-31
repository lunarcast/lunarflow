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
import Data.Either (Either(..))
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
import Lunarflow.Layout (LayoutError, addIndices, runLayoutM, unscopeLayout)
import Lunarflow.Parser (parseLambdaCalculus, unsafeParseLambdaCalculus)
import Lunarflow.Pipe ((|>))
import Lunarflow.Profile (profileApplication)
import Lunarflow.Render (render, runRenderM)
import Lunarflow.Renderer.Canvas (fillScreen, fitIntoBounds, onResize)
import Lunarflow.Renderer.WithHeight (withHeights)
import Partial.Unsafe (unsafeCrashWith)
import Prelude (class Show, Unit, bind, discard, map, pure, show, unit, ($), (<<<))
import Text.Parsing.Parser (ParseError)

geometryBenchmarks :: Effect Geometry
geometryBenchmarks =
  profileApplication "Converting shape to geometry" fromShape
    $ profileApplication "Rendering layout" (runRenderM <<< render)
    -- $ map debugSpy
    
    $ profileApplication "Adding height data" withHeights
    $ map
        ( case _ of
            Left err -> unsafeCrashWith $ show err
            Right a -> a
        )
    $ profileApplication "Creating layout"
        ( runLayoutM
            <<< unscopeLayout
            <<< addIndices
        )
    $ profileApplication "Grouping expression" groupExpression
    $ profileApplication "Adding de-brujin indices" withDebrujinIndices
    $ profileApplication "Parsing" unsafeParseLambdaCalculus """\f a b. f b a"""

-- $ call unit (call unit plus' (n 3))
--     ( call unit (call unit mult (n 7))
--         ( call unit (call unit exp (n 2))
--             (zero')
--         )
--     )
data Error
  = ParsingError ParseError
  | LayoutCreationError LayoutError

derive instance genericError :: Generic Error _

instance showError :: Show Error where
  show = genericShow

mkGeometry :: String -> Either Error Geometry
mkGeometry text = do
  rawExpression <- lmap ParsingError $ parseLambdaCalculus text
  layout <-
    rawExpression
      |> withDebrujinIndices
      |> groupExpression
      |> addIndices
      |> unscopeLayout
      |> runLayoutM
      |> lmap LayoutCreationError
  layout
    |> withHeights
    |> render
    |> runRenderM
    |> fromShape
    |> pure

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

type State
  = { error :: Maybe String
    , value :: String
    }

type Input
  = { ref :: Ref.Ref Geometry
    , render :: Effect Unit
    }

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

main :: Effect Unit
main = do
  maybeCanvas <- getCanvasElementById "canvas"
  case maybeCanvas of
    Nothing -> Console.log "No canvas found"
    Just canvas -> do
      ctx <- getContext2D canvas
      geometry <- geometryBenchmarks
      ref <- Ref.new geometry
      let
        runApp = app ref canvas ctx
      onResize runApp
      runApp
      runWidgetInDom "root"
        $ textBox { ref, render: runApp }
            { value: """\f a b. f b a"""
            , error: Nothing
            }
