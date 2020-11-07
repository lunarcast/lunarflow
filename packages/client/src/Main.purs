module Main where

import Prelude
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
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref as Ref
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getContext2D, restore, save)
import Lunarflow.Ast (withDebrujinIndices)
import Lunarflow.Ast.Binary (groupedToBinary, indexedToBinary, showBinaryAst, toBuffer)
import Lunarflow.Ast.Grouped (groupExpression, ungroupExpression)
import Lunarflow.Debug (table)
import Lunarflow.ErrorStack (ErrorStack)
import Lunarflow.Function ((|>))
import Lunarflow.Geometry.Foreign (Geometry, boundsImpl, fromShape, renderGeometry)
import Lunarflow.Layout (ScopedLayout, addIndices, markRoot, unscopeLayout)
import Lunarflow.LayoutM (LayoutError, LayoutState, ScopeId(..), runLayoutMWithState)
import Lunarflow.Parser (parseLambdaCalculus)
import Lunarflow.Reduction (tryEtaReducing)
import Lunarflow.Render (render, runRenderM)
import Lunarflow.Renderer.Canvas (fillScreen, fitIntoBounds, onResize)
import Lunarflow.Renderer.Constants (colors)
import Lunarflow.Renderer.WithHeight (withHeights)
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.Parser (ParseError)

-- | The state which the concur widget keeps track of.
type State
  = { error :: Maybe String
    , value :: String
    , layout :: Maybe (Tuple LayoutState ScopedLayout)
    }

-- | Initial config we pass to the concur widget.
type Input
  = { ref :: Ref.Ref Geometry
    , render :: Effect Unit
    }

-- | Actions our ui can emit
data Action
  = NewExpression String
  | EtaReduce
  | RenderLayout

-- | Possible errors which can occur while generating the geometry.
data Error
  = ParsingError ParseError
  | LayoutCreationError (ErrorStack String LayoutError)

-- | This takes a lambda calculus expression and tries to build an unscoped layout out of it.
mkScopedLayout :: String -> Either Error (Tuple LayoutState ScopedLayout)
mkScopedLayout text = do
  rawExpression <- lmap ParsingError $ parseLambdaCalculus text
  rawExpression
    |> withDebrujinIndices
    |> groupExpression
    |> addIndices
    |> map markRoot
    |> runLayoutMWithState state
    |> lmap LayoutCreationError
  where
  state =
    { colors: colors
    , indexMap: Map.singleton Root $ List.singleton 0
    , lastScope: -1
    }

-- | This t(Tuple stabd scopedLayout)es
mkGeometry :: Tuple LayoutState ScopedLayout -> Either Error Geometry
mkGeometry (Tuple state scopedLayout) = do
  -- traceM $ showPretty scopedLayout
  layout <-
    unscopeLayout scopedLayout
      |> runLayoutMWithState state
      |> lmap LayoutCreationError
      |> map snd
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

-- | TEA like action handler
handleAction :: Input -> State -> Action -> Widget HTML State
handleAction input state@{ value, error, layout } = case _ of
  EtaReduce -> case layout of
    Nothing -> pure state
    Just (Tuple layoutState scopedLayout) ->
      flip (handleAction input) RenderLayout
        { error:
          either (show >>> Just)
            (const Nothing)
            layout'
        , value
        , layout: either (const Nothing) Just layout'
        }
      where
      layout' = scopedLayout |> tryEtaReducing |> runLayoutMWithState layoutState |> lmap LayoutCreationError
  NewExpression text ->
    flip (handleAction input) RenderLayout
      { error:
        either (show >>> Just)
          (const Nothing)
          layout'
      , value: text
      , layout: either (const Nothing) Just layout'
      }
    where
    layout' = mkScopedLayout text
  RenderLayout -> case layout of
    Nothing -> pure state
    Just layout' -> case geometry of
      Left error' -> pure state { error = Just $ show error' }
      Right geometry' -> do
        liftEffect $ Ref.write geometry' input.ref
        liftEffect input.render
        let
          groupedBinaryAst = groupedToBinary $ snd layout'

          binaryAst = indexedToBinary $ ungroupExpression const $ snd layout'

          a = toBuffer groupedBinaryAst
        liftEffect
          $ table
              { normal:
                { size: binaryAst.size
                , bits: showBinaryAst binaryAst
                }
              , grouped:
                { size: groupedBinaryAst.size
                , bits: showBinaryAst groupedBinaryAst
                }
              }
        pure state { error = Nothing }
      where
      geometry = mkGeometry layout'

-- | This is the concur widget for the button thingy in the corner.
textBox :: forall a. Input -> State -> Widget HTML a
textBox input state'@{ value, error, layout } = do
  action <-
    orr
      [ NewExpression <$> textInputWithButton value "Render" [ P._id "input" ] []
      , D.button [ P.onClick $> EtaReduce ] [ D.text "Eta reduce" ]
      , case error of
          Just error' -> D.p [ P._id "error" ] [ D.text error' ]
          Nothing -> empty
      ]
  state <- handleAction input state' action
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
        layoutWithState =
          mkScopedLayout initialExpression
            |> either (show >>> unsafeCrashWith) identity

        geometry =
          mkGeometry layoutWithState
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
            , layout: Just layoutWithState
            }

---------- Tyeclass instances
derive instance genericError :: Generic Error _

instance showError :: Show Error where
  show = genericShow
