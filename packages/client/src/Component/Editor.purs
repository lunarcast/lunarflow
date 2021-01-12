module Lunarflow.Component.Editor where

import Lunarlude
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Data.HashMap as HashMap
import Data.List as List
import Data.Map as Map
import Effect.Aff.Bus as Bus
import Effect.Class.Console as Console
import Graphics.Canvas (CanvasElement, Context2D, getContext2D, restore, save)
import Lunarflow.Ast (RawExpression, printRawExpression, withDebrujinIndices)
import Lunarflow.Ast.Grouped (groupExpression)
import Lunarflow.Event (eventBus)
import Lunarflow.Geometry.Foreign (fromShape, geometryBounds, renderGeometry)
import Lunarflow.Layout (layoutingPipeline)
import Lunarflow.LayoutM (ScopeId(..), runLayoutMWithState)
import Lunarflow.Parser (unsafeParseLambdaCalculus)
import Lunarflow.Render (render, runRenderM)
import Lunarflow.Renderer.Canvas (clear, fitIntoBounds, stretch)
import Lunarflow.Renderer.Constants (backgroundColor, colors)
import Lunarflow.Renderer.WithHeight (withHeights)
import Lunarflow.Tea (TeaM, tea)
import Partial.Unsafe (unsafeCrashWith)
import React.Ref as Ref
import Run (EFFECT, Run, AFF)
import Run.Except (FAIL, fromJust, runFail)
import Run.State (STATE, get, gets, modify)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event, EventType(..))
import Web.HTML as HTML
import Web.HTML.Window as Window

--------- Types
type EditorM
  = Run
      ( effect :: EFFECT
      , aff :: AFF
      , state :: STATE EditorState
      , except :: FAIL
      )

type ExpressionEnvironment
  = { expressions :: HashMap.HashMap String RawExpression
    }

type EditorState
  = { env :: ExpressionEnvironment
    , selectedExpression :: Maybe String
    , context ::
        Maybe
          { node :: Ref.NativeNode
          , context :: Context2D
          }
    , initializing :: Boolean
    , resizeBus :: Bus.BusR Event
    }

type ExpressionInput
  = { name :: String
    , expression :: RawExpression
    , isSelected :: Boolean
    }

data EditorAction
  = SelectExpression String
  | WithRef Ref.NativeNode EditorAction
  | Render
  | Init
  | Resize

--------- Components
editor :: forall a. Widget HTML a
editor = do
  window <- liftEffect HTML.window
  resizeBus <- liftEffect $ eventBus (EventType "resize") true (Window.toEventTarget window)
  tea (defaultState resizeBus) editor' $ handleAction >>> runEditorM
  where
  defaultState :: Bus.BusR Event -> EditorState
  defaultState =
    { env:
        { expressions:
            HashMap.fromFoldable
              [ mkFunction "zero" """\s z. z"""
              , mkFunction "identity" """\x. x"""
              , mkFunction "one" """\s z. s z"""
              , mkFunction "succ" """\n s z. s (n s z)"""
              , mkFunction "three" """\s z. s (s (s z))"""
              , mkFunction "four" """succ three"""
              ]
        }
    , selectedExpression: Just "identity"
    , context: Nothing
    , initializing: true
    , resizeBus: _
    }

  mkFunction :: String -> String -> Tuple String RawExpression
  mkFunction name input = Tuple name $ unsafeParseLambdaCalculus input

editorExpression :: ExpressionInput -> Widget HTML EditorAction
editorExpression { name, expression, isSelected } =
  D.div
    [ P.onClick $> SelectExpression name
    , P.className ("expression" <> if isSelected then " expression--selected" else "")
    ]
    [ D.div [ P.className "expression__container" ]
        [ D.div [ P.className "expression__top" ]
            [ D.div [ P.className "expression__name" ] [ D.text name ]
            ]
        , D.div [ P.className "expression__text" ]
            [ D.text (printRawExpression expression)
            ]
        ]
    ]

editor' :: EditorState -> Widget HTML EditorAction
editor' state = do
  canvasRef <- liftEffect Ref.createNodeRef
  action <-
    D.div [ P.className "editor" ]
      [ D.div [ P.className "editor__expression" ]
          (state.env.expressions |> HashMap.toArrayBy mkExpression)
      , D.canvas [ P.className "editor__visualization", P.ref $ Ref.fromRef canvasRef ] []
      , D.div [ P.className "editor__reduction" ] []
      ]
      <|> spice
      <|> resizeEvent
  liftEffect (Ref.getCurrentRef canvasRef)
    <#> foldr WithRef action
  where
  resizeEvent = liftAff $ Bus.read state.resizeBus $> Resize

  mkExpression name expression =
    editorExpression
      { name
      , expression
      , isSelected: state.selectedExpression == Just name
      }

  -- This is a little hacky way to not wait for the first event before setting the ref
  spice
    | state.initializing = liftAff $ delay (Milliseconds 0.0) $> Init
    | otherwise = mempty

-- Action handlers
handleAction :: EditorAction -> EditorM Unit
handleAction = case _ of
  Init -> do
    Console.log "finished initializing"
    modify $ set (prop _initializing) false
  SelectExpression name -> do
    previouslySelected <- gets _.selectedExpression
    modify $ selectExpression name
    unless (previouslySelected == Just name) do
      handleAction Render
  WithRef node action -> do
    maybeContext <- get <#> _.context
    case maybeContext of
      Just _ -> pure unit
      Nothing -> do
        context <- liftEffect $ getContext2D (refToCanvas node)
        modify $ set (prop _context) $ Just { node, context }
        handleAction Render
    handleAction action
  Resize -> do
    Console.log "The window was resized!"
    handleAction Render
  Render -> do
    ctx <- getContext
    raw <- getSelectedExpression
    let
      state =
        { colors: colors
        , indexMap: Map.singleton Root $ List.singleton 0
        , lastScope: -1
        }

      shape =
        raw
          |> withDebrujinIndices
          |> groupExpression
          |> layoutingPipeline
          |> runLayoutMWithState state
          |> map snd
          |> flip either identity
              ( \a ->
                  unsafeCrashWith $ show a
              )
          -- |> debugSpy
          
          |> withHeights
          |> render
          |> runRenderM
          |> flip either identity
              ( \a ->
                  unsafeCrashWith $ show a
              )

      geometry = fromShape shape
    liftEffect do
      clear backgroundColor ctx
      stretch ctx
      save ctx
      case geometryBounds geometry of
        Just bounds -> fitIntoBounds bounds ctx
        Nothing -> pure unit
      renderGeometry geometry ctx
      restore ctx

--------- Helpers
-- | Mark an expression as selected
selectExpression :: String -> EditorState -> EditorState
selectExpression name = set (prop _selectedExpression) $ Just name

-- | Try getting the current expression, fail if we can't find one
getSelectedExpression :: EditorM RawExpression
getSelectedExpression = do
  state <- get
  state.selectedExpression
    >>= flip HashMap.lookup state.env.expressions
    |> fromJust

-- | Try getting the current canvas context, fail if we can't find one
getContext :: EditorM Context2D
getContext = get <#> _.context >>= fromJust <#> _.context

refToCanvas :: Ref.NativeNode -> CanvasElement
refToCanvas = unsafeCoerce

-- | Strip down the capabilities offered by the editor monad down to the bare-bones tea monad
runEditorM :: EditorM Unit -> TeaM EditorState Unit
runEditorM = runFail >>> voidRight unit

---------- SProxies (generated using a vscode snippet) 
_env :: SProxy "env"
_env = SProxy

_selectedExpression :: SProxy "selectedExpression"
_selectedExpression = SProxy

_context :: SProxy "context"
_context = SProxy

_initializing :: SProxy "initializing"
_initializing = SProxy
