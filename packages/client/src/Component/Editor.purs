module Lunarflow.Component.Editor where

import Prelude
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Control.Plus ((<|>))
import Data.Foldable (foldr, traverse_)
import Data.HashMap as Map
import Data.Lens (set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Graphics.Canvas (CanvasElement, Context2D, getContext2D)
import Lunarflow.Ast (Expression, printDeBrujin, withDebrujinIndices)
import Lunarflow.Function ((|>))
import Lunarflow.Parser (unsafeParseLambdaCalculus)
import Lunarflow.Tea (TeaM, tea)
import React.Ref (NativeNode)
import React.Ref as Ref
import Run.State (get, modify)
import Unsafe.Coerce (unsafeCoerce)

type ExpressionEnvironment
  = { expressions :: Map.HashMap String Expression
    }

type EditorState
  = { env :: ExpressionEnvironment
    , selectedExpression :: Maybe String
    , context ::
        Maybe
          { node :: NativeNode
          , context :: Context2D
          }
    , initializing :: Boolean
    }

type ExpressionInput
  = { name :: String
    , expression :: Expression
    , isSelected :: Boolean
    }

data EditorAction
  = SelectExpression String
  | WithRef NativeNode EditorAction
  | Render
  | Init

editor :: forall a. Widget HTML a
editor = tea defaultState editor' handleAction
  where
  defaultState :: EditorState
  defaultState =
    { env:
        { expressions:
            Map.fromFoldable
              [ mkFunction "zero" """\s z. z"""
              , mkFunction "one" """\s z. s z"""
              , mkFunction "succ" """\n s z. s (n s z)"""
              ]
        }
    , selectedExpression: Just "succ"
    , context: Nothing
    , initializing: true
    }

  mkFunction :: String -> String -> Tuple String Expression
  mkFunction name input = Tuple name (unsafeParseLambdaCalculus input |> withDebrujinIndices)

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
            [ D.text (printDeBrujin expression)
            ]
        ]
    ]

editor' :: EditorState -> Widget HTML EditorAction
editor' state = do
  canvasRef <- liftEffect Ref.createNodeRef
  action <-
    D.div [ P.className "editor" ]
      [ D.div [ P.className "editor__expression" ]
          (state.env.expressions |> Map.toArrayBy mkExpression)
      , D.canvas [ P.className "editor__visualization", P.ref $ Ref.fromRef canvasRef ] []
      , D.div [ P.className "editor__reduction" ] []
      ]
      <|> spice
  liftEffect (Ref.getCurrentRef canvasRef)
    <#> foldr WithRef action
  where
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
handleAction :: EditorAction -> TeaM EditorState Unit
handleAction = case _ of
  Init -> do
    Console.log "initializing"
    modify $ set (prop _initializing) false
  SelectExpression name -> modify $ selectExpression name
  WithRef node action -> do
    maybeContext <- get <#> _.context
    case maybeContext of
      Just _ -> pure unit
      Nothing -> do
        context <- liftEffect $ getContext2D (refToCanvas node)
        modify $ set (prop _context) $ Just { node, context }
        handleAction Render
    handleAction action
  Render ->
    get <#> _.context
      >>= traverse_ \context -> do
          Console.log "rendering"

-- State manipulating
selectExpression :: String -> EditorState -> EditorState
selectExpression name = set (prop _selectedExpression) $ Just name

-- Helpers
refToCanvas :: NativeNode -> CanvasElement
refToCanvas = unsafeCoerce

-- SProxies (generated using a vscode snippet) 
_env :: SProxy "env"
_env = SProxy

_selectedExpression :: SProxy "selectedExpression"
_selectedExpression = SProxy

_context :: SProxy "context"
_context = SProxy

_initializing :: SProxy "initializing"
_initializing = SProxy
