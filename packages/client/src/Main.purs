module Main where

import Prelude
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import Lunarflow.Component.Editor (editor)

{-
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
        -- let
        --   groupedBinaryAst = groupedToBinary $ snd layout'
        --   binaryAst = indexedToBinary $ ungroupExpression const $ snd layout'
        --   a = toBuffer groupedBinaryAst
        -- liftEffect
        --   $ table
        --       { normal:
        --         { size: binaryAst.size
        --         , bits: showBinaryAst binaryAst
        --         }
        --       , grouped:
        --         { size: groupedBinaryAst.size
        --         , bits: showBinaryAst groupedBinaryAst
        --         }
        --       }
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
-}
main :: Effect Unit
main = runWidgetInDom "root" editor
