-- | Naive reductions on layouts
-- TODO: find a more efficient way of implementing this
module Lunarflow.Reduction
  ( tryEtaReducing
  ) where

import Prelude
import Data.Foldable (foldr)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Lunarflow.Ast (AstF(..), call, lambda, var)
import Lunarflow.Ast.Grouped (contains, shift)
import Lunarflow.Function ((|>))
import Lunarflow.Layout (LayoutError(..), LayoutM, Position(..), ScopeId, ScopedLayout, currentScope, shiftLines, unscopePosition)
import Matryoshka (cata, embed, project)
import Run.Except (throw)
import Run.State (get, modify, put)
import Undefined (undefined)

betaReduce :: ScopedLayout -> ScopedLayout
betaReduce = undefined

-- | Map over all the positions in an ast.
updatePositions :: (Position -> Position) -> ScopeId -> ScopedLayout -> LayoutM ScopedLayout
updatePositions f scope =
  cata case _ of
    Call { position } function argument -> ado
      function' <- function
      argument' <- argument
      in call { position: go position } function' argument'
    Var { index, position } -> pure $ var { index, position: go position }
    Lambda data' body -> ado
      body' <- body
      in lambda data' { position = go data'.position } body'
  where
  go position@(Position _ scope') = if scope' == scope then f position else position

-- | Introduce an index list at any point inside another one
introduce :: Position -> { from :: ScopeId, into :: ScopeId } -> ScopedLayout -> LayoutM ScopedLayout
introduce position { from, into } layout = do
  state <- get
  case Map.lookup from state.indexMap, Map.lookup into state.indexMap of
    Just listFrom, Just listInto -> do
      past <- unscopePosition position
      shiftLines into { amount: height, past }
      let
        moved = listFrom <#> (\a -> a + past + 1)
      put
        state
          { indexMap =
            Map.update
              ( (_ <> moved) >>> Just
              )
              into
              state.indexMap
          }
      -- TODO: put stuff sharing a line with this at the middle
      layout' <- updatePositions (\(Position index _) -> Position (index + movedCount) into) from layout
      pure layout'
      where
      height = foldr max (-1) listFrom + 1

      movedCount = List.length listInto
    Nothing, _ -> throw $ MissingIndexMap from
    _, _ -> throw $ MissingIndexMap into

-- | Transform expressions of the shape (\x -> f x) into f
etaReduce :: ScopedLayout -> LayoutM (Maybe ScopedLayout)
etaReduce layout = case project layout |> map project |> map (map project) of
  Lambda { isRoot, scope, position, args: List.Cons _ remaining } (Call _ function (Var { index: 0 }))
    | not (contains 0 $ embed function) ->
      Just
        <$> case remaining of
            List.Nil -> do
              outerScope <- currentScope
              result <- introduce position { from: scope, into: outerScope } $ shift 0 (-1) $ embed function
              modify \s -> s { indexMap = Map.delete scope s.indexMap }
              pure result
            _ -> pure $ lambda { isRoot, scope, position, args: remaining } $ shift 0 (-1) $ embed function
  _ -> pure Nothing

-- TODO: abstract this away somehow
tryEtaReducing' :: ScopedLayout -> LayoutM (Maybe ScopedLayout)
tryEtaReducing' layout = case project layout of
  Var _ -> pure Nothing
  Call data' function argument -> do
    argument' <- tryEtaReducing' argument
    case argument' of
      Just argument'' -> pure $ Just $ call data' function argument''
      Nothing -> do
        function' <- tryEtaReducing' function
        case function' of
          Just function'' -> pure $ Just $ call data' function'' argument
          Nothing -> pure Nothing
  Lambda data' body -> do
    lambda' <- etaReduce layout
    case lambda' of
      Just _ -> pure lambda'
      Nothing -> do
        body' <- tryEtaReducing' body
        case body' of
          Just body'' -> pure $ Just $ lambda data' body''
          Nothing -> pure Nothing

tryEtaReducing :: ScopedLayout -> LayoutM ScopedLayout
tryEtaReducing layout = fromMaybe layout <$> tryEtaReducing' layout
