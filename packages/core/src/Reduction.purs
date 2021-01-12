-- | Naive reductions on layouts
-- TODO: find a more efficient way of implementing this
module Lunarflow.Reduction
  ( tryEtaReducing
  ) where

import Lunarlude
import Data.List as List
import Data.Map as Map
import Lunarflow.Ast (AstF(..), Name(..), call, lambda, var)
import Lunarflow.Ast.Grouped (contains, shift)
import Lunarflow.Layout (LayoutLike, shiftLines)
import Lunarflow.LayoutM (LayoutM, Position(..), PositionPointer(..), ScopeId, currentScope, getIndexMap, unscopePosition)
import Run.Reader (local)
import Run.State (modify)
import Undefined (undefined)

-- TODO: remove
type ScopedLayout
  = LayoutLike Position () () () ( scope :: ScopeId )

betaReduce :: ScopedLayout -> ScopedLayout
betaReduce = undefined

-- | Map over all the positions in an ast.
updatePositions :: (Position -> Position) -> ScopeId -> ScopedLayout -> LayoutM ScopedLayout
updatePositions f scope =
  cata case _ of
    Call data' function argument -> ado
      function' <- function
      argument' <- argument
      in call data' { position = go data'.position } function' argument'
    Var data' -> pure $ var data' { position = go data'.position }
    Lambda data' body -> ado
      body' <- body
      in lambda data' { position = go data'.position } body'
  where
  go position@(Position _ scope') = if scope' == scope then f position else position

-- | Introduce an index list at any point inside another one
introduce :: Position -> { from :: ScopeId, into :: ScopeId } -> ScopedLayout -> LayoutM ScopedLayout
introduce position { from, into } layout = do
  listFrom <- getIndexMap from
  listInto <- getIndexMap into
  let
    height = foldr max (-1) listFrom

    movedCount = List.length listInto
  past <- unscopePosition position <#> (_ - 1)
  shiftLines into { amount: height, past }
  let
    moved = listFrom <#> (\a -> a + past + 1)
  modify \state ->
    state
      { indexMap =
        Map.update
          ( (_ <> moved) >>> Just
          )
          into
          state.indexMap
      }
  -- TODO: put stuff sharing a line with this at the middle
  layout' <-
    updatePositions
      ( \(Position (PositionPointer index) _) ->
          Position (PositionPointer $ index + movedCount) into
      )
      from
      layout
  pure layout'

-- | Transform expressions of the shape (\x -> f x) into f
etaReduce :: ScopedLayout -> LayoutM (Maybe ScopedLayout)
etaReduce layout = case project layout |> map project |> map (map project) of
  Lambda data'@{ scope, position, args: List.Cons _ remaining } (Call _ function (Var { name: Bound 0 }))
    | not (contains (Bound 0) $ embed function) ->
      Just
        <$> case remaining of
            List.Nil -> do
              outerScope <- currentScope
              result <- introduce position { from: scope, into: outerScope } $ shift 0 (-1) $ embed function
              modify \s -> s { indexMap = Map.delete scope s.indexMap }
              pure result
            _ ->
              pure
                $ lambda
                    data'
                      { scope = scope
                      , position = position
                      , args = remaining
                      }
                $ shift 0 (-1)
                $ embed function
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
        body' <- local (_ { currentScope = data'.scope }) $ tryEtaReducing' body
        case body' of
          Just body'' -> pure $ Just $ lambda data' body''
          Nothing -> pure Nothing

tryEtaReducing :: ScopedLayout -> LayoutM ScopedLayout
tryEtaReducing layout = fromMaybe layout <$> tryEtaReducing' layout
