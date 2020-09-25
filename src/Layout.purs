module Lunarflow.Layout
  ( WithIndex
  , Layout
  , LayoutContext
  , LayoutState
  , LayoutM
  , addIndices
  , runLayoutM
  ) where

import Prelude
import Control.Monad.List.Trans as ListT
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.State (State, evalState)
import Data.Array as Array
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Debug.Trace (traceM)
import Lunarflow.Ast (Ast(..), GroupedLambdaData, GroupedExpression)
import Record as Record
import Type.Row (type (+))

-- | Extensible record for asts which horizontally index all the lines.
type WithIndex r
  = ( index :: Int | r )

-- | Layouts are expressions which have vertical indices for every line.
type Layout
  = Ast Int { | WithIndex () } { | WithIndex + GroupedLambdaData (WithIndex ()) }

-- | Sproxy used for adding indices to records.
_index :: SProxy "index"
_index = SProxy

-- | Read only context we can access at any time while generating layouts.
type LayoutContext
  = Set.Set Int

-- | State we manipulate while generating layouts.
type LayoutState
  = { indexMap :: Array Int }

-- | The monad we generate layouts in.
type LayoutM a
  = ReaderT LayoutContext (ListT.ListT (State LayoutState)) a

-- | Generate a layout from an ast.
addIndices :: GroupedExpression -> LayoutM Layout
addIndices = go
  where
  -- | Check if a var is used in a certain expression
  hasVar :: Int -> GroupedExpression -> Boolean
  hasVar target = case _ of
    Var value -> value == target
    Call _ function argument -> hasVar target function || hasVar target argument
    Lambda { arguments } body -> hasVar (List.length arguments) body

  -- | Get all the vars referenced in a given expression.
  usedVars :: GroupedExpression -> Set.Set Int
  usedVars = case _ of
    Var value -> Set.singleton value
    Call _ function argument -> usedVars function `Set.union` usedVars argument
    Lambda { arguments } body -> Set.mapMaybe mapVar (usedVars body)
      where
      mapVar :: Int -> Maybe Int
      mapVar a
        | a < argCount = Nothing
        | otherwise = Just (a - argCount)

      argCount :: Int
      argCount = List.length arguments

  go :: GroupedExpression -> LayoutM Layout
  go = case _ of
    Var name -> pure $ Var name
    Call _ func arg -> do
      vars <- ask
      traceM ("Calling " <> show func <> " with " <> show arg <> " vars in arg: " <> show (Array.fromFoldable inArg) <> " and vars in scope " <> show (Array.fromFoldable vars))
      func' <- local (append inArg) $ go func
      arg' <- go arg
      pure $ Call callData func' arg'
      where
      callData = { index: 0 }

      inArg = usedVars arg
    Lambda lambdaData body -> Lambda { arguments, index: 0 } <$> go body
      where
      arguments = flip List.mapWithIndex lambdaData.arguments \index arg -> Record.insert _index index arg

-- | Run the computations represented by a LayoutM monad.
runLayoutM :: forall a. LayoutM a -> List.List a
runLayoutM m = evalState noListT { indexMap: [] }
  where
  noListT = ListT.foldl (flip List.Cons) List.Nil noReader

  noReader = runReaderT m mempty
