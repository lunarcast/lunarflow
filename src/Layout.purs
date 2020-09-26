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
import Lunarflow.Ast (Ast(..), Expression)

-- | Extensible record for asts which horizontally index all the lines.
type WithIndex r
  = ( index :: Int | r )

-- | Layouts are expressions which have vertical indices for every line.
type Layout
  = Ast Int { | WithIndex () } { | WithIndex () }

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
addIndices :: Expression -> LayoutM Layout
addIndices = go
  where
  -- | Check if a var is used in a certain expression
  hasVar :: Int -> Expression -> Boolean
  hasVar target = case _ of
    Var value -> value == target
    Call _ function argument -> hasVar target function || hasVar target argument
    Lambda _ body -> hasVar (target + 1) body

  -- | Get all the vars referenced in a given expression.
  usedVars :: Expression -> Set.Set Int
  usedVars = case _ of
    Var value -> Set.singleton value
    Call _ function argument -> usedVars function `Set.union` usedVars argument
    Lambda _ body -> Set.mapMaybe mapVar (usedVars body)
      where
      mapVar :: Int -> Maybe Int
      mapVar 0 = Nothing

      mapVar a = Just (a - 1)

  go :: Expression -> LayoutM Layout
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
    Lambda _ body -> Lambda { index: 0 } <$> go body

-- | Run the computations represented by a LayoutM monad.
runLayoutM :: forall a. LayoutM a -> List.List a
runLayoutM m = evalState noListT { indexMap: [] }
  where
  noListT = ListT.foldl (flip List.Cons) List.Nil noReader

  noReader = runReaderT m mempty
