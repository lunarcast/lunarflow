module Lunarflow.Layout
  ( WithIndex
  , Layout
  , LayoutContext
  , LayoutState
  , LayoutLambdaData
  , LayoutM
  , addIndices
  , runLayoutM
  ) where

import Prelude
import Control.Monad.List.Trans (lift)
import Control.Monad.List.Trans as ListT
import Control.Monad.Reader (ReaderT, ask, asks, local, runReaderT)
import Control.Monad.State (State, evalState, get, put)
import Control.MonadZero (guard)
import Control.Plus ((<|>))
import Data.Default (def)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Debug.Trace (traceM)
import Lunarflow.Ast (Ast(..), Expression)

-- | Extensible record for asts which horizontally index all the lines.
type WithIndex r
  = ( index :: Int | r )

-- | Data carried around by lambdas in layouts.
type LayoutLambdaData
  = Record
      ( WithIndex
          ( argIndex :: Int
          )
      )

-- | Layouts are expressions which have vertical indices for every line.
type Layout
  = Ast Int { | WithIndex () } LayoutLambdaData

-- | Sproxy used for adding indices to records.
_index :: SProxy "index"
_index = SProxy

-- | Read only context we can access at any time while generating layouts.
type LayoutContext
  = { protected :: Set.Set Int
    , continuation :: Maybe Int
    , indices :: List.List Int
    }

-- | State we manipulate while generating layouts.
type LayoutState
  = { indexMap :: List.List Int
    }

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

  continueLambda :: forall a. (Int -> LayoutM a) -> LayoutM a
  continueLambda m = do
    continuation <- asks _.continuation
    let
      index = maybe 0 (_ + 1) continuation

      updateContinuation = _ { continuation = Just index }
    local updateContinuation (m index)

  noContinuation :: forall a. LayoutM a -> LayoutM a
  noContinuation = local (_ { continuation = Nothing })

  mapProtected :: forall a. (Set.Set Int -> Set.Set Int) -> LayoutM a -> LayoutM a
  mapProtected f = local (\a -> a { protected = f a.protected })

  getIndex :: Layout -> LayoutM Int
  getIndex = case _ of
    Var index -> do
      indices <- asks _.indices
      pure $ fromMaybe 0 (indices `List.index` index)
    Call { index } _ _ -> pure index
    Lambda { index } _ -> pure index

  setState :: LayoutState -> LayoutM Unit
  setState = lift <<< lift <<< put

  getState :: LayoutM LayoutState
  getState = lift $ lift get

  everywhere :: Set.Set Int -> (Int -> Layout) -> LayoutM Int
  everywhere exclude layout = do
    state <- getState
    ctx <- ask
    indices <- lift $ List.toUnfoldable $ pure <$> state.indexMap
    let
      except = exclude <> ctx.protected
    index <- indices <|> pure (-1)
    let
      existing = do
        guard $ not $ Set.member index except
        guard (index /= -1)
        pure index

      new = do
        setState state { indexMap = updateIndexMap state.indexMap }
        pure $ List.length state.indexMap
        where
        updateIndexMap input = (mapInput <$> input) <> pure (index + 1)

        mapInput a
          | a > index = a + 1
          | otherwise = a
    existing <|> new

  go :: Expression -> LayoutM Layout
  go = case _ of
    Var name -> pure $ Var name
    Call _ func arg ->
      noContinuation do
        vars <- ask
        traceM ("Calling " <> show func <> " with " <> show arg <> " vars in arg: " <> show (List.fromFoldable inArg) <> " and vars in scope " <> show (List.fromFoldable vars.protected))
        func' <- mapProtected (append inArg) $ go func
        arg' <- go arg
        pure $ Call callData func' arg'
      where
      callData = { index: 0 }

      inArg = usedVars arg
    Lambda _ body ->
      continueLambda \argIndex ->
        local (\a -> a { indices = List.Cons argIndex a.indices })
          $ Lambda { argIndex, index: 0 }
          <$> go body

-- | Run the computations represented by a LayoutM monad.
runLayoutM :: forall a. LayoutM a -> List.List a
runLayoutM m = evalState noListT def
  where
  noListT = ListT.foldl (flip List.Cons) List.Nil noReader

  noReader = runReaderT m emptyContext

  emptyContext :: LayoutContext
  emptyContext = { protected: Set.empty, continuation: Nothing, indices: List.Nil }
