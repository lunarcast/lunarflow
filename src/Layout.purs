module Lunarflow.Layout
  ( WithPositionId
  , Layout
  , LayoutContext
  , LayoutState
  , LayoutLambdaData
  , LayoutM
  , PositionId(..)
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
import Data.Bifunctor (lmap)
import Data.Default (def)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Debug.Trace (traceM)
import Lunarflow.Ast (Ast(..), Expression, collectLambdas)
import Lunarflow.Utils (indexed)
import Undefined (undefined)

-- | Extensible record for asts which horizontally index all the lines.
type WithPositionId r
  = ( position :: PositionId | r )

-- | Data carried around by lambdas in layouts.
type LayoutLambdaData
  = Record
      ( WithPositionId
          ( argumentPosition :: PositionId
          )
      )

-- | Layouts are expressions which have vertical indices for every line.
type Layout
  = Ast { | WithPositionId ( index :: Int ) } { | WithPositionId () } LayoutLambdaData

-- | Sproxy used for adding indices to records.
_index :: SProxy "index"
_index = SProxy

-- | The index at which the position we are looking for is stored in the indexMap. 
newtype PositionId
  = PositionId Int

derive instance newtypePositionid :: Newtype PositionId _

derive instance eqPositionId :: Eq PositionId

derive instance ordPositionId :: Ord PositionId

-- | Read only context we can access at any time while generating layouts.
type LayoutContext
  = { protected :: Set.Set PositionId
    , positions :: List.List PositionId
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

  protect :: forall a. Set.Set PositionId -> LayoutM a -> LayoutM a
  protect inputs = local (\a -> a { protected = a.protected <> inputs })

  getPosition :: Layout -> PositionId
  getPosition = case _ of
    Var { position } -> position
    Call { position } _ _ -> position
    Lambda { position } _ -> position

  setState :: LayoutState -> LayoutM Unit
  setState = lift <<< lift <<< put

  getState :: LayoutM LayoutState
  getState = lift $ lift get

  newPosition :: Int -> LayoutM PositionId
  newPosition index = do
    state <- getState
    setState state { indexMap = state.indexMap <> pure index }
    pure $ PositionId $ List.length state.indexMap

  everywhere :: Set.Set PositionId -> LayoutM PositionId
  everywhere exclude = do
    state <- getState
    ctx <- ask
    let
      indices :: LayoutM (Tuple PositionId Int)
      indices = lmap PositionId <$> list
        where
        list = lift $ List.toUnfoldable $ indexed state.indexMap

      except :: Set.Set PositionId
      except = exclude <> ctx.protected
    Tuple positionId index <- indices <|> pure (Tuple (PositionId $ -1) (-1))
    traceM { positionId, index }
    let
      existing = do
        guard $ not $ Set.member positionId except
        guard (index /= -1)
        pure positionId

      new = do
        position <- newPosition (index + 1)
        setState state { indexMap = updateIndexMap state.indexMap }
        pure position
        where
        updateIndexMap input = mapInput <$> input

        mapInput a
          | a > index = a + 1
          | otherwise = a
    existing <|> new

  go :: Expression -> LayoutM Layout
  go expression = do
    let
      (Tuple data' body) = collectLambdas expression
    case body of
      Var index -> do
        positions <- asks _.positions
        case positions `List.index` index of
          Just position -> pure $ Var { index, position }
          Nothing -> undefined
      -- Call _ func arg ->
      --   noContinuation do
      --     vars <- ask
      --     func' <- protect inArg $ go func
      --     arg' <- go arg
      --     funcIndex <- getIndex func'
      --     argIndex <- getIndex arg'
      --     index <- everywhere (Set.fromFoldable [ funcIndex, argIndex ])
      --     pure $ Call { index } func' arg'
      --   where
      --   inArg = usedVars arg
      other -> go other

-- | Run the computations represented by a LayoutM monad.
runLayoutM :: forall a. LayoutM a -> List.List a
runLayoutM m = evalState noListT def
  where
  noListT = ListT.foldl (flip List.Cons) List.Nil noReader

  noReader = runReaderT m emptyContext

  emptyContext :: LayoutContext
  emptyContext = { protected: Set.empty, positions: List.Nil }
