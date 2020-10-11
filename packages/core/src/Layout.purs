module Lunarflow.Layout
  ( WithPosition
  , ScopedLayout
  , Layout
  , LayoutContext
  , LayoutState
  , LayoutLambdaData
  , LayoutM
  , Position(..)
  , ScopeId(..)
  , IndexMap
  , addIndices
  , runLayoutM
  , fromScoped
  ) where

import Prelude
import Control.Monad.List.Trans (lift)
import Control.Monad.List.Trans as ListT
import Control.Monad.Reader (ReaderT, ask, asks, local, runReaderT)
import Control.Monad.State (State, evalState, get, gets, modify_, put)
import Control.MonadZero (empty, guard)
import Control.Plus ((<|>))
import Data.Debug (class Debug, genericDebug)
import Data.Debug as Debug
import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (for)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..), uncurry)
import Lunarflow.Ast (Ast(..), GroupedExpression)
import Lunarflow.Utils (indexed)

-- | Mapping from positions to vertical indices.
type IndexMap
  = List.List Int

-- | Extensible record for asts which horizontally index all the lines.
type WithPosition r
  = ( position :: Position | r )

-- | Data carried around by lambdas in layouts.
type LayoutLambdaData
  = { 
    | WithPosition
      ( args ::
        List.List
          { position :: Position
          , name :: String
          }
      , indexMap :: IndexMap
      , scope :: ScopeId
      )
    }

-- | Layouts are expressions which have vertical indices for every line.
-- | This is the scoped variant, which keeps track of what scope everything is from.
type ScopedLayout
  = Ast { | WithPosition ( index :: Int ) } { | WithPosition () } LayoutLambdaData

type Layout
  = Ast { position :: Int, index :: Int } Int { position :: Int, args :: List.List Int }

-- | Ids used to connect lambdas together in visual chunks.
data ScopeId
  = ScopeId Int
  | Root

derive instance eqScopeId :: Eq ScopeId

derive instance ordScopeId :: Ord ScopeId

derive instance genericScopeId :: Generic ScopeId _

instance debugScopeId :: Debug ScopeId where
  debug = genericDebug

-- | The index at which the position we are looking for is stored in the indexMap. 
data Position
  = Position Int ScopeId

derive instance eqPosition :: Eq Position

derive instance ordPosition :: Ord Position

derive instance genericPosition :: Generic Position _

instance debugPosition :: Debug.Debug Position where
  debug = genericDebug

-- | Read only context we can access at any time while generating layouts.
type LayoutContext
  = { protected :: Set.Set Position
    , positions :: List.List Position
    , currentScope :: ScopeId
    }

-- | State we manipulate while generating layouts.
type LayoutState
  = { indexMap :: IndexMap
    , lastScope :: Int
    }

-- | The monad we generate layouts in.
type LayoutM a
  = ReaderT LayoutContext (ListT.ListT (State LayoutState)) a

-- | Generate a layout from an ast.
addIndices :: GroupedExpression -> LayoutM ScopedLayout
addIndices = go
  where
  -- | Get all the vars referenced in a given expression.
  usedVars :: GroupedExpression -> Set.Set Int
  usedVars = case _ of
    Var value -> Set.singleton value
    Call _ function argument -> usedVars function `Set.union` usedVars argument
    Lambda vars body -> Set.mapMaybe mapVar (usedVars body)
      where
      mapVar :: Int -> Maybe Int
      mapVar a
        | a < varCount = Nothing
        | otherwise = Just (a - varCount)

      varCount = List.length vars

  protect :: forall a. Set.Set Position -> LayoutM a -> LayoutM a
  protect inputs = local (\a -> a { protected = a.protected <> inputs })

  getPosition :: ScopedLayout -> Position
  getPosition = case _ of
    Var { position } -> position
    Call { position } _ _ -> position
    Lambda { position } _ -> position

  setState :: LayoutState -> LayoutM Unit
  setState = lift <<< lift <<< put

  modifyState :: (LayoutState -> LayoutState) -> LayoutM Unit
  modifyState = lift <<< lift <<< modify_

  getState :: LayoutM LayoutState
  getState = lift $ lift get

  mkPosition :: Int -> LayoutM Position
  mkPosition index = do
    scope <- asks _.currentScope
    pure $ Position index scope

  newPosition :: Int -> LayoutM Position
  newPosition index = do
    state <- getState
    setState state { indexMap = state.indexMap <> pure index }
    mkPosition $ List.length state.indexMap

  everywhere :: Set.Set Position -> LayoutM Position
  everywhere exclude = do
    state <- getState
    ctx <- ask
    let
      indices :: LayoutM (Tuple Position Int)
      indices = list >>= uncurry updatePosition
        where
        updatePosition positionIndex index = do
          position <- mkPosition positionIndex
          pure $ Tuple position index

        list = lift $ List.toUnfoldable $ indexed state.indexMap

      except :: Set.Set Position
      except = exclude <> ctx.protected

      indexMapLength = List.length state.indexMap

      floor = Tuple (Position (-1) ctx.currentScope) (-1)
    Tuple position index <- indices <|> pure floor
    -- The List.toUnfoldable resets the state, so we set it to the previous value.
    setState state
    let
      existing = do
        guard $ not $ Set.member position except
        guard (index /= -1)
        pure position

      new = do
        position' <- mkPosition indexMapLength
        setState state { indexMap = updateIndexMap state.indexMap <> List.singleton (index + 1) }
        pure position'
        where
        updateIndexMap input = mapInput <$> input

        mapInput a
          | a > index = a + 1
          | otherwise = a
    existing <|> new

  newScope :: LayoutM ScopeId
  newScope = do
    state <- getState
    let
      scope = 1 + state.lastScope
    setState state { lastScope = scope }
    pure $ ScopeId scope

  getVarPosition :: Int -> LayoutM Position
  getVarPosition index = do
    positions <- asks _.positions
    case positions `List.index` index of
      Just position -> pure position
      Nothing -> empty

  usedPositions :: GroupedExpression -> LayoutM (Set.Set Position)
  usedPositions expression = do
    let
      -- This type annotation is here so the compiler
      -- knows which Unfoldable instance to use.
      vars :: Array _
      vars = Set.toUnfoldable $ usedVars expression
    Set.fromFoldable <$> for vars getVarPosition

  go :: GroupedExpression -> LayoutM ScopedLayout
  go = case _ of
    Var index -> do
      position <- getVarPosition index
      pure $ Var { index, position }
    Call _ func arg -> do
      inArg <- usedPositions arg
      func' <- protect inArg $ go func
      arg' <- go arg
      let
        funcPosition = getPosition func'

        argPosition = getPosition arg'
      position <- everywhere (Set.fromFoldable [ funcPosition, argPosition ])
      pure $ Call { position } func' arg'
    Lambda vars body -> do
      oldIndexMap <- getState <#> _.indexMap
      modifyState _ { indexMap = List.Nil }
      scope <- newScope
      { body', args } <-
        local (_ { currentScope = scope }) do
          let
            varCount = List.length vars
          args <-
            forWithIndex vars \index name -> do
              position <- newPosition (varCount - 1 - index)
              pure { name, position }
          let
            updateCtx ctx =
              ctx
                { positions = newPositions <> ctx.positions
                }
              where
              newPositions = _.position <$> args
          body' <- local updateCtx $ go body
          pure { body', args }
      indexMap <- getState <#> _.indexMap
      modifyState _ { indexMap = oldIndexMap }
      position <- everywhere Set.empty
      pure $ Lambda { position, args, indexMap, scope } body'

-- | Run the computations represented by a LayoutM monad.
runLayoutM :: forall a. LayoutM a -> List.List (Tuple a IndexMap)
runLayoutM m = evalState noListT def
  where
  noListT :: _
  noListT = ListT.foldl (flip List.Cons) List.Nil noReader

  noReader :: ListT.ListT (State LayoutState) (Tuple a IndexMap)
  noReader = runReaderT m' emptyContext

  emptyContext :: LayoutContext
  emptyContext = { protected: Set.empty, positions: List.Nil, currentScope: Root }

  m' :: LayoutM (Tuple a IndexMap)
  m' = ado
    v <- m
    s <- lift $ lift $ gets _.indexMap
    in Tuple v s

-- | Transform a scoped layout into an unscoped one.
fromScoped :: Tuple ScopedLayout IndexMap -> Maybe Layout
fromScoped (Tuple layout rootMap) = go Map.empty layout
  where
  getPosition :: Map.Map ScopeId IndexMap -> Position -> Maybe Int
  getPosition scopeMap (Position index scope) = do
    indexMap <-
      if scope == Root then
        Just rootMap
      else
        Map.lookup scope scopeMap
    indexMap `List.index` index

  go :: Map.Map ScopeId IndexMap -> ScopedLayout -> Maybe Layout
  go scopeMap = case _ of
    Var { index, position } -> ado
      position' <- getPosition scopeMap position
      in Var { position: position', index }
    Call { position } func arg -> ado
      position' <- getPosition scopeMap position
      func' <- go scopeMap func
      arg' <- go scopeMap arg
      in Call position' func' arg'
    Lambda { position, indexMap, scope, args } body -> ado
      position' <- getPosition scopeMap position
      body' <- go scopeMap' body
      args' <- for args (getPosition scopeMap' <<< _.position)
      in Lambda { position: position', args: args' } body'
      where
      scopeMap' = Map.insert scope indexMap scopeMap
