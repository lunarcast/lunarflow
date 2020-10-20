module Lunarflow.Layout
  ( WithPosition
  , ScopedLayoutF
  , ScopedLayout
  , LayoutF
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
import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (for)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..), uncurry)
import Lunarflow.Ast (AstF(..), call, lambda, var)
import Lunarflow.Ast.Grouped (GroupedExpressionF, GroupedExpression, references)
import Lunarflow.List (indexed)
import Matryoshka (Algebra, GAlgebra, cata, para, project)

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

-- | The base functor for scoped layouts.
type ScopedLayoutF
  = AstF { | WithPosition ( index :: Int ) } { | WithPosition () } LayoutLambdaData

-- | Layouts are expressions which have vertical indices for every line.
-- | This is the scoped variant, which keeps track of what scope everything is from.
type ScopedLayout
  = Mu ScopedLayoutF

-- | The base functor for layouts.
type LayoutF
  = AstF { position :: Int, index :: Int } Int { position :: Int, args :: List.List Int }

-- | Layouts are expressions which have vertical indices for every line.
type Layout
  = Mu LayoutF

-- | Ids used to connect lambdas together in visual chunks.
data ScopeId
  = ScopeId Int
  | Root

-- | The index at which the position we are looking for is stored in the indexMap. 
data Position
  = Position Int ScopeId

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
addIndices = para algebra
  where
  algebra :: GAlgebra (Tuple GroupedExpression) GroupedExpressionF (LayoutM ScopedLayout)
  algebra = case _ of
    Var index -> ado
      position <- getVarPosition index
      in var { index, position }
    Call _ (Tuple _ layoutFunction) (Tuple groupedArgument layoutArgument) -> do
      let
        -- This type annotation is here so the compiler
        -- knows which Unfoldable instance to use.
        vars :: Array _
        vars = Set.toUnfoldable $ references groupedArgument
      -- This holds all the variables referenced inside the argument of the call
      inArgument <- Set.fromFoldable <$> for vars getVarPosition
      -- When we draw a call, we draw the function first and then the argument.
      -- We don't want the space taken by the variables in the argument to be occupied
      -- by stuff while drawing the function.
      -- So we basically inform the call not to touch those spaces.
      function <- protect inArgument layoutFunction
      argument <- layoutArgument
      ado
        position <- everywhere (Set.fromFoldable $ getPosition <$> [ function, argument ])
        in call { position } function argument
    Lambda vars (Tuple groupedBody layoutBody) -> do
      oldIndexMap <- getState <#> _.indexMap
      modifyState _ { indexMap = List.Nil }
      scope <- newScope
      ado
        { body', args } <-
          local (_ { currentScope = scope }) do
            args <-
              forWithIndex vars \index name -> do
                state <- getState
                setState state { indexMap = List.snoc state.indexMap (varCount - 1 - index) }
                position <- mkPosition $ List.length state.indexMap
                pure { name, position }
            let
              updateCtx ctx =
                ctx
                  { positions = (_.position <$> args) <> ctx.positions
                  }
            body' <- local updateCtx layoutBody
            pure { body', args }
        indexMap <- getState <#> _.indexMap
        modifyState _ { indexMap = oldIndexMap }
        position <- everywhere Set.empty
        in lambda { position, args, indexMap, scope } body'
      where
      varCount = List.length vars

  protect :: Set.Set Position -> LayoutM ~> LayoutM
  protect inputs = local (\a -> a { protected = a.protected <> inputs })

  getPosition :: ScopedLayout -> Position
  getPosition layout = case project layout of
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

type UnscopingM a
  = ReaderT (Map.Map ScopeId IndexMap) Maybe a

-- | Transform a scoped layout into an unscoped one.
fromScoped :: Tuple ScopedLayout IndexMap -> Maybe Layout
fromScoped (Tuple layout rootMap) = runReaderT (cata algebra layout) Map.empty
  where
  getPosition :: Position -> UnscopingM Int
  getPosition (Position index scope) = do
    scopeMap <- ask
    indexMap <-
      lift
        $ if scope == Root then
            Just rootMap
          else
            Map.lookup scope scopeMap
    lift $ indexMap `List.index` index

  algebra :: Algebra ScopedLayoutF (UnscopingM Layout)
  algebra = case _ of
    Var { index, position } -> ado
      position' <- getPosition position
      in var { position: position', index }
    Call { position } function argument -> ado
      function' <- function
      argument' <- argument
      position' <- getPosition position
      in call position' function' argument'
    Lambda { position, indexMap, scope, args } body -> do
      position' <- getPosition position
      local updateMap ado
        body' <- body
        args' <- for args (getPosition <<< _.position)
        in lambda { position: position', args: args' } body'
      where
      updateMap = Map.insert scope indexMap

-- | Typeclass instances
derive instance eqScopeId :: Eq ScopeId

derive instance ordScopeId :: Ord ScopeId

derive instance genericScopeId :: Generic ScopeId _

instance debugScopeId :: Debug ScopeId where
  debug = genericDebug

derive instance eqPosition :: Eq Position

derive instance ordPosition :: Ord Position

derive instance genericPosition :: Generic Position _

instance debugPosition :: Debug.Debug Position where
  debug = genericDebug
