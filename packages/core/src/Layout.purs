module Lunarflow.Layout
  ( ScopedLayoutF
  , ScopedLayout
  , LayoutF
  , Layout
  , LayoutContext
  , LayoutState
  , LayoutError(..)
  , LayoutM
  , Position(..)
  , ScopeId(..)
  , IndexMap
  , addIndices
  , unscopeLayout
  , scopeLayout
  , runLayoutM
  , runLayoutMWithState
  , markRoot
  , shiftLines
  , unscopePosition
  , currentScope
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Debug (class Debug, genericDebug)
import Data.Debug as Debug
import Data.Either (Either)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Set as Set
import Data.Traversable (for, minimumBy)
import Data.Tuple (Tuple(..), fst, snd)
import Lunarflow.Ast (AstF(..), call, lambda, var)
import Lunarflow.Ast.Grouped (GroupedExpression, references)
import Lunarflow.Function ((|>))
import Lunarflow.List (indexed)
import Lunarflow.Mu (Mu)
import Matryoshka (cata, para, project)
import Run (Run, extract)
import Run.Except (EXCEPT, note, runExcept, throw)
import Run.Reader (READER, ask, local, runReader)
import Run.State (STATE, get, modify, put, runState)

---------- Types
-- | Mapping from positions to vertical indices.
type IndexMap
  = Map.Map ScopeId (List.List Int)

-- | The base functor for scoped layouts.
type ScopedLayoutF
  = AstF { index :: Int, position :: Position } { position :: Position }
      { args :: List.List Position
      , scope :: ScopeId
      , isRoot :: Boolean
      , position :: Position
      }

-- | The base functor for layouts.
type LayoutF
  = AstF { position :: Int, index :: Int } Int
      { position :: Int
      , args :: List.List Int
      , isRoot :: Boolean
      }

-- | Layouts are expressions which have vertical indices for every line.
type Layout
  = Mu LayoutF

-- | Layouts are expressions which have vertical indices for every line.
-- | This is the scoped variant, which keeps track of what scope everything is from.
type ScopedLayout
  = Mu ScopedLayoutF

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
    , near :: Int
    }

-- | State we manipulate while generating layouts.
type LayoutState
  = { indexMap :: IndexMap
    , lastScope :: Int
    }

-- TODO: support free variables 
data LayoutError
  = VarNotInScope Int
  | MissingIndexMap ScopeId
  | MissingPosition (List.List Int) ScopeId Int

-- | The monad we generate layouts in.
type LayoutM
  = Run
      ( reader :: READER LayoutContext
      , state :: STATE LayoutState
      , except :: EXCEPT LayoutError
      )

-- | Different ways we can constrain where expressions can be placed.
data LocationConstraint
  = Everywhere
  | Above
  | Below

-- | Actions we can take in order to place an expression.
data LocationSearchResult
  = At Int
  | NextTo Int

---------- Layout generation
-- | Generate a layout from an ast.
addIndices :: GroupedExpression -> LayoutM ScopedLayout
addIndices =
  para case _ of
    Var { index } -> ado
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
      scope <- currentScope
      let
        functionPosition@(Position functionIndex functionScope) = getPosition function

        -- PureScript doesn't genralize let bindings 
        -- so we have to write this type definition ourselves.
        nearFunction :: LayoutM ~> LayoutM
        nearFunction m = if functionScope == scope then attractTo functionIndex m else m
      argument <- protect (Set.singleton functionPosition) $ nearFunction layoutArgument
      let
        argumentPosition = getPosition argument

        except = Set.fromFoldable [ functionPosition, argumentPosition ]
      constraint <- constrain functionPosition argumentPosition
      position <- nearFunction $ ask <#> _.near >>= placeExpression except constraint
      pure $ call { position } function argument
    Lambda { args: vars } (Tuple groupedBody layoutBody) -> do
      scope <- newScope
      initialState <- get
      let
        args = flip List.mapWithIndex vars \index name -> Position (varCount - index - 1) scope
      put initialState { indexMap = Map.insert scope newList initialState.indexMap }
      body <-
        flip local layoutBody \ctx ->
          ctx
            { positions = args <> ctx.positions
            , near = 0
            , currentScope = scope
            }
      -- TODO: abstract this away.
      let
        -- This type annotation is here so the compiler
        -- knows which Unfoldable instance to use.
        referenced :: Array _
        referenced = Set.toUnfoldable $ references $ lambda { args: vars } groupedBody
      -- This holds all the variables referenced inside the body of the lambda
      inBody <- Set.fromFoldable <$> for referenced getVarPosition
      position <- ask <#> _.near >>= placeExpression inBody Everywhere
      pure $ lambda { position, args, scope, isRoot: false } body
      where
      newList = List.range 0 (varCount - 1)

      varCount = List.length vars

-- | Search for the optimal location to put something at.
findLocation ::
  forall r.
  { constraint :: LocationConstraint
  , positionIndex :: Int
  , positions :: List.List Int
  , except :: Set.Set Int
  , scope :: ScopeId
  } ->
  Run ( except :: EXCEPT LayoutError | r ) LocationSearchResult
findLocation { constraint, positionIndex, positions, except, scope } = ado
  position <- note (MissingPosition positions scope positionIndex) $ List.index positions positionIndex
  -- 
  let
    allowed :: Int -> Boolean
    allowed x = not $ Set.member x except

    passesConstraint :: Int -> Boolean
    passesConstraint x = case constraint of
      Everywhere -> true
      Above -> x > position
      Below -> x < position

    -- | If an optimal existing spot exists, this finds it.
    unused :: Maybe Int
    unused =
      positions
        |> indexed
        |> List.filter (snd >>> passesConstraint)
        |> List.filter (fst >>> allowed)
        |> map (map distanceToPosition)
        |> minimumBy (on compare snd)
        |> map snd
      where
      distanceToPosition x = abs (position - x)
  --
in case unused of
    Just index -> At index
    Nothing ->
      NextTo case constraint of
        Below -> position - 1
        _ -> position

-- | Gets the position of an expression and a constraint and places it somewhere.
placeExpression :: Set.Set Position -> LocationConstraint -> Int -> LayoutM Position
placeExpression except constraint positionIndex = do
  ctx <- ask
  positions <- currentIndexList
  result <-
    findLocation
      { constraint
      , positionIndex
      , positions
      , scope: ctx.currentScope
      , except:
        (ctx.protected <> except)
          |> Set.mapMaybe \(Position index scope) -> do
              guard (scope == ctx.currentScope)
              Just index
      }
  case result of
    At index -> createPosition index ctx.currentScope
    NextTo y -> do
      shiftLines ctx.currentScope { past: y, amount: 1 }
      createPosition (y + 1) ctx.currentScope

-- | Create and push a new position into a map.
createPosition :: Int -> ScopeId -> LayoutM Position
createPosition position scope = do
  state <- get
  -- TODO: consider a helper for updating the indexMap
  put
    state
      { indexMap =
        Map.update
          ( flip List.snoc position
              >>> Just
          )
          scope
          state.indexMap
      }
  case Map.lookup scope state.indexMap of
    Just list -> pure $ Position (List.length list) scope
    Nothing -> throw $ MissingIndexMap scope

---------- Converstion functions
-- | Transform a scoped layout into an unscoped one.
unscopeLayout :: ScopedLayout -> LayoutM Layout
unscopeLayout =
  cata case _ of
    Var { index, position } -> unscopePosition position <#> \position' -> var { position: position', index }
    Call { position } function argument -> ado
      function' <- function
      argument' <- argument
      position' <- unscopePosition position
      in call position' function' argument'
    Lambda { position, scope, args, isRoot } body -> ado
      position' <- unscopePosition position
      body' <- body
      args' <- for args unscopePosition
      in lambda { position: position', args: args', isRoot } body'

-- | Transform an unscoped layout into a scoped one
scopeLayout :: Layout -> LayoutM ScopedLayout
scopeLayout =
  cata case _ of
    Var { position, index } -> do
      position' <- getVarPosition index
      pure $ var { position: position', index }
    Call position function argument -> ado
      function' <- function
      argument' <- argument
      position' <- currentScope >>= createPosition position
      in call { position: position' } function' argument'
    Lambda { position, args, isRoot } body -> do
      scope <- newScope
      modify \s -> s { indexMap = Map.insert scope List.Nil s.indexMap }
      position' <- currentScope >>= createPosition position
      args' <- for args $ flip createPosition scope
      body' <-
        flip local body \ctx ->
          ctx
            { positions = args' <> ctx.positions
            , currentScope = scope
            }
      pure $ lambda { isRoot, scope, position: position', args: args' } body'

---------- Helpers
-- | Move all the lines past a certain point by an arbitrary amount.
shiftLines :: ScopeId -> { past :: Int, amount :: Int } -> LayoutM Unit
shiftLines scope { past, amount } =
  modify \state ->
    state
      { indexMap =
        Map.update (map update >>> Just)
          scope
          state.indexMap
      }
  where
  update x
    | x > past = x + amount
    | otherwise = x

-- | If the expression starts off with a lambda, mark it up as the root lambda.
-- | We do this to avoid adding an unnecessary box around the root lambda later in the pipeline.
markRoot :: LayoutM ScopedLayout -> LayoutM ScopedLayout
markRoot =
  map \x -> case project x of
    Lambda data' body -> lambda (data' { isRoot = true }) body
    _ -> x

-- | Get the absolute position described by a relative one.
unscopePosition :: Position -> LayoutM Int
unscopePosition (Position index scope) = do
  state <- get
  -- NOTE: we could use `note`, but I think this is cleaner, albeit more verbose
  case Map.lookup scope state.indexMap of
    Just list -> case List.index list index of
      Just position -> pure position
      Nothing -> throw $ MissingPosition list scope index
    Nothing -> throw $ MissingIndexMap scope

-- | Get the scope we are currently in.
currentScope :: LayoutM ScopeId
currentScope = ask <#> _.currentScope

---------- Internal helpers
-- | Run a computation in a context where 
-- an arbitrary set of positions is protected.
protect :: Set.Set Position -> LayoutM ~> LayoutM
protect inputs = local (\a -> a { protected = a.protected <> inputs })

-- | Run a computation in a context "attracted" to an arbitrary position.
attractTo :: Int -> LayoutM ~> LayoutM
attractTo near = local (_ { near = near })

-- | Get the position an expression already has.
getPosition :: ScopedLayout -> Position
getPosition layout = case project layout of
  Var { position } -> position
  Call { position } _ _ -> position
  Lambda { position } _ -> position

-- | Create a new scope with an unique id.
newScope :: LayoutM ScopeId
newScope = do
  state <- get
  let
    scope = 1 + state.lastScope
  put state { lastScope = scope }
  pure $ ScopeId scope

-- TODO: better error messages.
-- | Get the position a var has in the current scope.
getVarPosition :: Int -> LayoutM Position
getVarPosition index = do
  positions <- ask <#> _.positions
  case positions `List.index` index of
    Just position -> pure position
    Nothing -> throw $ VarNotInScope index

-- | Get the list of indices describing the scope we are in.
currentIndexList :: LayoutM (List.List Int)
currentIndexList = do
  state <- get
  ctx <- ask
  case Map.lookup ctx.currentScope state.indexMap of
    Just list -> pure list
    Nothing -> throw $ MissingIndexMap ctx.currentScope

-- | Find the correct way to constrain where the result of a call can be placed.
constrain :: Position -> Position -> LayoutM LocationConstraint
constrain functionPosition@(Position _ functionScope) argumentPosition@(Position _ argumentScope) = do
  scope <- ask <#> _.currentScope
  -- TODO: make this smarter 
  if functionScope /= scope || argumentScope /= scope then
    pure Everywhere
  else ado
    functionAbsolutePosition <- unscopePosition functionPosition
    argumentAbsolutePosition <- unscopePosition argumentPosition
    in if functionAbsolutePosition < argumentAbsolutePosition then
      Below
    else
      Above

-- | Run the computations in the Layout monad.
runLayoutM :: forall a. LayoutM a -> Either LayoutError (Tuple LayoutState a)
runLayoutM =
  runLayoutMWithState
    { indexMap: Map.singleton Root $ List.singleton 0
    , lastScope: -1
    }

-- | Run the computations in the Layout monad.
runLayoutMWithState :: forall a. LayoutState -> LayoutM a -> Either LayoutError (Tuple LayoutState a)
runLayoutMWithState state = runReader ctx >>> runState state >>> runExcept >>> extract
  where
  ctx :: LayoutContext
  ctx =
    { protected: Set.empty
    , positions: List.Nil
    , currentScope: Root
    , near: 0
    }

---------- Typeclass instances
derive instance eqScopeId :: Eq ScopeId

derive instance ordScopeId :: Ord ScopeId

derive instance genericScopeId :: Generic ScopeId _

instance debugScopeId :: Debug ScopeId where
  debug = genericDebug

instance showScopeId :: Show ScopeId where
  show = case _ of
    Root -> "<root>"
    ScopeId id -> show id

derive instance eqPosition :: Eq Position

derive instance ordPosition :: Ord Position

derive instance genericPosition :: Generic Position _

derive instance genericLayoutError :: Generic LayoutError _

instance debugPosition :: Debug.Debug Position where
  debug = genericDebug

instance debugLayoutError :: Debug LayoutError where
  debug = genericDebug

instance showLayoutError :: Show LayoutError where
  show = case _ of
    VarNotInScope index -> "Variable " <> show index <> " is not in scope."
    MissingIndexMap scope -> "Cannot find index list for scope " <> show scope <> "."
    MissingPosition list scope index ->
      "Cannot find position "
        <> show index
        <> " in scope "
        <> show scope
        <> " while looking inside "
        <> show (Array.fromFoldable list)
        <> "."
