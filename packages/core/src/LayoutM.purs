-- | Stuff related to the layout monad.
-- | Why a different module? So errors can only be thrown 
-- | from the helpers I want to allow to error out.
module Lunarflow.LayoutM
  ( Line
  , Binder
  , ScopeId(..)
  , Position(..)
  , LayoutContext
  , LayoutState
  , LayoutError
  , IndexMap
  , LayoutM
  , missingPosition
  , freshColor
  , getIndexMap
  , getBinder
  , runLayoutM
  , runLayoutMWithState
  , currentScope
  , unscopePosition
  , while
  ) where

import Prelude
import Data.Array as Array
import Data.Debug (class Debug, genericDebug)
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.List.Lazy as LazyList
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple)
import Lunarflow.ErrorStack (EXCEPT_STACKED, EitherStacked, throw)
import Lunarflow.ErrorStack as Stacked
import Run (Run, expand, extract)
import Run.Except (runExcept)
import Run.Reader (READER, ask, runReader)
import Run.State (STATE, get, put, runState)

---------- Type
-- | Stuff literally everything in a layout carries around.
type Line p r
  = { position :: p, color :: String | r }

-- | Data about a binder in scope. 
type Binder p
  = Line p ()

-- | Ids used to connect lambdas together in visual chunks.
data ScopeId
  = ScopeId Int
  | Root

-- | The index at which the position we are looking for is stored in the indexMap. 
data Position
  = Position Int ScopeId

-- | Mapping from positions to vertical indices.
type IndexMap
  = Map.Map ScopeId (List.List Int)

-- | Read only context we can access at any time while generating layouts.
type LayoutContext
  = { protected :: Set.Set Position
    , binders :: List.List (Binder Position)
    , currentScope :: ScopeId
    , near :: Int
    }

-- | State we manipulate while generating layouts.
type LayoutState
  = { indexMap :: IndexMap
    , colors :: LazyList.List String
    , lastScope :: Int
    }

-- TODO: support free variables 
data LayoutError
  = BinderNotInScope Int
  | MissingIndexMap ScopeId
  | MissingPosition (List.List Int) ScopeId Int
  | ColorDrought

-- | The monad we generate layouts in.
type LayoutM
  = Run
      ( reader :: READER LayoutContext
      , state :: STATE LayoutState
      , except :: EXCEPT_STACKED String LayoutError
      )

---------- Helpers
-- I don't want to export all the constructors for LayoutError.
missingPosition :: List.List Int -> ScopeId -> Int -> LayoutError
missingPosition = MissingPosition

-- | Generate a new color.
freshColor :: LayoutM String
freshColor = do
  state <- get
  case LazyList.uncons state.colors of
    Just { head, tail } -> do
      put state { colors = tail }
      pure head
    Nothing -> throw ColorDrought

-- | Try getting the index map at a certain location. Throw an error otherwise.
getIndexMap :: ScopeId -> LayoutM (List.List Int)
getIndexMap scope = do
  state <- get
  case Map.lookup scope state.indexMap of
    Just list -> pure list
    Nothing -> throw $ MissingIndexMap scope

-- TODO: better error messages.
-- | Get the position a var has in the current scope.
getBinder :: Int -> LayoutM (Binder Position)
getBinder index = do
  binders <- ask <#> _.binders
  case binders `List.index` index of
    Just binder -> pure binder
    Nothing -> throw $ BinderNotInScope index

-- | Run the computations in the Layout monad.
runLayoutM :: forall a. LayoutM a -> EitherStacked String LayoutError (Tuple LayoutState a)
runLayoutM =
  runLayoutMWithState
    { indexMap: Map.singleton Root $ List.singleton 0
    , lastScope: -1
    , colors: LazyList.fromFoldable [ "white", "black" ]
    }

-- | Run the computations in the Layout monad.
runLayoutMWithState :: forall a. LayoutState -> LayoutM a -> EitherStacked String LayoutError (Tuple LayoutState a)
runLayoutMWithState state = runReader ctx >>> runState state >>> runExcept >>> extract
  where
  ctx :: LayoutContext
  ctx =
    { protected: Set.empty
    , binders: List.Nil
    , currentScope: Root
    , near: 0
    }

-- | Get the scope we are currently in.
currentScope :: LayoutM ScopeId
currentScope = ask <#> _.currentScope

-- | Get the absolute position described by a relative one.
unscopePosition :: Position -> LayoutM Int
unscopePosition (Position index scope) =
  while "unscoping a position" do
    state <- get
    list <- getIndexMap scope
    case List.index list index of
      Just position -> pure position
      Nothing -> throw $ MissingPosition list scope index

-- | Add more context data to errors.
while :: String -> LayoutM ~> LayoutM
while place = expand >>> Stacked.while place

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

instance debugPosition :: Debug Position where
  debug = genericDebug

instance debugLayoutError :: Debug LayoutError where
  debug = genericDebug

instance showLayoutError :: Show LayoutError where
  show = case _ of
    BinderNotInScope index -> "Variable " <> show index <> " is not in scope."
    MissingIndexMap scope -> "Cannot find index list for scope " <> show scope <> "."
    MissingPosition list scope index ->
      "Cannot find position "
        <> show index
        <> " in scope "
        <> show scope
        <> " while looking inside "
        <> show (Array.fromFoldable list)
        <> "."
    ColorDrought -> "Run out of colors while generating the layout"
