-- | Stuff related to the layout monad.
-- | Why a different module? So errors can only be thrown 
-- | from the helpers I want to allow to error out.
module Lunarflow.LayoutM
  ( Line
  , LineRep
  , Binder
  , ScopeId(..)
  , Position(..)
  , PositionPointer(..)
  , AbsolutePosition(..)
  , LayoutContext
  , LayoutState
  , LayoutError
  , IndexMap
  , LayoutM
  , missingPosition
  , freshColor
  , getIndexMap
  , lookupBinders
  , runLayoutM
  , runLayoutMWithState
  , currentScope
  , unscopePosition
  , while
  , getVarPosition
  , insideScope
  , protect
  , attractTo
  , attractToIndex
  ) where

import Lunarlude
import Data.Array as Array
import Data.HashMap as HashMap
import Data.Hashable (class Hashable, hash)
import Data.List as List
import Data.List.Lazy as LazyList
import Data.Map as Map
import Data.Set as Set
import Lunarflow.Ast (Name(..))
import Lunarflow.ErrorStack (EXCEPT_STACKED, EitherStacked, throw)
import Lunarflow.ErrorStack as Stacked
import Record as Record
import Run (Run, expand, extract)
import Run.Except (runExcept)
import Run.Reader (READER, ask, local, runReader)
import Run.State (STATE, get, put, runState)

---------- Type
-- | Stuff literally everything in a layout carries around.
type LineRep p r
  = ( position :: p, color :: String | r )

-- | Same as LineRep but with kind Type instead of Row Type
type Line p r
  = Record (LineRep p r)

-- | Data about a binder in scope. 
type Binder p
  = Line p ()

-- | Ids used to connect lambdas together in visual chunks.
data ScopeId
  = ScopeId Int
  | Root

-- These 2 types are absolutely necessary to prevent having
-- a bazillition different confusing arguments with type Int
newtype PositionPointer
  = PositionPointer Int

type AbsolutePosition
  = Int

-- The index of an absolute position stored in the layout state
-- | The index at which the position we are looking for is stored in the indexMap. 
data Position
  = Position PositionPointer ScopeId

-- | Mapping from positions to vertical indices.
type IndexMap
  = Map.Map ScopeId (List.List AbsolutePosition)

-- | Read only context we can access at any time while generating layouts.
type LayoutContext
  = { protected :: Set.Set Position
    , binders :: List.List (Binder Position)
    , free :: HashMap.HashMap String (Binder Position)
    , currentScope :: ScopeId
    , near :: PositionPointer
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
  | MissingPosition (List.List AbsolutePosition) Position
  | ColorDrought
  | FreeNotAnnotated String

-- | The monad we generate layouts in.
type LayoutM
  = Run
      ( reader :: READER LayoutContext
      , state :: STATE LayoutState
      , except :: EXCEPT_STACKED String LayoutError
      )

---------- Helpers
-- I don't want to export all the constructors for LayoutError.
missingPosition :: List.List AbsolutePosition -> Position -> LayoutError
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
getIndexMap :: ScopeId -> LayoutM (List.List AbsolutePosition)
getIndexMap scope = do
  state <- get
  case Map.lookup scope state.indexMap of
    Just list -> pure list
    Nothing -> throw $ MissingIndexMap scope

-- | Get the position and color a var has in the current scope.
lookupBinders :: Name -> LayoutM (Binder Position)
lookupBinders = case _ of
  Bound index -> do
    binders <- ask <#> _.binders
    case binders `List.index` index of
      Just binder -> pure binder
      Nothing -> throw $ BinderNotInScope index
  Free name -> do
    free <- ask <#> _.free
    case name `HashMap.lookup` free of
      Just binder -> pure binder
      Nothing -> throw $ FreeNotAnnotated name

-- | Get the position a var has in the current scope
getVarPosition :: Name -> LayoutM Position
getVarPosition = lookupBinders >>> map _.position

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
    , free: HashMap.empty
    , currentScope: Root
    , near: PositionPointer 0
    }

-- | Get the scope we are currently in.
currentScope :: LayoutM ScopeId
currentScope = ask <#> _.currentScope

-- | Get the absolute position described by a relative one.
unscopePosition :: Position -> LayoutM AbsolutePosition
unscopePosition position@(Position (PositionPointer index) scope) =
  while "unscoping a position" do
    state <- get
    list <- getIndexMap scope
    case List.index list index of
      Just absolute -> pure absolute
      Nothing -> throw $ MissingPosition list position

-- | Run a computation within a given scope
insideScope :: ScopeId -> LayoutM ~> LayoutM
insideScope scope = local $ Record.set _currentScope scope

-- | Run a computation in a context where an arbitrary set of positions are protected.
protect :: Set.Set Position -> LayoutM ~> LayoutM
protect inputs = local (\a -> a { protected = a.protected <> inputs })

-- | Add more context data to errors.
while :: String -> LayoutM ~> LayoutM
while place = expand >>> Stacked.while place

-- | Run a computation in a context "attracted" to an arbitrary position pointer.
attractToIndex :: PositionPointer -> LayoutM ~> LayoutM
attractToIndex near = local (_ { near = near })

-- | Run a computation in a context "attracted" to an arbitrary position.
attractTo :: Position -> LayoutM ~> LayoutM
attractTo (Position near scope) m = do
  theCurrentScope <- currentScope
  let
    update
      | scope == theCurrentScope = const near
      | otherwise = identity
  flip local m $ over (prop _near) update

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

instance hashableScopeId :: Hashable ScopeId where
  hash scope =
    hash case scope of
      Root -> Nothing
      ScopeId id -> Just id

derive instance eqPositionPointer :: Eq PositionPointer

derive instance ordPositionPointer :: Ord PositionPointer

derive newtype instance showPositionPointer :: Show PositionPointer

derive instance newtypePositionPointer :: Newtype PositionPointer _

derive instance genericPositionPointer :: Generic PositionPointer _

instance debugPositionPointer :: Debug PositionPointer where
  debug = genericDebug

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
    FreeNotAnnotated name -> "Free variable " <> name <> " did not appear in any free-references annotation"
    BinderNotInScope index -> "Variable " <> show index <> " is not in scope."
    MissingIndexMap scope -> "Cannot find index list for scope " <> show scope <> "."
    MissingPosition list (Position index scope) ->
      "Cannot find position "
        <> show index
        <> " in scope "
        <> show scope
        <> " while looking inside "
        <> show (Array.fromFoldable list)
        <> "."
    ColorDrought -> "Run out of colors while generating the layout"

---------- SProxies
_currentScope :: SProxy "currentScope"
_currentScope = SProxy

_near :: SProxy "near"
_near = SProxy
