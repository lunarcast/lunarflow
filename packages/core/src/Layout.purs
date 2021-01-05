module Lunarflow.Layout
  ( ScopedLayoutF
  , ScopedLayout
  , LayoutF
  , Layout
  , LayoutLikeF
  , LayoutLike
  , generateLayout
  , unscopeLayout
  , markRoot
  , shiftLines
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Function (on)
import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Ord (abs)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Traversable (for, minimumBy)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Undefined (undefined)
import Lunarflow.Ast (AstF(..), Name(..), call, lambda, var)
import Lunarflow.Ast.Grouped (GroupedLike, GroupedLikeF, references)
import Lunarflow.ErrorStack (ErrorStack(..))
import Lunarflow.Function (Endomorphism, (|>))
import Lunarflow.LayoutM (AbsolutePosition, LayoutM, Line, LineRep, Position(..), PositionPointer(..), ScopeId(..), currentScope, freshColor, getBinder, getIndexMap, getVarPosition, missingPosition, unscopePosition, while)
import Lunarflow.List (indexed)
import Lunarflow.Mu (Mu)
import Matryoshka (cata, cataM, embed, para, project)
import Prim.Row as Row
import Record as Record
import Run.Except (note)
import Run.Reader (ask, local)
import Run.State (get, modify, put)

---------- Types
-- | Different ways positions can be placed in
data VarPosition p
  = ScopeShift { from :: ScopeId, to :: p }

-- We define this type to clean up the definition of generateLayout
-- | The base functor for all layouts
type LayoutLikeF p v c a l
  = GroupedLikeF (LineRep p v) (Line p c) (Line p a) (LineRep p l)

-- | General type for all layouts
type LayoutLike p v c a l
  = Mu (LayoutLikeF p v c a l)

-- | The base functor for layouts.
type LayoutF
  = LayoutLikeF Int () () () ( scope :: ScopeId )

-- | Layouts are expressions which have vertical indices for every line.
type Layout
  = Mu LayoutF

-- | The base functor for scoped layouts.
type ScopedLayoutF
  = LayoutLikeF Position () () ()
      ( scope :: ScopeId
      , isRoot :: Boolean
      )

-- | Layouts are expressions which have vertical indices for every line.
-- | This is the scoped variant, which keeps track of what scope everything is from.
type ScopedLayout
  = Mu ScopedLayoutF

-- | Different ways we can constrain where expressions can be placed.
data LocationConstraint
  = Everywhere
  | Above
  | Below

-- | Actions we can take in order to place an expression.
data LocationSearchResult
  = At PositionPointer
  | NextTo Int

---------- Layout generation
introduceScopes ::
  forall v c a l.
  Row.Lacks "scope" l =>
  GroupedLike v c a l ->
  LayoutM (GroupedLike v c a ( scope :: ScopeId | l ))
introduceScopes =
  cataM case _ of
    Var data' -> pure $ var data'
    Call data' function argument -> pure $ call data' function argument
    Lambda data' body -> ado
      scope <- newScope
      in lambda (Record.insert _scope scope data') body

-- | Generate a layout from an ast.
generateLayout ::
  forall v c a l.
  Row.Lacks "args" l =>
  GroupedLike v (Record c) (Record a) ( scope :: ScopeId | l ) ->
  LayoutM (LayoutLike Position v c a ( scope :: ScopeId | l ))
generateLayout =
  para case _ of
    Var data'@{ name: Bound name } ->
      while "adding indices to a variable" ado
        { position, color } <- getBinder name
        in var $ Record.union { position, color } data'
    Var { name: Free index } -> while "adding indices to a free variable" undefined
    Call data' (Tuple _ layoutFunction) (Tuple groupedArgument layoutArgument) ->
      while "adding indices to a call" do
        let
          -- This type annotation is here so the compiler
          -- knows which Unfoldable instance to use.
          vars :: Array Name
          vars = Set.toUnfoldable $ references groupedArgument
        -- This holds all the variables referenced inside the argument of the call
        inArgument <- Set.fromFoldable <$> for (onlyBound vars) (getBinder >>> map _.position)
        -- When we draw a call, we draw the function first and then the argument.
        -- We don't want the space taken by the variables in the argument to be occupied
        -- So we basically inform the call not to touch those spaces.
        function <- protect inArgument layoutFunction
        scope <- currentScope
        let
          functionPosition@(Position functionIndex functionScope) = getPosition function

          -- PureScript doesn't genralize let bindings 
          -- so we have to write this type definition ourselves.
          nearFunction :: LayoutM ~> LayoutM
          nearFunction
            | functionScope == scope = attractTo functionIndex
            | otherwise = identity
        argument <- protect (Set.singleton functionPosition) $ nearFunction layoutArgument
        let
          argumentPosition = getPosition argument

          except = Set.fromFoldable [ functionPosition, argumentPosition ]
        constraint <- constrain functionPosition argumentPosition
        position <- nearFunction $ ask <#> _.near >>= placeExpression { except, constraint }
        let
          data'' = Record.union { position, color: getColor function } data'
        pure $ call data'' function argument
    expression@(Lambda data'@{ args: vars } (Tuple _ layoutBody)) ->
      while "adding indices to a lambda" do
        -- Each lambda has it's own scope so we generate a new one
        scope <- newScope
        -- Generate colors and positions for the arguments
        (args :: _ (Line Position a)) <-
          forWithIndex vars \index argument -> ado
            color <- freshColor
            in Record.union { position: Position (PositionPointer $ varCount - index - 1) scope, color } argument
        -- Add the new scope to the index map
        modify $ over (prop _indexMap) $ Map.insert scope positionMemory
        -- TODO: create helper for 'flip local'
        body <-
          flip local layoutBody \ctx ->
            ctx
              { binders = (args <#> \{ position, color } -> { position, color }) <> ctx.binders -- introduce the argumens into scope
              , near = PositionPointer $ (varCount - 1) / 2 -- Reset the attraction point to the middle
              , currentScope = scope
              }
        -- TODO: abstract this away.
        let
          referenced :: Array Name
          referenced = Array.fromFoldable $ references $ embed $ fst <$> expression
        -- This holds all the variables referenced inside the body of the lambda
        inBody <- Set.fromFoldable <$> for (onlyBound referenced) getVarPosition
        -- The position to place the lambda (in the outer scope)
        position <- ask <#> _.near >>= placeExpression { except: inBody, constraint: Everywhere }
        pure $ flip lambda body
          $ Record.union { position, color: getColor body, args }
          $ Record.delete _args data'
      where
      positionMemory = List.range 0 (varCount - 1)

      varCount = List.length vars
  where
  onlyBound :: Array Name -> Array Int
  onlyBound =
    Array.mapMaybe case _ of
      Bound index -> Just index
      Free _ -> Nothing

-- | Search for the optimal location to put something at.
findLocation ::
  { constraint :: LocationConstraint
  , positionIndex :: PositionPointer
  , positions :: List.List Int
  , except :: Set.Set PositionPointer
  , scope :: ScopeId
  } ->
  LayoutM LocationSearchResult
findLocation { constraint, positionIndex, positions, except, scope } = ado
  position <-
    while "finding a location"
      $ note (Errored $ missingPosition positions (Position positionIndex scope))
      $ List.index positions
      $ unwrap positionIndex
  -- 
  let
    allowed :: PositionPointer -> Boolean
    allowed x = not $ Set.member x except

    passesConstraint :: Int -> Boolean
    passesConstraint x = case constraint of
      Everywhere -> true
      Above -> x > position
      Below -> x < position

    -- | If an optimal existing spot exists, this finds it.
    unused :: Maybe PositionPointer
    unused =
      positions
        |> indexed
        |> map (lmap PositionPointer)
        |> List.filter (snd >>> passesConstraint)
        |> List.filter (fst >>> allowed)
        |> map (map distanceToPosition)
        |> minimumBy (on compare snd)
        |> map fst
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
placeExpression ::
  { except :: Set.Set Position
  , constraint :: LocationConstraint
  } ->
  PositionPointer ->
  LayoutM Position
placeExpression { except, constraint } positionIndex = do
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
    At index -> pure $ Position index ctx.currentScope
    NextTo y -> do
      shiftLines ctx.currentScope { past: y, amount: 1 }
      createPosition (y + 1) ctx.currentScope

-- | Create and push a new position into a map.
createPosition :: Int -> ScopeId -> LayoutM Position
createPosition position scope = do
  -- TODO: consider a helper for updating the indexMap
  modify \state ->
    state
      { indexMap =
        Map.update
          ( flip List.snoc position
              >>> Just
          )
          scope
          state.indexMap
      }
  list <- getIndexMap scope
  pure $ Position (PositionPointer $ List.length list - 1) scope

---------- Converstion functions
-- | Transform a scoped layout into an unscoped one.
unscopeLayout :: forall v c a l. LayoutLike Position v c a l -> LayoutM (LayoutLike Int v c a l)
unscopeLayout a = while "unscoping layout" $ m a
  where
  m :: LayoutLike Position v c a l -> LayoutM (LayoutLike Int v c a l)
  m =
    cata case _ of
      Var data' -> ado
        position <- unscopePosition data'.position
        in var data' { position = position }
      Call data' function argument -> ado
        function' <- function
        argument' <- argument
        position <- unscopePosition data'.position
        in call data' { position = position } function' argument'
      Lambda data' body -> ado
        body' <- body
        args <-
          for data'.args \arg -> ado
            position <- unscopePosition arg.position
            in arg { position = position }
        position <- unscopePosition data'.position
        in lambda data' { position = position, args = args } body'

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
  update :: Endomorphism AbsolutePosition
  update x
    | x > past = x + amount
    | otherwise = x

-- | If the expression starts off with a lambda, mark it up as the root lambda.
-- | We do this to avoid adding an unnecessary box around the root lambda later in the pipeline.
markRoot :: ScopedLayout -> ScopedLayout
markRoot x = case project x of
  Lambda data' body -> lambda (data' { isRoot = true }) body
  _ -> x

---------- Internal helpers
-- | Run a computation in a context where 
-- an arbitrary set of positions is protected.
protect :: Set.Set Position -> LayoutM ~> LayoutM
protect inputs = local (\a -> a { protected = a.protected <> inputs })

-- | Run a computation in a context "attracted" to an arbitrary position.
attractTo :: PositionPointer -> LayoutM ~> LayoutM
attractTo near = local (_ { near = near })

-- | Get the position an expression already has.
getPosition :: forall p v c a l. LayoutLike p v c a l -> p
getPosition layout = case project layout of
  Var { position } -> position
  Call { position } _ _ -> position
  Lambda { position } _ -> position

-- | Get the color of any scoped layout.
getColor :: forall p v c a l. LayoutLike p v c a l -> String
getColor layout = case project layout of
  Var { color } -> color
  Call { color } _ _ -> color
  Lambda { color } _ -> color

-- | Create a new scope with an unique id.
newScope :: LayoutM ScopeId
newScope = do
  state <- get
  let
    scope = 1 + state.lastScope
  put state { lastScope = scope }
  pure $ ScopeId scope

-- | Get the list of indices describing the scope we are in.
currentIndexList :: LayoutM (List.List Int)
currentIndexList = do
  state <- get
  ctx <- ask
  getIndexMap ctx.currentScope

-- | Find the correct way to constrain where the result of a call can be placed.
constrain :: Position -> Position -> LayoutM LocationConstraint
constrain functionPosition@(Position _ functionScope) argumentPosition@(Position _ argumentScope) =
  while "generating constrains" do
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

---------- SProxies
_indexMap :: SProxy "indexMap"
_indexMap = SProxy

_scope :: SProxy "scope"
_scope = SProxy

_args :: SProxy "args"
_args = SProxy
