module Lunarflow.Layout
  ( ScopedLayoutF
  , ScopedLayout
  , LayoutF
  , Layout
  , LayoutLikeF
  , LayoutLike
  , addIndices
  , unscopeLayout
  , markRoot
  , shiftLines
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Function (on)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Set as Set
import Data.Traversable (for, minimumBy)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Lunarflow.Ast (AstF(..), call, lambda, var)
import Lunarflow.Ast.Grouped (GroupedExpression, references)
import Lunarflow.ErrorStack (ErrorStack(..))
import Lunarflow.Function ((|>))
import Lunarflow.LayoutM (LayoutM, Line, Position(..), ScopeId(..), while, currentScope, freshColor, getBinder, getIndexMap, missingPosition, unscopePosition)
import Lunarflow.List (indexed)
import Lunarflow.Mu (Mu)
import Matryoshka (cata, para, project)
import Run.Except (note)
import Run.Reader (ask, local)
import Run.State (get, modify, put)

---------- Type
-- | The base functor for all layouts
type LayoutLikeF p v c a l
  = AstF (Line p ( index :: Int | v )) (Line p c)
      ( Line
          p
          ( args :: List.List (Line p a)
          , isRoot :: Boolean
          | l
          )
      )

-- | General type for all layouts
type LayoutLike p v c a l
  = Mu (LayoutLikeF p v c a l)

-- | The base functor for scoped layouts.
type ScopedLayoutF
  = LayoutLikeF Position () () ()
      ( scope :: ScopeId
      )

-- | The base functor for layouts.
type LayoutF
  = LayoutLikeF Int () () () ( scope :: ScopeId )

-- | Layouts are expressions which have vertical indices for every line.
type Layout
  = Mu LayoutF

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
  = At Int
  | NextTo Int

---------- Layout generation
-- | Generate a layout from an ast.
addIndices :: GroupedExpression -> LayoutM ScopedLayout
addIndices =
  para case _ of
    Var { index } ->
      while "adding indices to a variable" ado
        { position, color } <- getBinder index
        in var { index, position, color }
    Call _ (Tuple _ layoutFunction) (Tuple groupedArgument layoutArgument) ->
      while "adding indices to a call" do
        let
          -- This type annotation is here so the compiler
          -- knows which Unfoldable instance to use.
          vars :: Array _
          vars = Set.toUnfoldable $ references groupedArgument
        -- This holds all the variables referenced inside the argument of the call
        inArgument <- Set.fromFoldable <$> for vars (getBinder >>> map _.position)
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
        pure $ call { position, color: getColor function } function argument
    Lambda { args: vars } (Tuple groupedBody layoutBody) ->
      while "adding indices to a lambda" do
        scope <- newScope
        args <-
          forWithIndex vars \index name -> ado
            color <- freshColor
            in { position: Position (varCount - index - 1) scope, color }
        initialState <- get
        put initialState { indexMap = Map.insert scope newList initialState.indexMap }
        body <-
          flip local layoutBody \ctx ->
            ctx
              { binders = args <> ctx.binders
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
        inBody <- Set.fromFoldable <$> for referenced (getBinder >>> map _.position)
        position <- ask <#> _.near >>= placeExpression inBody Everywhere
        pure $ lambda { position, args, scope, isRoot: false, color: getColor body } body
      where
      newList = List.range 0 (varCount - 1)

      varCount = List.length vars

-- | Search for the optimal location to put something at.
findLocation ::
  { constraint :: LocationConstraint
  , positionIndex :: Int
  , positions :: List.List Int
  , except :: Set.Set Int
  , scope :: ScopeId
  } ->
  LayoutM LocationSearchResult
findLocation { constraint, positionIndex, positions, except, scope } = ado
  position <-
    while "finding a location"
      $ note (Errored $ missingPosition positions scope positionIndex)
      $ List.index positions positionIndex
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
  pure $ Position (List.length list - 1) scope

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
attractTo :: Int -> LayoutM ~> LayoutM
attractTo near = local (_ { near = near })

-- | Get the position an expression already has.
getPosition :: ScopedLayout -> Position
getPosition layout = case project layout of
  Var { position } -> position
  Call { position } _ _ -> position
  Lambda { position } _ -> position

-- | Get the color of any scoped layout.
getColor :: ScopedLayout -> String
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
