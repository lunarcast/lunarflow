module Lunarflow.Layout
  ( LayoutLikeF
  , LayoutNodeData
  , LayoutLike
  , FreeRootsRep
  , HasFreeTerms
  , generateLayout
  , unscopeLayout
  , markRoot
  , markFreeRoots
  , shiftLines
  , layoutingPipeline
  ) where

import Lunarlude
import Data.Array as Array
import Data.HashMap as HashMap
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Lunarflow.Ast (AstF(..), Name(..), call, lambda, var)
import Lunarflow.Ast.Grouped (GroupedLike, GroupedLikeF, references)
import Lunarflow.ErrorStack (ErrorStack(..))
import Lunarflow.LayoutM (AbsolutePosition, Binder, LayoutM, Line, LineRep, Position(..), PositionPointer(..), ScopeId(..), attractTo, currentScope, freshColor, getIndexMap, getVarPosition, insideScope, lookupBinders, missingPosition, protect, unscopePosition, while)
import Lunarflow.List (indexed)
import Prim.Row as Row
import Record as Record
import Run.Except (note)
import Run.Reader (ask, local)
import Run.State (get, modify, put)

---------- Types
-- | Different ways positions can be placed in
data VarPosition p
  = ScopeShift { from :: ScopeId, to :: p }

-- | Row containing data about the free terms needed to render the current node
type FreeRootsRep r
  = ( freeTerms :: Set.Set String | r )

-- | Layout with annotations on all nodes with the free variales requied to render it.
type WithFreeRoots v c a l
  = GroupedLike (FreeRootsRep v) (FreeRootsRep c) a l

type HasFreeTerms p a
  = ( freeTerms :: HashMap.HashMap String (Line p ()) | a )

-- | Data all nodes in a layout have
type LayoutNodeData p a
  = LineRep p ( inScope :: ScopeId | a )

-- We define this type to clean up the definition of generateLayout
-- | The base functor for all layouts
type LayoutLikeF p v c a l
  = GroupedLikeF (LayoutNodeData p + HasFreeTerms p v)
      (LayoutNodeData p + HasFreeTerms p c)
      (LineRep p a)
      (LayoutNodeData p + ( scope :: ScopeId | l ))

-- | General type for all layouts
type LayoutLike p v c a l
  = Mu (LayoutLikeF p v c a l)

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
-- | The whole layouting pipeline combined
layoutingPipeline ::
  forall v c a l.
  Row.Lacks "inScope" v =>
  Row.Lacks "inScope" c =>
  Row.Lacks "inScope" l =>
  Row.Lacks "scope" l =>
  Row.Lacks "isRoot" l =>
  Row.Lacks "args" l =>
  Row.Lacks "freeTerms" v =>
  Row.Lacks "freeTerms" c =>
  GroupedLike v c a l -> LayoutM (LayoutLike Int v c a ( isRoot :: Boolean | l ))
layoutingPipeline = markFreeRoots >>> introduceScopes >=> generateLayout >>> map markRoot >=> unscopeLayout

-- | Annotate all the lambdas in an ast with unique scope ids
introduceScopes ::
  forall v c a l.
  Row.Lacks "inScope" v =>
  Row.Lacks "inScope" c =>
  Row.Lacks "inScope" l =>
  Row.Lacks "scope" l =>
  GroupedLike v c a l ->
  LayoutM (GroupedLike ( inScope :: ScopeId | v ) ( inScope :: ScopeId | c ) a ( scope :: ScopeId, inScope :: ScopeId | l ))
introduceScopes =
  cata case _ of
    Var data' -> currentScope <#> \scope -> var $ setScope scope data'
    Call data' function argument -> currentScope >>= \scope -> call (setScope scope data') <$> function <*> argument
    Lambda data' body -> do
      inScope <- currentScope
      scope <- newScope
      lambda (Record.union { scope, inScope } data') <$> insideScope scope body
  where
  setScope :: forall r. Row.Lacks "inScope" r => ScopeId -> Record r -> { inScope :: ScopeId | r }
  setScope = Record.insert _inScope

-- | Add annotations to all nodes with all the free variables needed for rendering the current expression.
markFreeRoots ::
  forall v c a l.
  Row.Lacks "freeTerms" v =>
  Row.Lacks "freeTerms" c =>
  GroupedLike v c a l -> WithFreeRoots v c a l
markFreeRoots = cata algebra >>> uncurry (|>)
  where
  algebra :: Algebra (GroupedLikeF v c a l) (Tuple (Set.Set String) (Set.Set String -> WithFreeRoots v c a l))
  algebra = case _ of
    Var varData@{ name: Free name } -> Tuple (Set.singleton name) \freeTerms -> var $ addRoots freeTerms varData
    Var varData -> Tuple mempty \_ -> var $ addNoRoots varData
    Call callData (Tuple functionReferences makeFunction) (Tuple argumentReferences makeArgument) ->
      Tuple (functionReferences <> argumentReferences) \references ->
        let
          common = references `Set.intersection` functionReferences `Set.intersection` argumentReferences

          function = makeFunction $ references `Set.difference` argumentReferences

          argument = makeArgument $ references `Set.difference` functionReferences
        in
          call (addRoots common callData) function argument
    Lambda lambdaData (Tuple bodyReferences makeBody) -> Tuple bodyReferences \references -> lambda lambdaData $ makeBody references

  addRoots :: forall r. Row.Lacks "freeTerms" r => Set.Set String -> Record r -> Record (FreeRootsRep r)
  addRoots = Record.insert _freeTerms

  addNoRoots :: forall r. Row.Lacks "freeTerms" r => Record r -> Record (FreeRootsRep r)
  addNoRoots = addRoots Set.empty

-- | Generate a layout from an ast.
generateLayout ::
  forall v c a l.
  Row.Lacks "args" l =>
  Row.Lacks "freeTerms" v =>
  Row.Lacks "freeTerms" c =>
  WithFreeRoots ( inScope :: ScopeId | v ) ( inScope :: ScopeId | c ) a ( inScope :: ScopeId, scope :: ScopeId | l ) ->
  LayoutM (LayoutLike Position v c a l)
generateLayout =
  para case _ of
    Var varData ->
      while "adding indices to a variable"
        $ withFreeAnnotations varData \freeTerms -> ado
            { position, color } <- lookupBinders varData.name
            in var $ Record.union { position, color, freeTerms } $ Record.delete _freeTerms varData
    Call data' tupleFunction@(Tuple _ layoutFunction) tupleArgument@(Tuple groupedArgument layoutArgument) ->
      while "adding indices to a call"
        $ withFreeAnnotations data' \freeTerms -> do
            Tuple function argument <- placeExpressions tupleFunction tupleArgument
            let
              argumentPosition = getPosition argument

              functionPosition = getPosition function

              except = Set.fromFoldable [ functionPosition, argumentPosition ]
            constraint <- constrain functionPosition argumentPosition
            position <- attractTo functionPosition $ ask <#> _.near >>= placeExpression { except, constraint }
            let
              data'' = Record.union { position, color: getColor function, freeTerms } $ Record.delete _freeTerms data'
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
        -- TODO: create helper for 'flip local'?
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
  withFreeAnnotations :: forall r b. Record (FreeRootsRep r) -> (HashMap.HashMap String (Binder Position) -> LayoutM b) -> LayoutM b
  withFreeAnnotations vars m = do
    varPositionsAndColors <-
      foldl go (pure HashMap.empty) (Array.fromFoldable vars.freeTerms)
    flip local (m varPositionsAndColors) $ over (prop _free) $ HashMap.union varPositionsAndColors
    where
    go m' name = do
      color <- freshColor
      ctx <- ask
      position <- ask <#> _.near >>= placeExpression { except: Set.empty, constraint: Everywhere }
      varMap <- protect (Set.singleton position) m'
      pure $ HashMap.insert name { color, position } varMap

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
    allowed x = not (Set.member x except) && x /= positionIndex

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
placeExpression { except, constraint } positionIndex =
  while "placing an expression" do
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
              |> Set.mapMaybe \(Position index scope) -> ado
                  guard (scope == ctx.currentScope)
                  in index
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
      Var varData -> var <$> unscopeLayoutNodeData varData
      Call callData function argument -> call <$> unscopeLayoutNodeData callData <*> function <*> argument
      Lambda data' body -> ado
        body' <- body
        args <-
          for data'.args \arg -> ado
            position <- unscopePosition arg.position
            in arg { position = position }
        position <- unscopePosition data'.position
        in flip lambda body' $ Record.set _position position $ Record.set _args args data'

  unscopeLayoutNodeData ::
    forall r.
    Record (LineRep Position + HasFreeTerms Position r) ->
    LayoutM (Record (LineRep Int + HasFreeTerms Int r))
  unscopeLayoutNodeData node = ado
    freeTerms <-
      for node.freeTerms \var -> ado
        position <- unscopePosition var.position
        in var { position = position }
    position <- unscopePosition node.position
    in node { position = position, freeTerms = freeTerms }

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
markRoot ::
  forall v c a l.
  Row.Lacks "isRoot" l =>
  GroupedLike v c a l -> GroupedLike v c a ( isRoot :: Boolean | l )
markRoot =
  addRoots
    >>> \x -> case project x of
        Lambda data' body -> lambda (data' { isRoot = true }) body
        _ -> x
  where
  addRoots =
    cata case _ of
      Var varData -> var varData
      Call callData function argument -> call callData function argument
      Lambda lambdaData body -> flip lambda body $ Record.insert _isRoot false lambdaData

---------- Internal helpers
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

-- | Filter out all the free variables from an array.
onlyBound :: Array Name -> Array Name
onlyBound =
  Array.filter case _ of
    Bound i -> true
    _ -> false

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

-- | Check if an expression has a high attraction towards some attraction point.
isAttracted :: forall v c a l. GroupedLike v c a l -> Boolean
isAttracted =
  project
    >>> case _ of
        -- NOTE: this assumes the name is in scope
        Var { name: Bound _ } -> true
        Call _ function argument -> true
        _ -> false

-- | Place 2 expressions (usually an argument and the function being called) 
-- | such that we keep ourselves as attracted to the attraction point as possible
placeExpressions ::
  forall v c a l v' c' a' l'.
  Row.Lacks "args" l' =>
  Tuple (GroupedLike v' c' a' l') (LayoutM (LayoutLike Position v c a l)) ->
  Tuple (GroupedLike v' c' a' l') (LayoutM (LayoutLike Position v c a l)) ->
  LayoutM (Tuple (LayoutLike Position v c a l) (LayoutLike Position v c a l))
placeExpressions a@(Tuple groupedFirst layoutFirst) b@(Tuple groupedSecond layoutSecond)
  | not (isAttracted groupedFirst) && isAttracted groupedSecond = swap <$> placeExpressions b a
  | otherwise = do
    { free } <- ask
    let
      vars =
        references groupedSecond
          |> Set.filter filterVarSet
          |> Array.fromFoldable
        where
        filterVarSet = case _ of
          Bound i -> true
          Free name -> HashMap.member name free
    -- This holds all the variables referenced inside the second expression 
    -- (usully the argument of a call)
    inSecond <- Set.fromFoldable <$> for vars getVarPosition
    -- When we draw a call, we draw the function first and then the argument.
    -- We don't want the space taken by the variables in the argument to be occupied
    -- So we basically inform the call not to touch those spaces.
    -- ( this is of course generalized for any 2 expressions )
    first <- protect inSecond layoutFirst
    scope <- currentScope
    let
      firstPosition = getPosition first
    -- place down the second expression 
    -- ( usually the argument in a call )
    second <- protect (Set.insert firstPosition $ takenPositions first) $ attractTo firstPosition layoutSecond
    pure $ Tuple first second

-- | Collects all the positions occupied by free variables
takenPositions :: forall v c a l. LayoutLike Position v c a l -> Set.Set Position
takenPositions =
  cata case _ of
    Lambda _ body -> body
    Var { freeTerms } -> freePositions freeTerms
    Call { freeTerms } function argument -> function <> argument <> (freePositions freeTerms)
  where
  freePositions = map _.position >>> Set.fromFoldable

---------- Typeclass instances
derive instance genericLocationConstraint :: Generic LocationConstraint _

instance debugLocationConstraint :: Debug LocationConstraint where
  debug = genericDebug

---------- SProxies
_indexMap :: SProxy "indexMap"
_indexMap = SProxy

_scope :: SProxy "scope"
_scope = SProxy

_args :: SProxy "args"
_args = SProxy

_isRoot :: SProxy "isRoot"
_isRoot = SProxy

_position :: SProxy "position"
_position = SProxy

_freeTerms :: SProxy "freeTerms"
_freeTerms = SProxy

_free :: SProxy "free"
_free = SProxy

_inScope :: SProxy "inScope"
_inScope = SProxy
