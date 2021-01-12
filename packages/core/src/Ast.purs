module Lunarflow.Ast
  ( AstF(..)
  , Ast
  , RawExpression
  , DeBrujinLikeF
  , DeBrujinLike
  , Name(..)
  , withDebrujinIndices
  , call
  , lambda
  , var
  , printDeBrujin
  , printRawExpression
  , printAst
  , isVar
  , mapIndex
  , shiftIndex
  ) where

import Lunarlude
import Control.Monad.Reader (Reader, asks, local, runReader)
import Data.Hashable (class Hashable, hash)
import Data.List as List
import Record as Record

-- | The meat and potatoes of representing an expression.
-- |
-- | - l represents the type lambdas carry around.
-- | - c represents the type calls carry around.
-- | - v represents the type variables carry around.
data AstF v c l r
  = Call (Record c) r r
  | Lambda (Record l) r
  | Var (Record v)

-- | The fixpoint of the ast functor.
type Ast v c l
  = Mu (AstF v c l)

---------- Constructors
call :: forall t v c l. Corecursive t (AstF v c l) => Record c -> t -> t -> t
call c f a = embed (Call c f a)

var :: forall t v c l. Corecursive t (AstF v c l) => Record v -> t
var = embed <<< Var

lambda :: forall t v c l. Corecursive t (AstF v c l) => Record l -> t -> t
lambda l t = embed (Lambda l t)

--------- De bruhin construction
-- | Basic lambda calculus expressions
type RawExpression
  = Ast ( name :: String ) () ( argument :: String )

-- | Variable names for stuff using de brujin indices.
data Name
  = Bound Int
  | Free String

-- | Base functor for expressions using de bruhin indices
type DeBrujinLikeF v c l
  = AstF ( name :: Name | v ) c l

-- | Expressions using de brujin indidces
type DeBrujinLike v c l
  = Mu (DeBrujinLikeF v c l)

-- | Add de brujin indices to a lambda calculus expression.
withDebrujinIndices :: forall v c l. Ast ( name :: String | v ) c ( argument :: String | l ) -> DeBrujinLike v c ( argument :: String | l )
withDebrujinIndices expr = runReader (cata algebra expr) List.Nil
  where
  algebra :: Algebra _ (Reader (List.List String) _)
  algebra = case _ of
    Call callData function argument -> call callData <$> function <*> argument
    Lambda lambdaData body -> ado
      body' <- local (List.Cons lambdaData.argument) body
      in lambda lambdaData body'
    Var varData -> ado
      maybeIndex <- asks $ List.findIndex (eq varData.name)
      in var
        $ Record.set
            _name
            ( case maybeIndex of
                Just index -> Bound index
                Nothing -> Free varData.name
            )
            varData

----------  Pretty printing stuff:
-- | Check if an ast chunk needs to be wrapped in parenthesis for printing
needsParenthesis :: forall v c l r. Boolean -> AstF v c l r -> Boolean
needsParenthesis left = case _ of
  (Lambda _ _) -> left
  (Call _ _ _) -> not left
  _ -> false

-- | Add parenthesis around a string
withParenthesis :: String -> String
withParenthesis a = "(" <> a <> ")"

-- | Add parenthesis around a string when a condition passes
parenthesiseWhen :: Boolean -> String -> String
parenthesiseWhen true = withParenthesis

parenthesiseWhen false = identity

-- | I don't have this on my keyboard so I just made a constant for it.
lambdaChar :: String
lambdaChar = "λ"

-- | Print an expression which doesn't use de brujin indices.
printRawExpression :: RawExpression -> String
printRawExpression = printAst _.name printCall printLambda
  where
  printCall _ f a = f <> " " <> a

  printLambda { argument } body = lambdaChar <> argument <> ". " <> body

-- | Print an expression which uses de brujin indices.
printDeBrujin :: forall v c l. DeBrujinLike v c l -> String
printDeBrujin = printAst (_.name >>> show) printCall (const (lambdaChar <> _))
  where
  printCall _ f a = f <> " " <> a

-- | Generalised ast printing
printAst ::
  forall v c l.
  (Record v -> String) ->
  (Record c -> String -> String -> String) ->
  (Record l -> String -> String) ->
  Ast v c l -> String
printAst printVar printCall printLambda =
  para case _ of
    Var varData -> printVar varData
    Call callData (Tuple functionAst function) (Tuple argumentAst argument) -> printCall callData functionWithParenthesis argumentWithParenthesis
      where
      functionWithParenthesis =
        parenthesiseWhen
          ( needsParenthesis true
              $ project functionAst
          )
          function

      argumentWithParenthesis =
        parenthesiseWhen
          ( needsParenthesis false
              $ project argumentAst
          )
          argument
    Lambda lambdaData (Tuple _ body) -> printLambda lambdaData body

---------- Helpers
-- | Apply a function over the index inside a name
mapIndex :: Endomorphism Int -> Endomorphism Name
mapIndex f = case _ of
  Bound i -> Bound $ f i
  expression -> expression

-- | Shift the index inside a name
shiftIndex :: Int -> Endomorphism Name
shiftIndex = (+) >>> mapIndex

-- | Check if an expression is a var
isVar :: forall v c l. Ast v c l -> Boolean
isVar =
  project
    >>> case _ of
        Var _ -> true
        _ -> false

---------- Typeclass instances
derive instance genericAst :: Generic (AstF v c l a) _

derive instance functorAst :: Functor (AstF v c l)

instance foldableAst :: Foldable (AstF v c l) where
  foldMap f = case _ of
    Var data' -> mempty
    Call data' function argument -> f function <> f argument
    Lambda data' body -> f body
  foldr f default = case _ of
    Var data' -> default
    Call data' function argument -> f function $ f argument default
    Lambda data' body -> f body default
  foldl f default = case _ of
    Var data' -> default
    Call data' function argument -> f (f default function) argument
    Lambda data' body -> f default body

instance traversavleAst :: Traversable (AstF v c l) where
  sequence = case _ of
    Var data' -> pure $ Var data'
    Call data' function argument -> Call data' <$> function <*> argument
    Lambda data' body -> Lambda data' <$> body
  traverse f = case _ of
    Var data' -> pure $ Var data'
    Call data' function argument -> Call data' <$> f function <*> f argument
    Lambda data' body -> Lambda data' <$> f body

instance showAst :: (Show (Record v), Show (Record c), Show (Record l), Show f) => Show (AstF v c l f) where
  show = genericShow

instance debugAst :: (Debug (Record v), Debug (Record c), Debug (Record l)) => Debug (AstF v c l TacitRepr) where
  debug = genericDebug

derive instance eqName :: Eq Name

derive instance ordName :: Ord Name

instance showName :: Show Name where
  show (Bound i) = show i
  show (Free name) = name

derive instance genericName :: Generic Name _

instance debugName :: Debug Name where
  debug = genericDebug

instance hashableAst :: Hashable Name where
  hash name =
    hash case name of
      Bound index -> Right index
      Free free -> Left free

---------- SProxies
_name :: SProxy "name"
_name = SProxy
