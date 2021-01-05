module Lunarflow.Ast
  ( AstF(..)
  , Ast
  , RawExpression
  , Expression
  , WithIndex
  , DeBrujinLikeF
  , DeBrujinLike
  , Name(..)
  , withDebrujinIndices
  , call
  , lambda
  , var
  , printDeBrujin
  , isVar
  , mapIndex
  , shiftIndex
  ) where

import Prelude
import Control.Monad.Reader (Reader, asks, local, runReader)
import Data.Debug (class Debug, genericDebug)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Traversable (class Foldable, class Traversable)
import Data.Tuple (Tuple(..))
import Lunarflow.Function (Endomorphism)
import Lunarflow.Mu (Mu, TacitRepr)
import Matryoshka (class Corecursive, Algebra, cata, embed, para, project)

-- | The meat and potatoes of representing an expression.
-- |
-- | - l represents the type lambdas carry around.
-- | - c represents the type calls carry around.
-- | - v represents the type variables carry around.
data AstF v c l r
  = Call c r r
  | Lambda l r
  | Var v

-- | The fixpoint of the ast functor.
type Ast v c l
  = Mu (AstF v c l)

call :: forall t v c l. Corecursive t (AstF v c l) => c -> t -> t -> t
call c f a = embed (Call c f a)

var :: forall t v c l. Corecursive t (AstF v c l) => v -> t
var = embed <<< Var

lambda :: forall t v c l. Corecursive t (AstF v c l) => l -> t -> t
lambda l t = embed (Lambda l t)

-- | Basic lambda calculus expressions
type RawExpression
  = Ast String Unit String

-- | Variable names for stuff using de brujin indices.
data Name
  = Bound Int
  | Free String

-- | Base functor for expressions using de bruhin indices
type DeBrujinLikeF v c l
  = AstF { name :: Name | v } c l

-- | Expressions using de brujin indidces
type DeBrujinLike v c l
  = Mu (DeBrujinLikeF v c l)

-- | Lambda calculus expression using de brujin indices.
type Expression
  = DeBrujinLike () Unit String

type WithIndex r
  = ( index :: Int | r )

-- | Add de brujin indices to a lambda calculus expression.
withDebrujinIndices :: RawExpression -> Expression
withDebrujinIndices expr = runReader (cata algebra expr) List.Nil
  where
  algebra :: Algebra (AstF String Unit String) (Reader (List.List String) Expression)
  algebra = case _ of
    Call _ func argument -> ado
      func' <- func
      argument' <- argument
      in call unit func' argument'
    Lambda name body -> ado
      body' <- local (List.Cons name) body
      in lambda name body'
    Var name -> ado
      maybeIndex <- asks $ List.findIndex (eq name)
      in case maybeIndex of
        Just index -> var { name: Bound index }
        Nothing -> var { name: Free name }

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

-- | Print an expression which uses de brujin indices.
printDeBrujin :: forall v c l. DeBrujinLike v c l -> String
printDeBrujin =
  para case _ of
    Var { name } -> show name
    Lambda _ (Tuple _ body) -> lambdaChar <> body
    Call _ (Tuple funcAst func) (Tuple argAst arg) ->
      parenthesiseWhen (needsParenthesis true $ project funcAst) func
        <> parenthesiseWhen (needsParenthesis false $ project argAst) arg

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

instance showAst :: (Show v, Show c, Show l, Show f) => Show (AstF v c l f) where
  show = genericShow

instance debugAst :: (Debug v, Debug c, Debug l) => Debug (AstF v c l TacitRepr) where
  debug = genericDebug

derive instance eqName :: Eq Name

derive instance ordName :: Ord Name

instance showName :: Show Name where
  show (Bound i) = "~" <> show i
  show (Free name) = name
