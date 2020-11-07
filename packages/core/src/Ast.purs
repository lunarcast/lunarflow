module Lunarflow.Ast
  ( AstF(..)
  , Ast
  , RawExpression
  , Expression
  , WithIndex
  , DeBrujinLike
  , withDebrujinIndices
  , call
  , lambda
  , var
  , printDeBrujin
  , isVar
  ) where

import Prelude
import Control.Monad.Reader (Reader, asks, local, runReader)
import Data.Debug (class Debug, genericDebug)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
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

derive instance genericAst :: Generic (AstF v c l a) _

derive instance functorAst :: Functor (AstF v c l)

instance showAst :: (Show v, Show c, Show l, Show f) => Show (AstF v c l f) where
  show = genericShow

instance debugAst :: (Debug v, Debug c, Debug l) => Debug (AstF v c l TacitRepr) where
  debug = genericDebug

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

-- | Expressions using de brujin indidces
type DeBrujinLike v c l
  = Ast { index :: Int | v } c l

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
        Just index -> var { index }
        Nothing -> var { index: (-1) }

--  Pretty printing stuff:
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
    Var { index } -> show index
    Lambda _ (Tuple _ body) -> lambdaChar <> body
    Call _ (Tuple funcAst func) (Tuple argAst arg) ->
      parenthesiseWhen (needsParenthesis true $ project funcAst) func
        <> parenthesiseWhen (needsParenthesis false $ project argAst) arg

-- | Check if an expression is a var
isVar :: forall v c l. Ast v c l -> Boolean
isVar =
  project
    >>> case _ of
        Var _ -> true
        _ -> false
