module Lunarflow.Ast.Grouped where

import Prelude
import Data.List (foldr)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Lunarflow.Ast (AstF(..), DeBrujinLike, call, lambda, var)
import Lunarflow.Function (Endomorphism)
import Matryoshka (cata, project)
import Prim.Row as Row
import Record as Record

-- TODO: Make the argument list nonempty.
-- | A chunk of lambda calculus ast which has itconsecutive lambdas gruoped.
type GroupedLike v c a l
  = DeBrujinLike v c { args :: List.List a | l }

-- | Grouped expressions merge consecutive lambdas into groups.
type GroupedExpression
  = GroupedLike () Unit String ()

-- | Typelevel strnig for the field the arg list is stored in
_args :: SProxy "args"
_args = SProxy

-- | Merge all the lambdas in groups.
groupExpression :: forall v c l. DeBrujinLike v c l -> GroupedLike v c l ()
groupExpression =
  cata case _ of
    Call data' a b -> call data' a b
    Var data' -> var data'
    Lambda name body -> case project body of
      Lambda { args } body' -> lambda { args: name `List.Cons` args } body'
      _ -> lambda { args: List.singleton name } body

-- | Split all the lambdas from groups.
ungroupExpression :: forall v c a l r. Row.Lacks "args" l => (a -> Record l -> r) -> GroupedLike v c a l -> DeBrujinLike v c r
ungroupExpression f =
  cata case _ of
    Call data' a b -> call data' a b
    Var data' -> var data'
    Lambda data' body -> foldr createLambda body data'.args
      where
      createLambda arg inner = lambda (f arg rest) inner

      args = data'.args

      rest = Record.delete _args data'

-- | Find all referenced vars inside an expression.
references :: GroupedExpression -> Set.Set Int
references =
  cata case _ of
    Call _ function argument -> function `Set.union` argument
    Var { index } -> Set.singleton index
    Lambda { args: vars } body -> Set.mapMaybe mapVar body
      where
      mapVar :: Int -> Maybe Int
      mapVar a
        | a < varCount = Nothing
        | otherwise = Just (a - varCount)

      varCount = List.length vars

-- | Check if a grouped expression references a var.
contains :: forall v l a c. Int -> GroupedLike v l a c -> Boolean
contains = flip $ cata algebra
  where
  algebra = case _ of
    Var { index } -> \var -> index == var
    Call _ f a -> \i -> f i || a i
    Lambda { args } body -> \i -> body (i + List.length args)

-- | Shift all the variables in an expression by an arbitrary amount
shift :: forall v l a c. Int -> Int -> Endomorphism (GroupedLike v l a c)
shift initialPast amount = flip (cata algebra) initialPast
  where
  algebra = case _ of
    Var data' -> \past ->
      var
        if data'.index > past then
          data' { index = data'.index + amount }
        else
          data'
    Call data' function argument -> \past -> call data' (function past) (argument past)
    Lambda data' body -> \past -> lambda data' $ body (List.length data'.args + past)
