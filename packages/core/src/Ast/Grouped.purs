module Lunarflow.Ast.Grouped
  ( GroupedLikeF
  , GroupedLike
  , GroupedExpression
  , _args
  , groupExpression
  , ungroupExpression
  , references
  , contains
  , shift
  ) where

import Lunarlude
import Data.List as List
import Data.Set as Set
import Lunarflow.Ast (AstF(..), DeBrujinLike, Name(..), DeBrujinLikeF, call, lambda, shiftIndex, var)
import Prim.Row as Row
import Record as Record

-- TODO: Make the argument list nonempty.
-- | Base functor for grouped expressions
type GroupedLikeF v c a l
  = DeBrujinLikeF v c { args :: List.List a | l }

-- | A chunk of lambda calculus ast which has itconsecutive lambdas gruoped.
type GroupedLike v c a l
  = Mu (GroupedLikeF v c a l)

-- | Grouped expressions merge consecutive lambdas into groups.
type GroupedExpression
  = GroupedLike () Unit String ()

-- | Typelevel string for the field the arg list is stored in
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
ungroupExpression ::
  forall v c a l r.
  Row.Lacks "args" l => (a -> Record l -> r) -> GroupedLike v c a l -> DeBrujinLike v c r
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
references :: forall v c a l. GroupedLike v c a l -> Set.Set Name
references =
  cata case _ of
    Call _ function argument -> function `Set.union` argument
    Var { name } -> Set.singleton name
    Lambda { args: vars } body -> Set.mapMaybe mapVar body
      where
      mapVar :: Name -> Maybe Name
      mapVar = case _ of
        Bound index
          | index < varCount -> Nothing
          | otherwise -> Just $ Bound $ index - varCount
        free -> Just free

      varCount = List.length vars

-- | Check if a grouped expression references a var.
contains :: forall v l a c. Name -> GroupedLike v l a c -> Boolean
contains = flip $ cata algebra
  where
  algebra expression name = case expression of
    Var data' -> data'.name == name
    Call _ f a -> f name || a name
    Lambda { args } body -> body $ shiftIndex (List.length args) name

-- | Shift all the variables in an expression by an arbitrary amount
shift :: forall v l a c. Int -> Int -> Endomorphism (GroupedLike v l a c)
shift initialPast amount = flip (cata algebra) initialPast
  where
  algebra expression past = case expression of
    Var data'@{ name: Bound index }
      | index > past -> var data' { name = Bound $ index + amount }
    Var data' -> var data'
    Call data' function argument -> call data' (function past) (argument past)
    Lambda data' body -> lambda data' $ body (List.length data'.args + past)
