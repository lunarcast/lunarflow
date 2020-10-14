module Lunarflow.Ast.Grouped where

import Prelude
import Data.Functor.Mu (Mu)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Lunarflow.Ast (AstF(..), Expression, call, lambda, var)
import Matryoshka (Algebra, cata, project)

-- | The base functor for grouped expressions.
type GroupedExpressionF
  = AstF Int Unit (List.List String)

-- | Grouped expressions merge consecutive lambdas into groups.
type GroupedExpression
  = Mu GroupedExpressionF

-- | Merge all the lambdas in groups.
groupExpression :: Expression -> GroupedExpression
groupExpression = cata algebra
  where
  algebra :: Algebra (AstF Int Unit String) GroupedExpression
  algebra = case _ of
    Call data' a b -> call data' a b
    Var data' -> var data'
    Lambda name body -> case project body of
      Lambda data' body' -> lambda (data' `List.snoc` name) body'
      _ -> lambda (List.singleton name) body

-- | Find all referenced vars inside an expression.
references :: GroupedExpression -> Set.Set Int
references = cata algebra
  where
  algebra :: Algebra GroupedExpressionF (Set.Set Int)
  algebra = case _ of
    Call _ function argument -> function `Set.union` argument
    Var a -> Set.singleton a
    Lambda vars body -> Set.mapMaybe mapVar body
      where
      mapVar :: Int -> Maybe Int
      mapVar a
        | a < varCount = Nothing
        | otherwise = Just (a - varCount)

      varCount = List.length vars
