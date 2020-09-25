module Lunarflow.Ast
  ( Ast(..)
  , RawExpression
  , Expression
  , GroupedExpression
  , WithIndex
  , GroupedLambdaData
  , groupExpression
  , withDebrujinIndices
  , printDeBrujin
  ) where

import Prelude
import Control.Monad.Reader (Reader, asks, local, runReader)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List as List
import Data.Maybe (Maybe(..))

-- | The meat and potatoes of representing an expression.
-- |
-- | - l represents the type lambdas carry around.
-- | - c represents the type calls carry around.
-- | - v represents the type variables carry around.
data Ast v c l
  = Call c (Ast v c l) (Ast v c l)
  | Lambda l (Ast v c l)
  | Var v

derive instance genericAst :: Generic (Ast v c l) _

instance showAst :: (Show c, Show l, Show v) => Show (Ast v c l) where
  show a = genericShow a

-- | Basic lambda calculus expressions
type RawExpression
  = Ast String Unit String

-- | Lambda calculus expression using de brujin indices.
type Expression
  = Ast Int Unit String

type WithIndex r
  = ( index :: Int | r )

-- | Add de brujin indices to a lambda calculus expression.
withDebrujinIndices :: RawExpression -> Expression
withDebrujinIndices = flip runReader List.Nil <<< go
  where
  go :: RawExpression -> Reader (List.List String) Expression
  go = case _ of
    Call _ func argument -> Call unit <$> go func <*> go argument
    Lambda name body -> Lambda name <$> (local (List.Cons name) $ go body)
    Var name -> do
      maybeIndex <- asks $ List.findIndex (eq name)
      pure case maybeIndex of
        Just index -> Var index
        Nothing -> Var (-1)

-- | Data for visually sugared grouped lambdas
type GroupedLambdaData r
  = ( arguments :: List.List { name :: String | r } )

-- | Ast which doesn't allow consecutive lambdas.
type GroupedExpression
  = Ast Int Unit { | GroupedLambdaData () }

-- | Group multiple consecutive lambdas into 1 as visual sugar.
-- | This is the textual equivalent of suagring \f. \a. \b. f b a into \f a b. f b a 
groupExpression :: Expression -> GroupedExpression
groupExpression = case _ of
  Lambda lambdaData body -> case nested of
    Lambda { arguments } nestedBody -> Lambda { arguments: List.Cons { name: lambdaData } arguments } nested
    _ -> Lambda { arguments: pure { name: lambdaData } } nested
    where
    nested = groupExpression body
  Call _ func arg -> Call unit (groupExpression func) (groupExpression arg)
  Var a -> Var a

--  Pretty printing stuff:
-- | Check if an ast chunk needs to be wrapped in parenthesis for printing
needsParenthesis :: forall v c l. Boolean -> Ast v c l -> Boolean
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
printDeBrujin :: forall c l. Ast Int c l -> String
printDeBrujin = case _ of
  Var index -> show index
  Lambda _ body -> lambdaChar <> printDeBrujin body
  Call _ func arg ->
    parenthesiseWhen (needsParenthesis true func) func'
      <> parenthesiseWhen (needsParenthesis false arg) arg'
    where
    func' = printDeBrujin func

    arg' = printDeBrujin arg
