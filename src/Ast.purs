module Lunarflow.Ast
  ( Ast(..)
  , RawExpression
  , Expression
  , WithIndex
  , withDebrujinIndices
  , printDeBrujin
  , collectLambdas
  ) where

import Prelude
import Control.Monad.Reader (Reader, asks, local, runReader)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

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

-- instance showDeBrujinAst :: Show (Ast Int c l) where
--   show = printDeBrujin
-- else
instance showAst :: (Show v, Show c, Show l) => Show (Ast v c l) where
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

-- | Collect consecutive lambdas into a list and return it alongside the deepest body.
collectLambdas :: forall c l v. Ast v c l -> (Tuple (List.List l) (Ast v c l))
collectLambdas = case _ of
  Lambda data' body -> Tuple (List.Cons data' data'') body'
    where
    (Tuple data'' body') = collectLambdas body
  other -> Tuple List.Nil other
