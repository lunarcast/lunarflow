module Lunarflow.Parser where

import Lunarlude
import Data.Either (fromRight)
import Data.Function.Uncurried (Fn2, mkFn2)
import Lunarflow.Ast (RawExpression, call, lambda, var)
import Partial.Unsafe (unsafePartial)

-- | Ts representation of asts.
type ForeignAst a
  = { abstraction :: Fn2 String a a
    , application :: Fn2 a a a
    , variable :: String -> a
    } ->
    a

-- | Parse a lambda calculus expression without processing the foreign ast away.
parseLambdaCalculus' :: forall a. String -> Either String (ForeignAst a)
parseLambdaCalculus' = parseImpl { left: Left, right: Right }

-- | Parse a lambda calculus expression.
parseLambdaCalculus :: String -> Either String RawExpression
parseLambdaCalculus = parseLambdaCalculus' >>> map run
  where
  run :: ForeignAst RawExpression -> RawExpression
  run ast =
    ast
      { variable: { name: _ } >>> var
      , abstraction: mkFn2 $ { argument: _ } >>> lambda
      , application: mkFn2 (call {})
      }

-- | Unsafe version of parseLambdaCalculus I usually use for debugging.
unsafeParseLambdaCalculus :: String -> RawExpression
unsafeParseLambdaCalculus = unsafePartial (parseLambdaCalculus >>> fromRight)

foreign import parseImpl ::
  forall r.
  { left :: forall e a. e -> Either e a, right :: forall e a. a -> Either e a } ->
  String -> Either String (ForeignAst r)
