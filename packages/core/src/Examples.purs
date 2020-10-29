{-- 
  Example lambda caclulus expressions I can use in the repl.
--}
module Lunarflow.Examples where

import Prelude
import Lunarflow.Ast (RawExpression)
import Lunarflow.Parser (unsafeParseLambdaCalculus)

-- | Basic exmple of how to encode `\a b. a` 
const' :: RawExpression
const' = unsafeParseLambdaCalculus """\a b -> a"""

flip' :: RawExpression
flip' = unsafeParseLambdaCalculus """\f a b -> f b a"""

compose' :: RawExpression
compose' = unsafeParseLambdaCalculus """\f g a. f (g a)"""

zero' :: RawExpression
zero' = unsafeParseLambdaCalculus """\s z. z"""

one' :: RawExpression
one' = unsafeParseLambdaCalculus """\s z. s z"""

n :: Int -> RawExpression
n x' = unsafeParseLambdaCalculus $ """\s z -> """ <> go x'
  where
  go x
    | x <= 0 = "z"
    | x == 1 = "s z"
    | otherwise = "s (" <> go (x - 1) <> ")"

succ :: RawExpression
succ = unsafeParseLambdaCalculus """\n s z. s (n s z)"""

plus' :: RawExpression
plus' = unsafeParseLambdaCalculus """\a b s z. a s (b s z)"""

mult :: RawExpression
mult = unsafeParseLambdaCalculus """\a b s z. a (b s) z"""

exp :: RawExpression
exp = unsafeParseLambdaCalculus """\m n. n m"""
