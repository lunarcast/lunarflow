{-- 
  Example lambda caclulus expressions I can use in the repl.
--}
module Lunarflow.Examples (const', flip') where

import Lunarflow.Ast (RawExpression)
import Lunarflow.Parser (unsafeParseLambdaCalculus)

-- | Basic exmple of how to encode `\a b. a` 
const' :: RawExpression
const' = unsafeParseLambdaCalculus "\\a b -> a"

flip' :: RawExpression
flip' = unsafeParseLambdaCalculus "\\f a b -> f b a"
