module Lunarflow.Examples where

import Prelude
import Lunarflow.Ast (AstChunk(..), Expression, mkAst)

-- | Basic exmple of how to encode `\a b. a` 
const' :: Expression
const' = lambda
  where
  lambda :: Expression
  lambda = mkAst (Lambda "x" lambda') {}

  lambda' :: Expression
  lambda' = mkAst (Lambda "y" varX) {}

  varX :: Expression
  varX = mkAst (Var "x") {}

flip' :: Expression
flip' =
  mkAst
    ( Lambda "f"
        $ mkAst
            ( Lambda "x"
                $ mkAst
                    ( Lambda "y"
                        $ mkAst (Call unit (mkAst (Call unit (mkAst (Var "f") {}) (mkAst (Var "y") {})) {}) (mkAst (Var "x") {})) {}
                    )
                    {}
            )
            {}
    )
    {}
