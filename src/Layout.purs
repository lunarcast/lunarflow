module Lunarflow.Layout where

import Prelude
import Control.Monad.State (State, evalState)
import Data.Symbol (SProxy(..))
import Lunarflow.Ast (Ast(..), AstChunk(..), GroupedExpression, GroupedLambdaData, WithId)
import Lunarflow.Utils (increase)
import Record as Record
import Type.Row (type (+))

-- | Extensible record for asts which horizontally index all the lines.
type WithIndex r
  = ( index :: Int | r )

-- | Extensible record for asts which put timestamps on all the lines
type WithStart r
  = ( start :: Int | r )

-- | A layout which isn't complete yet
type AstWithStart
  = Ast { | WithStart () } { | WithStart + GroupedLambdaData } (WithId ())

-- | Helper to add starting points to a grouped ast
addStarts :: GroupedExpression -> AstWithStart
addStarts = flip evalState (-1) <<< go
  where
  go :: GroupedExpression -> State Int AstWithStart
  go (Ast ast) = (Ast <<< ast { term = _ }) <$> term
    where
    term :: State Int _
    term = case ast.term of
      Var name -> pure $ Var name
      Call _ func arg -> do
        func' <- go func
        arg' <- go arg
        start <- increase
        pure $ Call { start } func' arg'
      Lambda lambdaData body -> do
        start <- increase
        body' <- go body
        pure $ Lambda (Record.insert (SProxy :: _ "start") start lambdaData) body'
