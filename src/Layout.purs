module Lunarflow.Layout
  ( WithIndex
  , WithStart
  , WithEnd
  , AstWithStart
  , Layout
  , addStarts
  , addEnds
  , addIndices
  ) where

import Prelude
import Control.Monad.Reader (ask, local, runReader)
import Control.Monad.State (State, evalState)
import Data.List (foldMap)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Ord.Max (Max(..))
import Data.Symbol (SProxy(..))
import Lunarflow.Ast (Ast(..), AstChunk(..), GroupedExpression, GroupedLambdaData, WithId, mkAst)
import Lunarflow.Utils (increase)
import Record as Record
import Type.Row (type (+))

-- | Extensible record for asts which horizontally index all the lines.
type WithIndex r
  = ( index :: Int | r )

-- | Extensible record for asts which put timestamps on all the lines
type WithStart r
  = ( start :: Int | r )

type WithEnd r
  = ( end :: Int | r )

-- | A layout which isn't complete yet
type AstWithStart
  = Ast { | WithStart () } { | WithStart + GroupedLambdaData () } (WithId ())

type AstWithIntervals
  = Ast { | WithStart () } { | WithStart + GroupedLambdaData (WithEnd ()) } (WithId ())

-- | Layouts are expressions which have vertical indices for every line.
type Layout
  = Ast { | WithStart + WithIndex () } { | WithStart + WithIndex + GroupedLambdaData (WithIndex + WithEnd ()) } (WithId ())

-- | Sproxy used for adding indices to records.
_index :: SProxy "index"
_index = SProxy

-- | SProxy used for adding starts to records.
_start :: SProxy "start"
_start = SProxy

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
        pure $ Lambda (Record.insert _start start lambdaData) body'

addEnds :: AstWithStart -> AstWithIntervals
addEnds (Ast ast) =
  (Ast <<< ast { term = _ }) case ast.term of
    Var a -> Var a
    Call callData function argument -> Call callData (addEnds function) (addEnds argument)
    Lambda lambdaData body ->
      Lambda
        { start: lambdaData.start
        , arguments: updateArg lambdaData.start <$> lambdaData.arguments
        }
        $ addEnds body
      where
      updateArg start { argumentName, argumentId } = { argumentName, argumentId, end: fromMaybe (start + 1) $ flip runReader Nothing $ getLastUsage argumentId body }

      getLastUsage target (Ast { term, id }) = case term of
        Var _
          | id == target -> ask
          | otherwise -> pure Nothing
        Call { start } func arg ->
          local (const $ Just start) do
            func' <- getLastUsage target func
            arg' <- getLastUsage target arg
            pure $ unwrap <$> foldMap (map Max) [ func', arg' ]
        Lambda { start } currentBody -> local (const $ Just start) $ getLastUsage target currentBody

addIndices :: AstWithIntervals -> Layout
addIndices (Ast { term, id }) =
  flip mkAst { id } case term of
    Var name -> Var name
    Call callData func arg -> Call callData' (addIndices func) (addIndices arg)
      where
      callData' = Record.insert _index 0 callData
    Lambda lambdaData body -> Lambda lambdaData' $ addIndices body
      where
      lambdaData' = { arguments, index: 0, start: lambdaData.start }

      arguments = flip List.mapWithIndex lambdaData.arguments $ \index a -> Record.insert _index index a
