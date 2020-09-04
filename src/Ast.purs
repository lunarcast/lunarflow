module Lunarflow.Ast
  ( AstChunk(..)
  , Ast(..)
  , Expression
  , IndexedAst
  , GroupedExpression
  , WithId
  , IndexedLambdaData
  , groupExpression
  , indexExpression
  ) where

import Prelude
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.State (State, evalState, modify)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Symbol (SProxy(..))
import Prim.Row as Row
import Record as Record

-- | The meat and potatoes of representing an expression.
-- |
-- | - a represents the type we use as childre.
-- | - l represents the type lambdas carry around.
-- | 
-- | Thre reason we take an argument for a is so we don't have to 
-- | take an extra type argumet and write `Ast l r` on each occurence. 
data AstChunk l a
  = Call a a
  | Lambda l a
  | Var String

derive instance genericAstChunk :: Generic (AstChunk l a) _

instance showAstChunk :: (Show l, Show a) => Show (AstChunk l a) where
  show = genericShow

-- | Generic Ast type
-- |
-- | This is extensible so we don't have to create a different dsl 
-- | each time we want to augument a tree with new information.
newtype Ast l r
  = Ast
  { term :: AstChunk l (Ast l r)
  | r
  }

derive instance genericAst :: Generic (Ast l r) _

instance showAst :: Show l => Show (Ast l r) where
  show (Ast { term }) = genericShow term

-- | Helper for packing an ast
mkAst :: forall r l. Row.Lacks "term" r => AstChunk l (Ast l r) -> Record r -> Ast l r
mkAst inner = Ast <<< Record.insert (SProxy :: _ "term") inner

-- | Basic lambda calculus expressions
type Expression
  = Ast String ()

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

-- | Basic extensible record for stuff which has an unique id represented as an int.
type WithId r
  = ( id :: Int | r )

-- | Data  carried around by lambdas. 
-- | The argumentId is an unique code variables 
-- | can use to reference that argument (basically removeing the need for shadowing).
-- | Thre reason we do this instead of changing names to indices is for ease of generating the layouts.
-- I made this into a separate type because I use it in the GroupedExpression type as well
type IndexedLambdaData
  = { argumentName :: String, argumentId :: Int }

-- | Indexed lambda calculus expressions
type IndexedAst
  = Ast IndexedLambdaData
      (WithId ())

-- | Add incidces for all nodes in an ast
indexExpression :: Expression -> IndexedAst
indexExpression = flip evalState 0 <<< flip runReaderT Map.empty <<< go
  where
  -- | Internal version of fromExpression which runs in an actual monad
  go :: Expression -> ReaderT (Map String Int) (State Int) IndexedAst
  go (Ast { term }) = do
    indexed <- case term of
      Call func argument -> Call <$> go func <*> go argument
      Lambda name body -> do
        argumentId <- getId
        Lambda { argumentName: name, argumentId } <$> local (Map.insert name argumentId) (go body)
      Var name -> pure $ Var name
    id <- case term of
      Var name -> asks (Map.lookup name) >>= maybe getId pure
      _ -> getId
    pure
      $ Ast
          { id
          , term: indexed
          }

  -- | Helper for getting a new unique id
  getId = modify ((+) 1)

-- | Ast which doesn't allow consecutive lambdas.
type GroupedExpression
  = Ast { arguments :: List.List IndexedLambdaData } (WithId ())

-- | Group multiple consecutive lambdas into 1 as visual sugar.
-- | This is the textual equivalent of suagring \f. \a. \b. f b a into \f a b. f b a 
groupExpression :: IndexedAst -> GroupedExpression
groupExpression ast@(Ast astData) = Ast $ astData { term = term }
  where
  term = case astData.term of
    Lambda lambdaData body -> case nestedTerm of
      Lambda { arguments } nestedBody -> Lambda { arguments: List.Cons lambdaData arguments } nestedBody
      _ -> Lambda { arguments: pure lambdaData } nestedAst
      where
      nestedAst@(Ast { term: nestedTerm }) = groupExpression body
    Call func arg -> Call (groupExpression func) (groupExpression arg)
    Var a -> Var a
