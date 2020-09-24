module Lunarflow.Ast
  ( AstChunk(..)
  , Ast(..)
  , RawExpression
  , Expression
  , IndexedAst
  , GroupedExpression
  , WithId
  , WithIndex
  , IndexedLambdaData
  , GroupedLambdaData
  , IndexedVarData
  , AstTerm
  , groupExpression
  , indexExpression
  , mkAst
  , withDebrujinIndices
  , printDeBrujin
  , _term
  ) where

import Prelude
import Control.Monad.Reader (ReaderT, Reader, asks, local, runReader, runReaderT)
import Control.Monad.State (State, evalState, modify)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Prim.Row as Row
import Record as Record

-- | The meat and potatoes of representing an expression.
-- |
-- | - a represents the type we use as children.
-- | - l represents the type lambdas carry around.
-- | - c represents the type calls carry around.
-- | - v represents the type variables carry around.
-- | 
-- | Thre reason we take an argument for a is so we don't have to 
-- | take an extra type argumet and write `Ast v c l r` on each occurence. 
data AstChunk v c l a
  = Call c a a
  | Lambda l a
  | Var v

derive instance genericAstChunk :: Generic (AstChunk v c l a) _

instance showAstChunk :: (Show c, Show l, Show a, Show v) => Show (AstChunk v c l a) where
  show = genericShow

-- | AstChunk with asts as children
type AstTerm v c l r
  = AstChunk v c l (Ast v c l r)

-- | Generic Ast type
-- |
-- | This is extensible so we don't have to create a different dsl 
-- | each time we want to augument a tree with new information.
newtype Ast v c l r
  = Ast
  { term :: AstTerm v c l r
  | r
  }

derive instance genericAst :: Generic (Ast v c l r) _

instance showAst :: (Show l, Show v, Show c) => Show (Ast v c l r) where
  show (Ast { term }) = genericShow term

derive instance newtypeAst :: Newtype (Ast v c l r) _

-- | SProxy for the term prop of asts.
termProxy :: SProxy "term"
termProxy = SProxy

_term :: forall c l r v. Lens' (Ast v c l r) (AstTerm v c l r)
_term = _Newtype <<< prop termProxy

-- | Helper for packing an ast
mkAst :: forall v r l c. Row.Lacks "term" r => AstTerm v c l r -> Record r -> Ast v c l r
mkAst inner = Ast <<< Record.insert termProxy inner

-- | Basic lambda calculus expressions
type RawExpression
  = Ast String Unit String ()

-- | We use this type so we keep access to the name of the variables for documentation purpouses.
type IndexedVarData r
  = { | WithIndex ( name :: String | r ) }

-- | Lambda calculus expression using de brujin indices.
type Expression
  = Ast (IndexedVarData ()) Unit String ()

-- | Basic extensible record for stuff which has an unique id represented as an int.
type WithId r
  = ( id :: Int | r )

type WithIndex r
  = ( index :: Int | r )

-- | Data  carried around by lambdas. 
-- | The argumentId is an unique code variables 
-- | can use to reference that argument (basically removeing the need for shadowing).
-- | Thre reason we do this instead of changing names to indices is for ease of generating the layouts.
-- I made this into a separate type because I use it in the GroupedExpression type as well
type IndexedLambdaData r
  = { argumentName :: String, argumentId :: Int | r }

-- | Indexed lambda calculus expressions
type IndexedAst
  = Ast (IndexedVarData ()) Unit (IndexedLambdaData ())
      (WithId ())

-- | Add de brujin indices to a lambda calculus expression.
withDebrujinIndices :: RawExpression -> Expression
withDebrujinIndices = flip runReader List.Nil <<< go
  where
  go :: RawExpression -> Reader (List.List String) Expression
  go (Ast { term }) =
    flip mkAst {}
      <$> case term of
          Call _ func argument -> Call unit <$> go func <*> go argument
          Lambda name body -> Lambda name <$> (local (List.Cons name) $ go body)
          Var name -> do
            maybeIndex <- asks $ List.findIndex (eq name)
            pure case maybeIndex of
              Just index -> Var { index, name }
              Nothing -> Var { name, index: -1 }

-- | Add incidces for all nodes in an ast
indexExpression :: Expression -> IndexedAst
indexExpression = flip evalState 0 <<< flip runReaderT List.Nil <<< go
  where
  -- | Internal version of fromExpression which runs in an actual monad
  go :: Expression -> ReaderT (List.List Int) (State Int) IndexedAst
  go (Ast { term }) = do
    indexed <- case term of
      Call _ func argument -> Call unit <$> go func <*> go argument
      Lambda name body -> do
        argumentId <- getId
        Lambda { argumentName: name, argumentId } <$> local (List.Cons argumentId) (go body)
      Var data' -> pure $ Var data'
    id <- case term of
      Var { index } -> asks (flip List.index index) >>= maybe getId pure
      _ -> getId
    pure
      $ Ast
          { id
          , term: indexed
          }

  -- | Helper for getting a new unique id
  getId = modify ((+) 1)

-- | Data for visually sugared grouped lambdas
type GroupedLambdaData r
  = ( arguments :: List.List (IndexedLambdaData r) )

-- | Ast which doesn't allow consecutive lambdas.
type GroupedExpression
  = Ast (IndexedVarData ()) Unit { | GroupedLambdaData () } (WithId ())

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
    Call _ func arg -> Call unit (groupExpression func) (groupExpression arg)
    Var a -> Var a

--  Pretty printing stuff:
-- | Check if an ast chunk needs to be wrapped in parenthesis for printing
needsParenthesis :: forall v c l r. Row.Lacks "term" r => Boolean -> Ast v c l r -> Boolean
needsParenthesis left (Ast { term }) = go term
  where
  go (Lambda _ _) = left

  go (Call _ _ _) = not left

  go _ = false

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
printDeBrujin :: forall v c l r. Row.Lacks "term" r => Ast ({ | WithIndex v }) c l r -> String
printDeBrujin (Ast { term }) = case term of
  Var { index } -> show index
  Lambda _ body -> lambdaChar <> printDeBrujin body
  Call _ func arg ->
    parenthesiseWhen (needsParenthesis true func) func'
      <> parenthesiseWhen (needsParenthesis false arg) arg'
    where
    func' = printDeBrujin func

    arg' = printDeBrujin arg
