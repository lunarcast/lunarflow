module Lunarflow.Parser
  ( LunarflowParser
  , expression
  , parseLambdaCalculus
  , parseLambdaCalculus'
  , unsafeParseLambdaCalculus
  ) where

import Prelude
import Control.Lazy (fix)
import Control.Plus (empty, (<|>))
import Data.Array.NonEmpty (some)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either, fromRight)
import Data.Foldable (foldr)
import Data.Foldable as Foldable
import Data.Identity (Identity)
import Data.NonEmpty (NonEmpty(..))
import Lunarflow.Ast (AstChunk(..), Expression, mkAst)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (ParseError, Parser, ParserT, runParser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.String (oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), GenTokenParser, LanguageDef, alphaNum, letter, makeTokenParser)

-- | Punctuation to start the declaration of a lambda expression.
lambdaStarts :: Array String
lambdaStarts = [ "\\", "λ" ]

-- | Punctuation to start the declaration of the body of a lambda expression.
lambdaBodyStarts :: Array String
lambdaBodyStarts = [ "\\", "." ]

-- | Declaration for lambda calculus (with comments).
-- | This is used to generate the lexer
language :: LanguageDef
language =
  LanguageDef
    { commentStart: "{-"
    , commentEnd: "-}"
    , commentLine: "--"
    , nestedComments: true
    , opStart: empty
    , opLetter: empty
    , caseSensitive: true
    , reservedOpNames: lambdaStarts <> lambdaBodyStarts
    , reservedNames: []
    , identStart: letter
    , identLetter: alphaNum <|> oneOf [ '_', '\'' ]
    }

-- | The lexer
tokenParser :: GenTokenParser String Identity
tokenParser = makeTokenParser language

-- | Parsers which run in the identity monad and parse strings
type LunarflowParser r
  = Parser String r

-- | Parser for individual lambda calculus expressions.
-- | This references itself so we use it within a fixpoint operator
expression' :: LunarflowParser Expression -> LunarflowParser Expression
expression' expr = do
  expression'' <- atom
  withCall expression'' <|> pure expression''
  where
  { parens, identifier, reservedOp } = tokenParser

  withCall :: Expression -> LunarflowParser Expression
  withCall expression'' = do
    argument <- try atom
    let
      app = flip mkAst {} $ Call unit expression'' argument
    withCall app <|> pure app

  atom :: LunarflowParser Expression
  atom = wrapped <|> lambdaExpr <|> variable

  wrapped :: LunarflowParser Expression
  wrapped = parens expr

  variable :: ParserT String Identity Expression
  variable = (flip mkAst {} <<< Var) <$> identifier

  lambdaExpr :: ParserT String Identity Expression
  lambdaExpr = do
    Foldable.oneOf $ reservedOp <$> lambdaStarts
    (NonEmpty arg args) <- NonEmptyArray.toNonEmpty <$> NonEmptyArray.reverse <$> some identifier
    Foldable.oneOf $ reservedOp <$> lambdaBodyStarts
    body <- expr
    let
      baseAst = mkAst (Lambda arg body) {}

      go = (flip mkAst {} <<< _) <<< Lambda
    pure $ foldr go baseAst args

-- | Parser for lambda calculus.
expression :: LunarflowParser Expression
expression = fix expression'

-- | Try parsing a string into a lambda calculus ast.
parseLambdaCalculus :: String -> Either ParseError Expression
parseLambdaCalculus = flip runParser expression

-- | Partial function I usually use in the repl.
parseLambdaCalculus' :: Partial => String -> Expression
parseLambdaCalculus' = fromRight <<< parseLambdaCalculus

-- | Unsafe version of parseLambdaCalculus I usually use for debugging.
unsafeParseLambdaCalculus :: String -> Expression
unsafeParseLambdaCalculus = unsafePartial parseLambdaCalculus'
