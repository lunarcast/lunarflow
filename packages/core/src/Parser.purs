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
import Data.Foldable (foldl)
import Data.Foldable as Foldable
import Data.Identity (Identity)
import Data.NonEmpty (NonEmpty(..))
import Lunarflow.Ast (RawExpression, call, lambda, var)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (ParseError, Parser, ParserT, runParser)
import Text.Parsing.Parser.String (oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), GenTokenParser, LanguageDef, alphaNum, letter, makeTokenParser)

-- | Punctuation to start the declaration of a lambda expression.
lambdaStarts :: Array String
lambdaStarts = [ "\\", "λ" ]

-- | Punctuation to start the declaration of the body of a lambda expression.
lambdaBodyStarts :: Array String
lambdaBodyStarts = [ "->", "." ]

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
expression' :: LunarflowParser RawExpression -> LunarflowParser RawExpression
expression' expr = do
  -- expression'' <- atom
  (NonEmpty expression'' args) <- NonEmptyArray.toNonEmpty <$> some atom
  pure $ foldl (call unit) expression'' args
  where
  { parens, identifier, reservedOp } = tokenParser

  atom :: LunarflowParser RawExpression
  atom = wrapped <|> lambdaExpr <|> variable

  wrapped :: LunarflowParser RawExpression
  wrapped = parens expr

  variable :: ParserT String Identity RawExpression
  variable = var <$> identifier

  lambdaExpr :: ParserT String Identity RawExpression
  lambdaExpr = do
    Foldable.oneOf $ reservedOp <$> lambdaStarts
    (NonEmpty arg args) <- NonEmptyArray.toNonEmpty <$> NonEmptyArray.reverse <$> some identifier
    Foldable.oneOf $ reservedOp <$> lambdaBodyStarts
    body <- expr
    let
      baseAst = lambda arg body
    pure $ foldl (flip lambda) baseAst args

-- | Parser for lambda calculus.
expression :: LunarflowParser RawExpression
expression = fix expression'

-- | Try parsing a string into a lambda calculus ast.
parseLambdaCalculus :: String -> Either ParseError RawExpression
parseLambdaCalculus = flip runParser expression

-- | Partial function I usually use in the repl.
parseLambdaCalculus' :: Partial => String -> RawExpression
parseLambdaCalculus' = fromRight <<< parseLambdaCalculus

-- | Unsafe version of parseLambdaCalculus I usually use for debugging.
unsafeParseLambdaCalculus :: String -> RawExpression
unsafeParseLambdaCalculus = unsafePartial parseLambdaCalculus'