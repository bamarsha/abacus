module Calculator.Tokenizer (Token (..), tokenize) where

import Control.Applicative (empty)
import Data.Functor.Identity (Identity)

import Text.Parsec.Combinator (choice, eof)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Pos (SourceName)
import Text.Parsec.Prim (Parsec, many, runParser, (<|>))
import qualified Text.Parsec.Token as PT

-- A token.
data Token = Identifier String
           | NumberT Double
           | Operator String
           | Symbol String
  deriving (Eq, Show)

-- The list of valid operator names.
operatorNames :: [String]
operatorNames = words "+ - * / ^"

-- The list of valid symbol names.
symbolNames :: [String]
symbolNames = words "( ) , ="

-- Parses tokens in a string.
tokenParser :: PT.GenTokenParser String u Identity
tokenParser =
  PT.makeTokenParser emptyDef { PT.opStart = empty,
                                PT.opLetter = empty,
                                PT.reservedOpNames = operatorNames }

-- The parser for any operator token.
operator :: Parsec String () Token
operator =
  choice $ map (\o -> PT.reservedOp tokenParser o >> return (Operator o))
               operatorNames

-- The parser for any symbol token.
symbol :: Parsec String () Token
symbol =
  choice $ map (\o -> PT.symbol tokenParser o >> return (Symbol o))
           symbolNames

-- The parser for a function or variable identifier.
identifier :: Parsec String () Token
identifier = Identifier <$> PT.identifier tokenParser

-- The parser for a number token. It accepts both naturals and floats, but
-- naturals are converted to floats.
number :: Parsec String () Token
number = alwaysFloat <$> PT.naturalOrFloat tokenParser
  where
    alwaysFloat :: Either Integer Double -> Token
    alwaysFloat (Left n) = NumberT (fromIntegral n)
    alwaysFloat (Right f) = NumberT f

-- The parser for taking a complete string of math and returning a list of
-- tokens.
tokens :: Parsec String () [Token]
tokens = do
  t <- many $ operator <|> symbol <|> identifier <|> number
  t <$ eof

-- Parses a string of math and returns either a ParseError (Left) or the list of
-- tokens in the string.
tokenize :: SourceName -> String -> Either ParseError [Token]
tokenize = runParser tokens ()
