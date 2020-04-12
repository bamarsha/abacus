module Abacus.Core.Tokenizer
    ( Token(..)
    , tokenize
    ) where

import Control.Applicative (empty)
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Language

import qualified Text.Parsec.Token as Parsec

-- A token.
data Token
    = Identifier String
    | NumberToken Double
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
tokenParser :: Parsec.GenTokenParser String u Identity
tokenParser = Parsec.makeTokenParser emptyDef
    { Parsec.opStart = empty
    , Parsec.opLetter = empty
    , Parsec.reservedOpNames = operatorNames
    }

-- The parser for any operator token.
operator :: Parsec String () Token
operator = choice $
    map (\op -> Parsec.reservedOp tokenParser op >> return (Operator op)) operatorNames

-- The parser for any symbol token.
symbol :: Parsec String () Token
symbol = choice $
    map (\sym -> Parsec.symbol tokenParser sym >> return (Symbol sym)) symbolNames

-- The parser for a function or variable identifier.
identifier :: Parsec String () Token
identifier = Identifier <$> Parsec.identifier tokenParser

-- The parser for a number token. It accepts both naturals and floats, but naturals are converted to
-- floats.
number :: Parsec String () Token
number = alwaysFloat <$> Parsec.naturalOrFloat tokenParser
  where
    alwaysFloat (Left natural) = NumberToken (fromIntegral natural)
    alwaysFloat (Right float) = NumberToken float

-- Parses all of the tokens in the string.
allTokens :: Parsec String () [Token]
allTokens = do
    toks <- many $ operator <|> symbol <|> identifier <|> number
    toks <$ eof

-- Parses a string and returns either a ParseError (Left) or the list of tokens in the string.
tokenize :: SourceName -> String -> Either ParseError [Token]
tokenize = runParser allTokens ()
