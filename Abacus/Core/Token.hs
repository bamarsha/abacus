module Abacus.Core.Token
    ( Token(..)
    , tokenize
    ) where

import Control.Applicative (empty)
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Number (fractional3)

import qualified Text.Parsec.Token as Token

-- A token.
data Token
    = Identifier String
    | Number Rational
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
tokenParser :: Token.GenTokenParser String u Identity
tokenParser = Token.makeTokenParser emptyDef
    { Token.opStart = empty
    , Token.opLetter = empty
    , Token.reservedOpNames = operatorNames
    }

-- The parser for any operator token.
operator :: Parsec String () Token
operator = choice $
    map (\op -> Token.reservedOp tokenParser op >> return (Operator op)) operatorNames

-- The parser for any symbol token.
symbol :: Parsec String () Token
symbol = choice $
    map (\sym -> Token.symbol tokenParser sym >> return (Symbol sym)) symbolNames

-- The parser for a function or variable identifier.
identifier :: Parsec String () Token
identifier = Identifier <$> Token.identifier tokenParser

-- The parser for a rational number token.
number :: Parsec String () Token
number = Number <$> Token.lexeme tokenParser (fractional3 False)

-- Parses a string and returns either a ParseError (Left) or the list of tokens in the string.
tokenize :: SourceName -> String -> Either ParseError [Token]
tokenize = flip runParser () $ do
    Token.whiteSpace tokenParser
    many (operator <|> symbol <|> identifier <|> number) <* eof
