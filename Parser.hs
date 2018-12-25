module Parser (Parser.parse) where

import AST
import Control.Applicative (empty)
import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token

-- The math token parser.
math :: GenTokenParser String u Identity
math = makeTokenParser mathDef

-- The math language definition.
mathDef :: GenLanguageDef String u Identity
mathDef = emptyDef { opStart = empty,
                     opLetter = empty,
                     reservedOpNames = ["+", "-", "*", "/", "^", "="] }

-- The expression parser.
expression :: ParsecT String u Identity Expression
expression = buildExpressionParser table term

-- The table of math operators.
table :: OperatorTable String u Identity Expression
table =
  [
    [
      -- Special case for negative numbers in the exponent.
      let negativeExponent = try $
                             reservedOp math "^" >>
                             reservedOp math "-" >>
                             return (\x y -> Raise x (Negate y))
      in Infix negativeExponent AssocRight,

      Infix (reservedOp math "^" >> return Raise) AssocRight
    ],
    [
      Prefix (reservedOp math "-" >> return Negate)
    ],
    [
      Infix (reservedOp math "*" >> return Multiply) AssocLeft,
      Infix (reservedOp math "/" >> return Divide) AssocLeft
    ],
    [
      Infix (reservedOp math "+" >> return Add) AssocLeft,
      Infix (reservedOp math "-" >> return Subtract) AssocLeft
    ]
  ]

-- The parser for terms in an expression.
term :: ParsecT String u Identity Expression
term = parens math expression <|>
       number <|>
       call

-- The parser for numbers in a term. It accepts both naturals and floats, but
-- naturals are converted to floats.
number :: ParsecT String u Identity Expression
number = alwaysFloat <$> (naturalOrFloat math)
  where
    alwaysFloat :: Either Integer Double -> Expression
    alwaysFloat (Left n) = Number (fromIntegral n)
    alwaysFloat (Right f) = Number f

-- The parser for lists. It tries to apply the given parser until it fails, with
-- each application of the parser separated by a comma. The list fails if there
-- isn't at least one element.
list :: ParsecT String u Identity a -> ParsecT String u Identity [a]
list parser = do
  head <- parser
  tail <- option [] (reservedOp math "," >> list parser)
  return (head : tail)

-- The parser for a function call.
call :: ParsecT String u Identity Expression
call = do
  name <- identifier math
  arguments <- option [] (parens math (list expression))
  return (Call name arguments)

-- The parser for a function binding.
binding :: ParsecT String u Identity Statement
binding = do
  name <- identifier math
  parameters <- option [] (parens math (list (identifier math)))
  reservedOp math "="
  e <- expression
  return (Binding name parameters e)

-- The parser for a statement.
statement :: ParsecT String u Identity Statement
statement =
  (try binding) <|>
  (expression >>= return . Expression)

-- The parser for a complete string of input.
input :: ParsecT String u Identity Statement
input = do
  s <- statement
  eof
  return s

-- Parses a string of math and returns either a ParseError (Left) or a Statement
-- that is the result of parsing the string (Right).
parse :: String -> Either ParseError Statement
parse string = Text.Parsec.parse input "" string
