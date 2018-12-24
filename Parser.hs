module Parser (parseMath) where

import Control.Applicative (empty)
import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token

-- A math expression.
data Expression = Raise Expression Expression  -- (base exponent)
                | Negate Expression
                | Multiply Expression Expression
                | Divide Expression Expression
                | Add Expression Expression
                | Subtract Expression Expression
                | Call String Expression  -- (name argument)
                | Variable String  -- (name)
                | Number Double
  deriving Show

-- The math token parser.
math :: GenTokenParser String u Identity
math = makeTokenParser mathDef

-- The math language definition.
mathDef :: GenLanguageDef String u Identity
mathDef = emptyDef { opStart = empty,
                     opLetter = empty,
                     reservedOpNames = ["+", "-", "*", "/", "^"] }

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
       callOrVariable

-- The parser for numbers in a term. It accepts both naturals and floats, but
-- naturals are converted to floats.
number :: ParsecT String u Identity Expression
number = alwaysFloat <$> (naturalOrFloat math)
  where
    alwaysFloat :: Either Integer Double -> Expression
    alwaysFloat (Left n) = Number (fromIntegral n)
    alwaysFloat (Right f) = Number f

-- The parser for a function call or variable name.
callOrVariable = do
  name <- identifier math
  maybeArgument <- optionMaybe (parens math expression)
  case maybeArgument of
    Nothing -> return (Variable name)
    Just argument -> return (Call name argument)

-- The parser for a complete string of input.
input :: ParsecT String u Identity Expression
input = do
  e <- expression
  eof
  return e

-- Parses a string of math and returns either a ParseError (Left) or an
-- Expression that is the result of parsing the string (Right).
parseMath :: String -> Either ParseError Expression
parseMath string = parse input "" string
