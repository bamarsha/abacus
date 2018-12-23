module Main (main) where

import Control.Applicative (empty)
import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token

import System.Console.Haskeline

-- The math token parser.
math :: GenTokenParser String u Identity
math = makeTokenParser mathDef

-- The math language definition.
mathDef :: GenLanguageDef String u Identity
mathDef = emptyDef { opStart = empty,
                     opLetter = empty,
                     reservedOpNames = ["+", "-", "*", "/", "^"] }

-- The expression parser.
expression :: ParsecT String u Identity Double
expression = buildExpressionParser table term

-- The table of math operators.
table :: OperatorTable String u Identity Double
table =
  [
    [
      -- Special case for negative numbers in the exponent.
      let negativeExponent = try $
                             reservedOp math "^" >>
                             reservedOp math "-" >>
                             return (\x y -> x ** (-y))
      in Infix negativeExponent AssocRight,

      Infix (reservedOp math "^" >> return (**)) AssocRight
    ],
    [
      Prefix (reservedOp math "-" >> return negate)
    ],
    [
      Infix (reservedOp math "*" >> return (*)) AssocLeft,
      Infix (reservedOp math "/" >> return (/)) AssocLeft
    ],
    [
      Infix (reservedOp math "+" >> return (+)) AssocLeft,
      Infix (reservedOp math "-" >> return (-)) AssocLeft
    ]
  ]

-- The parser for terms in an expression.
term :: ParsecT String u Identity Double
term = parens math expression <|>
       number

-- The parser for numbers in a term. It accepts both naturals and floats, but
-- naturals are converted to floats.
number :: ParsecT String u Identity Double
number = alwaysFloat <$> (naturalOrFloat math)
  where
    alwaysFloat :: Either Integer Double -> Double
    alwaysFloat (Left n) = fromIntegral n
    alwaysFloat (Right f) = f

-- The parser for a complete string of input.
input :: ParsecT String u Identity Double
input = do
  e <- expression
  eof
  return e

-- Runs a read-eval-print-loop for the calculator.
main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      maybeLine <- getInputLine "= "
      case maybeLine of
        Nothing -> return ()
        Just line -> (case parse input "" line of
                        Left error -> outputStrLn (show error)
                        Right result -> outputStrLn (show result)) >>
                     loop
