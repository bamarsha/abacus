{-# LANGUAGE LambdaCase #-}

module Calculator.Parser (Calculator.Parser.parse) where

import Calculator.AST
  (Expression (Add, Call, Divide, Negate, Number, Multiply, Raise, Subtract),
   Statement (Expression, Binding))
import Calculator.Tokenizer (Token (Identifier, NumberT, Operator, Symbol),
                             tokenize)
import Calculator.Utils (intersperseWhen)

import Data.Functor.Identity (Identity)

import Text.Parsec.Combinator (between, eof, option)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Expr
  (Assoc (AssocLeft, AssocRight), Operator (Infix, Prefix), OperatorTable,
   buildExpressionParser)
import Text.Parsec.Prim (Parsec, ParsecT, parse, tokenPrim, try, (<|>))

-- The table of math operators.
table :: OperatorTable [Token] () Identity Expression
table =
  [
    [
      -- Special case for negative numbers in the exponent.
      let negativeExponent = try $
                             operator "^" >>
                             operator "-" >>
                             return (\x y -> Raise x (Negate y))
      in Infix negativeExponent AssocRight,

      Infix (operator "^" >> return Raise) AssocRight
    ],
    [
      Prefix (operator "-" >> return Negate)
    ],
    [
      Infix (operator "*" >> return Multiply) AssocLeft,
      Infix (operator "/" >> return Divide) AssocLeft
    ],
    [
      Infix (operator "+" >> return Add) AssocLeft,
      Infix (operator "-" >> return Subtract) AssocLeft
    ]
  ]

-- The parser satisfy f succeeds for any token for which the given function f
-- returns True. Returns the token that is actually parsed.
satisfy :: (Token -> Maybe a) -> Parsec [Token] () a
satisfy = tokenPrim show (\pos _ _ -> pos)

-- The parser for the given operator o.
operator :: String -> Parsec [Token] () ()
operator o = satisfy $ \token ->
  if token == Operator o
  then Just ()
  else Nothing

-- The parser for the given symbol s.
symbol :: String -> Parsec [Token] () ()
symbol s = satisfy $ \token ->
  if token == Symbol s
  then Just ()
  else Nothing

-- The parser for an identifier. Returns the identifier as a string.
identifier :: Parsec [Token] () String
identifier = satisfy $ \case
  Identifier ident -> Just ident
  _ -> Nothing

-- The parser for a number. Returns the number as an Expression.
number :: Parsec [Token] () Expression
number = satisfy $ \case
  NumberT value -> Just (Number value)
  _ -> Nothing

-- parens p parses p enclosed in parentheses, returning the value of p.
parens :: ParsecT [Token] () Identity a -> ParsecT [Token] () Identity a
parens = between (symbol "(") (symbol ")")

-- The expression parser.
expression :: ParsecT [Token] () Identity Expression
expression = buildExpressionParser table term

-- The parser for terms in an expression.
term :: ParsecT [Token] () Identity Expression
term = parens expression <|>
       number <|>
       call

-- The parser for lists. It tries to apply the given parser until it fails, with
-- each application of the parser separated by a comma. The list fails if there
-- isn't at least one element.
list :: ParsecT [Token] () Identity a -> ParsecT [Token] () Identity [a]
list parser = (:) <$> parser <*> option [] (symbol "," >> list parser)

-- The parser for a function call.
call :: ParsecT [Token] () Identity Expression
call = do
  name <- identifier
  arguments <- option [] (parens $ list expression)
  return (Call name arguments)

-- The parser for a function binding.
binding :: ParsecT [Token] () Identity Statement
binding = do
  name <- identifier
  parameters <- option [] (parens $ list identifier)
  symbol "="
  Binding name parameters <$> expression

-- The parser for a statement.
statement :: ParsecT [Token] () Identity Statement
statement =
  try binding <|>
  Expression <$> expression

-- The parser for a complete stream of input.
input :: ParsecT [Token] () Identity Statement
input = do
  s <- statement
  s <$ eof

-- Parses a string of math and returns either a ParseError (Left) or a Statement
-- that is the result of parsing the string (Right).
parse :: String -> Either ParseError Statement
parse string =
  case tokenize "" string of
    Left err -> Left err
    Right tokens -> Text.Parsec.Prim.parse input "" $ withExplicitMult tokens

-- Replaces all cases of implicit multiplication in the token list with explicit
-- multiplication by inserting multiplication operators.
withExplicitMult :: [Token] -> [Token]
withExplicitMult =
  intersperseWhen isImplicitMult $ Operator "*"
  where
    isImplicitMult :: (Token, Token) -> Bool
    isImplicitMult = \case
      (Identifier _, Identifier _) -> True
      (NumberT _, Identifier _) -> True
      (NumberT _, Symbol "(") -> True
      (Symbol ")", Identifier _) -> True
      (Symbol ")", NumberT _) -> True
      (Symbol ")", Symbol "(") -> True
      -- The parser can't tell if this is multiplication or a function call
      -- since it depends on the environment.
      (Identifier _, Symbol "(") -> False
      _ -> False
