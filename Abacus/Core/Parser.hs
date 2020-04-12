{-# LANGUAGE LambdaCase #-}

module Abacus.Core.Parser
    ( parseStatement
    ) where

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Expr

import Abacus.Core.AST
import Abacus.Core.Tokenizer
import Abacus.Core.Utils

-- The table of math operators.
table :: OperatorTable [Token] () Identity Expression
table =
    [ [ Infix negativeExponent AssocRight
      , Infix (operator "^" >> return (call2 "^")) AssocRight
      ]
    , [Prefix (operator "-" >> return (call1 "neg"))]
    , [ Infix (operator "*" >> return (call2 "*")) AssocLeft
      , Infix (operator "/" >> return (call2 "/")) AssocLeft
      ]
    , [ Infix (operator "+" >> return (call2 "+")) AssocLeft
      , Infix (operator "-" >> return (call2 "-")) AssocLeft
      ]
    ]
  where
    -- Special case for negative numbers in the exponent.
    negativeExponent = try $
        operator "^" >> operator "-" >> return (\base -> call2 "^" base . call1 "neg")

-- Returns a Call expression with one argument.
call1 :: String -> Expression -> Expression
call1 name x = Call name [x]

-- Returns a Call expression with two arguments.
call2 :: String -> Expression -> Expression -> Expression
call2 name x y = Call name [x, y]

-- The parser satisfyToken f succeeds for any token for which the given function f returns True.
-- Returns the token that is actually parsed.
satisfyToken :: (Token -> Maybe a) -> Parsec [Token] () a
satisfyToken = tokenPrim show (\pos _ _ -> pos)

-- The parser for the given operator op.
operator :: String -> Parsec [Token] () ()
operator op = satisfyToken $ \tok -> if tok == Operator op then Just () else Nothing

-- The parser for the given symbol sym.
symbol :: String -> Parsec [Token] () ()
symbol sym = satisfyToken $ \tok -> if tok == Symbol sym then Just () else Nothing

-- The parser for an identifier. Returns the identifier as a string.
identifier :: Parsec [Token] () String
identifier = satisfyToken $ \case
    Identifier ident -> Just ident
    _ -> Nothing

-- The parser for a number. Returns the number as an Expression.
number :: Parsec [Token] () Expression
number = satisfyToken $ \case
    NumberToken n -> Just $ Number n
    _ -> Nothing

-- parens p parses p enclosed in parentheses, returning the value of p.
parens :: ParsecT [Token] () Identity a -> ParsecT [Token] () Identity a
parens = between (symbol "(") (symbol ")")

-- The expression parser.
expression :: ParsecT [Token] () Identity Expression
expression = buildExpressionParser table term

-- The parser for terms in an expression.
term :: ParsecT [Token] () Identity Expression
term = parens expression <|> number <|> call

-- The parser for lists. It tries to apply the given parser until it fails, with each application of
-- the parser separated by a comma. The list fails if there isn't at least one element.
list :: ParsecT [Token] () Identity a -> ParsecT [Token] () Identity [a]
list parser = (:) <$> parser <*> option [] (symbol "," >> list parser)

-- The parser for a function call.
call :: ParsecT [Token] () Identity Expression
call = do
    name <- identifier
    args <- option [] (parens $ list expression)
    return $ Call name args

-- The parser for a function binding.
binding :: ParsecT [Token] () Identity Statement
binding = do
    name <- identifier
    params <- option [] (parens $ list identifier)
    symbol "="
    Binding name params <$> expression

-- The parser for a statement.
statement :: ParsecT [Token] () Identity Statement
statement = try binding <|> Expression <$> expression

-- The parser for a complete stream of input.
input :: ParsecT [Token] () Identity Statement
input = do
    s <- statement
    s <$ eof

-- Parses a statement and returns either a ParseError (Left) or the parsed Statement (Right).
parseStatement :: String -> Either ParseError Statement
parseStatement str = case tokenize "" str of
    Left err -> Left err
    Right toks -> parse input "" $ explicitMultiplication toks

-- Replaces all cases of implicit multiplication in the token list with explicit multiplication by
-- inserting multiplication operators.
explicitMultiplication :: [Token] -> [Token]
explicitMultiplication = intersperseWhen isImplicitMultiplication $ Operator "*"
  where
    isImplicitMultiplication :: (Token, Token) -> Bool
    isImplicitMultiplication = \case
        (Identifier _, Identifier _) -> True
        (NumberToken _, Identifier _) -> True
        (NumberToken _, Symbol "(") -> True
        (Symbol ")", Identifier _) -> True
        (Symbol ")", NumberToken _) -> True
        (Symbol ")", Symbol "(") -> True
        -- The parser can't tell if this is multiplication or a function call since it depends on
        -- the environment.
        (Identifier _, Symbol "(") -> False
        _ -> False
