{-# LANGUAGE LambdaCase #-}

module Abacus.Interpreter.Parser
    ( parseStatement
    ) where

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Expr

import Abacus.Interpreter.Token
import Abacus.Interpreter.Utils

import qualified Abacus.Interpreter.Ast as Ast

-- The table of math operators.
table :: OperatorTable [Token] () Identity Ast.Expression
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
call1 :: String -> Ast.Expression -> Ast.Expression
call1 name x = Ast.Call name [x]

-- Returns a Call expression with two arguments.
call2 :: String -> Ast.Expression -> Ast.Expression -> Ast.Expression
call2 name x y = Ast.Call name [x, y]

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
number :: Parsec [Token] () Ast.Expression
number = satisfyToken $ \case
    Number n -> Just $ Ast.Number n
    _ -> Nothing

-- parens p parses p enclosed in parentheses, returning the value of p.
parens :: ParsecT [Token] () Identity a -> ParsecT [Token] () Identity a
parens = between (symbol "(") (symbol ")")

-- The expression parser.
expression :: ParsecT [Token] () Identity Ast.Expression
expression = buildExpressionParser table term

-- The parser for terms in an expression.
term :: ParsecT [Token] () Identity Ast.Expression
term = parens expression <|> number <|> call

-- The parser for lists. It tries to apply the given parser until it fails, with each application of
-- the parser separated by a comma. The list fails if there isn't at least one element.
list :: ParsecT [Token] () Identity a -> ParsecT [Token] () Identity [a]
list parser = (:) <$> parser <*> option [] (symbol "," >> list parser)

-- The parser for a function call.
call :: ParsecT [Token] () Identity Ast.Expression
call = do
    name <- identifier
    args <- option [] (parens $ list expression)
    return $ Ast.Call name args

-- The parser for a function binding.
binding :: ParsecT [Token] () Identity Ast.Statement
binding = do
    name <- identifier
    params <- option [] (parens $ list identifier)
    symbol "="
    Ast.Binding name params <$> expression

-- The parser for a statement.
statement :: ParsecT [Token] () Identity Ast.Statement
statement = try binding <|> Ast.Expression <$> expression

-- The parser for a complete stream of input.
input :: ParsecT [Token] () Identity Ast.Statement
input = do
    s <- statement
    s <$ eof

-- Parses a statement and returns either a ParseError (Left) or the parsed Statement (Right).
parseStatement :: String -> Either ParseError Ast.Statement
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
        (Number _, Identifier _) -> True
        (Number _, Symbol "(") -> True
        (Symbol ")", Identifier _) -> True
        (Symbol ")", Number _) -> True
        (Symbol ")", Symbol "(") -> True
        -- The parser can't tell if this is multiplication or a function call since it depends on
        -- the environment.
        (Identifier _, Symbol "(") -> False
        _ -> False
