{-# LANGUAGE LambdaCase #-}

module Abacus.Interpreter.Eval
    ( Environment
    , EvalError(..)
    , EvalResult
    , defaultEnv
    , evalString
    , evalStatement
    ) where

import Data.Either.Combinators
import Data.List.Extra
import Data.Maybe
import Data.Ratio

import Abacus.Interpreter.Ast
import Abacus.Interpreter.Parser

-- Maps names to functions that can be called by other expressions.
newtype Environment = Environment { unEnvironment :: [(String, Function)] }

-- A function that can be called from the environment.
data Function
    -- Closures wrap an expression with the environment it had when it was defined and a list of
    -- parameter names to add to the environment when the function is called.
    = Closure Environment [String] Expression
    -- A native Haskell function with one parameter.
    | Native1 (Rational -> Rational)
    -- A native Haskell function with two parameters.
    | Native2 (Rational -> Rational -> Rational)

-- An evaluation error.
data EvalError
    = ParseError String
    | InterpretError String

instance Show EvalError where
    show (ParseError message) = "Parse Error " ++ replace "\n" " " message
    show (InterpretError message) = "Interpret Error: " ++ message

-- An evaluation result.
type EvalResult = Either EvalError (Environment, Maybe Rational)

-- The default environment.
defaultEnv :: Environment
defaultEnv = Environment
    [ ("^", Native2 power)
    , ("neg", Native1 negate)
    , ("*", Native2 (*))
    , ("/", Native2 (/))
    , ("+", Native2 (+))
    , ("-", Native2 (-))
    , ("pi", constant $ toRational pi)
    , ("e", constant $ toRational $ exp 1)
    , ("sin", Native1 $ toRational . sin . fromRational)
    , ("cos", Native1 $ toRational . cos . fromRational)
    , ("tan", Native1 $ toRational . tan . fromRational)
    , ("sqrt", Native1 $ toRational . sqrt . fromRational)
    , ("cbrt", function ["x"] $ Call "root" [Call "x" [], Number 3])
    , ("root", function ["x", "base"] $ Call "^" [Call "x" [], Call "/" [Number 1, Call "base" []]])
    , ("ln", Native1 $ toRational . log . fromRational)
    , ("log", function ["base", "x"] $ Call "/" [ Call "ln" [Call "x" []]
                                                , Call "ln" [Call "base" []] ])
    , ("log2", function ["x"] $ Call "log" [Number 2, Call "x" []])
    , ("log10", function ["x"] $ Call "log" [Number 10, Call "x" []])
    ]
  where
    power x y
        | denominator y == 1 = x ^^ numerator y
        | otherwise          = toRational $ fromRational x ** fromRational y

-- A constant function.
constant :: Rational -> Function
constant = Closure (Environment []) [] . Number

-- A function enclosed by the default environment.
function :: [String] -> Expression -> Function
function = Closure defaultEnv

-- Evaluates an expression in the given environment.
evalExpression :: Environment -> Expression -> Either EvalError Rational
evalExpression env = \case
    Number n -> return n
    Call name args -> case (args, lookup name $ unEnvironment env) of
        (_, Nothing) -> Left $ InterpretError $ "undefined function or variable " ++ name
        (_, Just (Closure closureEnv params expr))
            | length args == 1 && null params ->
                -- Treat this as implicit multiplication.
                (*) <$> evalExpression closureEnv expr <*> evalExpression env (head args)
            | length args == length params -> do
                args' <- mapM (evalExpression env) args
                let closureEnv' = zip params (map constant args') ++ unEnvironment closureEnv
                evalExpression (Environment closureEnv') expr
        ([x], Just (Native1 f)) -> f <$> evalExpression env x
        ([x, y], Just (Native2 f)) -> f <$> evalExpression env x <*> evalExpression env y
        _ -> Left $ InterpretError $ "wrong number of arguments for function " ++ name

-- Evaluates a statement with the given environment.
evalStatement :: Environment -> Statement -> EvalResult
evalStatement env (Expression expr) = do
    result <- evalExpression env expr
    Right (Environment $ ("ans", constant result) : unEnvironment env, Just result)
evalStatement env (Binding name params expr)
    | isJust $ lookup name (unEnvironment defaultEnv) =
        Left $ InterpretError $ "can't redefine built-in function or variable " ++ name
    | null params = do
        result <- evalExpression env expr
        Right (Environment $ (name, constant result) : unEnvironment env, Just result)
    | otherwise =
        Right (Environment $ (name, Closure env params expr) : unEnvironment env, Nothing)

-- Evaluates a string with the given environment.
evalString :: Environment -> String -> EvalResult
evalString env str = do
    statement <- mapLeft (ParseError . show) $ parseStatement str
    evalStatement env statement
