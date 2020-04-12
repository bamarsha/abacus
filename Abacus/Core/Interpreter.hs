{-# LANGUAGE LambdaCase #-}

module Abacus.Core.Interpreter
    ( Environment
    , InterpreterError(..)
    , InterpreterResult
    , defaultEnv
    , evalString
    , evalStatement
    ) where

import Data.Either.Combinators
import Data.List.Extra
import Data.Maybe

import Abacus.Core.AST
import Abacus.Core.Parser

-- Maps names to functions that can be called by other expressions.
newtype Environment = Environment { unEnvironment :: [(String, Function)] }

-- A function that can be called from the environment.
data Function
    -- Closures wrap an expression with the environment it had when it was defined and a list of
    -- parameter names to add to the environment when the function is called.
    = Closure Environment [String] Expression
    -- A native Haskell function with one parameter.
    | Native1 (Double -> Double)
    -- A native Haskell function with two parameters.
    | Native2 (Double -> Double -> Double)

-- An interpreter error.
data InterpreterError
    = EvalError String
    | ParseError String

instance Show InterpreterError where
    show (EvalError message) = "Evaluation Error: " ++ message
    show (ParseError message) = "Parse Error " ++ replace "\n" " " message

-- An interpreter result.
type InterpreterResult = Either InterpreterError (Environment, Maybe Double)

-- The default environment.
defaultEnv :: Environment
defaultEnv = Environment
    [ ("^", Native2 (**))
    , ("neg", Native1 negate)
    , ("*", Native2 (*))
    , ("/", Native2 (/))
    , ("+", Native2 (+))
    , ("-", Native2 (-))
    , ("pi", constant pi)
    , ("e", constant (exp 1))
    , ("sin", Native1 sin)
    , ("cos", Native1 cos)
    , ("tan", Native1 tan)
    , ("sqrt", Native1 sqrt)
    , ("cbrt", function ["x"] $ Call "root" [Call "x" [], Number 3.0])
    , ("root", function ["x", "k"] $ Call "^" [Call "x" [], Call "/" [Number 1.0, Call "k" []]])
    , ("ln", Native1 log)
    , ("log", function ["b", "x"] $ Call "/" [Call "ln" [Call "x" []], Call "ln" [Call "b" []]])
    , ("log2", function ["x"] $ Call "log" [Number 2.0, Call "x" []])
    , ("log10", function ["x"] $ Call "log" [Number 10.0, Call "x" []])
    ]

-- A constant function.
constant :: Double -> Function
constant = Closure (Environment []) [] . Number

-- A function enclosed by the default environment.
function :: [String] -> Expression -> Function
function = Closure defaultEnv

-- Evaluates an expression in the given environment.
evalExpression :: Environment -> Expression -> Either InterpreterError Double
evalExpression env = \case
    Number n -> return n
    Call name args -> case (args, lookup name $ unEnvironment env) of
        (_, Nothing) -> Left $ EvalError $ "undefined function or variable " ++ name
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
        _ -> Left $ EvalError $ "wrong number of arguments for function " ++ name

-- Evaluates a statement with the given environment.
evalStatement :: Environment -> Statement -> InterpreterResult
evalStatement env (Expression expr) = do
    result <- evalExpression env expr
    Right (Environment $ ("ans", constant result) : unEnvironment env, Just result)
evalStatement env (Binding name params expr)
    | isJust $ lookup name (unEnvironment defaultEnv) =
        Left $ EvalError $ "can't redefine built-in function or variable " ++ name
    | null params = do
        result <- evalExpression env expr
        Right (Environment $ (name, constant result) : unEnvironment env, Just result)
    | otherwise =
        Right (Environment $ (name, Closure env params expr) : unEnvironment env, Nothing)

-- Evaluates a string with the given environment.
evalString :: Environment -> String -> InterpreterResult
evalString env str = do
    statement <- mapLeft (ParseError . show) $ parseStatement str
    evalStatement env statement
