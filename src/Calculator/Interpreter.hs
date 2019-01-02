{-# LANGUAGE LambdaCase #-}

module Calculator.Interpreter (Environment, defaultEnv, evalString,
                               evalStatement) where

import Calculator.AST
  (Expression (Add, Call, Divide, Multiply, Negate, Number, Raise, Subtract),
   Statement (Binding, Expression))
import Calculator.Parser (parse)
import Data.Maybe (isJust)
import Text.Parsec.Error (ParseError)

-- Maps names to functions that can be called by other expressions.
type Environment = [(String, Function)]

-- A function that can be called from the environment.
data Function
  -- Closures wrap an expression with the environment it had when it was defined
  -- and a list of parameter names to add to the environment when the function
  -- is called.
  = Closure Environment [String] Expression
  -- Native functions declare the number of arguments they accept, which are
  -- passed to a Haskell function that takes those arguments as a list.
  | Native Int ([Double] -> Double)

-- An evaluation error.
type EvalError = String

-- The default environment.
defaultEnv :: Environment
defaultEnv =
  [("cos", Native 1 $ \[x] -> cos x),
   ("pi", constant pi),
   ("sin", Native 1 $ \[x] -> sin x)]

--- Returns a closure for a constant function.
constant :: Double -> Function
constant = Closure [] [] . Number

-- Evaluates an expression with the given environment.
evalExpression :: Environment -> Expression -> Either EvalError Double
evalExpression env = \case
  Raise base power -> do
    base' <- evalExpression env base
    power' <- evalExpression env power
    Right (base' ** power')
  Negate e -> negate <$> evalExpression env e
  Multiply e1 e2 -> do
    v1 <- evalExpression env e1
    v2 <- evalExpression env e2
    Right (v1 * v2)
  Divide num denom -> do
    num' <- evalExpression env num
    denom' <- evalExpression env denom
    Right (num' / denom')
  Add e1 e2 -> do
    v1 <- evalExpression env e1
    v2 <- evalExpression env e2
    Right (v1 + v2)
  Subtract e1 e2 -> do
    v1 <- evalExpression env e1
    v2 <- evalExpression env e2
    Right (v1 - v2)
  Call name arguments -> call name arguments env
  Number n -> return n

-- Calls a function in the environment with the given name and arguments.
call :: String -> [Expression] -> Environment -> Either EvalError Double
call name arguments env =
  case lookup name env of
    Nothing -> Left ("undefined function or variable " ++ name)
    Just (Closure env' parameters e)
      | length arguments == 1 && null parameters -> do
          -- Treat this as implicit multiplication.
          v1 <- evalExpression env' e
          v2 <- evalExpression env (head arguments)
          Right (v1 * v2)
      | length arguments /= length parameters ->
          Left ("wrong number of arguments for function " ++ name)
      | otherwise -> do
          arguments' <- mapM (evalExpression env) arguments
          let env'' = zip parameters (map constant arguments') ++ env'
          evalExpression env'' e
    Just (Native arity f) ->
      if length arguments /= arity
      then Left ("wrong number of arguments for function " ++ name)
      else f <$> mapM (evalExpression env) arguments

-- Evaluates a statement with the given environment. Returns the result, if any,
-- and the new environment.
evalStatement :: Environment -> Statement
              -> Either EvalError (Environment, Maybe Double)
evalStatement env (Expression e) = do
  result <- evalExpression env e
  Right (env, Just result)
evalStatement env (Binding name parameters e)
  | isJust (lookup name defaultEnv) =
      Left ("can't redefine built-in function or variable " ++ name)
  | null parameters = do
      result <- evalExpression env e
      Right ((name, constant result) : env, Just result)
  | otherwise = Right ((name, Closure env parameters e) : env, Nothing)

-- Evaluates a string with the given environment. Returns the result, if any,
-- and the new environment.
evalString :: Environment -> String
           -> Either ParseError (Either EvalError (Environment, Maybe Double))
evalString env str =
  case parse str of
    Left err -> Left err
    Right statement -> Right (evalStatement env statement)
