module Calculator.Interpreter (Environment, empty, evaluate) where

import Calculator.AST
import Control.Monad (sequence)

-- The context of an expression.
type Environment = [(String, Closure)]

-- A closure for a function and its environment.
data Closure = Closure Environment [String] Expression

-- An evaluation error.
type EvaluationError = String

-- The empty environment.
empty :: Environment
empty = []

-- Returns a closure for a constant function.
constant :: Double -> Closure
constant = Closure empty [] . Number

-- Evaluates an expression with the given environment.
evaluateE :: Environment -> Expression -> Either EvaluationError Double
evaluateE env e =
  case e of
    Raise base exponent -> do
      baseV <- evaluateE env base
      exponentV <- evaluateE env exponent
      Right (baseV ** exponentV)
    Negate e -> evaluateE env e >>= (return . negate)
    Multiply e1 e2 -> do
      v1 <- evaluateE env e1
      v2 <- evaluateE env e2
      Right (v1 * v2)
    Divide e1 e2 -> do
      v1 <- evaluateE env e1
      v2 <- evaluateE env e2
      Right (v1 / v2)
    Add e1 e2 -> do
      v1 <- evaluateE env e1
      v2 <- evaluateE env e2
      Right (v1 + v2)
    Subtract e1 e2 -> do
      v1 <- evaluateE env e1
      v2 <- evaluateE env e2
      Right (v1 - v2)
    Call name arguments -> do
      case lookup name env of
        Nothing -> Left ("undefined function or variable " ++ name)
        Just (Closure env parameters e) ->
          if (length arguments) /= (length parameters)
          then Left ("wrong number of arguments for function " ++ name)
          else do
            argumentsV <- sequence (map (evaluateE env) arguments)
            let env' = (zip parameters (map constant argumentsV)) ++ env
            evaluateE env' e
    Number n -> return n

-- Evaluates a statement with the given environment and returns the result, if
-- any, as well as the updated environment.
evaluate :: Environment
         -> Statement
         -> Either EvaluationError (Maybe Double, Environment)
evaluate env (Expression e) = do
  result <- evaluateE env e
  return (Just result, env)
evaluate env (Binding name parameters e) =
  if null parameters
  then do
    result <- evaluateE env e
    return (Just result, (name, constant result) : env)
  else return (Nothing, (name, Closure env parameters e) : env)
