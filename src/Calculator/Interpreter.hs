module Calculator.Interpreter (Environment, empty, evalString, evalStatement)
where

import Calculator.AST
import Calculator.Parser
import Control.Monad (sequence)
import Text.Parsec.Error (ParseError)

-- The context of an expression.
type Environment = [(String, Closure)]

-- A closure for a function and its environment.
data Closure = Closure Environment [String] Expression

-- An evaluation error.
type EvalError = String

-- The empty environment.
empty :: Environment
empty = []

-- Returns a closure for a constant function.
constant :: Double -> Closure
constant = Closure empty [] . Number

-- Evaluates an expression with the given environment.
evalExpression :: Environment -> Expression -> Either EvalError Double
evalExpression env exp =
  case exp of
    Raise base exponent -> do
      base' <- evalExpression env base
      exponent' <- evalExpression env exponent
      Right (base' ** exponent')
    Negate e -> evalExpression env e >>= (return . negate)
    Multiply e1 e2 -> do
      v1 <- evalExpression env e1
      v2 <- evalExpression env e2
      Right (v1 * v2)
    Divide e1 e2 -> do
      v1 <- evalExpression env e1
      v2 <- evalExpression env e2
      Right (v1 / v2)
    Add e1 e2 -> do
      v1 <- evalExpression env e1
      v2 <- evalExpression env e2
      Right (v1 + v2)
    Subtract e1 e2 -> do
      v1 <- evalExpression env e1
      v2 <- evalExpression env e2
      Right (v1 - v2)
    Call name arguments -> do
      case lookup name env of
        Nothing -> Left ("undefined function or variable " ++ name)
        Just (Closure env' parameters e) ->
          if length arguments == 1 && null parameters
          then
            -- Treat this as implicit multiplication.
            evalExpression env (Multiply e $ head arguments)
          else if (length arguments) /= (length parameters)
          then Left ("wrong number of arguments for function " ++ name)
          else do
            arguments' <- sequence (map (evalExpression env') arguments)
            let env'' = (zip parameters (map constant arguments')) ++ env'
            evalExpression env'' e
    Number n -> return n

-- Evaluates a statement with the given environment. Returns the result, if any,
-- and the new environment.
evalStatement :: Environment
              -> Statement
              -> Either EvalError (Environment, Maybe Double)
evalStatement env (Expression exp) = do
  result <- evalExpression env exp
  Right (env, Just result)
evalStatement env (Binding name parameters e) =
  if null parameters
  then do result <- evalExpression env e
          Right ((name, constant result) : env, Just result)
  else Right ((name, Closure env parameters e) : env, Nothing)

-- Evaluates a string with the given environment. Returns the result, if any,
-- and the new environment.
evalString :: Environment
           -> String
           -> Either ParseError (Either EvalError (Environment, Maybe Double))
evalString env str =
  case parse str of
    Left error -> Left error
    Right statement -> Right (evalStatement env statement)
