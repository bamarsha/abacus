{-# LANGUAGE LambdaCase #-}

module Calculator.Interpreter (Environment, defaultEnv, evalString,
                               evalStatement) where

import Calculator.AST (Expression (Call, Number),
                       Statement (Binding, Expression))
import Calculator.Parser (parse)
import Data.Maybe (isJust)
import Text.Parsec.Error (ParseError)

-- Maps names to functions that can be called by other expressions.
newtype Environment = Environment { list :: [(String, Function)] }

-- A function that can be called from the environment.
data Function
  -- Closures wrap an expression with the environment it had when it was defined
  -- and a list of parameter names to add to the environment when the function
  -- is called.
  = Closure Environment [String] Expression
  -- A native Haskell function with one parameter.
  | Native1 (Double -> Double)
  -- A native Haskell function with two parameters.
  | Native2 (Double -> Double -> Double)

-- An evaluation error.
type EvalError = String

-- The default environment.
defaultEnv :: Environment
defaultEnv =
  Environment
    [("^", Native2 (**)),
     ("neg", Native1 negate),
     ("*", Native2 (*)),
     ("/", Native2 (/)),
     ("+", Native2 (+)),
     ("-", Native2 (-)),
     ("pi", constant pi),
     ("e", constant (exp 1)),
     ("sin", Native1 sin),
     ("cos", Native1 cos),
     ("tan", Native1 tan),
     ("sqrt", Native1 sqrt),
     ("cbrt", function["x"] $
        Call "root" [Call "x" [], Number 3.0]),
     ("root", function ["x", "k"] $
        Call "^" [Call "x" [], Call "/" [Number 1.0, Call "k" []]]),
     ("ln", Native1 log),
     ("log", function ["b", "x"] $
        Call "/" [Call "ln" [Call "x" []], Call "ln" [Call "b" []]]),
     ("log2", function ["x"] (Call "log" [Number 2.0, Call "x" []])),
     ("log10", function ["x"] (Call "log" [Number 10.0, Call "x" []]))]

--- Returns a closure for a constant with an empty environment.
constant :: Double -> Function
constant = Closure (Environment []) [] . Number

-- Returns a closure for a function with the default environment.
function :: [String] -> Expression -> Function
function = Closure defaultEnv

-- Evaluates an expression with the given environment.
evalExpression :: Environment -> Expression -> Either EvalError Double
evalExpression env = \case
  Number n -> return n
  Call name args ->
    case (args, lookup name (list env)) of
      (_, Nothing) -> Left ("undefined function or variable " ++ name)
      (_, Just (Closure env' params e))
        | length args == 1 && null params -> do
            -- Treat this as implicit multiplication.
            v1 <- evalExpression env' e
            v2 <- evalExpression env (head args)
            Right (v1 * v2)
        | length args == length params -> do
            args' <- mapM (evalExpression env) args
            let env'' = zip params (map constant args') ++ list env'
            evalExpression (Environment env'') e
      ([x], Just (Native1 f)) -> evalExpression env x >>= Right . f
      ([x, y], Just (Native2 f)) -> do
        x' <- evalExpression env x
        y' <- evalExpression env y
        Right (f x' y')
      _ -> Left ("wrong number of arguments for function " ++ name)

-- Evaluates a statement with the given environment. Returns the result, if any,
-- and the new environment.
evalStatement :: Environment -> Statement
              -> Either EvalError (Environment, Maybe Double)
evalStatement env (Expression e) = do
  result <- evalExpression env e
  Right (env, Just result)
evalStatement env (Binding name params e)
  | isJust $ lookup name (list defaultEnv) =
      Left ("can't redefine built-in function or variable " ++ name)
  | null params = do
      result <- evalExpression env e
      Right (Environment $ (name, constant result) : list env, Just result)
  | otherwise =
      Right (Environment $ (name, Closure env params e) : list env, Nothing)

-- Evaluates a string with the given environment. Returns the result, if any,
-- and the new environment.
evalString :: Environment -> String
           -> Either ParseError (Either EvalError (Environment, Maybe Double))
evalString env str =
  case parse str of
    Left err -> Left err
    Right statement -> Right (evalStatement env statement)
