{-# LANGUAGE LambdaCase #-}

module Calculator.UI.Web.TeX (fromStatement) where

import Calculator.AST
import Calculator.Utils
import Data.List (intercalate)

-- A math operator corresponding to a node in the AST.
data Operator = RaiseOperator
              | NegateOperator
              | MultiplyOperator
              | DivideOperator
              | AddOperator
              | SubtractOperator
  deriving Eq

-- The sides of an infix operator.
data Side = LeftSide | RightSide
  deriving Eq

-- The precedence of an operator.
precedence :: Operator -> Int
precedence RaiseOperator = 3
precedence NegateOperator = 2
precedence MultiplyOperator = 1
precedence DivideOperator = 1
precedence AddOperator = 0
precedence SubtractOperator = 0

-- Returns Just the associativity of an operator if the operator is binary, or
-- Nothing if the operator is unary.
associativity :: Operator -> Maybe Side
associativity RaiseOperator = Just RightSide
associativity NegateOperator = Nothing
associativity _ = Just LeftSide

-- Returns True if the operator is commutative or False otherwise.
commutative :: Operator -> Bool
commutative MultiplyOperator = True
commutative AddOperator = True
commutative _ = False

-- Transforms an expression into TeX.
fromExpression :: Maybe Operator -> Maybe Side -> Expression -> String
fromExpression parent side = \case
  Raise base power ->
    let base' = fromExpression (Just RaiseOperator) (Just LeftSide) base
        power' = fromExpression (Just RaiseOperator) (Just RightSide) power
    in contextualParens parent side RaiseOperator
                        (base' ++ "^{" ++ power' ++ "}")
  Negate exp ->
    "-" ++ fromExpression (Just NegateOperator) Nothing exp
  Multiply exp1 exp2 ->
    let exp1' = fromExpression (Just MultiplyOperator) (Just LeftSide) exp1
        exp2' = fromExpression (Just MultiplyOperator) (Just RightSide) exp2
    in contextualParens parent side MultiplyOperator
                        (exp1' ++ "\\cdot " ++ exp2')
  Divide num denom ->
    let num' = fromExpression (Just DivideOperator) (Just LeftSide) num
        denom' = fromExpression (Just DivideOperator) (Just RightSide) denom
    in contextualParens parent side DivideOperator
                        ("\\frac{" ++ num' ++ "}{" ++ denom' ++ "}")
  Add exp1 exp2 ->
    let exp1' = fromExpression (Just AddOperator) (Just LeftSide) exp1
        exp2' = fromExpression (Just AddOperator) (Just RightSide) exp2
    in contextualParens parent side AddOperator
                        (exp1' ++ "+" ++ exp2')
  Subtract exp1 exp2 ->
    let exp1' = fromExpression (Just SubtractOperator) (Just LeftSide) exp1
        exp2' = fromExpression (Just SubtractOperator) (Just RightSide) exp2
    in contextualParens parent side SubtractOperator
                        (exp1' ++ "-" ++ exp2')
  Call name arguments ->
    if null arguments
    then name
    else name ++ parens (intercalate "," $
                         map (fromExpression Nothing Nothing) arguments)
  Number value -> showFloat value

-- Transforms a statement into TeX.
fromStatement :: Statement -> String
fromStatement (Expression exp) = fromExpression Nothing Nothing exp
fromStatement (Binding name parameters exp) =
  fromExpression Nothing Nothing
                 (Call name $ map (\p -> Call p []) parameters) ++
  "=" ++
  fromExpression Nothing Nothing exp

-- Wraps a TeX expression with parentheses.
parens :: String -> String
parens exp = "(" ++ exp ++ ")"

-- Wraps a TeX expression with parentheses only if they are necessary from
-- context.
contextualParens :: Maybe Operator -> Maybe Side -> Operator -> String -> String
contextualParens parent side child =
  if needsParens parent side child then parens else id
  where
    needsParens Nothing _ _ = False
    needsParens (Just parent) side child =
      (precedence parent > precedence child ||
       precedence parent == precedence child && associativity parent /= side) &&
      not (parent == child && commutative parent)
