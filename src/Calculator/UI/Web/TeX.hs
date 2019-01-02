{-# LANGUAGE LambdaCase #-}

module Calculator.UI.Web.TeX (fromStatement) where

import Calculator.AST
  (Expression (Add, Call, Divide, Multiply, Negate, Number, Raise, Subtract),
   Statement (Binding, Expression))
import Calculator.Utils (showFloat)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

-- A math operator corresponding to a node in the AST.
data Operator = Exponent
              | Negative
              | Times
              | Division
              | Plus
              | Minus
  deriving Eq

-- The sides of an infix operator.
data Side = LeftSide | RightSide
  deriving Eq

-- A list of special names that have TeX control sequences.
specialNames :: [(String, String)]
specialNames =
  [("cos", "\\cos"),
   ("pi", "\\pi"),
   ("sin", "\\sin")]

-- The precedence of an operator.
precedence :: Operator -> Int
precedence Exponent = 3
precedence Negative = 2
precedence Times = 1
precedence Division = 1
precedence Plus = 0
precedence Minus = 0

-- Returns Just the associativity of an operator if the operator is binary, or
-- Nothing if the operator is unary.
associativity :: Operator -> Maybe Side
associativity Exponent = Just RightSide
associativity Negative = Nothing
associativity _ = Just LeftSide

-- Returns True if the operator is commutative or False otherwise.
commutative :: Operator -> Bool
commutative Times = True
commutative Plus = True
commutative _ = False

-- Transforms an expression into TeX.
fromExpression :: Maybe Operator -> Maybe Side -> Expression -> String
fromExpression parent side = \case
  Raise base power ->
    let base' = fromExpression (Just Exponent) (Just LeftSide) base
        power' = fromExpression (Just Exponent) (Just RightSide) power
    in contextualParens parent side Exponent (base' ++ "^{" ++ power' ++ "}")
  Negate e -> "-" ++ fromExpression (Just Negative) Nothing e
  Multiply e1 e2 ->
    let s1 = fromExpression (Just Times) (Just LeftSide) e1
        s2 = fromExpression (Just Times) (Just RightSide) e2
    in contextualParens parent side Times (s1 ++ "\\cdot " ++ s2)
  Divide num denom ->
    let num' = fromExpression (Just Division) (Just LeftSide) num
        denom' = fromExpression (Just Division) (Just RightSide) denom
    in contextualParens parent side Division
                        ("\\frac{" ++ num' ++ "}{" ++ denom' ++ "}")
  Add e1 e2 ->
    let s1 = fromExpression (Just Plus) (Just LeftSide) e1
        s2 = fromExpression (Just Plus) (Just RightSide) e2
    in contextualParens parent side Plus (s1 ++ "+" ++ s2)
  Subtract e1 e2 ->
    let s1 = fromExpression (Just Minus) (Just LeftSide) e1
        s2 = fromExpression (Just Minus) (Just RightSide) e2
    in contextualParens parent side Minus (s1 ++ "-" ++ s2)
  Call name args ->
    let args' = intercalate "," (map (fromExpression Nothing Nothing) args)
    in identifier name ++ (if null args' then "" else parens args')
  Number value -> showFloat value

-- Transforms a statement into TeX.
fromStatement :: Statement -> String
fromStatement (Expression e) = fromExpression Nothing Nothing e
fromStatement (Binding name params e) =
  fromExpression Nothing Nothing (Call name $ map (\p -> Call p []) params) ++
  "=" ++
  fromExpression Nothing Nothing e

-- Formats an identifier as a TeX string.
identifier :: String -> String
identifier ident =
  fromMaybe (if length ident > 1
             then "\\mathrm{" ++ ident ++ "}"
             else ident)
            (lookup ident specialNames)

-- Wraps a string with parentheses.
parens :: String -> String
parens s = "(" ++ s ++ ")"

-- Wraps a TeX expression with parentheses only if they are necessary from
-- context.
contextualParens :: Maybe Operator -> Maybe Side -> Operator -> String -> String
contextualParens Nothing _ _ = id
contextualParens (Just parent) side child =
  if needsParens then parens else id
  where
    needsParens :: Bool
    needsParens =
      (precedence parent > precedence child ||
       precedence parent == precedence child && associativity parent /= side) &&
      not (parent == child && commutative parent)
