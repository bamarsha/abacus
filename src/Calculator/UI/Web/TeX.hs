{-# LANGUAGE LambdaCase #-}

module Calculator.UI.Web.TeX (fromStatement) where

import Calculator.AST (Expression (Call, Number),
                       Statement (Binding, Expression))
import Calculator.Utils (showFloat)
import Data.List (intercalate)
import Text.Printf (printf)

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
  Call "^" [base, power] -> binary (Just Exponent) "%s^{%s}" base power
  Call "neg" [x] -> unary (Just Negative) "-%s" x
  Call "*" [x, y] -> binary (Just Times) "%s \\cdot %s" x y
  Call "/" [num, denom] -> binary (Just Division) "\\frac{%s}{%s}" num denom
  Call "+" [x, y] -> binary (Just Plus) "%s + %s" x y
  Call "-" [x, y] -> binary (Just Minus) "%s - %s" x y
  Call "pi" [] -> "\\pi"
  Call "sin" [x] -> unary Nothing "\\sin(%s)" x
  Call "cos" [x] -> unary Nothing "\\cos(%s)" x
  Call "sqrt" [x] -> unary Nothing "\\sqrt{%s}" x
  Call "cbrt" [x] -> unary Nothing "\\sqrt[3]{%s}" x
  Call "root" [x, k] -> binary Nothing "\\sqrt[%s]{%s}" k x
  Call "ln" [x] -> unary Nothing "\\ln(%s)" x
  Call "log" [b, x] -> binary Nothing "\\log_{%s}(%s)" b x
  Call "log2" [x] -> unary Nothing "\\log_2(%s)" x
  Call "log10" [x] -> unary Nothing "\\log_{10}(%s)" x
  Call name args ->
    let args' = intercalate "," (map (fromExpression Nothing Nothing) args)
    in identifier name ++ (if null args' then "" else parens args')
  Number value -> showFloat value
  where
    unary op format x = printf format (fromExpression op Nothing x)
    binary op format x y =
      let x' = fromExpression op (Just LeftSide) x
          y' = fromExpression op (Just RightSide) y
      in contextualParens parent side op (printf format x' y')

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
  if length ident > 1
  then "\\mathrm{" ++ ident ++ "}"
  else ident

-- Wraps a string with parentheses.
parens :: String -> String
parens s = "(" ++ s ++ ")"

-- Wraps a TeX expression with parentheses only if they are necessary from
-- context.
contextualParens :: Maybe Operator -> Maybe Side -> Maybe Operator -> String
                 -> String
contextualParens (Just parent) side (Just child) =
  if needsParens then parens else id
  where
    needsParens :: Bool
    needsParens =
      (precedence parent > precedence child ||
       precedence parent == precedence child && associativity parent /= side) &&
      not (parent == child && commutative parent)
contextualParens _ _ _ = id
