{-# LANGUAGE LambdaCase #-}

module Abacus.UI.TeX
    ( fromStatement
    ) where

import Data.List
import Text.Printf

import Abacus.Core.AST
import Abacus.Core.Utils

-- A math operator corresponding to a node in the AST.
data Operator
    = Exponent
    | Negative
    | Times
    | Division
    | Plus
    | Minus
    deriving Eq

-- The sides of an infix operator.
data Side
    = LeftSide
    | RightSide
    deriving Eq

-- The precedence of an operator.
precedence :: Operator -> Int
precedence Exponent = 3
precedence Negative = 2
precedence Times = 1
precedence Division = 1
precedence Plus = 0
precedence Minus = 0

-- Returns Just the associativity of an operator if the operator is binary, or Nothing if the
-- operator is unary.
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
    Call "sin" [x] -> unary Nothing "\\sin \\left( %s \\right)" x
    Call "cos" [x] -> unary Nothing "\\cos \\left( %s \\right)" x
    Call "sqrt" [x] -> unary Nothing "\\sqrt{%s}" x
    Call "cbrt" [x] -> unary Nothing "\\sqrt[3]{%s}" x
    Call "root" [x, base] -> binary Nothing "\\sqrt[%s]{%s}" base x
    Call "ln" [x] -> unary Nothing "\\ln \\left( %s \\right)" x
    Call "log" [base, x] -> binary Nothing "\\log_{%s} \\left( %s \\right)" base x
    Call "log2" [x] -> unary Nothing "\\log_2 \\left( %s \\right)" x
    Call "log10" [x] -> unary Nothing "\\log_{10} \\left( %s \\right)" x
    Call name args ->
        let args' = intercalate "," $ map (fromExpression Nothing Nothing) args
        in identifier name ++ if null args' then "" else parens args'
    Number value -> showWithoutTrailingZero value
  where
    unary op format x = printf format $ fromExpression op Nothing x
    binary op format x y = contextualParens parent side op $ printf format x' y'
      where
        x' = fromExpression op (Just LeftSide) x
        y' = fromExpression op (Just RightSide) y

-- Transforms a statement into TeX.
fromStatement :: Statement -> String
fromStatement (Expression expr) = fromExpression Nothing Nothing expr
fromStatement (Binding name [] expr) =
    fromExpression Nothing Nothing (Call name [])
        ++ "\\leftarrow "
        ++ fromExpression Nothing Nothing expr
fromStatement (Binding name params expr) =
    fromExpression Nothing Nothing (Call name $ map (\param -> Call param []) params)
        ++ "="
        ++ fromExpression Nothing Nothing expr

-- Formats an identifier as a TeX string.
identifier :: String -> String
identifier ident
    | length ident > 1 = "\\mathrm{" ++ ident ++ "}"
    | otherwise        = ident

-- Wraps a string with parentheses.
parens :: String -> String
parens s = "\\left(" ++ s ++ "\\right)"

-- Wraps a TeX expression with parentheses only if they are necessary from context.
contextualParens :: Maybe Operator -> Maybe Side -> Maybe Operator -> String -> String
contextualParens (Just parent) side (Just child)
    | needsParens = parens
    | otherwise   = id
  where
    needsParens =
        (wrongPrecedence || wrongAssociativity) && not (parent == child && commutative parent)
    wrongPrecedence = precedence parent > precedence child
    wrongAssociativity = precedence parent == precedence child && associativity parent /= side
contextualParens _ _ _ = id
