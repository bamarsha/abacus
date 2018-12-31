module Calculator.UI.Web.MathML (fromStatement) where

import Calculator.AST
import Calculator.Utils
import Data.List (intersperse)
import Graphics.UI.Threepenny.Core hiding (element)

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

-- Transforms an expression into a MathML fragment.
fromExpression :: Maybe Operator -> Maybe Side -> Expression -> [UI Element]
fromExpression parent side exp =
  case exp of
    Raise base exponent ->
      contextualParens
        parent side RaiseOperator
        [element "msup" #+
           (fromExpression (Just RaiseOperator) (Just LeftSide) base ++
            fromExpression (Just RaiseOperator) (Just RightSide) exponent)]
    Negate e1 ->
      [operator "-"] ++ fromExpression (Just NegateOperator) Nothing e1
    Multiply e1 e2 ->
      contextualParens
        parent side MultiplyOperator
        (fromExpression (Just MultiplyOperator) (Just LeftSide) e1 ++
         [operator "⋅"] ++
         fromExpression (Just MultiplyOperator) (Just RightSide) e2)
    Divide e1 e2 ->
      contextualParens
        parent side DivideOperator
        [element "mfrac" #+
           [element "mrow" #+
              (fromExpression (Just DivideOperator) (Just LeftSide) e1),
            element "mrow" #+
              (fromExpression (Just DivideOperator) (Just RightSide) e2)]]
    Add e1 e2 ->
      contextualParens
        parent side AddOperator
        (fromExpression (Just AddOperator) (Just LeftSide) e1 ++
         [operator "+"] ++
         fromExpression (Just AddOperator) (Just RightSide) e2)
    Subtract e1 e2 ->
      contextualParens
        parent side SubtractOperator
        (fromExpression (Just SubtractOperator) (Just LeftSide) e1 ++
         [operator "-"] ++
         fromExpression (Just SubtractOperator) (Just RightSide) e2)
    Call name arguments ->
      if null arguments
      then [identifier name]
      else [identifier name, operator "("] ++
           intersperse (operator ",")
                       (arguments >>= fromExpression Nothing Nothing) ++
           [operator ")"]
    Number n -> [number n]

-- Transforms a statement into MathML.
fromStatement :: Statement -> UI Element
fromStatement (Expression e) =
  element "math" #+
    [element "mrow" #+
       fromExpression Nothing Nothing e]
fromStatement (Binding name parameters e) =
  element "math" #+
    [element "mrow" #+
       (fromExpression Nothing Nothing
                       (Call name $ map (\p -> Call p []) parameters) ++
        [operator "="] ++
        fromExpression Nothing Nothing e)]

-- Creates a MathML element with the given name.
element :: String -> UI Element
element = mkElementNamespace (Just "http://www.w3.org/1998/Math/MathML")

-- Creates a MathML operator element with the given symbol.
operator :: String -> UI Element
operator symbol = element "mo" # set text symbol

-- Creates a MathML identifier element with the given name.
identifier :: String -> UI Element
identifier name = element "mi" # set text name

-- Creates a MathML number element with the given value.
number :: Double -> UI Element
number value = element "mn" # set text (showFloat value)

-- Wraps a list of MathML elements with parentheses.
parens :: [UI Element] -> [UI Element]
parens elements =
  [element "mrow" #+
     ([operator "("] ++ elements ++ [operator ")"])]

-- Wraps a list of MathML elements with parentheses only if they are necessary
-- from context.
contextualParens :: Maybe Operator -> Maybe Side -> Operator -> [UI Element]
                 -> [UI Element]
contextualParens parent side child =
  if needsParens parent side child then parens else id
  where
    needsParens Nothing _ _ = False
    needsParens (Just parent) side child =
      (precedence parent > precedence child ||
       precedence parent == precedence child && associativity parent /= side) &&
      not (parent == child && commutative parent)
