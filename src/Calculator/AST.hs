module Calculator.AST (Expression (..), Statement (..)) where

-- A math expression.
data Expression = Number Double
                | Call String [Expression]
  deriving Show

-- A math statement.
data Statement = Expression Expression
               | Binding String [String] Expression
  deriving Show
