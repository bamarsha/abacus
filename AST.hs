module AST where

-- A math expression.
data Expression = Raise Expression Expression
                | Negate Expression
                | Multiply Expression Expression
                | Divide Expression Expression
                | Add Expression Expression
                | Subtract Expression Expression
                | Call String [Expression]
                | Number Double
  deriving Show

-- A math statement.
data Statement = Expression Expression
               | Binding String [String] Expression
  deriving Show
