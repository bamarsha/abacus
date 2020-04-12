module Abacus.Core.AST
    ( Expression(..)
    , Statement(..)
    ) where

-- A math expression.
data Expression
    = Number Double
    | Call String [Expression]

-- A math statement.
data Statement
    = Expression Expression
    | Binding String [String] Expression
