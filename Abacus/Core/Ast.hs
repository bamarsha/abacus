module Abacus.Core.Ast
    ( Expression(..)
    , Statement(..)
    ) where

-- A math expression.
data Expression
    = Number Rational
    | Call String [Expression]
    deriving Show

-- A math statement.
data Statement
    = Expression Expression
    | Binding String [String] Expression
    deriving Show
