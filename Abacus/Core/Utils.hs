module Abacus.Core.Utils
    ( intersperseWhen
    , replaceSublist
    ) where

-- intersperseWhen f sep xs inserts sep between each pair of elements in xs only when f returns True
-- for that pair.
intersperseWhen :: ((a, a) -> Bool) -> a -> [a] -> [a]
intersperseWhen _ _ [] = []
intersperseWhen f sep (x1 : x2 : xs)
    | f (x1, x2) = x1 : sep : intersperseWhen f sep (x2 : xs)
    | otherwise  = x1 : intersperseWhen f sep (x2 : xs)
intersperseWhen _ _ [x] = [x]

-- Replaces the sublist from start to end in xs with ys.
replaceSublist :: Int -> Int -> [a] -> [a] -> [a]
replaceSublist start end ys xs = left ++ ys ++ right
  where
    left = take start xs
    right = drop end xs
