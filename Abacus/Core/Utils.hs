module Abacus.Core.Utils
    ( showFloat
    , intersperseWhen
    , replaceSublist
    )
where

import Data.Text.Format (shortest)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder (toLazyText)

-- Shows a float without trailing zeroes.
showFloat :: Double -> String
showFloat = unpack . toLazyText . shortest

-- intersperseWhen f sep xs inserts sep between each pair of elements in xs only
-- when f returns True for that pair.
intersperseWhen :: ((a, a) -> Bool) -> a -> [a] -> [a]
intersperseWhen _ _ [] = []
intersperseWhen f sep (x1 : x2 : xs) =
    if f (x1, x2)
        then x1 : sep : intersperseWhen f sep (x2 : xs)
        else x1 : intersperseWhen f sep (x2 : xs)
intersperseWhen _ _ [x] = [x]

-- Replaces the sublist from start to end in xs with ys.
replaceSublist :: Int -> Int -> [a] -> [a] -> [a]
replaceSublist start end ys xs =
    let left = take start xs
        right = drop end xs
    in  left ++ ys ++ right
