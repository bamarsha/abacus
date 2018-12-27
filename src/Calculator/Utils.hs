module Calculator.Utils (showFloat) where

import Data.Text.Format (shortest)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder (toLazyText)

-- Shows a float without trailing zeroes.
showFloat :: Double -> String
showFloat = unpack . toLazyText . shortest
