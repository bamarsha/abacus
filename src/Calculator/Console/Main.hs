module Calculator.Console.Main (main) where

import Calculator.Interpreter
import Data.Text.Format (shortest)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder (toLazyText)
import System.Console.Haskeline

-- Runs a read-eval-print loop for the calculator.
main :: IO ()
main = runInputT defaultSettings (loop empty)

-- The read-eval-print loop.
loop :: Environment -> InputT IO ()
loop env = do
  maybeLine <- getInputLine "= "
  case maybeLine of
    Nothing -> return ()
    Just line ->
      case eval env line of
        Left error -> outputStrLn (show error) >> loop env
        Right (Left error) -> outputStrLn error >> loop env
        Right (Right (env', Nothing)) -> loop env'
        Right (Right (env', Just result)) ->
          (outputStrLn $ unpack $ toLazyText $ shortest result) >> loop env'
