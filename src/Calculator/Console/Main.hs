module Calculator.Console.Main (main) where

import Calculator.AST
import Calculator.Interpreter
import Calculator.Parser
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
  maybe (return ()) parseLine maybeLine
  where
    parseLine :: String -> InputT IO ()
    parseLine line =
      case parse line of
        Left error -> outputStrLn (show error) >> loop env
        Right result -> evaluateLine result

    evaluateLine :: Statement -> InputT IO ()
    evaluateLine line =
      case evaluate env line of
        Left error -> outputStrLn error >> loop env
        Right (Just n, env') ->
          (outputStrLn $ unpack $ toLazyText $ shortest n) >>
          loop env'
        Right (Nothing, env') -> loop env'
