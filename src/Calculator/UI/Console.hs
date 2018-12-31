module Calculator.UI.Console (main) where

import Calculator.Interpreter
import Calculator.Utils
import System.Console.Haskeline

-- Runs a read-eval-print loop for the calculator.
main :: IO ()
main = runInputT defaultSettings (repl empty)

-- The read-eval-print loop.
repl :: Environment -> InputT IO ()
repl env = do
  maybeLine <- getInputLine "= "
  case maybeLine of
    Nothing -> return ()
    Just line ->
      case evalString env line of
        Left parseError -> outputStrLn (show parseError) >> repl env
        Right (Left evalError) -> outputStrLn evalError >> repl env
        Right (Right (env', Nothing)) -> repl env'
        Right (Right (env', Just result)) -> outputStrLn (showFloat result) >>
                                             repl env'
