module Calculator.Console.Main (main) where

import Calculator.Interpreter
import Calculator.Utils
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
        Left parseError -> outputStrLn (show parseError) >> loop env
        Right (Left evalError) -> outputStrLn evalError >> loop env
        Right (Right (env', Nothing)) -> loop env'
        Right (Right (env', Just result)) -> outputStrLn (showFloat result) >>
                                             loop env'
