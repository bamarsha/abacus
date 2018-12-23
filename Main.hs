module Main (main) where

import Parser
import System.Console.Haskeline

-- Runs a read-eval-print-loop for the calculator.
main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      maybeLine <- getInputLine "= "
      case maybeLine of
        Nothing -> return ()
        Just line -> (case parseMath line of
                        Left error -> outputStrLn (show error)
                        Right result -> outputStrLn (show result)) >>
                     loop
