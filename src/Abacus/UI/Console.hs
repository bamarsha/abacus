{-# LANGUAGE LambdaCase #-}

module Abacus.UI.Console
    ( main
    ) where

import Abacus.Interpreter (Environment, defaultEnv, evalString)
import Abacus.Utils (showFloat)
import System.Console.Haskeline
    ( InputT
    , defaultSettings
    , getInputLine
    , outputStrLn
    , runInputT
    )

-- Runs a read-eval-print loop for the calculator.
main :: IO ()
main = runInputT defaultSettings (repl defaultEnv)

-- The read-eval-print loop.
repl :: Environment -> InputT IO ()
repl env =
    getInputLine "= " >>= \case
        Just line ->
            case evalString env line of
                Left err -> outputStrLn (show err) >> repl env
                Right (env', Nothing) -> repl env'
                Right (env', Just result) ->
                    outputStrLn (showFloat result) >> repl env'
        Nothing -> return ()
