{-# LANGUAGE RecursiveDo #-}

module Calculator.Web.Main (main) where

import Calculator.Interpreter
import Calculator.Parser
import Calculator.Utils
import Control.Monad
import Control.Monad.IO.Class
import Data.Either.Combinators
import Data.IORef
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

-- The keycode for the Enter key.
enterKey = 13

-- Starts the GUI for the calculator.
main :: IO ()
main = startGUI defaultConfig setup

-- Sets up the main window.
setup :: Window -> UI ()
setup window = void $ mdo
  pure window # set UI.title "Calculator"

  input <- UI.input
  error <- UI.p
  getBody window
    #+ [UI.div #+ [element input],
        element error]

  -- Evaluate the input when the Enter key is pressed.
  let eSubmit = filterE (== enterKey) (UI.keydown input)
  eEval <- accumE (Calculator.Interpreter.empty, Right Nothing) $
    updateResult <$> (bInput <@ eSubmit)

  -- Update the input with the value of the result after evaluating (if there
  -- wasn't an error).
  let eEvalValue = filterJust $ (rightToMaybe . snd) <$> eEval
  bInput <- stepper "" $ unionWith const
    (maybe "" showFloat <$> eEvalValue)
    (UI.valueChange input)

  -- Show the last error message (if any).
  let eEvalError = (fromLeft "" . snd) <$> eEval
  bError <- stepper "" eEvalError

  element input # sink value bInput
  element error # sink text bError

-- Returns the result of evaluating the input using the environment from the
-- previous result. The result contains the new environment and either an error
-- message or the result value, if any.
updateResult :: String
             -> (Environment, Either String (Maybe Double))
             -> (Environment, Either String (Maybe Double))
updateResult input (env, _) =
  case eval env input of
    Left parseError -> (env, Left (show parseError))
    Right (Left evalError) -> (env, Left evalError)
    Right (Right (env', maybeValue)) -> (env', Right maybeValue)
