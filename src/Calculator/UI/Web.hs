{-# LANGUAGE RecursiveDo #-}

module Calculator.UI.Web (main) where

import Calculator.Interpreter
import Calculator.Parser
import Calculator.Utils
import Control.Monad
import Control.Monad.IO.Class
import Data.Either.Combinators
import Data.Maybe (listToMaybe)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Environment (getArgs)

-- The keycode for the Enter key.
enterKey = 13

-- Starts the GUI for the calculator.
main :: IO ()
main = do
  args <- getArgs
  let port = read <$> listToMaybe args
  startGUI defaultConfig { jsPort = port } setup

-- Sets up the main window.
setup :: Window -> UI ()
setup window = void $ mdo
  pure window # set UI.title "Calculator"

  history <- UI.dlist
  input <- UI.input

  body <- getBody window
  element body #+
    [element history,
     UI.div #+ [element input]]
  UI.setFocus input

  -- Evaluate the input when the Enter key is pressed.
  let eSubmit = filterE (== enterKey) (UI.keydown input)
  eEval <- accumE (Calculator.Interpreter.empty, Right Nothing) $
    updateResult <$> (bInput <@ eSubmit)

  -- Update the input as the user types, and clear it after evaluating if there
  -- wasn't an error.
  let eEvalValue = filterJust $ (rightToMaybe . snd) <$> eEval
  bInput <- stepper "" $ unionWith const
    ("" <$ eEvalValue)
    (UI.valueChange input)
  element input # sink value' bInput

  -- Update the history after evaluating.
  onEvent ((,) <$> bInput <@> eEvalValue) $ addHistory body history

  -- Show an alert if an error happens.
  let eEvalError = filterJust $ (leftToMaybe . snd) <$> eEval
  onEvent eEvalError $ runFunction . alert

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

-- Adds a result to the history.
addHistory :: Element -> Element -> (String, Maybe Double) -> UI ()
addHistory body history (input, value) = do
  element history #+
    [UI.dterm # set text input,
     UI.ddef # set text (maybe "" showFloat value)]
  UI.scrollToBottom body

-- Displays an alert dialog with a message.
alert :: String -> JSFunction ()
alert = ffi "alert(%1)"

-- A version of the value attribute that only sets itself if the new value is
-- not equal to the current value. This keeps the cursor from moving to the end
-- of an input box in some browsers if the input box is "controlled" (i.e., it
-- has a behavior that updates with valueChange, and a sink that updates the
-- value with the behavior).
value' :: Attr Element String
value' = mkReadWriteAttr get set
  where
    get = get' value
    set v el = runFunction $ ffi "if ($(%1).val() != %2) $(%1).val(%2)" el v
