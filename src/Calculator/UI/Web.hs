{-# LANGUAGE RecursiveDo #-}

module Calculator.UI.Web (main) where

import Calculator.AST
import Calculator.Interpreter
import Calculator.Parser
import Calculator.UI.Web.MathML
import Calculator.UI.Web.Utils
import Calculator.Utils
import Control.Monad (void)
import Data.Either.Combinators
import Data.Maybe (listToMaybe)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Environment (getArgs)

-- The result of evaluating the calculator's input.
data EvalResult = EvalResult { env :: Environment,
                               statement :: Statement,
                               value :: Maybe Double }

-- Starts the GUI for the calculator.
main :: IO ()
main = do
  args <- getArgs
  let port = read <$> listToMaybe args
  startGUI defaultConfig { jsPort = port, jsStatic = Just "." } setup

-- Sets up the main window.
setup :: Window -> UI ()
setup window = void $ mdo
  pure window # set UI.title "Calculator"
  UI.addStyleSheet window "mathml.css"
  UI.addStyleSheet window "calculator.css"

  history <- UI.dlist
  input <- UI.input

  body <- getBody window
  element body #+ [element history, element input]
  UI.setFocus input

  -- Evaluate the input when the Enter key is pressed.
  let eSubmit = filterE (== 13) (UI.keydown input)
  let eEval = evalInput <$> ((,) <$> bEnv <*> bInput) <@ eSubmit

  -- Update the input as the user types, and clear it after evaluating if there
  -- wasn't an error.
  let eEvalSuccess = filterJust $ rightToMaybe <$> eEval
  bInput <- stepper "" $ unionWith const
    ("" <$ eEvalSuccess)
    (UI.valueChange input)
  element input # sink value' bInput
  bEnv <- stepper Calculator.Interpreter.empty (env <$> eEvalSuccess)

  -- Update the history after evaluating.
  onEvent eEvalSuccess $ addHistory body history

  -- Show an alert if an error happens.
  let eEvalError = filterJust $ leftToMaybe <$> eEval
  onEvent eEvalError $ runFunction . alert

-- Evaluates the input with the current environment and returns either an error
-- message (Left) or the result (Right).
evalInput :: (Environment, String) -> Either String EvalResult
evalInput (env, input) =
  case parse input of
    Left error -> Left (show error)
    Right statement -> case evalStatement env statement of
      Left error -> Left error
      Right (env', value) -> Right (EvalResult env' statement value)

-- Adds a result to the history.
addHistory :: Element -> Element -> EvalResult -> UI ()
addHistory body history (EvalResult _ statement value) = do
  element history #+
    [UI.div #. "item" #+
       [UI.dterm #+ [fromStatement statement],
        UI.ddef # set text (maybe "" showFloat value)]]
  UI.scrollToBottom body
