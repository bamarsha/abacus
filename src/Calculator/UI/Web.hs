{-# LANGUAGE RecursiveDo #-}

module Calculator.UI.Web (main) where

import Calculator.AST (Statement)
import Calculator.Interpreter (Environment, defaultEnv, evalStatement)
import Calculator.Parser (parse)
import Calculator.UI.Web.TeX (fromStatement)
import Calculator.UI.Web.Utils (addScript, alert, value')
import Calculator.Utils (showFloat)

import Control.Monad (void)
import Data.Either.Combinators (leftToMaybe, rightToMaybe)
import Data.Maybe (listToMaybe)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core (Element, JSFunction, UI, Window, (#), (#+),
                                    (#.), (<@))

import System.Environment (getArgs)

-- The result of evaluating the calculator's input.
data Result = Result { _inputStatement :: Statement,
                       _resultValue :: Maybe Double,
                       resultEnv :: Environment }

-- Starts the GUI for the calculator.
main :: IO ()
main = do
  args <- getArgs
  let port = read <$> listToMaybe args
  UI.startGUI UI.defaultConfig { UI.jsPort = port,
                                 UI.jsStatic = Just ".",
                                 UI.jsWindowReloadOnDisconnect = False }
           setup

-- Sets up the main window.
setup :: Window -> UI ()
setup window = void $ mdo
  _ <- return window # UI.set UI.title "Calculator"
  UI.addStyleSheet window "calculator.css"
  UI.addStyleSheet window "../node_modules/katex/dist/katex.min.css"
  addScript window "../node_modules/katex/dist/katex.min.js"

  history <- UI.dlist
  input <- UI.input

  body <- UI.getBody window
  _ <- UI.element body #+ [UI.element history, UI.element input]
  UI.setFocus input

  -- Evaluate the input when the Enter key is pressed.
  let eSubmit = UI.filterE (== 13) (UI.keydown input)
  let eEval = evalInput <$> ((,) <$> bEnv <*> bInput) <@ eSubmit

  -- Update the input as the user types, and clear it after evaluating if there
  -- wasn't an error.
  let eEvalSuccess = UI.filterJust $ rightToMaybe <$> eEval
  bInput <- UI.stepper "" $ UI.unionWith const
    ("" <$ eEvalSuccess)
    (UI.valueChange input)
  _ <- UI.element input # UI.sink value' bInput
  bEnv <- UI.stepper defaultEnv (resultEnv <$> eEvalSuccess)

  -- Update the history after evaluating.
  _ <- UI.onEvent eEvalSuccess $ addHistory body history

  -- Show an alert if an error happens.
  let eEvalError = UI.filterJust $ leftToMaybe <$> eEval
  UI.onEvent eEvalError $ UI.runFunction . alert

-- Evaluates the input with the current environment and returns either an error
-- message (Left) or the result (Right).
evalInput :: (Environment, String) -> Either String Result
evalInput (env, input) =
  case parse input of
    Left err -> Left (show err)
    Right statement -> case evalStatement env statement of
      Left err -> Left err
      Right (env', value) -> Right (Result statement value env')

-- Adds a result to the history.
addHistory :: Element -> Element -> Result -> UI ()
addHistory body history (Result statement value _) = do
  input <- UI.dterm
  output <- UI.ddef
  UI.runFunction $ renderKatex (fromStatement statement) input
  UI.runFunction $ renderKatex (maybe "" showFloat value) output
  _ <- UI.element history #+
    [UI.div #. "item" #+ [UI.element input, UI.element output]]
  UI.scrollToBottom body

-- Renders a TeX string to the element using KaTeX.
renderKatex :: String -> Element -> JSFunction ()
renderKatex = UI.ffi "katex.render(%1, %2, { displayMode: true })"
