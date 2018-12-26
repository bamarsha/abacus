module Calculator.Web.Main (main) where

import Calculator.Interpreter
import Calculator.Parser
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Text.Format (shortest)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

-- The keycode for the Enter key.
enterKey = 13

-- Starts the GUI for the calculator.
main :: IO ()
main = do
  startGUI defaultConfig setup

-- Sets up the main window.
setup :: Window -> UI ()
setup window = do
  pure window # set UI.title "Calculator"

  input <- UI.input
  error <- UI.p
  getBody window
    #+ [UI.div #+ [element input],
        element error]

  bInput <- stepper "" $ UI.valueChange input
  let eSubmit = filterE (== enterKey) (UI.keydown input)
  eEval <- accumE (Calculator.Interpreter.empty, Right Nothing) $
    updateResult <$> (bInput <@ eSubmit)

  onEvent eEval $ \result ->
    case result of
      (_, Left errorMsg) -> element error # set UI.text errorMsg
      (_, Right Nothing) -> do
        element error # set UI.text ""
        element input # set UI.value ""
      (_, Right (Just value)) -> do
        element error # set UI.text ""
        element input # set UI.value (unpack $ toLazyText $ shortest value)

  return ()

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
