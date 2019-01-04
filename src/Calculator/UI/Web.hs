{-# LANGUAGE RecursiveDo #-}

module Calculator.UI.Web (main) where

import Calculator.AST (Statement)
import Calculator.Interpreter (Environment, defaultEnv, evalStatement)
import Calculator.Parser (parse)
import Calculator.UI.Web.TeX (fromStatement)
import Calculator.UI.Web.Utils
  (addScript, alert, getSelectionEnd, getSelectionStart, renderKatex,
   setSelection, value')
import Calculator.Utils (showFloat, replaceSublist)

import Control.Monad (void)
import Data.Either.Combinators (leftToMaybe, rightToMaybe)
import Data.List (elemIndex)
import Data.Maybe (listToMaybe)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core (Behavior, Element, Event, Handler, UI,
                                    Window, (#), (#+), (#.), (<@))

import System.Environment (getArgs)

-- The result of evaluating the calculator's input.
data Result = Result { _inputStatement :: Statement,
                       _resultValue :: Maybe Double,
                       resultEnv :: Environment }

-- The labels and command text for each button in the panel.
buttonText :: [(String, String)]
buttonText =
  [("+", "+"),
   ("-", "-"),
   ("\\times", "*"),
   ("\\div", "/"),
   ("x^2", "^2"),
   ("x^y", "^"),
   ("\\sqrt{x}", "sqrt(x)"),
   ("\\sqrt[y]{x}", "root(x, y)"),
   ("e^x", "e^"),
   ("\\ln", "ln(x)"),
   ("\\log_{10}", "log10(x)"),
   ("\\log_y", "log(y, x)"),
   ("\\pi", "pi"),
   ("\\sin", "sin(x)"),
   ("\\cos", "cos(x)"),
   ("\\tan", "tan(x)"),
   (":=", "x = "),
   ("f(x)", "f(x) = ")]

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
  historyWrap <- UI.div #. "history" #+ [UI.element history]
  input <- UI.input
  (panel, ePanel) <- makePanel

  body <- UI.getBody window
  _ <- UI.element body #+
    [UI.element historyWrap,
     UI.element input,
     UI.element panel #. "panel"]
  UI.setFocus input

  -- Create an event to insert a command when a button in the panel is clicked.
  (eInsert, fireInsert) <- UI.liftIO UI.newEvent
  _ <- UI.onEvent ePanel $ insert input bInput fireInsert

  -- Evaluate the input when the Enter key is pressed.
  let eSubmit = UI.filterE (== 13) (UI.keydown input)
  let eEval = evalInput <$> ((,) <$> bEnv <*> bInput) <@ eSubmit

  -- Update the input as the user types, and clear it after evaluating if there
  -- wasn't an error.
  let eEvalSuccess = UI.filterJust $ rightToMaybe <$> eEval
  bInput <- UI.stepper "" $ head <$> UI.unions ["" <$ eEvalSuccess,
                                                eInsert,
                                                UI.valueChange input]
  _ <- UI.element input # UI.sink value' bInput
  bEnv <- UI.stepper defaultEnv (resultEnv <$> eEvalSuccess)

  -- Update the history after evaluating.
  _ <- UI.onEvent eEvalSuccess $ addHistory historyWrap history

  -- Show an alert if an error happens.
  let eEvalError = UI.filterJust $ leftToMaybe <$> eEval
  UI.onEvent eEvalError $ UI.runFunction . alert

-- Inserts the command into the input box and fire the given event handler with
-- the new input string.
insert :: Element -> Behavior String -> Handler String -> String -> UI ()
insert input bInput fire command = do
  str <- UI.currentValue bInput
  -- Insert assignment commands at the beginning and all other commands at the
  -- selection/cursor.
  str' <- if '=' `elem` command
          then atBeginning str
          else replaceSelection str
  UI.setFocus input
  UI.liftIO $ fire str'
  where
    replaceSelection str = do
      -- Replace the selected text in the input box with the command.
      start <- UI.callFunction $ getSelectionStart input
      end <- UI.callFunction $ getSelectionEnd input
      let str' = replaceSublist start end command str

      -- If the command is a function, select its arguments. Otherwise, put the
      -- caret at the end of the command.
      _ <- UI.element input # UI.set value' str'
      let (start', end') =
            case (elemIndex '(' command, elemIndex ')' command) of
              (Just s, Just e) -> (start + s + 1, start + e)
              _ -> (start + length command, start + length command)
      UI.runFunction $ setSelection start' end' input
      return str'

    atBeginning str = do
      let str' = command ++ str
      _ <- UI.element input # UI.set value' str'
      return str'

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
addHistory wrap history (Result statement value _) = do
  input <- UI.dterm
  output <- UI.ddef
  UI.runFunction $ renderKatex (fromStatement statement) input
  UI.runFunction $ renderKatex (maybe "" showFloat value) output
  _ <- UI.element history #+
    [UI.div #. "item" #+ [UI.element input, UI.element output]]
  UI.scrollToBottom wrap

-- Makes the panel for all of the calculator command buttons. When any button is
-- clicked, the event is fired with that button's command string.
makePanel :: UI (Element, Event String)
makePanel = do
  buttons <- mapM makeButton buttonText
  panel <- UI.ul #+
    map (\(b, _) -> UI.li #+ [UI.element b]) buttons
  let eClick = head <$> UI.unions (map snd buttons)
  return (panel, eClick)

-- Makes a button with a label and command. When the button is clicked, the
-- event is fired with the command string.
makeButton :: (String, String) -> UI (Element, Event String)
makeButton (label, command) = do
  button <- UI.button
  UI.runFunction $ renderKatex label button
  let eClick = command <$ UI.click button
  return (button, eClick)
