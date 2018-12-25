module GUI (main) where

import AST
import Interpreter
import Parser

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
  return window # set UI.title "Calculator"

  input <- UI.input
  error <- UI.p
  getBody window
    #+ [UI.div #+ [element input],
        element error]

  envRef <- liftIO $ newIORef Interpreter.empty

  on UI.keydown input $ \key ->
    if key == enterKey
    then do
      line <- get UI.value input
      env <- liftIO $ readIORef envRef

      case evaluateLine env line of
        Left msg -> element error # set UI.text msg
        Right (result, env') -> do liftIO $ modifyIORef envRef (const env')
                                   element input # set UI.value result
                                   element error # set UI.text ""
    else element input

-- Parses and evaluates a line. Returns either an error message (Left) or the
-- result with the new environment (Right).
evaluateLine :: Environment -> String -> Either String (String, Environment)
evaluateLine env line =
  case parse line of
    Left error -> Left (show error)
    Right result ->
      case evaluate env statement of
        Left error -> Left error
        Right (Just n, env') -> Right (unpack $ toLazyText $ shortest n, env')
        Right (Nothing, env') -> Right ("", env')
