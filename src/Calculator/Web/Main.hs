module Calculator.Web.Main (main) where

import Calculator.AST
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
  return window # set UI.title "Calculator"

  input <- UI.input
  error <- UI.p
  getBody window
    #+ [UI.div #+ [element input],
        element error]

  envRef <- liftIO $ newIORef Calculator.Interpreter.empty

  on UI.keydown input $ \key ->
    if key == enterKey
    then do
      env <- liftIO $ readIORef envRef
      line <- get UI.value input

      case eval env line of
        Left error' -> element error # set UI.text (show error')
        Right (Left error') -> element error # set UI.text error'
        Right (Right (env', maybeResult)) -> do
          liftIO $ modifyIORef envRef (const env')
          element error # set UI.text ""

          let input' = case maybeResult of
                         Nothing -> ""
                         Just result -> unpack $ toLazyText $ shortest result
          element input # set UI.value input'
    else element input
