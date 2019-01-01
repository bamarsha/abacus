module Calculator.UI.Web.Utils (alert, value', addScript) where

import Control.Monad (void)
import Graphics.UI.Threepenny.Core

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

-- Adds a script to the head. The filename is relative to the "/static/js"
-- directory.
addScript :: Window -> FilePath -> UI ()
addScript window filename = void $ do
  script <- mkElement "script" #
            set (attr "type") "text/javascript" #
            set (attr "src") ("/static/js/" ++ filename)
  getHead window #+ [element script]
