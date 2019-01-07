module Abacus.UI.Web.Utils
  (alert, value', addScript, renderKatex, getSelectionStart, getSelectionEnd,
   setSelection) where

import Control.Monad (void)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core (Attr, Element, JSFunction, UI, Window, (#),
                                    (#+))

-- Displays an alert dialog with a message.
alert :: String -> JSFunction ()
alert = UI.ffi "alert(%1)"

-- A version of the value attribute that only sets itself if the new value is
-- not equal to the current value. This keeps the cursor from moving to the end
-- of an input box in some browsers if the input box is "controlled" (i.e., it
-- has a behavior that updates with valueChange, and a sink that updates the
-- value with the behavior).
value' :: Attr Element String
value' = UI.mkReadWriteAttr get set
  where
    get = UI.get' UI.value
    set v el =
      UI.runFunction $ UI.ffi "if ($(%1).val() != %2) $(%1).val(%2)" el v

-- Adds a script to the head. The filename is relative to the "/static/js"
-- directory.
addScript :: Window -> FilePath -> UI ()
addScript window filename = void $ do
  script <- UI.mkElement "script" #
            UI.set (UI.attr "type") "text/javascript" #
            UI.set (UI.attr "src") ("/static/js/" ++ filename)
  UI.getHead window #+ [UI.element script]

-- Renders a TeX string to the element using KaTeX.
renderKatex :: String -> Element -> JSFunction ()
renderKatex = UI.ffi "katex.render(%1, %2, { displayMode: true })"

-- Returns the starting index of the input element's selection.
getSelectionStart :: Element -> JSFunction Int
getSelectionStart = UI.ffi "%1.selectionStart"

-- Returns the ending index of the input element's selection.
getSelectionEnd :: Element -> JSFunction Int
getSelectionEnd = UI.ffi "%1.selectionEnd"

-- Sets the starting and ending indices of the input element's selection.
setSelection :: Int -> Int -> Element -> JSFunction ()
setSelection = UI.ffi "%3.setSelectionRange(%1, %2)"
