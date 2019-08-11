{-# LANGUAGE OverloadedStrings #-}

module Abacus.UI.Web
    ( main
    )
where

import Abacus.Interpreter
import Abacus.Utils
import Control.Monad
import Data.Text
import Reflex.Dom

main :: IO ()
main = mainWidget $ el "div" $ do
    input <- textInput def
    clicked <- button "="
    let entered = void $ ffilter (isKey Enter) (_textInput_keypress input)
    let submitted = evalString defaultEnv . unpack <$> tagPromptlyDyn
            (_textInput_value input)
            (clicked <> entered)
    let result = pack . maybe "" showFloat . snd <$> filterRight submitted
    holdDyn "" result >>= dynText
    where isKey key code = key == keyCodeLookup (fromIntegral code)
