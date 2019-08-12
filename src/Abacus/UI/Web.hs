{-# LANGUAGE MonoLocalBinds #-}
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
    submitted <- inputBox
    let getValue = pack . maybe "" showFloat . snd <$> filterRight submitted
    let getError = pack . either show (const "") <$> submitted
    el "div" $ holdDyn "" getValue >>= dynText
    el "div" $ holdDyn "" getError >>= dynText

inputBox :: MonadWidget t m => m (Event t InterpretResult)
inputBox = el "div" $ do
    input <- textInput def
    clicked <- button "="
    let entered = void $ ffilter isEnter (_textInput_keypress input)
    return $ evalString defaultEnv . unpack <$> tagPromptlyDyn
        (_textInput_value input)
        (clicked <> entered)
  where
    isEnter code = keyCodeLookup (fromIntegral code) == Enter
