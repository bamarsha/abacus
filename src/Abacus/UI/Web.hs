{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

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
inputBox = el "div" $ mdo
    input <- textInput $ def
        & textInputConfig_setValue .~ (const "" <$> filterRight submitted)
    clicked <- button "="
    let entered = void $ ffilter isEnter (_textInput_keypress input)
    let submitted = evalString defaultEnv . unpack <$> tagPromptlyDyn
            (_textInput_value input)
            (clicked <> entered)
    return submitted
  where
    isEnter code = keyCodeLookup (fromIntegral code) == Enter
