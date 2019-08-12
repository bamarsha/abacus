{-# LANGUAGE MonoLocalBinds, OverloadedStrings, RecursiveDo #-}

module Abacus.UI.Web
    ( main
    )
where

import Abacus.Interpreter
import Abacus.UI.Web.History
import Abacus.Utils
import Data.Maybe
import Data.Text hiding (empty)
import Reflex.Dom

main :: IO ()
main = mainWidget $ el "div" $ do
    submitted <- inputBox
    let newValue = pack . maybe "" showFloat . snd <$> filterRight submitted
    let newError = pack . either show (const "") <$> submitted
    el "div" $ holdDyn "" newValue >>= dynText
    el "div" $ holdDyn "" newError >>= dynText

inputBox :: MonadWidget t m => m (Event t InterpretResult)
inputBox = el "div" $ mdo
    input <- textInput $ def
        & textInputConfig_setValue .~ (fromMaybe "" . now <$> historyChanged)
    clicked <- button "="
    let submitted = evalString defaultEnv . unpack <$> tagPromptlyDyn
            (_textInput_value input)
            (clicked <> keypress Enter input)
    historyChanged <- accum (&) empty $ leftmost
        [ record <$> current (_textInput_value input) <@ filterRight submitted
        , const back <$> keydown ArrowUp input
        , const forward <$> keydown ArrowDown input
        ]
    return submitted
