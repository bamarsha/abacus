{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Abacus.UI.Widgets
    ( abacus
    )
where

import Abacus.Core.Interpreter
import Abacus.Core.Utils
import Data.Text
import Reflex.Dom
import qualified Abacus.UI.History as History

abacus :: MonadWidget t m => m ()
abacus = el "div" $ do
    submitted <- inputBox
    let newValue = pack . maybe "" showFloat . snd <$> filterRight submitted
    let newError = pack . either show (const "") <$> submitted
    el "div" $ holdDyn "" newValue >>= dynText
    el "div" $ holdDyn "" newError >>= dynText

inputBox :: MonadWidget t m => m (Event t InterpretResult)
inputBox = el "div" $ mdo
    input <- textInput def
        { _textInputConfig_setValue = difference
            (History.present <$> historyChanged)
            (_textInput_input input)
        }
    clicked <- button "="
    let submitted = evalString defaultEnv . unpack <$> tagPromptlyDyn
            (_textInput_value input)
            (clicked <> keypress Enter input)
    historyChanged <- accum (&) (History.singleton "") $ leftmost
        [ History.append "" <$ filterRight submitted
        , History.amend <$> _textInput_input input
        , History.back <$ keydown ArrowUp input
        , History.forward <$ keydown ArrowDown input
        ]
    return submitted
