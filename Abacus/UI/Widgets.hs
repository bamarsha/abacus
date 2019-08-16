{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Abacus.UI.Widgets
    ( abacus
    )
where

import Abacus.Core.Interpreter
import Abacus.Core.Utils
import Data.Text hiding (reverse)
import Reflex.Dom
import qualified Abacus.UI.History as History

abacus :: MonadWidget t m => m ()
abacus = el "div" $ mdo
    errorBox submitted
    valueList submitted
    submitted <- inputBox
    return ()

errorBox :: MonadWidget t m => Event t InterpretResult -> m ()
errorBox submitted =
    let errorResult = pack . either show (const "") <$> submitted
    in  el "div" $ holdDyn "" errorResult >>= dynText

valueList :: MonadWidget t m => Event t InterpretResult -> m ()
valueList submitted = el "ul" $ do
    let valueResult = pack . maybe "" showFloat . snd <$> filterRight submitted
    values <- foldDyn (:) [] valueResult
    _ <- simpleList (reverse <$> values) $ el "li" . dynText
    return ()

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
