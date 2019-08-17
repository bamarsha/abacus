{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module Abacus.UI.Widgets
    ( abacus
    )
where

import Abacus.Core.Interpreter
import Abacus.Core.Utils
import Data.Text hiding (reverse)
import Reflex.Dom hiding (Input)
import qualified Abacus.UI.History as History

newtype Input = Input Text

newtype Output = Output (Environment, Maybe Double)

type SubmitResult = Either InterpretError (Input, Output)

abacus :: MonadWidget t m => m ()
abacus = el "div" $ mdo
    errorBox submitted
    resultList submitted
    submitted <- inputBox
    return ()

errorBox :: MonadWidget t m => Event t SubmitResult -> m ()
errorBox submitted = el "div" $ holdDyn "" errorResult >>= dynText
    where errorResult = pack . either show (const "") <$> submitted

resultList :: MonadWidget t m => Event t SubmitResult -> m ()
resultList submitted = el "dl" $ do
    results <- foldDyn (:) [] $ filterRight submitted
    _ <- simpleList (reverse <$> results) $ \result -> do
        el "dt" $ dynText $ input <$> result
        el "dd" $ dynText $ output <$> result
    return ()
  where
    input (Input i, _) = i
    output (_, Output (_, o)) = pack $ maybe "" showFloat o

inputBox :: MonadWidget t m => m (Event t SubmitResult)
inputBox = el "div" $ mdo
    input <- textInput def
        { _textInputConfig_setValue = difference
            (History.present <$> historyChanged)
            (_textInput_input input)
        }
    clicked <- button "="
    let submitted = eval <$> tagPromptlyDyn
            (_textInput_value input)
            (clicked <> keypress Enter input)
    historyChanged <- accum (&) (History.singleton "") $ leftmost
        [ History.append "" <$ filterRight submitted
        , History.amend <$> _textInput_input input
        , History.back <$ keydown ArrowUp input
        , History.forward <$ keydown ArrowDown input
        ]
    return submitted
  where
    eval i = (Input i,) . Output <$> evalString defaultEnv (unpack i)
