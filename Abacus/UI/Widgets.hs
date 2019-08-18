{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module Abacus.UI.Widgets
    ( mainWidget
    )
where

import Abacus.Core.Interpreter
import Abacus.Core.Utils
import qualified Abacus.UI.History as History
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Javascript.JSaddle.Object
import Language.Javascript.JSaddle.Types
import Reflex.Dom hiding (mainWidget, mainWidgetWithHead)
import Reflex.Dom.Main (mainWidgetWithHead)

newtype SubmitInput = SubmitInput Text

newtype SubmitOutput = SubmitOutput (Environment, Maybe Double)

type SubmitResult = Either InterpretError (SubmitInput, SubmitOutput)

mainWidget :: JSM ()
mainWidget = mainWidgetWithHead headElement bodyElement

headElement :: MonadWidget t m => m ()
headElement = do
    elAttr "link" ("rel" =: "stylesheet" <> "href" =: katexCss) blank
    elAttr "script" ("src" =: katexJs) blank
  where
    katexCss = "node_modules/katex/dist/katex.css"
    katexJs = "node_modules/katex/dist/katex.js"

bodyElement :: MonadWidget t m => m ()
bodyElement = el "div" $ mdo
    errorBox submitted
    resultList submitted
    submitted <- inputBox
    return ()

errorBox :: MonadWidget t m => Event t SubmitResult -> m ()
errorBox submitted = el "div" $ holdDyn "" errorResult >>= dynText
    where errorResult = Text.pack . either show (const "") <$> submitted

resultList :: MonadWidget t m => Event t SubmitResult -> m ()
resultList submitted = el "dl" $ do
    resultsWithKey <- getResultsWithKey
    _ <- listHoldWithKey Map.empty resultsWithKey $ \_ result -> do
        dt <- fst <$> el' "dt" blank
        dd <- fst <$> el' "dd" blank
        -- TODO: Use the TeX module to convert the input statement to TeX.
        _ <- katex dt $ input result
        _ <- katex dd $ output result
        return ()
    return ()
  where
    getResultsWithKey = mapAccum_
        (\n r -> (succ n, Map.singleton n $ Just r))
        (0 :: Integer)
        (filterRight submitted)
    input (SubmitInput i, _) = i
    output (_, SubmitOutput (_, o)) = maybe "" showFloat o
    katex e t = liftJSM $ do
        options <- obj
        options <# ("throwOnError" :: String) $ False
        jsg ("katex" :: String) # ("render" :: String)
            $ (t, _element_raw e, options)

inputBox :: MonadWidget t m => m (Event t SubmitResult)
inputBox = el "div" $ mdo
    input <- textInput def
        { _textInputConfig_setValue = difference
            (History.present <$> historyChanged)
            (_textInput_input input)
        }
    clicked <- button "="
    let submitted = evalInput <$> tagPromptlyDyn
            (_textInput_value input)
            (clicked <> keypress Enter input)
    historyChanged <- accum (&) (History.singleton "") $ leftmost
        [ submitHistory
            <$> tagPromptlyDyn (_textInput_value input) (filterRight submitted)
        , History.amend <$> _textInput_input input
        , History.back <$ keydown ArrowUp input
        , History.forward <$ keydown ArrowDown input
        ]
    return submitted
  where
    -- TODO: Save the environment returned by evalString.
    evalInput i = (SubmitInput i, ) . SubmitOutput <$> evalString
        defaultEnv
        (Text.unpack i)
    submitHistory i h
        | Text.null $ History.present h' =
            History.insert "" $ History.amend i h'
        | i == History.present h' = History.insert "" h'
        | otherwise               = History.insert "" $ History.insert i h'
        where h' = History.end h
