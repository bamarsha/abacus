{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Abacus.UI.Widgets
    ( mainWidget
    )
where

import Abacus.Core.Interpreter
import Abacus.Core.Parser
import Abacus.Core.Utils
import qualified Abacus.UI.History as History
import qualified Abacus.UI.TeX as TeX
import Control.Lens ((^.))
import Control.Monad
import Data.Either.Unwrap
import Data.FileEmbed
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import Language.Javascript.JSaddle.Evaluate
import Language.Javascript.JSaddle.Object
import Language.Javascript.JSaddle.Types
import Reflex.Dom hiding (mainWidget, mainWidgetWithHead)
import Reflex.Dom.Main (mainWidgetWithHead)

newtype SubmitInput = SubmitInput Text

newtype SubmitOutput = SubmitOutput (Environment, Maybe Double)

type SubmitResult = Either InterpretError (SubmitInput, SubmitOutput)

mainWidget :: JSM ()
mainWidget = do
    _ <- eval $ decodeUtf8 $(embedFile "node_modules/katex/dist/katex.min.js")
    mainWidgetWithHead headElement bodyElement
    jsg ("document" :: String)
        ^. js1 ("querySelector" :: String) (".input input" :: String)
        ^. js0 ("focus" :: String)
    return ()

headElement :: MonadWidget t m => m ()
headElement = do
    el "title" $ text "Abacus"
    el "style" $ text $ decodeUtf8 $(embedFile "abacus.css")

bodyElement :: MonadWidget t m => m ()
bodyElement = mdo
    errorBox submitted
    resultList submitted
    submitted <- inputBox
    return ()

errorBox :: MonadWidget t m => Event t SubmitResult -> m ()
errorBox submitted = el "div" $ holdDyn "" errorResult >>= dynText
    where errorResult = Text.pack . either show (const "") <$> submitted

resultList :: MonadWidget t m => Event t SubmitResult -> m ()
resultList submitted = elClass "div" "results" $ el "dl" $ do
    resultsWithKey <- getResultsWithKey
    _ <- listHoldWithKey Map.empty resultsWithKey $ \_ result -> do
        dt <- fst <$> el' "dt" blank
        dd <- fst <$> el' "dd" blank
        _ <- katex dt $ input result
        _ <- katex dd $ output result
        return ()
    return ()
    where
        getResultsWithKey = mapAccum_
            (\n r -> (succ n, Map.singleton n $ Just r))
            (0 :: Integer)
            (filterRight submitted)
        input (SubmitInput text, SubmitOutput (_, value)) =
            (TeX.fromStatement $ fromRight $ parse $ Text.unpack text)
                ++ if isJust value then " =" else ""
        output (_, SubmitOutput (_, value)) =
            maybe "" showWithoutTrailingZero value
        katex e t = liftJSM $ do
            options <- obj
            options <# ("throwOnError" :: String) $ False
            options <# ("displayMode" :: String) $ True
            jsg ("katex" :: String) # ("render" :: String)
                $ (t, _element_raw e, options)

inputBox :: MonadWidget t m => m (Event t SubmitResult)
inputBox = elClass "div" "input" $ mdo
    input <- inputElement $ def
        & inputElementConfig_setValue .~ difference
            (History.present <$> historyChanged)
            (_inputElement_input input)
    clicked <- button "="
    submitted <- mapAccum_ evalInput defaultEnv $ tagPromptlyDyn
            (value input)
            (clicked <> keypress Enter input)
    historyChanged <- accum (&) (History.singleton "") $ leftmost
        [ submitHistory
            <$> tagPromptlyDyn (value input) (filterRight submitted)
        , History.amend <$> _inputElement_input input
        , History.back <$ keydown ArrowUp input
        , History.forward <$ keydown ArrowDown input
        ]
    return submitted
    where
        evalInput env input = case evalString env $ Text.unpack input of
            Left error -> (env, Left error)
            Right (env', value) -> (env', Right (SubmitInput input, SubmitOutput (env', value)))
        submitHistory i h
            | Text.null latest = History.amend i h' & History.insert ""
            | i == latest      = History.insert "" h'
            | otherwise        = History.insert i h' & History.insert ""
            where h' = History.end h
                  latest = History.present h'
