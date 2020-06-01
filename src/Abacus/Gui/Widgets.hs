{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Abacus.Gui.Widgets
    ( calculator
    ) where

import Data.Either.Unwrap
import Data.FileEmbed
import Data.Maybe
import Data.Text (Text)
import Language.Javascript.JSaddle
import Reflex.Dom

import Abacus.Interpreter.Eval
import Abacus.Interpreter.Parser

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Abacus.Gui.History as History
import qualified Abacus.Gui.TeX as TeX

newtype SubmitInput = SubmitInput Text

newtype SubmitOutput = SubmitOutput (Environment, Maybe Rational)

type SubmitResult = Either EvalError (SubmitInput, SubmitOutput)

calculator :: IO ()
calculator = mainWidgetWithHead headElement bodyElement

headElement :: MonadWidget t m => m ()
headElement = do
    el "title" $ text "Abacus"
    el "style" $ text $ Text.decodeUtf8 $(embedFile "abacus.css")
    _ <- liftJSM $ eval $ Text.decodeUtf8 $(embedFile "node_modules/katex/dist/katex.min.js")
    return ()

bodyElement :: MonadWidget t m => m ()
bodyElement = mdo
    resultList submitted
    errorBox submitted
    submitted <- inputBox
    _ <- liftJSM $ eval
        ("setTimeout(() => document.querySelector('.input input').focus())" :: String)
    return ()

errorBox :: MonadWidget t m => Event t SubmitResult -> m ()
errorBox submitted = elClass "div" "error" $ holdDyn "" errorResult >>= dynText
  where
    errorResult = Text.pack . either show (const "") <$> submitted

resultList :: MonadWidget t m => Event t SubmitResult -> m ()
resultList submitted = elClass "div" "results" $ el "dl" $ do
    resultsWithKey <- getResultsWithKey
    _ <- listHoldWithKey Map.empty resultsWithKey $ \_ result -> do
        fst <$> el' "dt" blank >>= renderKatex (input result)
        fst <$> el' "dd" blank >>= renderKatex (output result)
    return ()
  where
    getResultsWithKey = mapAccum_
        (\key result -> (succ key, Map.singleton key $ Just result))
        (0 :: Integer)
        (filterRight submitted)
    input (SubmitInput txt, SubmitOutput (_, result)) =
        (TeX.showStatement $ fromRight $ parseStatement $ Text.unpack txt)
            ++ if isJust result then " =" else ""
    output (_, SubmitOutput (_, result)) = maybe "" TeX.showRational result
    renderKatex txt target = liftJSM $ do
        options <- obj
        options <# ("throwOnError" :: String) $ False
        options <# ("displayMode" :: String) $ True
        _ <- jsg ("katex" :: String) # ("render" :: String) $ (txt, _element_raw target, options)
        return ()

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
        Left err -> (env, Left err)
        Right (env', result) -> (env', Right (SubmitInput input, SubmitOutput (env', result)))
    submitHistory input history
        | Text.null latest = History.amend input history' & History.insert ""
        | input == latest  = History.insert "" history'
        | otherwise        = History.insert input history' & History.insert ""
      where
        history' = History.end history
        latest = History.present history'
