{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Abacus.UI.Window
    ( run
    )
where

import Language.Javascript.JSaddle.Types

#ifdef __GHCJS__

run :: JSM () -> IO ()
run = id

#else

import Control.Concurrent
import qualified Data.Text as Text
import GI.GLib
import GI.Gtk hiding (init, main)
import qualified GI.Gtk as Gtk (init, main)
import GI.WebKit2
import Language.Javascript.JSaddle.WebKitGTK hiding (run)
import System.Directory

run :: JSM () -> IO ()
run mainWidget = do
    _ <- Gtk.init Nothing
    _ <- timeoutAdd PRIORITY_HIGH 10 (yield >> return True)
    webView <- makeWebView
    window <- makeWindow webView
    loadMainWidget webView mainWidget $ widgetShowAll window
    _ <- onWidgetDestroy window mainQuit
    Gtk.main

makeWebView :: IO WebView
makeWebView = do
    webView <- userContentManagerNew >>= webViewNewWithUserContentManager
    settings <- webViewGetSettings webView
    setSettingsEnableDeveloperExtras settings True
    setSettingsEnableWriteConsoleMessagesToStdout settings True
    webViewSetSettings webView settings
    return webView

makeWindow :: IsWidget a => a -> IO Window
makeWindow widget = do
    window <- windowNew WindowTypeToplevel
    windowSetDefaultSize window 400 400
    windowSetPosition window WindowPositionCenter
    scrolledWindow <- scrolledWindowNew noAdjustment noAdjustment
    window `containerAdd` scrolledWindow
    scrolledWindow `containerAdd` widget
    return window

loadMainWidget :: WebView -> JSM () -> IO () -> IO ()
loadMainWidget webView mainWidget finished = do
    _ <- onWebViewLoadChanged webView $ \case
        LoadEventFinished -> runInWebView mainWidget webView >> finished
        _ -> return ()
    indexUrl <- getIndexUrl
    webViewLoadHtml webView indexHtml $ Just indexUrl
  where
    indexHtml = "<!DOCTYPE html><html><head></head><body></body></html>"
    getIndexUrl = do
        dir <- getCurrentDirectory
        return $ "file://" <> Text.pack dir <> "/index.html"

#endif
