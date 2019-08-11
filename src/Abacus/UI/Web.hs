{-# LANGUAGE OverloadedStrings #-}

module Abacus.UI.Web (main) where

import Reflex.Dom

main :: IO ()
main = mainWidget $ el "div" $ text "Welcome to Reflex"