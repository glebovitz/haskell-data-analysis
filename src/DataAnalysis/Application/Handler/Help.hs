{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module DataAnalysis.Application.Handler.Help where

import Data.Default
import Yesod
import Yesod.Default.Util

import DataAnalysis.Application.Foundation

getHelpR :: Handler Html
getHelpR = do
    currentRoute <- getCurrentRoute
    defaultLayout $ do
        setTitle "Help"
        $(widgetFileReload def "help")
