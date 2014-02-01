{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module DataAnalysis.Application.Dispatch where

import Yesod

import DataAnalysis.Application.Foundation
import DataAnalysis.Application.Handler.Datasources
import DataAnalysis.Application.Handler.Help
import DataAnalysis.Application.Handler.Home
import DataAnalysis.Application.Handler.Import
import DataAnalysis.Application.Handler.Review
import DataAnalysis.Application.Handler.StartTime
import DataAnalysis.Application.Handler.Export
import DataAnalysis.Application.Types

mkYesodDispatch "App" resourcesApp
