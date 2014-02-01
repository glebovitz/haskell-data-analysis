{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point to my analysis app.

module Skel.Custom.Main where

import DataAnalysis.Application.Prelude
import Skel.Custom.UserAnalysis

-- | Start the analysis server with the following configuration.
main :: IO ()
main = runAnalysisApp "Custom analysis" userAnalysis
