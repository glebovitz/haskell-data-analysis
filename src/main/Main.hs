{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point to my analysis app.

module Main where

import DataAnalysis.Application.Prelude
import UserAnalysis

-- | Start the analysis server with the following configuration.
main :: IO ()
main = runAnalysisApp "RSI analysis" userAnalysis
