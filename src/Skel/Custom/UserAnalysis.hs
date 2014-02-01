-- This is the module where an end user writes the actual domain specific analysis.
{-# LANGUAGE OverloadedStrings #-}
module Skel.Custom.UserAnalysis (userAnalysis) where

import           Skel.Custom.UserParameters

-- Our analysis consists of a number of individual pipeline components.
-- Each component is combined together with the =$= operator, which
-- causes the output of one component to become the input of the next.

-- For a list of commonly used functions to use in an analysis, see:
--
-- http://download.fpcomplete.com/tempdocs/data-analysis-library/DataAnalysis-Library.html
userAnalysis :: MonadIO m => CustomParams -> Conduit Stock m DataPoint
userAnalysis (CustomParams dummy) =
    -- Replace with your analysis
    return ()
