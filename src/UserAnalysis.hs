-- This is the module where an end user writes the actual domain specific analysis.
{-# LANGUAGE OverloadedStrings #-}
module UserAnalysis (userAnalysis) where

import           UserParameters

-- The following is an implementation of relative strength index.
-- For more information, see: http://en.wikipedia.org/wiki/Relative_strength_index

-- Our analysis consists of a number of individual pipeline components.
-- Each component is combined together with the =$= operator, which
-- causes the output of one component to become the input of the next.

-- For a list of commonly used functions to use in an analysis, see:
--
-- http://download.fpcomplete.com/tempdocs/data-analysis-library/DataAnalysis-Library.html
userAnalysis :: MonadIO m => RsiParams -> Conduit Stock m DataPoint
userAnalysis (RsiParams size alpha bars) =
    -- begin the analysis
        startAnalysis

    -- compute the change in stock price on two consecutive days
    =$= stocksToUpDown stockDate stockAdjClose

    -- group the changes into vectors of the user-specified size
    =$= movingGroupsOf size

    -- perform the RSI calculation on each packed vector
    =$= mapStream (calculateRSI alpha)

    -- limit output to the number of data points requested by the user
    -- note that no unnecessary computations will be performed
    =$= isolate bars

-- | Calculate the RSI value.
calculateRSI
    :: Double -- ^ alpha
    -> (UpDown, Vector UpDown)
    -> DataPoint
calculateRSI alpha (firstUpDown, v) =
    DP (firstUpDown ^. udDate . shown) rsi' Nothing
  where
    rs = exponentialMovingAverage udUp   alpha v
       / exponentialMovingAverage udDown alpha v
    rsi' = 100 - (100 / (1 + rs))
