-- This module controls the parameters that the analysis accepts from a user.
-- In the near future, this will be replaced by a higher level DSL.
{-# LANGUAGE OverloadedStrings #-}
module UserParameters
    ( module UserParameters
    , module UserModel
    , module DataAnalysis.Application.Import
    ) where

import UserModel
import DataAnalysis.Application.Import

-- | Parameters to the analysis.
data RsiParams = RsiParams
    { rpSize  :: !Int -- ^ RSI grouping size
    , rpAlpha :: !Double -- ^ alpha for exponential moving average
    , rpBars  :: !Int -- ^ number of bars to display
    }

-- | Make a form for the parameters, uses the 'Default' instance for
-- the default values.
instance HasForm RsiParams where
    form = RsiParams
        <$> areq intField "Grouping size" (Just (rpSize def))
        <*> areq doubleField "Alpha (for exponential moving average)" (Just (rpAlpha def))
        <*> areq intField "Number of bars (up to 20 for a readable graph)" (Just (rpBars def))

-- | Default values for the parameters.
instance Default RsiParams where
  def = RsiParams 14
                  0.6
                  16
