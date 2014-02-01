-- This module controls the parameters that the analysis accepts from a user.
-- In the near future, this will be replaced by a higher level DSL.
{-# LANGUAGE OverloadedStrings #-}
module Skel.Custom.UserParameters
    ( module Skel.Custom.UserParameters
    , module UserModel
    , module DataAnalysis.Application.Import
    ) where

import UserModel
import DataAnalysis.Application.Import

-- | Parameters to the analysis.
data CustomParams = CustomParams
    { cpDummy :: !Int -- ^ Replace with your own parameter(s)
    }

-- | Make a form for the parameters, uses the 'Default' instance for
-- the default values.
instance HasForm CustomParams where
    form = CustomParams
        <$> areq intField "Dummy parameter" (Just (cpDummy def))

-- | Default values for the parameters.
instance Default CustomParams where
  def = CustomParams 0
