{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module DataAnalysis.Application.Prelude
  (runAnalysisApp)
  where

import Data.Conduit (Conduit)
import Data.Text (Text)
import Data.Time
import Yesod
import Yesod.Static

import DataAnalysis.Application.Types
import DataAnalysis.Application.Dispatch ()

#if FPHC
import Network.HTTP.Client (defaultManagerSettings, newManager)
#else
import Network.HTTP.Client (defaultManagerSettings, newManager)
#endif

-- | Run the analysis web app.
runAnalysisApp :: (PersistEntity b,HasForm params)
               => Text
               -> (params -> Conduit b (HandlerT App IO) DataPoint)
               -> IO ()
runAnalysisApp title analysis = do
  s <- static "static"
  man <- newManager defaultManagerSettings
  now <- getCurrentTime
  warpEnv
    (App man
         title
         (getSomeAnalysis analysis)
         s
         now)
