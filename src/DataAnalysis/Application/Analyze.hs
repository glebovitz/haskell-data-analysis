{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- | Functionality to perform an analysis on the imported data source,
-- the submitted paramaters and return some data points.

module DataAnalysis.Application.Analyze where

import Control.Applicative
import Data.Conduit
import Data.Conduit.Binary (sourceFile)
import Data.IORef
import Data.Text (Text)
import DataAnalysis.Application.Foundation
import Yesod

import DataAnalysis.Application.Types

-- | Analyze the imported data with the submitted parameters (if any),
-- and return the data points from it.
analysisSource :: Text -> HandlerT App IO (Source Handler DataPoint)
analysisSource ident = do
    app <- getYesod
    source <- getById ident
    SomeAnalysis{..} <- return (appAnalysis app)
    ((result, _), _) <- runFormGet (renderDivs ((,) <$> analysisForm <*> graphType))
    let params =
          case result of
            FormSuccess (p,_::Text) -> p
            _ -> analysisDefaultParams
    countRef <- liftIO (newIORef 0)
    return (sourceFile (srcPath source) $= analysisConduit countRef params)
  where graphType =
          areq hiddenField
               "" {fsName = l,fsId = l}
               (Just "Bar")
          where l = Just "graph_type"
