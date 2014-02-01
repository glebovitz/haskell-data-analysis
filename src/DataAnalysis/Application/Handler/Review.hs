{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Review the imported data, and the analysis upon that data.

module DataAnalysis.Application.Handler.Review where

import           Control.Applicative
import           Data.Aeson
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Default
import           Data.IORef
import           Data.Text (Text)
import           Data.Text.Lazy.Encoding
import           Data.Time
import           DataAnalysis.Application.Foundation
import           System.Locale
import           Yesod
import           Yesod.Default.Util

import           DataAnalysis.Application.Analyze
import           DataAnalysis.Application.Types

-- | Review the imported data, and the analysis upon that data.
getReviewR :: Text -> Handler Html
getReviewR ident = do
    app <- getYesod
    source <- getById ident
    currentRoute <- getCurrentRoute
    let title = toHtml (formatTime defaultTimeLocale "Import %T" (srcTimestamp source))
    SomeAnalysis{..} <- return (appAnalysis app)
    ((result, widget), enctype) <- runFormGet (renderDivs ((,) <$> analysisForm <*> graphType))
    let params =
          case result of
            FormSuccess (p,_::Text) -> p
            _ -> analysisDefaultParams
    countRef <- liftIO (newIORef 0)
    start <- liftIO getCurrentTime
    !datapoints <- analysisSource ident >>= ($$ CL.consume)
    rowsProcessed :: Int <- liftIO (readIORef countRef)
    now <- liftIO getCurrentTime
    defaultLayout $ do
        setTitle title
        let datapointsJson = toHtml (decodeUtf8 (encode (take 100 datapoints)))
            generationTime = diffUTCTime now start
        $(widgetFileReload def "review")
  where graphType =
          areq hiddenField
               "" {fsName = l,fsId = l}
               (Just "Bar")
          where l = Just "graph_type"

-- | Show a number that's counting something so 1234 is 1,234.
showCount :: (Show n,Integral n) => n -> String
showCount = reverse . foldr merge "" . zip ("000,00,00,00"::String) . reverse . show where
  merge (f,c) rest | f == ',' = "," ++ [c] ++ rest
                   | otherwise = [c] ++ rest

-- | Review the imported data, and the analysis upon that data.
postReviewR :: Text -> Handler Html
postReviewR = getReviewR
