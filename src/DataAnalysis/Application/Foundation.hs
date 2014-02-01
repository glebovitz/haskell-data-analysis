{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS -fno-warn-orphans #-}

module DataAnalysis.Application.Foundation where

import qualified Control.Exception as E
import Control.Monad
import Data.Default
import Data.List
import Data.Text (Text,pack)
import Data.Time
import Data.Time.Clock.POSIX
import DataAnalysis.Application.Types
import System.Directory
import System.FilePath
import System.IO
import System.Time
import Text.Blaze
import Text.Hamlet
import Yesod
import Yesod.Default.Util
import Yesod.Static

mkYesodData "App" $(parseRoutesFile "config/routes")

instance HasManager App where
    manager = appManager

instance ToMarkup (Route App) where
  toMarkup r =
    case r of
      HomeR        -> "Home"
      ReviewR _    -> "Review"
      HelpR        -> "Help"
      ImportR      -> "Import"
      DatasourcesR -> "Data Sources"
      ExportR {}   -> "Export"
      StaticR {}   -> "Static"
      StartTimeR{} -> "Start time"

instance Yesod App where
  defaultLayout widget = do
    pc <- widgetToPageContent $ do
      addStylesheet $ StaticR flot_jquery_flot_js
      $(widgetFileNoReload def "default-layout")
    currentRoute <- getCurrentRoute
    yesod <- getYesod
    case yesod of
      App{appTitle}  ->
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

-- | Add a new source.
--
-- TODO: Generate a proper temporary filename.
--
-- TODO: Currently this just moves the uploaded file to another place,
-- leaving it in CSV format. That means when we're doing analysis,
-- we're always re-parsing, in chunks the CSV from file. While that's
-- nice and good for constant memory usage, a CSV is possibly not the
-- fastest format in the long-term (or maybe it is not a bottle
-- neck?). We can probably use something like cereal/binary as
-- conduits, like here:
-- https://hackage.haskell.org/package/cereal-conduit-0.7.2/docs/Data-Conduit-Cereal.html
--
-- But in the meantime, for the demo, this should be fine. Though it
-- looks pretty easy to use the cereal conduit laid out there.
--
-- If/when we decide we're going to read from an alternate data form,
-- this function should use the fileSource function to make a Producer
-- to write into some other format, and then store whatever ID is used
-- for that format (file, DB, redis, w/e), and the conduit source
-- changed from sourceFile to whatever else.
addSource :: FileInfo -> Handler Text
addSource fi = do
  fp <- liftIO getTemporaryFileName
  liftIO (fileMove fi fp)
  return (pack (takeFileName fp))
  where getTemporaryFileName =
          do appDir <- getAppDir
             createDirectoryIfMissing True appDir
             (fp,h) <- openTempFile appDir "upload.csv"
             hClose h
             return fp

-- | Get a source by its id.
getById :: Text -> Handler DataSource
getById ident = do
  files <- getList
  case find ((==ident).srcName) files of
    Nothing -> error "No such imported data source."
    Just s -> return s

-- | Get all sources.
getList :: Handler [DataSource]
getList =
  liftIO (do dir <- getAppDir
             list <- fmap (filter (not . all (=='.')))
                          (E.catch (getDirectoryContents dir)
                                   (\(_::E.IOException) -> return []))
             forM list
                  (\item ->
                     do let fp = dir ++ "/" ++ item
                        time <- liftIO (fmap utcTimeFromClockTime
                                             (getModificationTime fp))
                        return (DataSource (pack item) fp time)))

-- | Get the directory used for uploads.
getAppDir :: IO FilePath
getAppDir =
  do tmp <- getTemporaryDirectory
     let appDir = tmp ++ "/analysis-app"
     return appDir

utcTimeFromClockTime :: ClockTime -> UTCTime
utcTimeFromClockTime (TOD seconds picoseconds) =
  posixSecondsToUTCTime . fromRational
     $ fromInteger seconds + fromInteger picoseconds / 1000000000000
