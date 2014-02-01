{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

module DataAnalysis.Application.Handler.Import where

import Data.Default
import Yesod
import Yesod.Default.Util

import DataAnalysis.Application.Foundation

-- | Show the import form.
getImportR :: Handler Html
getImportR = do
    (formWidget, formEncType) <- generateFormPost uploadForm
    currentRoute <- getCurrentRoute
    defaultLayout $ do
        setTitle "File Processor"
        $(widgetFileReload def "import")

-- | Add the given file as a data source and redirect to the new
-- source.
postImportR :: Handler Html
postImportR = do
  ((result, _), _) <- runFormPost uploadForm
  case result of
    FormSuccess fi -> do
      i <- addSource fi
      redirect (ReviewR i)
    _ -> return ()
  redirect ImportR

-- | Upload form.
uploadForm = renderDivs $ fileAFormReq "source"
