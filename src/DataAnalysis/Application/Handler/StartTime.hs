{-# LANGUAGE OverloadedStrings #-}

-- | Used by the browser to determine whether the server has restarted.

module DataAnalysis.Application.Handler.StartTime where

import           Blaze.ByteString.Builder.Char.Utf8
import           Data.Conduit
import qualified Data.Conduit.List                   as CL
import           DataAnalysis.Application.Foundation
import           DataAnalysis.Application.Types
import           Yesod

-- | Return when the server was started.
getStartTimeR :: Handler TypedContent
getStartTimeR = do
    app <- getYesod
    respondSource "text/plain"
                  (CL.sourceList [appStart app] $= CL.map (Chunk . fromShow))
