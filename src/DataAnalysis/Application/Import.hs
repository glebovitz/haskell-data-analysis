{-# OPTIONS_GHC -fno-warn-orphans #-}
module DataAnalysis.Application.Import
    ( module X
    , startAnalysis
    ) where

import           Control.Applicative            as X
import           Control.Lens                   as X
import           Data.Conduit                   as X
import           Data.Conduit.Analysis          as X
import           Data.Conduit.List              as X (isolate)
import           Data.Default                   as X
import           Data.String                    (IsString (fromString))
import           Data.Time                      as X
import           Data.Vector                    as X (Vector)
import           DataAnalysis.Application.Types as X
import           DataAnalysis.Library           as X
import           Safe                           (readMay)
import           Yesod                          as X hiding (filterField, (.=),
                                                      (<.), parseTime)

-- | Performs no transformation on the stream. The main purpose of this
-- function is to make analysis pipelines easier to manipulate, by ensuring
-- that each step begins with the @=$=@ operator.
startAnalysis :: Monad m => Conduit a m a
startAnalysis = awaitForever yield

-- orphan instance!
instance IsString Day where
    fromString s =
        case readMay s of
            Nothing -> error $ "Invalid date literal in source code: " ++ show s
            Just d -> d
