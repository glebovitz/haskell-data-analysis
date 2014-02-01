{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
-- | A library of commonly used data analysis functions.
--
-- Note that this is an initial, demo version of the library. The final version
-- will be much more fully featured.
module DataAnalysis.Library
    ( -- * General purpose
      -- ** Streaming functions
      -- $streaming
      mapStream
    , mapField
    , filterStream
    , filterField
    , sumStream
    , sumField
    , movingGroupsOf
      -- ** Lens helpers
    , shown
      -- * Mathematical
    , exponentialMovingAverage
      -- * Financial
      -- ** Price differentials
    , UpDown (..)
    , stocksToUpDown
      -- *** Lenses
    , HasUpDown (..)
    ) where

-- Nota bene! The documentation for this module will be generated as Haddocks
-- and given to users, so it must be kept clean and understandable.

import           Control.Lens
import           Data.Conduit
import           Data.Conduit.Analysis
import qualified Data.Conduit.List     as CL
import           Data.Text             (Text, pack)
import           Data.Time             (Day)

-- $streaming
--
-- The following functions come in two flavors: those operating on the streamed
-- value itself, and those operating on a single field of the streamed value.
-- The latter are convenience functions to make many kinds of common analysis
-- simple, such as taking the logarithm of a specific field in a data
-- structure.

-- | Perform the given transformation on all values in the stream.
--
-- @
-- yieldMany [1..10] =$= mapStream (+1) =$= sumStream
-- @
mapStream :: Monad m => (a -> b) -> Conduit a m b
mapStream = CL.map

-- | Perform the given transformation on the given field in the stream.
--
-- @
-- yieldMany (zip [1..10] [11..20]) =$= mapField _1 (+1) =$= sumField _1
-- @
mapField :: Monad m => Lens s t a b -> (a -> b) -> Conduit s m t
mapField field f = CL.map (field %~ f)

-- | Keep only the values from the stream passing the given predicate function.
--
-- @
-- yieldMany [1..10] =$= filterStream even =$= sumStream
-- @
filterStream :: Monad m => (a -> Bool) -> Conduit a m a
filterStream = CL.filter

-- | Keep only the values from the stream where the given field passes the
-- given predicate function.
--
-- @
-- yieldMany ([1..10], [11..20]) =$= filterField _1 even =$= sumField _2
-- @
filterField :: Monad m => Lens' s a -> (a -> Bool) -> Conduit s m s
filterField field f = CL.filter (f . view field)

-- | Sum all of the values in a stream.
sumStream :: (Monad m, Num a) => Consumer a m a
sumStream = CL.fold (+) 0

-- | Sum up a specific field in a stream.
sumField :: (Monad m, Num a) => Getter s a -> Consumer s m a
sumField field = CL.fold (\total s -> total + (s ^. field)) 0

-- | Convert a value to its textual representation.
--
-- Uses the @Show@ instance for the type.
shown :: Show a => IndexPreservingGetter a Text
shown = to (pack . show)

-- | The difference either up or down of a stock price from one day to the
-- next.
data UpDown = UpDown
    { _udDate :: !Day
    , _udUp   :: !Double
    , _udDown :: !Double
    }
    deriving Show
makeClassy ''UpDown

-- | Convert a stream of stock prices to a stream of up/down values.
--
-- You must provide the names of the date and adjusted close price fields.
stocksToUpDown :: Monad m
               => Getter stock Day    -- ^ date field
               -> Getter stock Double -- ^ adjusted close
               -> Conduit stock m UpDown
stocksToUpDown stockDate stockAdjClose =
    await >>= maybe (return ()) loop
  where
    loop today = do
        myesterday <- await
        case myesterday of
            Nothing -> return ()
            Just yesterday -> do
                let ud = UpDown
                        { _udDate = today ^. stockDate
                        , _udUp = max 0 $ (today ^. stockAdjClose) - (yesterday ^. stockAdjClose)
                        , _udDown = max 0 $ (yesterday ^. stockAdjClose) - (today ^. stockAdjClose)
                        }
                yield ud
                loop yesterday
