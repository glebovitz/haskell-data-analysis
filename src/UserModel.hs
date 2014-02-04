{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module UserModel
       (module UserModel, module UserTypes,
        module DataAnalysis.Application.Import)
       where
import Data.CSV.Conduit.Persist
import UserTypes
import DataAnalysis.Application.Import
 
mkCsvPersist
  [persistCsv|
Stock invalidRows=stop format=csv
    date            Day "format=%F"
    open            Double
    high            Double
    low             Double
    close           Double
    volume          Int default=1000
    adjClose        Double
    deriving Show
|]