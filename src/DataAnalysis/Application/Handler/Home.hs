module DataAnalysis.Application.Handler.Home where

import Yesod

import DataAnalysis.Application.Foundation

getHomeR :: Handler Html
getHomeR = redirect ImportR
