{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SelectBusRoute.ScreenData where

import ConfigProvider
import Screens.Types as ST
import Data.Maybe (Maybe(..))
import Services.API (FRFSRouteAPI(..), FrfsQuote(..))
import Foreign.Object (Object, empty)
import Foreign (Foreign)

initData :: SelectBusRouteScreenState
initData = {
  data : {
      srcLoc : ""
    , destLoc : ""
    , quotes : Nothing
    , selectedQuote : Nothing
    , logField : empty
    , cheapestRoute : Nothing
    , fastestRoute : Nothing
    , isSortByPillClicked : false
    , eta : []
  },
  props : {
    enableSeeRoute : false
  }
}

type SelectBusRouteScreenState = {
    data :: SelectBusRouteScreenData
  , props :: SelectBusRouteScreenProp
}

type SelectBusRouteScreenData = {
    srcLoc :: String
  , destLoc :: String
  , quotes :: Maybe (Array FrfsQuote)
  , selectedQuote :: Maybe FrfsQuote
  , logField :: Object Foreign
  , cheapestRoute :: Maybe String
  , fastestRoute :: Maybe String
  , isSortByPillClicked :: Boolean
  , eta :: Array EtaBasedOnRoute
}

type SelectBusRouteScreenProp = {
    enableSeeRoute :: Boolean
}

type EtaBasedOnRoute = {
    etas :: Maybe Int
  , routeCode :: String
}