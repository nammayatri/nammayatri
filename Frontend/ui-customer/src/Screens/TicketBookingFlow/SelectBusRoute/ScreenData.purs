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

initData :: SelectBusRouteScreenState
initData = {
  data : {
      srcLoc : ""
    , destLoc : ""
    , quotes : Nothing
    , selectedQuote : Nothing
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
}

type SelectBusRouteScreenProp = {
    enableSeeRoute :: Boolean
}