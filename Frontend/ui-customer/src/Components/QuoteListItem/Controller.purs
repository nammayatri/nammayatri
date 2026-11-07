{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.QuoteListItem.Controller where

import MerchantConfig.Types (AppConfig)
import Data.Maybe (Maybe(..))
import ConfigProvider
import Screens.Types (QuoteListItemState(..))
import Common.Types.App  (City(..))

data Action = Click QuoteListItemState
              | NoAction 
              | CountDown Int String String 
              | ConfirmRide
              | CancelAutoAssigning


config :: QuoteListItemState
config = {
   seconds : 15
  , id : ""  
  , timer : "-"
  , timeLeft : 0
  , driverRating : 4.0
  , profile : ""
  , price : "0"
  , vehicleType : "auto"
  , driverName : "Drive_Name"
  , vehicleImage : ""
  , serviceTierName : Nothing
  , selectedQuote : Nothing
  , appConfig : getAppConfig appConfig
  , city : AnyCity
  }