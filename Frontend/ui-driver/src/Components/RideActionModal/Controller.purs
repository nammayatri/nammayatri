{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RideActionModal.Controller where
import Data.Maybe as Mb
import MerchantConfig.Types (AppConfig)
import MerchantConfig.DefaultConfig as DC
import Screens.Types (HomeScreenStage(..), DisabilityType(..))

data Action = StartRide 
            | EndRide 
            | CancelRide 
            | OnNavigate 
            | CallCustomer 
            | LocationTracking
            | ButtonTimer Int String String String
            | NotifyCustomer
            | MessageCustomer
            | TimerCallback String String Int
            | WaitingInfo
            | LoadMessages
            | SecondaryTextClick
            | VisuallyImpairedCustomer
            | NoAction

type Config = { 
  startRideActive :: Boolean,
  totalDistance :: String,
  customerName :: String,
  sourceAddress :: AddressConfig,
  destinationAddress :: AddressConfig,
  isDriverArrived :: Boolean,
  estimatedRideFare :: Int,
  notifiedCustomer :: Boolean,
  id :: String,
  buttonTimeOut :: Int,
  currentStage :: HomeScreenStage,
  unReadMessages :: Boolean,
  specialLocationTag :: Mb.Maybe String,
  waitTime :: String,
  isChatOpened :: Boolean,
  requestedVehicleVariant :: Mb.Maybe String,
  accessibilityTag :: Mb.Maybe DisabilityType,
  appConfig :: AppConfig,
  gotoTag :: Boolean
  }

type AddressConfig = {
  titleText :: String,
  detailText :: String
}

config :: Config
config = {
  startRideActive : false,
  totalDistance : "",
  customerName : "",
  sourceAddress : {
    titleText : "",
    detailText : ""
    },
  destinationAddress : {
  titleText : "",
  detailText : ""
  },
  isDriverArrived : true,
  estimatedRideFare : 0,
  notifiedCustomer : true,
  buttonTimeOut : 10,
  waitTime : "__",
  id : "buttonTimer",
  currentStage : RideAccepted,
  unReadMessages : false,
  specialLocationTag : Mb.Nothing,
  isChatOpened : false,
  requestedVehicleVariant : Mb.Nothing,
  accessibilityTag : Mb.Nothing,
  appConfig : DC.config,
  gotoTag : false
}