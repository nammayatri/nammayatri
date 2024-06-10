{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RideActionModal.Controller where

import ConfigProvider

import MerchantConfig.Types (CityConfig)
import Constants.Configs (dummyPrice, dummyDistance)
import Common.Types.Config as CTC
import Common.Types.App (Price(..), Distance(..))
import Data.Maybe as Mb
import MerchantConfig.Types (AppConfig)
import Screens.Types as ST
import Helpers.Utils as HU
import Prelude (negate, ($))
import Storage (KeyStore(..), getValueToLocalStore)
import Data.Maybe 

data Action = StartRide 
            | EndRide 
            | CancelRide 
            | OnNavigate 
            | CallCustomer 
            | LocationTracking
            | MessageCustomer
            | TimerCallback String String Int
            | WaitingInfo
            | LoadMessages
            | SecondaryTextClick
            | VisuallyImpairedCustomer
            | NoAction
            | ArrivedAtStop

type Config = { 
  startRideActive :: Boolean,
  arrivedStopActive :: Boolean,
  totalDistanceWithUnit :: Distance,
  customerName :: String,
  sourceAddress :: AddressConfig,
  destinationAddress :: Mb.Maybe AddressConfig,
  stopAddress :: Mb.Maybe AddressConfig,
  lastStopAddress :: Mb.Maybe AddressConfig,
  estimatedRideFareWithCurrency :: Price,
  notifiedCustomer :: Boolean,
  id :: String,
  buttonTimeOut :: Int,
  currentStage :: ST.HomeScreenStage,
  unReadMessages :: Boolean,
  specialLocationTag :: Mb.Maybe String,
  requestedVehicleVariant :: Mb.Maybe String,
  accessibilityTag :: Mb.Maybe ST.DisabilityType,
  appConfig :: AppConfig,
  gotoTag :: Boolean,
  waitTimeStatus :: ST.TimerStatus,
  waitTimeSeconds :: Int,
  rideType :: ST.TripType,
  rideScheduledTime :: Mb.Maybe String,
  rideStartTime :: Mb.Maybe String,
  startODOReading :: String,
  tripDuration :: Mb.Maybe String,
  durationTravelled :: String,
  rideStartRemainingTime :: Int,
  estimatedTollCharges :: Number,
  driverVehicle :: String,
  cityConfig :: CityConfig,
  capacity :: Mb.Maybe Int,
  serviceTierAndAC :: String,
  acRide :: Mb.Maybe Boolean,
  isAdvanced :: Boolean,
  bookingFromOtherPlatform :: Boolean,
  bapName :: String,
  isOdometerReadingsRequired :: Boolean,
  distance ::  Int
, parkingCharge :: Number
}

type AddressConfig = {
  titleText :: String,
  detailText :: String
}

config :: Config
config = {
  startRideActive : false,
  arrivedStopActive : false,
  cityConfig : HU.getCityConfig (getAppConfig appConfig).cityConfig (getValueToLocalStore DRIVER_LOCATION),
  driverVehicle : "",
  totalDistanceWithUnit : dummyDistance,
  customerName : "",
  sourceAddress : {
    titleText : "",
    detailText : ""
    },
  destinationAddress : Mb.Just ({
  titleText : "",
  detailText : ""
  }),
  stopAddress : Mb.Nothing,
  lastStopAddress : Mb.Nothing,
  estimatedRideFareWithCurrency : dummyPrice,
  notifiedCustomer : true,
  buttonTimeOut : 10,
  id : "buttonTimer",
  currentStage : ST.RideAccepted,
  unReadMessages : false,
  specialLocationTag : Mb.Nothing,
  requestedVehicleVariant : Mb.Nothing,
  accessibilityTag : Mb.Nothing,
  appConfig : getAppConfig appConfig,
  gotoTag : false,
  waitTimeStatus : ST.NoStatus,
  waitTimeSeconds : -1,
  rideType: ST.OneWay,
  rideScheduledTime : Mb.Nothing,
  startODOReading : "0",
  tripDuration : Mb.Nothing,
  durationTravelled : "0",
  rideStartRemainingTime : -1,
  rideStartTime : Mb.Nothing,
  estimatedTollCharges : 0.0,
  capacity : Mb.Nothing,
  serviceTierAndAC : "",
  acRide : Mb.Nothing,
  isAdvanced : false,
  bookingFromOtherPlatform : false,
  bapName : "",
  isOdometerReadingsRequired : false,
  distance : 0
, parkingCharge : 0.0
}
