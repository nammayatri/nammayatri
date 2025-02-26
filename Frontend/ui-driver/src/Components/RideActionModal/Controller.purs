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
import Data.Maybe as Mb
import MerchantConfig.Types (AppConfig)
import Screens.Types as ST
import Helpers.Utils as HU
import Prelude (negate, ($),class Eq)
import Storage (KeyStore(..), getValueToLocalStore)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe 
import Services.API as API 
import Components.PrimaryButton as PB
import PrestoDOM (Margin(..))
import Styles.Colors as Color
import Language.Strings (getString)
import Language.Types (STR(..))
import Debug

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
            | SecondaryTextClick LearnMorePopUp
            | VisuallyImpairedCustomer
            | NoAction
            | ArrivedAtStop
            | GetFare
            | MoreDetails
            | StopActionButton PB.Action
            | ShowEndRideWithStops

type Config = { 
  startRideActive :: Boolean,
  arrivedStopActive :: Boolean,
  totalDistance :: String,
  customerName :: String,
  sourceAddress :: AddressConfig,
  destinationAddress :: Mb.Maybe AddressConfig,
  stopAddress :: Mb.Maybe AddressConfig,
  lastStopAddress :: Mb.Maybe AddressConfig,
  estimatedRideFare :: Int,
  notifiedCustomer :: Boolean,
  id :: String,
  buttonTimeOut :: Int,
  currentStage :: ST.HomeScreenStage,
  unReadMessages :: Boolean,
  vehicleType :: String,
  vehicleServiceTier :: String,
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
  distance ::  Int,
  parkingCharge :: Number,
  isDelivery :: Boolean,
  delivery :: Mb.Maybe DeliveryDetails,
  isSourceDetailsExpanded :: Boolean,
  isDestinationDetailsExpanded :: Boolean,
  stops :: Array API.Stop
}

type DeliveryDetails = {
  sender :: PersonAndDeliveryInfo,
  receiver :: PersonAndDeliveryInfo
}

type PersonAndDeliveryInfo = {
  name :: String,
  premises :: Maybe String,
  phoneNumber :: String,
  exophoneNumber :: Maybe String,
  instructions :: Maybe String
}

type AddressConfig = {
  titleText :: String,
  detailText :: String
}

data LearnMorePopUp = AccessibilityInfo | RentalInfo | NoInfo | IntercityInfo

derive instance genericLearnMorePopUp :: Generic LearnMorePopUp _
instance eqLearnMorePopUp :: Eq LearnMorePopUp where eq = genericEq

config :: Config
config = {
  startRideActive : false,
  arrivedStopActive : false,
  cityConfig : HU.getCityConfig (getAppConfig appConfig).cityConfig (getValueToLocalStore DRIVER_LOCATION),
  driverVehicle : "",
  totalDistance : "",
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
  estimatedRideFare : 0,
  notifiedCustomer : true,
  buttonTimeOut : 10,
  id : "buttonTimer",
  currentStage : ST.RideAccepted,
  unReadMessages : false,
  vehicleType : "",
  vehicleServiceTier : "",
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
  distance : 0,
  parkingCharge : 0.0,
  isDelivery : false,
  delivery : Nothing,
  isSourceDetailsExpanded : false,
  isDestinationDetailsExpanded : false,
  stops : []
}

stopActionButtonConfig :: Config -> PB.Config
stopActionButtonConfig state = do 
  let stopToDepart = HU.getStopToDepart state.stops
      text' = getString if isJust stopToDepart then RESUME_RIDE else ARRIVED_AT_STOP
      bgColor = if isJust stopToDepart then Color.green900 else Color.black700
  PB.config {
    textConfig { text = text', color = Color.white900 }
    , margin = Margin 16 0 16 8
    , background = bgColor
    , id = "StopActionButton"
    , enableRipple = true
  }