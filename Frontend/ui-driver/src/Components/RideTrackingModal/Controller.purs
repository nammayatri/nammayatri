module Components.RideTrackingModal.Controller where

import MerchantConfig.Types (CityConfig, AppConfig)
import Screens.Types as ST
import Helpers.Utils as HU
import Prelude (negate, ($),class Eq)
import Storage (KeyStore(..), getValueToLocalStore)
import Data.Maybe 

data Action = NoAction | EndRide

type Config = {
    sourceStopName :: String,
    destinationStopName :: String,
    busNumber :: String,
    busType :: String,
    driverBadgeName :: String,
    routeNumber :: String,
    isPrivateFlow :: Boolean
}

config :: Config
config = {
    sourceStopName: "",
    destinationStopName: "",
    busNumber: "",
    busType: "",
    routeNumber: "",
    isPrivateFlow : true,
    driverBadgeName : ""
}