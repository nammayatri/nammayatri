module Components.RideTrackingModal.Controller where

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

data Action = NoAction | EndRide

type Config = {
    sourceStopName :: String,
    destinationStopName :: String,
    busNumber :: String,
    busType :: String,
    routeNumber :: String
}

config :: Config
config = {
    sourceStopName: "Source Stop",
    destinationStopName: "Destination Stop",
    busNumber: "Bus Number",
    busType: "Bus Type",
    routeNumber: "Route Number"
}