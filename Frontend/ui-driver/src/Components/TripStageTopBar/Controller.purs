module Components.TripStageTopBar.Controller where

import MerchantConfig.DefaultConfig (defaultCityConfig)
import MerchantConfig.Types (CityConfig)
import Services.API (BookingTypes(..))
import Data.Maybe 
import Prelude
import Screens.Types (ActiveRide, HomeScreenStage(..))
import Screens.HomeScreen.ScreenData (dummyRideData)

data Action = NoAction 
            | SwitchBookingStage BookingTypes
            | HelpAndSupportScreen

instance showAction :: Show Action where
  show (NoAction) = "NoAction"
  show (SwitchBookingStage var) = "SwitchBookingStage_" <> show var
  show (HelpAndSupportScreen) = "HelpAndSupportScreen"

type Config = {
    data :: {
        cityConfig :: CityConfig,
        advancedRideData :: Maybe ActiveRide,
        activeRide :: ActiveRide,
        linkedVehicleVariant :: String
    }
    , props :: {
        currentStage :: HomeScreenStage,
        bookingStage :: BookingTypes
    }
}

defaultConfig :: Config
defaultConfig = {
    data : {
        cityConfig : defaultCityConfig,
        advancedRideData : Nothing,
        activeRide : dummyRideData,
        linkedVehicleVariant : ""
    }
    , props : {
        currentStage : HomeScreen,
        bookingStage : CURRENT
    } 
}