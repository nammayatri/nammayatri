module Components.ChooseYourRide.Controller where

import Data.Maybe (Maybe(..))
import Components.ChooseVehicle.Controller as ChooseVehicleController
import Components.PrimaryButton.Controller as PrimaryButtonController
import ConfigProvider
import MerchantConfig.Types

data Action
  = NoAction
  | ChooseVehicleAC ChooseVehicleController.Action
  | PrimaryButtonActionController PrimaryButtonController.Action
  | PreferencesDropDown
  | RadioButtonClick Boolean
  | OnIconClick Boolean


type Config
  = { rideDistance :: String
    , rideDuration :: String
    , activeIndex :: Int
    , quoteList :: Array ChooseVehicleController.Config
    , showTollExtraCharges :: Boolean
    , nearByDrivers :: Maybe Int
    , showPreferences :: Boolean
    , bookingPreferenceEnabled :: Boolean
    , flowWithoutOffers :: Boolean
    }

config :: Config
config =
  { rideDistance: ""
  , rideDuration: ""
  , activeIndex: 0
  , quoteList: []
  , showTollExtraCharges : (getAppConfig appConfig).searchLocationConfig.showAdditionalChargesText
  , nearByDrivers : Nothing
  , showPreferences : false
  , bookingPreferenceEnabled : false
  , flowWithoutOffers : false
  }
