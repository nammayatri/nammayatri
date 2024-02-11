module Components.ChooseYourRide.Controller where

import Data.Maybe (Maybe(..))
import Components.ChooseVehicle.Controller as ChooseVehicleController
import Components.PrimaryButton.Controller as PrimaryButtonController
import ConfigProvider
import MerchantConfig.Types
import PrestoDOM
import Data.Map as Map
import Screens.Types as ST


data Action
  = NoAction
  | ChooseVehicleAC ChooseVehicleController.Action
  | PrimaryButtonActionController PrimaryButtonController.Action
  | PreferencesDropDown
  | RadioButtonClick Boolean
  | OnIconClick Boolean
  | ProviderClick String


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
    , filterFavProvider :: Boolean
    , selectedProvider :: String
    , selectedProviderQuote :: String
    , providersQuoteList :: Array ST.ProvidersQuote
    , iopEnabled :: Boolean
    , favProvider :: String
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
  , filterFavProvider : true
  , selectedProvider : ""
  , selectedProviderQuote : ""
  , providersQuoteList : []
  , iopEnabled : false
  , favProvider : ""
  }
