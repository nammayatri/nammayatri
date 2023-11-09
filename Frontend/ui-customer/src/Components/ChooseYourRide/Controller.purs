module Components.ChooseYourRide.Controller where

import Data.Maybe (Maybe(..))
import Components.ChooseVehicle.Controller as ChooseVehicleController
import Components.PrimaryButton.Controller as PrimaryButtonController
import MerchantConfig.DefaultConfig as DC
import MerchantConfig.Types

data Action
  = NoAction
  | ChooseVehicleAC ChooseVehicleController.Action
  | PrimaryButtonActionController PrimaryButtonController.Action

type Config
  = { rideDistance :: String
    , rideDuration :: String
    , activeIndex :: Int
    , quoteList :: Array ChooseVehicleController.Config
    , showTollExtraCharges :: Boolean
    , nearByDrivers :: Maybe Int
    , isBusQuoteSelected :: Boolean
    , quantity :: Int
    }

config :: Config
config =
  { rideDistance: ""
  , rideDuration: ""
  , activeIndex: 0
  , quoteList: []
  , showTollExtraCharges : DC.config.searchLocationConfig.showAdditionalChargesText
  , nearByDrivers : Nothing
  , isBusQuoteSelected : true
  , quantity : 1
  }
