module Components.ChooseYourRide.Controller where

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
    }

config :: Config
config =
  { rideDistance: ""
  , rideDuration: ""
  , activeIndex: 0
  , quoteList: []
  , showTollExtraCharges : DC.config.searchLocationConfig.showAdditionalChargesText
  }
