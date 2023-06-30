module Components.ChooseYourRide.Controller where

import Components.ChooseVehicle.Controller as ChooseVehicleController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.MenuButton as MenuButtonController
import PrestoDOM (Eval,continue)
import Data.Maybe(Maybe(..))

data Action
  = NoAction
  | ChooseVehicleAC ChooseVehicleController.Action
  | PrimaryButtonActionController PrimaryButtonController.Action
  | MenuButtonActionController MenuButtonController.Action
  | PrimaryButtonConfirmActionController PrimaryButtonController.Action
  | ShowPaymentMode
  | DisapperPaymentPage


type Config
  = { rideDistance :: String
    , rideDuration :: String
    , activeIndex :: Int
    , quoteList :: Array ChooseVehicleController.Config
    , modeOfPayment :: Maybe String
    , choosePaymentMode :: Boolean
    }

config :: Config
config =
  { rideDistance: ""
  , rideDuration: ""
  , activeIndex: 0
  , quoteList: []
  , modeOfPayment: Nothing
  , choosePaymentMode: false
  }
