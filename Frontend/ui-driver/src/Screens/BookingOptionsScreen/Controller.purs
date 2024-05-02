module Screens.BookingOptionsScreen.Controller where

import Components.ChooseVehicle as ChooseVehicle
import Components.PrimaryButton as PrimaryButton
import Data.Array (filter, length, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Log (trackAppScreenRender)
import Prelude (class Show, map, pure, show, unit, (<>), (==), not, ($), (>))
import PrestoDOM (Eval, update, continue, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (BookingOptionsScreenState, VehicleP, RidePreference)
import Common.Types.App (LazyCheck(..))
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Helpers.Utils (getVehicleVariantImage)
import Language.Strings (getString)
import Language.Types (STR(..))

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "BookingOptionsScreen"
    BackPressed -> pure unit
    _ -> pure unit

data Action
  = BackPressed
  | AfterRender
  | PrimaryButtonAC PrimaryButton.Action
  | ToggleRidePreference RidePreference

data ScreenOutput
  = GoBack
  | ChangeRidePreference BookingOptionsScreenState RidePreference

eval :: Action -> BookingOptionsScreenState -> Eval Action ScreenOutput BookingOptionsScreenState
eval BackPressed state = exit GoBack

eval (ToggleRidePreference service) state = exit $ ChangeRidePreference state service

eval _ state = update state

downgradeOptionsConfig :: Array VehicleP -> String -> ChooseVehicle.Config
downgradeOptionsConfig vehicles vehicleType =
  ChooseVehicle.config
    { vehicleImage = getVehicleVariantImage vehicleType
    , isCheckBox = true
    , vehicleVariant = vehicleType
    , isBookingOption = true
    , capacity = getVehicleCapacity vehicleType
    , isSelected = (fromMaybe dummyVehicleP $ (filter (\item -> item.vehicleName == vehicleType) vehicles) !! 0).isSelected
    }

getVehicleCapacity :: String -> String
getVehicleCapacity vehicleType = case vehicleType of
  "TAXI" -> getString ECONOMICAL <> " · " <> "4 " <> getString PEOPLE
  "SUV" -> getString SPACIOUS <> " · " <> "6 " <> getString PEOPLE
  _ -> getString COMFY <> " · " <> "4 " <> getString PEOPLE

dummyVehicleP :: VehicleP
dummyVehicleP =
  { vehicleName: ""
  , isSelected: false
  }
