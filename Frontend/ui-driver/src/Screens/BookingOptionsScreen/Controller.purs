module Screens.BookingOptionsScreen.Controller where

import Components.ChooseVehicle as ChooseVehicle
import Components.PrimaryButton as PrimaryButton
import Data.Array (filter, length, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Log (trackAppScreenRender)
import Prelude (class Show, map, pure, show, unit, (<>), (==), not, ($), (>))
import PrestoDOM (Eval, continue, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (BookingOptionsScreenState, VehicleP)
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Common.Types.App (LazyCheck(..))

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" "BookingOptionsScreen"
    BackPressed -> pure unit
    _ -> pure unit

data Action = BackPressed
            | AfterRender
            | ChooseVehicleAC ChooseVehicle.Action
            | PrimaryButtonAC PrimaryButton.Action

data ScreenOutput = GoBack | SelectCab BookingOptionsScreenState

eval :: Action -> BookingOptionsScreenState -> Eval Action ScreenOutput BookingOptionsScreenState
eval BackPressed state = exit GoBack
eval (ChooseVehicleAC (ChooseVehicle.OnSelect config)) state = do
  let updatedConfig = map (\item -> item{isSelected = if item.vehicleVariant == config.vehicleVariant then (not item.isSelected) else item.isSelected}) state.data.downgradeOptions
      -- btnActive = length (filter (\item -> item.isSelected) updatedConfig) > 0
  continue state{data{downgradeOptions = updatedConfig}, props {isBtnActive = true}}
eval (PrimaryButtonAC PrimaryButton.OnClick) state = exit $ SelectCab state
eval _ state = continue state

downgradeOptionsConfig :: Array VehicleP -> String -> ChooseVehicle.Config
downgradeOptionsConfig vehicles vehicleType =
  ChooseVehicle.config
    { vehicleImage = getVehicleImage vehicleType
    , isCheckBox = true
    , vehicleVariant = vehicleType
    , isBookingOption = true
    , capacity = getVehicleCapacity vehicleType Nothing
    , isSelected = (fromMaybe dummyVehicleP $ (filter (\item -> item.vehicleName == vehicleType) vehicles) !! 0 ).isSelected
    }

getVehicleCapacity :: String -> Maybe Int -> String
getVehicleCapacity vehicleType capacity = case vehicleType of
  "SEDAN" -> "Comfy, upto " <> (show (fromMaybe 4 capacity)) <> " people"
  "SUV" -> "Spacious, upto " <> (show (fromMaybe 6 capacity)) <> " people"
  "HATCHBACK" -> "Easy on wallet, upto " <> (show (fromMaybe 4 capacity)) <> " people"
  "TAXI_PLUS" -> "Comfy, upto " <>  (show (fromMaybe 4 capacity)) <> " people"
  "TAXI" -> "Economical, upto " <> (show (fromMaybe 4 capacity)) <> " people"
  _ -> "Comfy, upto " <> (show (fromMaybe 4 capacity)) <> " people"

getVehicleImage :: String -> String
getVehicleImage vehicleType = case vehicleType of
  "SEDAN" -> "ic_sedan," <> (getCommonAssetStoreLink FunctionCall) <> "ic_sedan.png"
  "SUV" -> "ic_suv," <> (getCommonAssetStoreLink FunctionCall) <> "ic_suv.png"
  "HATCHBACK" -> "ic_hatchback," <> (getCommonAssetStoreLink FunctionCall) <> "ic_hatchback.png"
  "TAXI" -> "ic_sedan_non_ac," <> (getCommonAssetStoreLink FunctionCall) <> "ic_sedan_non_ac.png"
  "TAXI_PLUS" -> "ic_sedan_ac," <> (getCommonAssetStoreLink FunctionCall) <> "ic_sedan_ac.png"
  _ -> "ic_sedan_ac," <> (getCommonAssetStoreLink FunctionCall) <> "ic_sedan_ac.png"

dummyVehicleP :: VehicleP
dummyVehicleP = {
  vehicleName : "",
  isSelected : false
}