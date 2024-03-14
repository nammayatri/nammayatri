module Components.ProviderModel.Controller where

import PrestoDOM (Visibility(..))
import Components.PrimaryButton as PrimaryButton

data Action = FavClick Config
            | ButtonClick PrimaryButton.Action

type Config = {
    isActive :: Boolean,
    pillsVisibility :: Visibility,
    id :: String,
    name :: String,
    logo :: String,
    selectButtonVisibility :: Visibility,
    showExpandAnim :: Boolean,
    priceRange :: String,
    vehicleType :: String,
    capacity :: String,
    vehicleImage :: String

}

config :: Config
config = {
    isActive : false,
    pillsVisibility : GONE,
    id : "",
    name : "",
    logo : "",
    selectButtonVisibility : GONE,
    showExpandAnim : false,
    priceRange : "",
    vehicleType : "",
    capacity : "",
    vehicleImage : ""
}