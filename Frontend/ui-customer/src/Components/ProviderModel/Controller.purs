module Components.ProviderModel.Controller where

import PrestoDOM (Visibility(..))

data Action = FavClick Config

type Config = {
    isActive :: Boolean,
    pillsVisibility :: Visibility,
    id :: String,
    name :: String,
    logo :: String,
    selectButtonVisibility :: Visibility
}

config :: Config
config = {
    isActive : false,
    pillsVisibility : GONE,
    id : "",
    name : "",
    logo : "",
    selectButtonVisibility : GONE
}