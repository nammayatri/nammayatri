module Components.GenericMessageModal.Controller where

import Prelude
import Components.PrimaryButton.Controller as PrimaryButtonController

data Action = PrimaryButtonActionController PrimaryButtonController.Action

type Config = {
        text :: String
        , buttonText :: String
        , openGenericMessageModal :: Boolean
}

config :: Config
config = {
    text : ""
    , buttonText : ""
    , openGenericMessageModal : false
}