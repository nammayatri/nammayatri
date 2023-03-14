module Components.ReferralMobileNumber.Controller where

import Prelude
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.PrimaryButton.Controller as PrimaryButtonController

data Action =  OnBackClick
            | PrimaryEditTextActionController PrimaryEditTextController.Action
            | PrimaryButtonActionController PrimaryButtonController.Action

type Config = {
    isApplyButtonActive :: Boolean
    , referralNumber :: String
}

config :: Config
config = {
    isApplyButtonActive : false
    , referralNumber : ""
}