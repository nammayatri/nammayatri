module Screens.DriverProfileScreen.ComponentConfig where

import Components.PopUpModal as PopUpModal
import Language.Strings
import Language.Types (STR(..))
import PrestoDOM
import Screens.Types as ST

logoutPopUp :: ST.DriverProfileScreenState -> PopUpModal.Config
logoutPopUp  state = let 
  config' = PopUpModal.config
  popUpConfig' = config' {
    primaryText {text = (getString LOGOUT)},
    secondaryText {text = (getString ARE_YOU_SURE_YOU_WANT_TO_LOGOUT)},
    option1 {text = (getString GO_BACK)},
    option2 {text = (getString LOGOUT)}
  }
  in popUpConfig'