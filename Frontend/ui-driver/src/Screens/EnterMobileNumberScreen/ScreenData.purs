module Screens.EnterMobileNumberScreen.ScreenData where

import Screens.Types (EnterMobileNumberScreenState)

initData :: EnterMobileNumberScreenState
initData =  {
  data:  { mobileNumber : "" },
  props: {
    btnActive :false,
    isValid : false
    }
}