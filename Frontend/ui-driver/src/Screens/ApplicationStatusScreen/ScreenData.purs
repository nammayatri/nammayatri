module Screens.ApplicationStatusScreen.ScreenData where

import Screens.Types

initData :: ApplicationStatusScreenState
initData = {
  data: {
    dlVerificationStatus : "",
    rcVerificationStatus : ""
    },
  props : {
      isSelected : true
  }
}