module Screens.AboutUsScreen.ScreenData where

import Screens.Types(AboutUsScreenState)

initData :: AboutUsScreenState
initData = {
  data:  { 
    versionNumber : ""
  },
  props: { 
    demoModePopup : false,
    enableConfirmPassword : false,
    enableDemoModeCount : 0
  }
}