module Screens.WelcomeScreen.ScreenData where

import MerchantConfig.DefaultConfig as DC
import Screens.Types (WelcomeScreenStage(..), WelcomeScreenState)

initData :: WelcomeScreenState
initData = {
  data : {
    carouselModel : [
      {image : "ny_ic_welcome_screen_1", title : "Direct payment.\nNo commissions", description : "Customer pays to you directly via \ncash or UPI"},
      {image : "ny_ic_welcome_screen_2", title : "100% of the fare\ngoes to you!", description : "The fare shown is the fare you get.\nNo hidden charges."},
      {image : "ny_ic_welcome_screen_3", title : "Be a part of the Open\nMobility Revolution!", description : "Our data and product roadmap are\ntransparent for all."}
    ],
    config : DC.config
  },
  props : {
    selectedLanguage: "EN_US",
    currentStage : DETECT_LOCATION
  }
}