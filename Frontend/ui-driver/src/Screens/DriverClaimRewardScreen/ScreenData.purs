module Screens.DriverClaimRewardScreen.ScreenData where

import Prelude
import Screens.Types (DriverClaimRewardScreenState)
import ConfigProvider
import Data.Maybe

initData :: DriverClaimRewardScreenState
initData = {
  data : {
    config: getAppConfig appConfig
    ,numberOfRides: Nothing
    ,safetyScore: Nothing
    ,rating: Nothing
    ,cancellationRateInWindow: Nothing
    ,driverTag: ""

  },
  props: { 
    showAllBenefits: false
  , showAllEligibility: false
  , showNominationView: false
  , showFaq: false
  , openFaqIndex: Nothing
  , openBenefitIndex: Nothing
  }
} 