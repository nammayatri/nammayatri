module Screens.DriverClaimRewardScreen.ScreenData where

import Prelude
import Screens.Types (DriverClaimRewardScreenState)
import ConfigProvider
import Data.Maybe
import RemoteConfig.Types (DriverRewardConfig)

initData :: DriverClaimRewardScreenState
initData = {
  data : {
    config: getAppConfig appConfig
    ,numberOfRides: Nothing
    ,safetyScore: Nothing
    ,rating: Nothing
    ,cancellationRateInWindow: Nothing
    ,driverTag: ""
    , driverRewardConfig: dummyDriverRewardConfig
    , nyMemberProbationTill : Nothing
    , bannerData: {
      bannerItem: Nothing
    , currentBanner: 0
    , bannerScrollState: ""
    , currentPage: 0
    }

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

dummyDriverRewardConfig :: DriverRewardConfig
dummyDriverRewardConfig = {
  nominationViewConfig : {
    visibility : false
  , videoLink : ""
  , formLink : "https://forms.gle/t3E4mMSDNQbdS4me8"
  }
  , visibility : false
  , whatsappSupportNumber : "7795865480"
  , youtubeVideoLink : ""
  , termsAndConditionsLink : ""
  , claimButtonConfig : {
    visibility : false
  }
  , carouselVisibility : false
  , carousel : []
}