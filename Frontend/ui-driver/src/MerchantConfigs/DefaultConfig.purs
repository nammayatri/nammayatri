module MerchantConfig.DefaultConfig where

import MerchantConfig.Types

config :: AppConfig
config =
  { primaryTextColor: "#FCC32C"
  , primaryBackground: "#2C2F3A"
  , fontType: "Assets"
  , languageList:
      [ { name: "English", value: "EN_US", subtitle: "" }
      , { name: "ಕನ್ನಡ", value: "KN_IN", subtitle: "Kannada" }
      , { name: "हिंदी", value: "HI_IN", subtitle: "Hindi" }
      , { name: "தமிழ்", value: "TA_IN", subtitle: "Tamil" }
      ]
  , popupBackground : "#FFFFFF"
  , defaultLanguage : "EN_US"
  , imageUploadOptional : false
  , leaderBoard :{
    isMaskedName : true
  }
  , rideCompletedCardConfig : {
      showSavedCommission : false
    }
  , subscriptionConfig : {
    enableBlocking : false,
    completePaymentPopup : false,
    onBoardingSubscription : false,
    showDUOfferBanner : false,
    offerBannerValidTill : "",
    offerBannerDeadline : "",
    currentPlanCacheExpTime : 3600,
    lowDuesLimit : 25.0,
    maxDuesLimit : 100.0,
    highDueWarningLimit : 75.0,
    moveDriverToOfflineInHighDueDaily : false
  },
  rideActionModelConfig : {
    showVehicleVariant : true
  }
  , profile :
      { bookingOptionMenuForTaxi : false
      }
  }
