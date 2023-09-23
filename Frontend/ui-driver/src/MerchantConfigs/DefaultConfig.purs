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
  },
  subscriptionConfig : {
    enableBlocking : false,
    completePaymentPopup : false,
    onBoardingSubscription : false
  },
  rideActionModelConfig : {
    showVehicleVariant : true
  }
  }
