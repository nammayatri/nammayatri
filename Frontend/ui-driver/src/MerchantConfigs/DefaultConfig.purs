module MerchantConfig.DefaultConfig where

import MerchantConfig.Types
import Common.Types.Config

config :: AppConfig
config =
  { primaryTextColor: "#FCC32C"
  , primaryBackground: "#2C2F3A"
  , languageList:
      [ { name: "English", value: "EN_US", subtitle: "" }
      , { name: "ಕನ್ನಡ", value: "KN_IN", subtitle: "Kannada" }
      , { name: "हिंदी", value: "HI_IN", subtitle: "Hindi" }
      , { name: "தமிழ்", value: "TA_IN", subtitle: "Tamil" }
      ]
  , popupBackground : "#FFFFFF"
  , defaultLanguage : "EN_US"
  , leaderBoard :{
    isMaskedName : true
  }
  , rideCompletedCardConfig : {
      showSavedCommission : false
    }
  , subscriptionConfig : {
      enableBlocking : false,
      onBoardingSubscription : false,
      completePaymentPopup : false,
      showLaterButtonforTimeRange : false,
      offerBannerConfig : {
        showDUOfferBanner : false,
        offerBannerValidTill : "",
        offerBannerDeadline : "",
        offerBannerPlans : []
      },
      lowDuesLimit : 25.0,
      maxDuesLimit : 100.0,
      highDueWarningLimit : 75.0,
      moveDriverToOfflineInHighDueDaily : false,
      enableSubscriptionPopups : false,
      supportNumber : "",
      faqLink : "",
      whatsappSupportLink : "",
      myPlanYoutubeLink : "",
      overlayYoutubeLink : "",
      enableIntroductoryView : false,
      optionsMenuItems : {
        managePlan : false,
        paymentHistory : false,
        viewFaqs : false,
        callSupport : false,
        chatSupport : false,
        kioskLocation : false,
        viewAutopayDetails : false
      },
      gradientConfig : [],
      enableSubscriptionSupportPopup : false
  },
  showPaymentDetails : true,
  rideActionModelConfig : {
    showVehicleVariant : true
  }
  , profileVerification : {
      aadharVerificationRequired : false
    } 
  , gotoConfig : {
    enableGoto : false,
    maxGotoLocations : 5}
  , purpleRideConfig : {
      showPurpleVideos : false,
      visualImpairmentVideo : "",
      physicalImpairmentVideo : "",
      hearingImpairmentVideo : "",
      genericAccessibilityVideo : ""
  }
  , profile : { 
    bookingOptionMenuForTaxi : false
  }
  , waitTimeConfig : {
    enableWaitTime : true,
    thresholdDist : 0.05
  }
  , bottomNavConfig : {
      home : 
        { isVisible : true,
          showNew : false
        },
      rideHistory : 
        { isVisible : true,
          showNew : false
        },
      subscription : 
        { isVisible : false,
          showNew : false
        },
      referral : 
        { isVisible : true,
          showNew : false
        },
      notifications :
        { isVisible : true,
          showNew : false
        }
    }
  , mapConfig : 
      { animationDuration : 500
      }
  , colors : defaultColors
  , primaryButtonConfig : defaultPrimaryButtonConfig
  , fontConfig : defaultFontConfig
  , loaderConfig : defaultLoaderConfig
  , others : defaultOthers
  , navigationAppConfig : defaultNavigationAppConfig
  , genericHeaderConfig : defaultGenericHeader
  , currency: "₹"
  , internationalNumberEnabled : false
  , features : {
    enableBonus : false
  , enableImageUpload : true
  , enableGender: false
  , enableOtpRide: false
  }
  , showCorporateAddress : false
  , engilshInNative: "English"
  , allowAllMobileNumber: false
  , vehicle : {
    validationPrefix :  "KA|AP|TS|DL"
  }
  , appDatas : defaultAppData
  , banners :{
    autoPay : true
  }
  ,referral: {
    "type" : "QRScreen"
  , link : "https://nammayatri.in/link/rider/mvnw"
  }
}
