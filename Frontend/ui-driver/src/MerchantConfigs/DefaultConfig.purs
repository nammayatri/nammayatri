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
      , { name : "తెలుగు", value: "TE_IN", subtitle : "Telugu"}
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
    showLaterButtonforTimeRange : false,
    onBoardingSubscription : false,
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
    gradientConfig : []
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
  , cityConfig : [
      {
        cityName : "Bangalore",
        mapImage : "ny_ic_bengalore_map,",
        cityCode : "std:080",
        showSubscriptions : true,
        cityLat : 12.971599,
        cityLong : 77.594566
      },
      {
        cityName : "Hyderabad",
        mapImage : "ny_ic_hyderabad_map,",
        cityCode : "std:040",
        showSubscriptions : false,
        cityLat : 17.387140,
        cityLong : 78.491684
      },
      {
        cityName : "Mysore",
        mapImage : "ny_ic_mysuru_map,",
        cityCode : "std:0821",
        showSubscriptions : false,
        cityLat : 12.295810,
        cityLong : 76.639381
      },
      {
        cityName : "Delhi",
        mapImage : "ny_ic_delhi_map,",
        cityCode : "std:011",
        showSubscriptions : false,
        cityLat : 28.644800,
        cityLong : 77.216721
      },
      {
        cityName : "Chennai",
        mapImage : "ny_ic_chennai_map,",
        cityCode : "std:044",
        showSubscriptions : false,
        cityLat : 13.067439,
        cityLong : 80.237617
      },
      {
        cityName : "Coimbatore",
        mapImage : "ny_ic_coimbatore_map,",
        cityCode : "std:0422",
        showSubscriptions : false,
        cityLat : 11.004556,
        cityLong : 76.961632
      }
    ]
}
