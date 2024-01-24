module MerchantConfig.DefaultConfig where

import MerchantConfig.Types
import Common.DefaultConfig

config :: AppConfig
config =
  { primaryTextColor: "#FCC32C"
  , primaryBackground: "#2C2F3A"
  , languageList:
      [ { name: "English", value: "EN_US", subtitle: "" }
      , { name: "ಕನ್ನಡ", value: "KN_IN", subtitle: "Kannada" }
      , { name: "हिंदी", value: "HI_IN", subtitle: "Hindi" }
      , { name: "தமிழ்", value: "TA_IN", subtitle: "Tamil" }
      , { name : "తెలుగు", value: "TE_IN", subtitle : "Telugu"}
      ]
  , popupBackground : "#FFFFFF"
  , defaultLanguage : "EN_US"
  , leaderBoard :{
    isMaskedName : true
  }
  , rideCompletedCardConfig : {
      showSavedCommission : false,
      lottieQRAnim : false
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
    gradientConfig : [],
    enableSubscriptionSupportPopup : false,
    earnAmountInADay : 2500,
    showFeeBreakup : true
  },
  showPaymentDetails : true,
  enableDriverReferral : false,
  enableCustomerReferral : false,
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
    bookingOptionMenuForTaxi : false,
    showBookingOption : true
  , checkRCStatusForBookingOption : true 
  }
  , waitTimeConfig : {
    enableWaitTime : true,
    thresholdDist : 0.05,
    thresholdTime : 180
  }
  , bottomNavConfig : {
      home : 
        { isVisible : true,
          showNew : false
        },
      rideHistory : 
        { isVisible : false,
          showNew : false
        },
      driverEarnings : 
        { isVisible : true,
          showNew : false
        },
      subscription : 
        { isVisible : true,
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
  , unserviceableThreshold : 250.0
  , cityConfig : [
            {
              cityName : "Bangalore",
              mapImage : "ny_ic_bengalore_map",
              cityCode : "std:080",
              showSubscriptions : true,
              cityLat : 12.971599,
              cityLong : 77.594566,
              supportNumber : "",
              languageKey : "KN_IN",
              showDriverReferral : true,
              showCustomerReferral : false,
              uploadRCandDL : true,
              enableYatriCoins : true
            },
            {
              cityName : "Hyderabad",
              mapImage : "ny_ic_hyderabad_map",
              cityCode : "std:040",
              showSubscriptions : false,
              cityLat : 17.387140,
              cityLong : 78.491684,
              supportNumber : "+918069724900",
              languageKey : "TE_IN",
              showDriverReferral : true,
              showCustomerReferral : false,
              uploadRCandDL : true,
              enableYatriCoins : false
            },
            {
              cityName : "Mysore",
              mapImage : "ny_ic_mysuru_map",
              cityCode : "std:0821",
              showSubscriptions : false,
              cityLat : 12.295810,
              cityLong : 76.639381,
              supportNumber : "",
              languageKey : "KN_IN",
              showDriverReferral : true,
              showCustomerReferral : false,
              uploadRCandDL : true,
              enableYatriCoins : false
            },
            {
              cityName : "Delhi",
              mapImage : "ny_ic_delhi_map",
              cityCode : "std:011",
              showSubscriptions : false,
              cityLat : 28.644800,
              cityLong : 77.216721,
              supportNumber : "+918069724848",
              languageKey : "HI_IN",
              showDriverReferral : true,
              showCustomerReferral : false,
              uploadRCandDL : true,
              enableYatriCoins : false
            },
            {
              cityName : "Chennai",
              mapImage : "ny_ic_chennai_map",
              cityCode : "std:044",
              showSubscriptions : false,
              cityLat : 13.067439,
              cityLong : 80.237617,
              supportNumber : "08069724899",
              languageKey : "TA_IN",
              showDriverReferral : true,
              showCustomerReferral : false,
              uploadRCandDL : false,
              enableYatriCoins : false
            },
            {
              cityName : "Coimbatore",
              mapImage : "ny_ic_coimbatore_map",
              cityCode : "std:0422",
              showSubscriptions : false,
              cityLat : 11.004556,
              cityLong : 76.961632,
              supportNumber : "",
              languageKey : "TA_IN",
              showDriverReferral : true,
              showCustomerReferral : false,
              uploadRCandDL : true,
              enableYatriCoins : false
            },
            {
              cityName : "Puducherry",
              mapImage : "ny_ic_puducherry_map",
              cityCode : "std:0413",
              showSubscriptions : false,
              cityLat : 11.943852,
              cityLong : 79.808292,
              supportNumber : "08069724899",
              languageKey : "TA_IN",
              showDriverReferral : true,
              showCustomerReferral : false,
              uploadRCandDL : true,
              enableYatriCoins : false
            }--, For future use
            -- {
            --   cityName : "Madurai",
            --   mapImage : "ny_ic_madurai_map",
            --   cityCode : "std:0452",
            --   showSubscriptions : false,
            --   cityLat : 9.93069,
            --   cityLong : 78.11956,
            --   supportNumber : "",
            --   languageKey : "TA_IN",
            --   enableYatriCoins : false
            -- }
        ]
  , enableMockLocation : false
  , permissions : {
      locationPermission : false,
      notification : true
  }
  , flowConfig : {
      chooseCity : {
        runFlow : true,
        defCity : "Bangalore"
      }
  }
  , homeScreen : {
    specialRideOtpView : false,
    showGenderBanner : true
  }
  , colors : defaultColors
  , primaryButtonConfig : defaultPrimaryButtonConfig
  , fontConfig : defaultFontConfig
  , loaderConfig : defaultLoaderConfig
  , otpRegex :  "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
  , termsLink : "https://docs.google.com/document/d/1-oRR_oI8ncZRPZvFZEJZeCVQjTmXTmHA"
  , privacyLink : "https://docs.google.com/document/d/128VU80K5E1iz-x6QnP1R127m_lwmDO3F"
  , navigationAppConfig : defaultNavigationAppConfig
  , genericHeaderConfig : defaultGenericHeader
  , currency: "₹"
  , internationalNumberEnabled : false
  , feature : {
    enableBonus : false
  , enableImageUpload : true
  , enableGender: false
  , enableOtpRide: false
  , enableSuggestions : true
  , enableYatriCoins : false
  }
  , showCorporateAddress : false
  , engilshInNative: "English"
  , allowAllMobileNumber: false
  , vehicle : {
    validationPrefix :  "KA|AP|TS|DL|TN|PY"
  }
  , appData : defaultAppData
  , banners :{
    autoPay : true
  }
  ,referral: {
    "type" : "QRScreen"
  , link : "https://nammayatri.in/link/rider/mvnw"
  }
  , dashboard : {
      enable : false,
      url : ""
    }
  , logFunctionCalls : false
  , rideRequest : {
      negotiationUnit : {
        cab : "20",
        auto : "10"
      }
    }
  , coinsConfig : {
      minCoinSliderValue : 250,
      maxCoinSliderValue : 2500,
      numOfRideThresholdForCoins : "8+",
      leaderBoardThresholdForCoins : "+500",
      customerReferralCoins : "+200",
      eightPlusRidesCoins : "+25",
      purpleRideCoins : "+5",
      rideCompletedCoins : "+1",
      fiveStarRatingCoins : "+1",
      oneOrTwoStarRatingCoins : "-1",
      rideCancellationCoins : "-5",
      whatAreYatriCoinFAQ : "https://www.youtube.com/shorts/vt_Z4wu4otY",
      howToEarnYatriCoinFAQ : "https://www.youtube.com/shorts/teQyPdP1fRc",
      howToRedeemYatriCoinFAQ : "https://www.youtube.com/shorts/dU3XxAisGjo"
  }
  , inAppKeyboardModalConfig : {
      enableDeviceKeyboard : true
    }
  , chooseCity : {
      straightLineDistLogic : false
  }
}
