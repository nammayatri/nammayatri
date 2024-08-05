module MerchantConfig.DefaultConfig where

import MerchantConfig.Types
import Common.DefaultConfig
import Common.Types.Config as CTC
import Engineering.Helpers.Commons as EHC

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
    showFeeBreakup : true,
    noChargesTillDate : "Oct 1st 2024-*$*-ಅಕ್ಟೋಬರ್ 01, ರವರೆಗೆ-*$*-1 अक्टूबर 2024-*$*-১লা অক্টোবর, ২০২৪-*$*-ഒക്ടോബര്‍ 1, 2024-*$*-1 அக்டோபர் 2024-*$*-1 అక్టోబర్ 2024",
    lowestFeesFromDate : "Oct 2nd 2024-*$*-ಅಕ್ಟೋಬರ್ 2, 2024-*$*-2 अक्टूबर 2024-*$*-২য় অক্টোবর, ২০২৪-*$*-രണ്ടാം ഒക്ടോബര്‍, 2024-*$*-அக்டோபர் 2, 2024-*$*-అక్టోబరు 2, 2024"
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
    thresholdTime : 180,
    routeDistance : 30,
    diffBtwTwoHeartBeats : 10,
    straightLineDist : 0.015
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
          showNew : true
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
              enableAdvancedBooking : true,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 12.971599,
              cityLong : 77.594566,
              supportNumber : "",
              languageKey : "KN_IN",
              showDriverReferral : true,
              showCustomerReferral : false,
              uploadRCandDL : true,
              enableYatriCoins : true,
              vehicleNSImg : "ny_ic_auto_image",
              registration : registrationConfig,
              variantSubscriptionConfig : {
                enableVariantBasedSubscription : true,
                variantList : ["AutoCategory"],
                enableCabsSubscriptionView : false,
                staticViewPlans : getStaticViewPlans
              },
              showEarningSection : true,
              referral : {
                  domain : "https://nammayatri.in"
                , customerAppId : "in.juspay.nammayatri"
                , driverAppId : "in.juspay.nammayatripartner"
              },
              waitingCharges : 1.50,
              waitingChargesConfig : defWaitingChargesConfig,
              rentalWaitingChargesConfig : defRentalWaitingChargesConfig,
              gstPercentage : "18",
              rateCardConfig : { showLearnMore : true, learnMoreVideoLink : "https://www.youtube.com/shorts/NUTNKPzslpw" },
              assets :{
                auto_image : "ny_ic_auto_side_view",
                onboarding_auto_image : "ny_ic_auto_side"
              },
              enableHvSdk : true
            },
            {
              cityName : "Hyderabad",
              mapImage : "ny_ic_hyderabad_map",
              cityCode : "std:040",
              showSubscriptions : false,
              enableAdvancedBooking : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 17.387140,
              cityLong : 78.491684,
              supportNumber : "+918069724900",
              languageKey : "TE_IN",
              showDriverReferral : true,
              showCustomerReferral : false,
              uploadRCandDL : true,
              enableYatriCoins : false,
              vehicleNSImg : "ny_ic_auto_image_old",
              registration : registrationConfig,
              variantSubscriptionConfig : {
                enableVariantBasedSubscription : false,
                variantList : [],
                enableCabsSubscriptionView : false,
                staticViewPlans : getStaticViewPlans
              },
              showEarningSection : true,
              referral : {
                  domain : "https://www.manayatri.in"
                , customerAppId : "in.mobility.manayatri"
                , driverAppId : "in.mobility.manayatripartner"
              },
              waitingCharges : 2.00,
              waitingChargesConfig : defWaitingChargesConfig,
              rentalWaitingChargesConfig : defRentalWaitingChargesConfig,
              gstPercentage : "18",
              rateCardConfig : defRateCardConfig,
              assets :{
                auto_image :  "ny_ic_black_yellow_auto_side_view",
                onboarding_auto_image : "ny_ic_auto_right_side_yellow" 
              },
              enableHvSdk : false
            },
            {
              cityName : "Mysore",
              mapImage : "ny_ic_mysuru_map",
              cityCode : "std:0821",
              showSubscriptions : false,
              enableAdvancedBooking : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 12.295810,
              cityLong : 76.639381,
              supportNumber : "",
              languageKey : "KN_IN",
              showDriverReferral : true,
              showCustomerReferral : false,
              uploadRCandDL : true,
              enableYatriCoins : false,
              vehicleNSImg : "ny_ic_auto_image",
              registration : registrationConfig,
              variantSubscriptionConfig : {
                enableVariantBasedSubscription : false,
                variantList : [],
                enableCabsSubscriptionView : false,
                staticViewPlans : getStaticViewPlans
              },
              showEarningSection : true,
              referral : {
                  domain : "https://nammayatri.in"
                , customerAppId : "in.juspay.nammayatri"
                , driverAppId : "in.juspay.nammayatripartner"
              },
              waitingCharges : 1.50,
              waitingChargesConfig : defWaitingChargesConfig,
              rentalWaitingChargesConfig : defRentalWaitingChargesConfig,
              gstPercentage : "18",
              rateCardConfig : { showLearnMore : true, learnMoreVideoLink : "https://www.youtube.com/shorts/NUTNKPzslpw" },
              assets :{
                auto_image : "ny_ic_auto_side_view",
                onboarding_auto_image : "ny_ic_auto_side"
              },
              enableHvSdk : false
            },
            {
              cityName : "Delhi",
              mapImage : "ny_ic_delhi_map",
              cityCode : "std:011",
              showSubscriptions : false,
              enableAdvancedBooking : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 28.644800,
              cityLong : 77.216721,
              supportNumber : "+918069724848",
              languageKey : "HI_IN",
              showDriverReferral : true,
              showCustomerReferral : false,
              uploadRCandDL : true,
              enableYatriCoins : false,
              vehicleNSImg : "ny_ic_auto_image",
              registration : registrationConfig,
              variantSubscriptionConfig : {
                enableVariantBasedSubscription : false,
                variantList : [],
                enableCabsSubscriptionView : false,
                staticViewPlans : getStaticViewPlans
              },
              showEarningSection : true,
              referral : {
                  domain : "https://nammayatri.in"
                , customerAppId : "in.juspay.nammayatri"
                , driverAppId : "in.juspay.nammayatripartner"
              },
              waitingCharges : 0.75,
              waitingChargesConfig : defWaitingChargesConfig,
              rentalWaitingChargesConfig : defRentalWaitingChargesConfig,
              gstPercentage : "18",
              rateCardConfig : defRateCardConfig,
              assets :{
                auto_image : "ny_ic_auto_side_view",
                onboarding_auto_image : "ny_ic_auto_side"
              },
              enableHvSdk : false
            },
            {
              cityName : "Chennai",
              mapImage : "ny_ic_chennai_map",
              cityCode : "std:044",
              showSubscriptions : false,
              enableAdvancedBooking : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 13.067439,
              cityLong : 80.237617,
              supportNumber : "08069724899",
              languageKey : "TA_IN",
              showDriverReferral : true,
              showCustomerReferral : false,
              uploadRCandDL : false,
              enableYatriCoins : false,
              vehicleNSImg : "ny_ic_auto_image_old",
              registration : registrationConfig,
              variantSubscriptionConfig : {
                enableVariantBasedSubscription : true,
                variantList : [],
                enableCabsSubscriptionView : false,
                staticViewPlans : getStaticViewPlans
              },
              showEarningSection : false,
              referral : {
                  domain : "https://nammayatri.in"
                , customerAppId : "in.juspay.nammayatri"
                , driverAppId : "in.juspay.nammayatripartner"
              },
              waitingCharges : 1.00,
              waitingChargesConfig : defWaitingChargesConfig,
              rentalWaitingChargesConfig : defRentalWaitingChargesConfig,
              gstPercentage : "18",
              rateCardConfig : defRateCardConfig,
              assets :{
                auto_image :  "ny_ic_black_yellow_auto_side_view",
                onboarding_auto_image : "ny_ic_auto_right_side_yellow"
              },
              enableHvSdk : false
            },
            {
              cityName : "Coimbatore",
              mapImage : "ny_ic_coimbatore_map",
              cityCode : "std:0422",
              showSubscriptions : false,
              enableAdvancedBooking : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 11.004556,
              cityLong : 76.961632,
              supportNumber : "",
              languageKey : "TA_IN",
              showDriverReferral : true,
              showCustomerReferral : false,
              uploadRCandDL : true,
              enableYatriCoins : false,
              vehicleNSImg : "ny_ic_auto_image",
              registration : registrationConfig,
              variantSubscriptionConfig : {
                enableVariantBasedSubscription : false,
                variantList : [],
                enableCabsSubscriptionView : false,
                staticViewPlans : getStaticViewPlans
              },
              showEarningSection : true,
              referral : {
                  domain : "https://nammayatri.in"
                , customerAppId : "in.juspay.nammayatri"
                , driverAppId : "in.juspay.nammayatripartner"
              },
              waitingCharges : 1.50,
              waitingChargesConfig : defWaitingChargesConfig,
              rentalWaitingChargesConfig : defRentalWaitingChargesConfig,
              gstPercentage : "18",
              rateCardConfig : defRateCardConfig,
              assets :{
                auto_image :  "ny_ic_black_yellow_auto_side_view",
                onboarding_auto_image : "ny_ic_auto_right_side_yellow"
              },
              enableHvSdk : false
            },
            {
              cityName : "Puducherry",
              mapImage : "ny_ic_puducherry_map",
              cityCode : "std:0413",
              showSubscriptions : false,
              enableAdvancedBooking : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 11.943852,
              cityLong : 79.808292,
              supportNumber : "08069724899",
              languageKey : "TA_IN",
              showDriverReferral : true,
              showCustomerReferral : false,
              uploadRCandDL : true,
              enableYatriCoins : false,
              vehicleNSImg : "ny_ic_auto_image",
              registration : registrationConfig,
              variantSubscriptionConfig : {
                enableVariantBasedSubscription : false,
                variantList : [],
                enableCabsSubscriptionView : false,
                staticViewPlans : getStaticViewPlans
              },
              showEarningSection : false,
              referral : {
                  domain : "https://nammayatri.in"
                , customerAppId : "in.juspay.nammayatri"
                , driverAppId : "in.juspay.nammayatripartner"
              },
              waitingCharges : 1.50,
              waitingChargesConfig : defWaitingChargesConfig,
              rentalWaitingChargesConfig : defRentalWaitingChargesConfig,
              gstPercentage : "18",
              rateCardConfig : defRateCardConfig,
              assets :{
                auto_image :  "ny_ic_black_yellow_auto_side_view",
                onboarding_auto_image : "ny_ic_auto_right_side_yellow"
              },
              enableHvSdk : false
            },
            {
              cityName : "Gurugram",
              mapImage : "ny_ic_gurugram_map",
              cityCode : "std:0124",
              showSubscriptions : false,
              enableAdvancedBooking : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 28.457523,
              cityLong : 77.026344,
              supportNumber : "",
              languageKey : "HI_IN",
              showDriverReferral : true,
              showCustomerReferral : true,
              uploadRCandDL : true,
              enableYatriCoins : false,
              vehicleNSImg : "ny_ic_auto_image",
              registration : registrationConfig,
              variantSubscriptionConfig : {
                enableVariantBasedSubscription : false,
                variantList : [],
                enableCabsSubscriptionView : false,
                staticViewPlans : getStaticViewPlans
              },
              showEarningSection : true,
              referral : {
                  domain : "https://nammayatri.in"
                , customerAppId : "in.juspay.nammayatri"
                , driverAppId : "in.juspay.nammayatripartner"
              },
              waitingCharges : 1.50,
              waitingChargesConfig : defWaitingChargesConfig,
              rentalWaitingChargesConfig : defRentalWaitingChargesConfig,
              gstPercentage : "18",
              rateCardConfig : defRateCardConfig,
              assets :{
                auto_image : "ny_ic_auto_side_view",
                onboarding_auto_image : "ny_ic_auto_side"
              },
              enableHvSdk : false
            },
            {             
              cityName : "Noida",
              mapImage : "ny_ic_noida_map",
              cityCode : "std:01189",
              showSubscriptions : false,
              enableAdvancedBooking : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 28.535517,
              cityLong : 77.391029,
              supportNumber : "",
              languageKey : "HI_IN",
              showDriverReferral : true,
              showCustomerReferral : true,
              uploadRCandDL : true,
              enableYatriCoins : false,
              vehicleNSImg : "ny_ic_auto_image",
              registration : registrationConfig,
              variantSubscriptionConfig : {
                enableVariantBasedSubscription : false,
                variantList : [],
                enableCabsSubscriptionView : false,
                staticViewPlans : getStaticViewPlans
              },
              showEarningSection : true,
              referral : {
                  domain : "https://nammayatri.in"
                , customerAppId : "in.juspay.nammayatri"
                , driverAppId : "in.juspay.nammayatripartner"
              },
              waitingCharges : 1.50,
              waitingChargesConfig : defWaitingChargesConfig,
              rentalWaitingChargesConfig : defRentalWaitingChargesConfig,
              gstPercentage : "18",
              rateCardConfig : defRateCardConfig,
              assets :{
                auto_image : "ny_ic_auto_side_view",
                onboarding_auto_image : "ny_ic_auto_side"
              },
              enableHvSdk : false
            },
            {
              cityName : "TamilNaduCities",
              mapImage : "ny_ic_tamilnadu_map",
              cityCode :  "std:0422",
              showSubscriptions : false,
              enableAdvancedBooking : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 11.1271,
              cityLong : 78.6569,
              supportNumber : "08069724899",
              languageKey : "TA_IN",
              showDriverReferral : true,
              showCustomerReferral : true,
              uploadRCandDL : false,
              enableYatriCoins : false,
              vehicleNSImg : "ny_ic_auto_image_old",
              registration : registrationConfig,
              variantSubscriptionConfig : {
                enableVariantBasedSubscription : false,
                variantList : [],
                enableCabsSubscriptionView : false,
                staticViewPlans : getStaticViewPlans
              },
              showEarningSection : false,
              referral : {
                  domain : "https://nammayatri.in"
                , customerAppId : "in.juspay.nammayatri"
                , driverAppId : "in.juspay.nammayatripartner"
              },
              waitingCharges : 1.50,
              waitingChargesConfig : defWaitingChargesConfig,
              rentalWaitingChargesConfig : defRentalWaitingChargesConfig,
              gstPercentage : "18",
              rateCardConfig : defRateCardConfig,
              assets :{
                auto_image :  "ny_ic_black_yellow_auto_side_view",
                onboarding_auto_image : "ny_ic_auto_right_side_yellow"
              },
              enableHvSdk : false
            },
            {
              cityName : "Kolkata",
              mapImage : "",
              cityCode : "std:033",
              showSubscriptions : true,
              enableAdvancedBooking : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 22.5354064,
              cityLong : 88.2649516,
              supportNumber : "",
              languageKey : "BN_IN",
              showDriverReferral : true,
              showCustomerReferral : true,
              uploadRCandDL : true, 
              enableYatriCoins : false,
              vehicleNSImg : "",
              registration : {
                  supportWAN : "",
                  callSupport : false,
                  whatsappSupport : false
              },
              variantSubscriptionConfig : {
                enableVariantBasedSubscription : false,
                variantList : [],
                enableCabsSubscriptionView : false,
                staticViewPlans : getStaticViewPlans
              },
              showEarningSection : true,
              referral : {
                  domain : "https://www.yatrisathi.in"
                , customerAppId : "in.juspay.jatrisaathi"
                , driverAppId : "in.juspay.jatrisaathidriver"
              },
              waitingCharges : 1.50,
              waitingChargesConfig : defWaitingChargesConfig,
              rentalWaitingChargesConfig : defRentalWaitingChargesConfig,
              gstPercentage : "18",
              rateCardConfig : defRateCardConfig,
              assets :{
                auto_image :  "ny_ic_black_yellow_auto_side_view",
                onboarding_auto_image : "ny_ic_auto_side"
              },
              enableHvSdk : false
            }, 
            {
              cityName : "Kochi",
              mapImage : "ny_ic_kochi_map",
              cityCode : "std:0484",
              showSubscriptions : true,
              cityLat : 9.931233,
              cityLong : 76.267303,
              enableAdvancedBooking: false,
              advancedRidePopUpYoutubeLink: "",
              callDriverInfoPost: false,
              supportNumber : "",
              languageKey : "ML_IN",
              showDriverReferral : true,
              showCustomerReferral : true,
              uploadRCandDL : true,
              enableYatriCoins : true,
              vehicleNSImg : "ny_ic_auto_image",
              registration : {
                  supportWAN : "918618963188",
                  callSupport : true,
                  whatsappSupport : true
              },
              variantSubscriptionConfig : {
                enableVariantBasedSubscription : true,
                variantList : [],
                enableCabsSubscriptionView : true,
                staticViewPlans : getStaticViewPlans
              },
              showEarningSection : true,
              referral : {
                domain : "https://nammayatri.in"
              , customerAppId : "in.juspay.nammayatri"
              , driverAppId : "in.juspay.nammayatripartner"
            },
            waitingCharges : 1.00,
            waitingChargesConfig : defWaitingChargesConfig,
            rentalWaitingChargesConfig : defRentalWaitingChargesConfig,
            gstPercentage : "18",
            rateCardConfig : defRateCardConfig,
            assets :{
              auto_image : "ic_auto_rickshaw",
              onboarding_auto_image : "ny_ic_auto_right_side_black"
            },
            enableHvSdk : false
          }
            --, For future use
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
  , termsLink : "https://docs.google.com/document/d/1K68xvtReD9FVpx-IshtKNMt4baQNgKXt"
  , termsVersion : 1.0
  , privacyLink : "https://docs.google.com/document/d/1tF96MwtaEiq70y_P40E29Sy3X61moTc9"
  , navigationAppConfig : defaultNavigationAppConfig
  , genericHeaderConfig : defaultGenericHeader
  , currency: "₹"
  , internationalNumberEnabled : false
  , countryCodeConfig : []
  , feature : {
    enableBonus : false
  , enableImageUpload : true
  , enableGender: false
  , enableOtpRide: false
  , enableSuggestions : true
  , enableYatriCoins : false
  , enableAutoReferral : true
  , enableSpecialPickup : EHC.jBridgeMethodExists "locateOnMapV2"
  , enableInterOperability : true
  }
  , showCorporateAddress : false
  , engilshInNative: "English"
  , allowAllMobileNumber: false
  , vehicle : {
    validationPrefix : "KA|AP|TS|DL|TN|PY|UP|HR"
  }
  , appData : defaultAppData
  , banners :{
    autoPay : true
  }
  ,referral: {
      "type" : "QRScreen"
    , link : "https://nammayatri.in/link/rider/mvnw"
    , customerAppId : "in.juspay.nammayatri"
    , driverAppId : "in.juspay.nammayatripartner"
    }
  , dashboard : {
      enable : false,
      url : ""
    }
  , logFunctionCalls : false
  , rideRequest : {
      negotiationUnit : {
        cab : "10",
        auto : "10"
      }
    }
  , coinsConfig : {
      minCoinSliderValue : 250,
      maxCoinSliderValue : 2500,
      stepFunctionForCoinConversion : 250,
      twoRidesCompletedThresholdForCoins : "2",
      fiveRidesCompletedThresholdForCoins : "5",
      tenRidesCompletedThresholdForCoins : "10",
      numOfRideThresholdForCoins : "8+",
      leaderBoardThresholdForCoins : "+500",
      customerReferralCoins : "+200",
      twoPlusRidesCoins : "+10",
      fivePlusRidesCoins : "+30",
      eightPlusRidesCoins : "+50",
      tenPlusRidesCoins : "+60",
      purpleRideCoins : "+5",
      rideCompletedCoins : "+1",
      fiveStarRatingCoins : "+1",
      oneOrTwoStarRatingCoins : "-1",
      rideCancellationCoins : "-5",
      whatAreYatriCoinFAQ : "",
      coinTermsAndConditions : "https://docs.google.com/document/d/1tF96MwtaEiq70y_P40E29Sy3X61moTc9",
      howToEarnYatriCoinFAQ : "",
      howToRedeemYatriCoinFAQ : "",
      rideCompletedCoinEvent : false,
      twoRideCoinEvent : false,
      fiveRideCoinEvent : false,
      eightRideCoinEvent : false,
      tenRideCoinEvent : false,
      prupleRideCoinEvent : false,
      bookingCancelCoinEvent : false,
      fiveStarCoinEvent : false,
      oneTwoStarCoinEvent : false,
      driverToCustomerRefCoinEvent : false,
      coinConversionPopupLottie : "",
      driverToCustomerRefPopupEndDate : "",
      monsoonOfferDate : "",
      coinsValidTill : 150
  }
  , inAppKeyboardModalConfig : {
      enableDeviceKeyboard : true
    }
  , chooseCity : {
      straightLineDistLogic : false
  }
  , bannerCarousel : defaultBannerCarousel
  , safetyRide : {
    startTime : "21:00:00"
  , endTime : "06:00:00"
  }
  , appUpdatePopupUrl : "https://play.google.com/store/apps/details?id=in.juspay.nammayatripartner&pcampaignid=web_share"
  , showProfileAadhaarPan : false
}

registrationConfig :: CTC.RegistrationConfig
registrationConfig = {
  supportWAN : "919625724848",
  callSupport : true,
  whatsappSupport : false
}

getStaticViewPlans :: Array CTC.StaticViewPlans
getStaticViewPlans = [
  {price : 45.0, frequency : "PER_DAY", variantCategory : "CarCategory", name : "DAILY_UNLIMITED", introductoryOffer : "FREE_RIDE_OFFER", showSelected : false, planDesc : "CAB_DAILY_UNLIMITED_OFFER"},
  {price : 9.0, frequency : "PER_RIDE", variantCategory : "CarCategory", name : "DAILY_PER_RIDE", introductoryOffer : "", showSelected : false, planDesc : "CAB_DAILY_PER_RIDE_OFFER"},
  {price : 25.0, frequency : "PER_DAY", variantCategory : "AutoCategory", name : "DAILY_UNLIMITED", introductoryOffer : "NO_CHARGES_TILL", showSelected : true, planDesc : ""}
]

defWaitingChargesConfig :: CTC.WaitingChargesConfig
defWaitingChargesConfig = {
  cab : {
    freeSeconds : 300,
    perMinCharges : 1.50
  },
  auto : {
    freeSeconds : 180,
    perMinCharges : 1.50
  },
  bike : {
    freeSeconds : 180,
    perMinCharges : 1.50
  }
}

defRentalWaitingChargesConfig :: CTC.WaitingChargesConfig
defRentalWaitingChargesConfig = {
  cab : {
    freeSeconds : 180,
    perMinCharges : 2.0
  },
  auto : {
    freeSeconds : 180,
    perMinCharges : 2.0
  },
  bike: {
    freeSeconds : 180,
    perMinCharges : 1.5
  }
}

defRateCardConfig :: CTC.RateCardConfig
defRateCardConfig = {
  showLearnMore : false,
  learnMoreVideoLink : ""
}