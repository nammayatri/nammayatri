module MerchantConfig.DefaultConfig where

import MerchantConfig.Types
import Common.DefaultConfig
import Engineering.Helpers.Commons as EHC
import MerchantConfig.Utils as MU
import Common.Types.App as CTA
import Data.Maybe

config :: AppConfig
config =
  { primaryTextColor: "#FCC32C"
  , primaryBackground: "#2C2F3A"
  , primaryButtonBackground : "#2C2F3A"
  , buttonInactiveBackground : "#2C2F3A"
  , buttonInactiveTextColor : "#FCC32C"
  , alphaInPrimaryButtonAllowed : true
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
   , rateCardScreen : {
      showYoutubeVideo : true,
      showRateCard : true,
      showTollCharges : true,
      showDriverAdditions : true
    },
    rcLimit : 3
  , acExplanation : true
  , showMonthlyLeaderBoard : false
  , hotspotConfig : {
      veryHighHotspotColor : "#E55454",
      highHotspotColor : "#FFB800",
      moderateHotspotColor : "#AEC708",
      veryHighRange : 67.0,
      highRange : 33.0,
      circleRadius : 1000.0,
      centerDeviation : 0.0,
      showColorWithRelativeWeight : false,
      minCirclesNeededForSortedWeights : 10
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
    enableSubscriptionSupportPopup : true,
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
              enableGullak : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 12.971599,
              cityLong : 77.594566,
              supportNumber : "",
              languageKey : "KN_IN",
              showScheduledRides : true,
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
                onboarding_auto_image : "ny_ic_auto_side",
                empty_referral_auto : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
                empty_referral_cab : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
              },
              enableHvSdk : true,
              purpleRideConfig : {
                purpleRideConfigForAuto : {
                  vehicleVariant : "Auto",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForCabs : {
                  vehicleVariant : "Cab",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForBikes : {
                  vehicleVariant : "Bike",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                }
              },
              rideStartAudio : {
                acCab : {
                  tollAudio :  Just "https://assets.moving.tech/beckn/audios/toll_charges_background/kn.mp3",
                  acAudio : Just "https://assets.moving.tech/beckn/audios/ac_background/kn.mp3",
                  parkingAudio : Nothing,
                  defaultAudio : Nothing
                },
                nonAcCab : {
                  tollAudio : Just "https://assets.moving.tech/beckn/audios/toll_charges_background/kn.mp3",
                  acAudio : Just "https://assets.moving.tech/beckn/audios/non_ac_background/kn.mp3",
                  parkingAudio : Nothing,
                  defaultAudio : Nothing
                },
                auto : defaultStartAudioUrls,
                bike : defaultStartAudioUrls
              }
            },
            {
              cityName : "Hyderabad",
              mapImage : "ny_ic_hyderabad_map",
              cityCode : "std:040",
              showSubscriptions : false,
              enableAdvancedBooking : false,
              enableGullak : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 17.387140,
              cityLong : 78.491684,
              supportNumber : "+918069724900",
              languageKey : "TE_IN",
              showScheduledRides : false,
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
                onboarding_auto_image : "ny_ic_auto_right_side_yellow" ,
                empty_referral_auto : "ny_ic_refer_now_auto_my,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_my.png",
                empty_referral_cab : "ny_ic_refer_now_cab_my,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_my.png"
              },
              enableHvSdk : false,
              purpleRideConfig : {
                purpleRideConfigForAuto : {
                  vehicleVariant : "Auto",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForCabs : {
                  vehicleVariant : "Cab",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForBikes : {
                  vehicleVariant : "Bike",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                }
              },
              rideStartAudio : {
                acCab : {
                  tollAudio : Nothing,
                  acAudio : Nothing,
                  parkingAudio : Just "https://assets.moving.tech/beckn/audios/parking_charges_background/te.mp3",
                  defaultAudio : Nothing
                },
                nonAcCab : defaultStartAudioUrls,
                auto : defaultStartAudioUrls,
                bike : defaultStartAudioUrls
              }
            },
            {
              cityName : "Mysore",
              mapImage : "ny_ic_mysuru_map",
              cityCode : "std:0821",
              showSubscriptions : false,
              enableAdvancedBooking : false,
              enableGullak : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 12.295810,
              cityLong : 76.639381,
              supportNumber : "",
              languageKey : "KN_IN",
              showScheduledRides : false,
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
                onboarding_auto_image : "ny_ic_auto_side",
                empty_referral_auto : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
                empty_referral_cab : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
              },
              enableHvSdk : false,
              purpleRideConfig : {
                purpleRideConfigForAuto : {
                  vehicleVariant : "Auto",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForCabs : {
                  vehicleVariant : "Cab",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForBikes : {
                  vehicleVariant : "Bike",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                }
              },
              rideStartAudio : {
                acCab : defaultStartAudioUrls,
                nonAcCab : defaultStartAudioUrls,
                auto : defaultStartAudioUrls,
                bike : defaultStartAudioUrls
              }
            },
            {
              cityName : "Delhi",
              mapImage : "ny_ic_delhi_map",
              cityCode : "std:011",
              showSubscriptions : false,
              enableAdvancedBooking : false,
              enableGullak : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 28.644800,
              cityLong : 77.216721,
              supportNumber : "+918069724848",
              languageKey : "HI_IN",
              showScheduledRides : false,
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
                onboarding_auto_image : "ny_ic_auto_side",
                empty_referral_auto : "ny_ic_refer_now_auto_yatri_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_yatri_green.png",
                empty_referral_cab : "ny_ic_refer_now_cab_yatri,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_yatri.png"
              },
              enableHvSdk : false,
              purpleRideConfig : {
                purpleRideConfigForAuto : {
                  vehicleVariant : "Auto",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForCabs : {
                  vehicleVariant : "Cab",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForBikes : {
                  vehicleVariant : "Bike",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                }
              },
              rideStartAudio : {
                acCab : defaultStartAudioUrls,
                nonAcCab : defaultStartAudioUrls,
                auto : defaultStartAudioUrls,
                bike : defaultStartAudioUrls
              }
            },
            {
              cityName : "Chennai",
              mapImage : "ny_ic_chennai_map",
              cityCode : "std:044",
              showSubscriptions : false,
              enableAdvancedBooking : false,
              enableGullak : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 13.067439,
              cityLong : 80.237617,
              supportNumber : "08069724899",
              languageKey : "TA_IN",
              showScheduledRides : false,
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
                onboarding_auto_image : "ny_ic_auto_right_side_yellow",
                empty_referral_auto : "ny_ic_refer_now_auto_ny_yellow,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_yellow.png",
                empty_referral_cab : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
              },
              enableHvSdk : false,
              purpleRideConfig : {
                purpleRideConfigForAuto : {
                  vehicleVariant : "Auto",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForCabs : {
                  vehicleVariant : "Cab",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForBikes : {
                  vehicleVariant : "Bike",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                }
              },
              rideStartAudio : {
                acCab : defaultStartAudioUrls,
                nonAcCab : defaultStartAudioUrls,
                auto : defaultStartAudioUrls,
                bike : defaultStartAudioUrls
              }
            },
            {
              cityName : "Coimbatore",
              mapImage : "ny_ic_coimbatore_map",
              cityCode : "std:0422",
              showSubscriptions : false,
              enableAdvancedBooking : false,
              enableGullak : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 11.004556,
              cityLong : 76.961632,
              supportNumber : "",
              languageKey : "TA_IN",
              showScheduledRides : false,
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
                onboarding_auto_image : "ny_ic_auto_right_side_yellow",
                empty_referral_auto : "ny_ic_refer_now_auto_ny_yellow,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_yellow.png",
                empty_referral_cab : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
              },
              enableHvSdk : false,
              purpleRideConfig : {
                purpleRideConfigForAuto : {
                  vehicleVariant : "Auto",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForCabs : {
                  vehicleVariant : "Cab",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForBikes : {
                  vehicleVariant : "Bike",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                }
              },
              rideStartAudio : {
                acCab : defaultStartAudioUrls,
                nonAcCab : defaultStartAudioUrls,
                auto : defaultStartAudioUrls,
                bike : defaultStartAudioUrls
              }
            },
            {
              cityName : "Puducherry",
              mapImage : "ny_ic_puducherry_map",
              cityCode : "std:0413",
              showSubscriptions : false,
              enableAdvancedBooking : false,
              enableGullak : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 11.943852,
              cityLong : 79.808292,
              supportNumber : "08069724899",
              languageKey : "TA_IN",
              showScheduledRides : false,
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
                onboarding_auto_image : "ny_ic_auto_right_side_yellow",
                empty_referral_auto : "ny_ic_refer_now_auto_ny_yellow,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_yellow.png",
                empty_referral_cab : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
              },
              enableHvSdk : false,
              purpleRideConfig : {
                purpleRideConfigForAuto : {
                  vehicleVariant : "Auto",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForCabs : {
                  vehicleVariant : "Cab",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForBikes : {
                  vehicleVariant : "Bike",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                }
              },
              rideStartAudio : {
                acCab : defaultStartAudioUrls,
                nonAcCab : defaultStartAudioUrls,
                auto : defaultStartAudioUrls,
                bike : defaultStartAudioUrls
              }
            },
            {
              cityName : "Gurugram",
              mapImage : "ny_ic_gurugram_map",
              cityCode : "std:0124",
              showSubscriptions : false,
              enableAdvancedBooking : false,
              enableGullak : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 28.457523,
              cityLong : 77.026344,
              supportNumber : "",
              languageKey : "HI_IN",
              showScheduledRides : false,
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
                onboarding_auto_image : "ny_ic_auto_side",
                empty_referral_auto : "ny_ic_refer_now_auto_yatri_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_yatri_green.png",
                empty_referral_cab : "ny_ic_refer_now_cab_yatri,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_yatri.png"
              },
              enableHvSdk : false,
              purpleRideConfig : {
                purpleRideConfigForAuto : {
                  vehicleVariant : "Auto",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForCabs : {
                  vehicleVariant : "Cab",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForBikes : {
                  vehicleVariant : "Bike",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                }
              },
              rideStartAudio : {
                acCab : defaultStartAudioUrls,
                nonAcCab : defaultStartAudioUrls,
                auto : defaultStartAudioUrls,
                bike : defaultStartAudioUrls
              }
            },
            {             
              cityName : "Noida",
              mapImage : "ny_ic_noida_map",
              cityCode : "std:01189",
              showSubscriptions : false,
              enableAdvancedBooking : false,
              enableGullak : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 28.535517,
              cityLong : 77.391029,
              supportNumber : "",
              languageKey : "HI_IN",
              showScheduledRides : false,
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
                onboarding_auto_image : "ny_ic_auto_side",
                empty_referral_auto : "ny_ic_refer_now_auto_yatri_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_yatri_green.png",
                empty_referral_cab : "ny_ic_refer_now_cab_yatri,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_yatri.png"
              },
              enableHvSdk : false,
              purpleRideConfig : {
                purpleRideConfigForAuto : {
                  vehicleVariant : "Auto",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForCabs : {
                  vehicleVariant : "Cab",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForBikes : {
                  vehicleVariant : "Bike",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                }
              },
              rideStartAudio : {
                acCab : defaultStartAudioUrls,
                nonAcCab : defaultStartAudioUrls,
                auto : defaultStartAudioUrls,
                bike : defaultStartAudioUrls
              }
            },
            {
              cityName : "TamilNaduCities",
              mapImage : "ny_ic_tamilnadu_map",
              cityCode :  "std:0422",
              showSubscriptions : false,
              enableAdvancedBooking : false,
              enableGullak : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 11.1271,
              cityLong : 78.6569,
              supportNumber : "08069724899",
              languageKey : "TA_IN",
              showScheduledRides : false,
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
                onboarding_auto_image : "ny_ic_auto_right_side_yellow",
                empty_referral_auto : "ny_ic_refer_now_auto_ny_yellow,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_yellow.png",
                empty_referral_cab : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
              },
              enableHvSdk : false,
              purpleRideConfig : {
                purpleRideConfigForAuto : {
                  vehicleVariant : "Auto",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForCabs : {
                  vehicleVariant : "Cab",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForBikes : {
                  vehicleVariant : "Bike",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                }
              },
              rideStartAudio : {
                acCab : defaultStartAudioUrls,
                nonAcCab : defaultStartAudioUrls,
                auto : defaultStartAudioUrls,
                bike : defaultStartAudioUrls
              }
            },
            {
              cityName : "Kolkata",
              mapImage : "",
              cityCode : "std:033",
              showSubscriptions : true,
              enableAdvancedBooking : false,
              enableGullak : false,
              advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
              callDriverInfoPost : false,
              cityLat : 22.5354064,
              cityLong : 88.2649516,
              supportNumber : "",
              languageKey : "BN_IN",
              showScheduledRides : false,
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
              rentalWaitingChargesConfig : defRentalWaitingChargesConfig {
                cab {
                  freeSeconds = 180,
                  perMinCharges = 1.0
                },
                auto {
                  freeSeconds = 180,
                  perMinCharges = 1.0
                }
              },
              gstPercentage : "18",
              rateCardConfig : defRateCardConfig,
              assets :{
                auto_image : "ny_ic_auto_side_view",
                onboarding_auto_image : "ny_ic_auto_side",
                empty_referral_auto : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
                empty_referral_cab : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
              },
              enableHvSdk : false,
              purpleRideConfig : {
                purpleRideConfigForAuto : {
                  vehicleVariant : "Auto",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForCabs : {
                  vehicleVariant : "Cab",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForBikes : {
                  vehicleVariant : "Bike",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                }
              },
              rideStartAudio : {
                acCab : defaultStartAudioUrls,
                nonAcCab : defaultStartAudioUrls,
                auto : defaultStartAudioUrls,
                bike : defaultStartAudioUrls
              }
            }, 
            {
              cityName : "Kochi",
              mapImage : "ny_ic_kochi_map",
              cityCode : "std:0484",
              showSubscriptions : true,
              cityLat : 9.931233,
              cityLong : 76.267303,
              enableAdvancedBooking: false,
              enableGullak : false,
              advancedRidePopUpYoutubeLink: "",
              callDriverInfoPost: false,
              supportNumber : "",
              languageKey : "ML_IN",
              showScheduledRides : false,
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
              onboarding_auto_image : "ny_ic_auto_right_side_black",
              empty_referral_auto : "ny_ic_refer_now_auto_yatri_black,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_yatri_black.png",
              empty_referral_cab : "ny_ic_refer_now_cab_yatri,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_yatri.png"
            },
            enableHvSdk : false,
            purpleRideConfig : {
                purpleRideConfigForAuto : {
                  vehicleVariant : "Auto",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForCabs : {
                  vehicleVariant : "Cab",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                },
                purpleRideConfigForBikes : {
                  vehicleVariant : "Bike",
                  showVideo : false,
                  disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
                  genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
                }
              },
              rideStartAudio : {
                acCab : defaultStartAudioUrls,
                nonAcCab : defaultStartAudioUrls,
                auto : defaultStartAudioUrls,
                bike : defaultStartAudioUrls
              }
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
  , enableChangeVehicleType : true
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
  , themeColors : {
    navBarBackground : "#FFFFFF",
    primaryStrokeColor : "#E5E7EB",
    openMapsStrokeColor : "#00FFFFFF",
    secondaryStrokeColor : "#B9BABE",
    openMapsTextColor : "#FFFFFF",
    onlineStatusColor : "#53BB6F",
    onboardingHeaderTextColor : "#FFFFFF",
    onboardingHeaderStroke : "#FFFFFF",
    radioInactiveBackground : "#FFFFFF",
    radioActiveBackground : "#f4F7FF",
    editTextFocusedStroke : "#e4e4e4",
    editTextNormalStroke : "#e4e4e4",
    mobileNumberScreenHeaderText : "LETS_GET_YOU_TRIP_READY",
    defaultBackButton : "ny_ic_chevron_left_white",
    highlightedTextColor : "#2194FF",
    radioSelectedImage : "ny_ic_radio_selected",
    radioActiveStroke : "#0066FF",
    onboardingStepImgBg : "#f4F7FF",
    welcomeScreenBackground : "#FFFAED",
    quizButtonStrokeAndText : "#6D7280",
    quizOptionStrokeColor : "#0066FF",
    quizOptionSelectedBgColor : "#1A0066FF",
    quizOptionTextColor : "#454545"
  }
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
  , showFaqsWhileOnboarding : false
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
  , clientName : ""
  , appUpdatePopupUrl : "https://play.google.com/store/apps/details?id=in.juspay.nammayatripartner&pcampaignid=web_share"
  , showProfileAadhaarPan : true -- Only for backward compatibility and testing in PROD environment
  , rentalRideVideoConfig : {
      auto : "https://www.youtube.com/watch?v=nwXV-vT_X_8",
      cab : "https://www.youtube.com/watch?v=aKGPp5A2M0E"
  }
  , scheduledRideConfig : {
    scheduledBannerTimerValue : 1800
  }
}

registrationConfig :: RegistrationConfig
registrationConfig = {
  supportWAN : "919625724848",
  callSupport : true,
  whatsappSupport : false
}

getStaticViewPlans :: Array StaticViewPlans
getStaticViewPlans = [
  {price : 45.0, frequency : "PER_DAY", variantCategory : "CarCategory", name : "DAILY_UNLIMITED", introductoryOffer : "FREE_RIDE_OFFER", showSelected : false, planDesc : "CAB_DAILY_UNLIMITED_OFFER"},
  {price : 9.0, frequency : "PER_RIDE", variantCategory : "CarCategory", name : "DAILY_PER_RIDE", introductoryOffer : "", showSelected : false, planDesc : "CAB_DAILY_PER_RIDE_OFFER"},
  {price : 25.0, frequency : "PER_DAY", variantCategory : "AutoCategory", name : "DAILY_UNLIMITED", introductoryOffer : "NO_CHARGES_TILL", showSelected : true, planDesc : ""}
]

defWaitingChargesConfig :: WaitingChargesConfig
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
  },
  ambulance : {
    freeSeconds : 480,
    perMinCharges : 2.0
  }
}

defRentalWaitingChargesConfig :: WaitingChargesConfig
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
  },
  ambulance: {
    freeSeconds : 480,
    perMinCharges : 1.5
  }
}

defRateCardConfig :: RateCardConfig
defRateCardConfig = {
  showLearnMore : false,
  learnMoreVideoLink : ""
}

defaultCityConfig :: CityConfig
defaultCityConfig = 
  case MU.getMerchant CTA.FunctionCall of
    MU.YATRISATHI -> ysDefaultCityConfig
    _ -> allCitiesDefaultCityConfig


allCitiesDefaultCityConfig :: CityConfig
allCitiesDefaultCityConfig = {
  cityName : "",
  mapImage : "",
  cityCode : "",
  showSubscriptions : false,
  enableAdvancedBooking : false,
  advancedRidePopUpYoutubeLink : "" , --- Dummy link need to change
  callDriverInfoPost : false,
  cityLat : 0.0,
  cityLong : 0.0,
  supportNumber : "",
  languageKey : "",
  showScheduledRides : false,
  enableYatriCoins : false,
  showDriverReferral : false,
  showCustomerReferral : false,
  uploadRCandDL : true,
  vehicleNSImg : "",
  registration : { 
    callSupport : false,
    supportWAN : "", 
    whatsappSupport : false
  },
  variantSubscriptionConfig : {
    enableVariantBasedSubscription : true,
    variantList : ["AutoCategory"],
    enableCabsSubscriptionView : false,
    staticViewPlans : []
  },
  showEarningSection: true,
  referral : {
      domain : ""
    , customerAppId : ""
    , driverAppId : ""
  },
  waitingCharges : 1.50,
  waitingChargesConfig : {
    cab : {
      freeSeconds : 300,
      perMinCharges : 1.0
    },
    auto : {
      freeSeconds : 180,
      perMinCharges : 1.50
    },
    bike: {
      freeSeconds : 3,
      perMinCharges : 1.50
    },
    ambulance: {
      freeSeconds : 480,
      perMinCharges : 2.0
    }
  },
  rentalWaitingChargesConfig : {
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
    },
    ambulance: {
      freeSeconds : 480,
      perMinCharges : 2.0
    }
  },
  rateCardConfig : { showLearnMore : false, learnMoreVideoLink : "" },
  assets :{
    auto_image :  "ny_ic_black_yellow_auto_side_view",
    onboarding_auto_image : "ny_ic_auto_right_side_yellow",
    empty_referral_auto : "",
    empty_referral_cab : ""
  },
  gstPercentage : "18",
  enableHvSdk : false,
  enableGullak : false,
  purpleRideConfig : {
    purpleRideConfigForAuto : {
      vehicleVariant : "Auto",
      showVideo : false,
      disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
      genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
    },
    purpleRideConfigForCabs : {
      vehicleVariant : "Cab",
      showVideo : false,
      disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
      genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
    },
    purpleRideConfigForBikes : {
      vehicleVariant : "Bike",
      showVideo : false,
      disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
      genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
    }
  },
  rideStartAudio : {
    acCab : defaultStartAudioUrls,
    nonAcCab : defaultStartAudioUrls,
    auto : defaultStartAudioUrls,
    bike : defaultStartAudioUrls
  }
}

defaultStartAudioUrls :: StartAudioUrls
defaultStartAudioUrls = {
    tollAudio : Nothing,
    acAudio : Nothing,
    parkingAudio : Nothing,
    defaultAudio : Nothing
}

ysDefaultCityConfig :: CityConfig
ysDefaultCityConfig = 
  allCitiesDefaultCityConfig {
      showSubscriptions = true
    , enableAdvancedBooking = true
    , supportNumber = "+918069724949"
    , languageKey = "BN_IN"
    , showScheduledRides = false
    , showDriverReferral = true
    , showCustomerReferral = true
    , vehicleNSImg = "ny_ic_location_unserviceable" -- Unserviceable Image when Driver Not able to go online (isVehicleSupported false in driver/profile resp)
    , registration {
        supportWAN = "918088065549"
      , callSupport = true
      , whatsappSupport = false
      }
    , variantSubscriptionConfig {
        enableVariantBasedSubscription = true
      , variantList = ["CarCategory"] -- To be updated after variant specific plans are introduced in BE
      , enableCabsSubscriptionView = true
      , staticViewPlans = [] -- No Static Plans to be shown as per external requirement
      }
    , referral {
        domain = "https://www.yatrisathi.in"
      , customerAppId = "in.juspay.jatrisaathi"
      , driverAppId = "in.juspay.jatrisaathidriver"
      }
    , waitingCharges = 1.50
    , waitingChargesConfig {
        cab {
          freeSeconds = 180
        , perMinCharges = 2.0
        }
      , auto {
          freeSeconds = 180
        , perMinCharges = 1.50
        }
      , bike {
          freeSeconds = 180
        , perMinCharges = 2.0
        }
      , ambulance {
        freeSeconds = 480,
        perMinCharges = 2.0
        }
      }
    , rentalWaitingChargesConfig {
        cab {
          freeSeconds = 180
        , perMinCharges = 2.0
        }
      , auto {
          freeSeconds = 180
        , perMinCharges = 2.0
        }
      , bike {
          freeSeconds = 180
        , perMinCharges = 2.0
        },
    ambulance {
      freeSeconds = 480,
      perMinCharges = 2.0
    }
      }
    , enableHvSdk = true -- Hyperverge Integration Activation at launch
    , enableGullak = false
    , purpleRideConfig = {
        purpleRideConfigForAuto : {
          vehicleVariant : "Auto",
          showVideo : false,
          disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
          genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
        },
        purpleRideConfigForCabs : {
          vehicleVariant : "Cab",
          showVideo : false,
          disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
          genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
        },
        purpleRideConfigForBikes : {
          vehicleVariant : "Bike",
          showVideo : false,
          disabilityToVideo : [{disabilityType : "BLIND_AND_LOW_VISION", videoUrl : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {disabilityType : "HEAR_IMPAIRMENT", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "LOCOMOTOR_DISABILITY", videoUrl : "https://www.youtube.com/watch?v=udkWOt0serg"}, {disabilityType : "SAFETY", videoUrl : ""}, {disabilityType : "SPECIAL_ZONE_PICKUP", videoUrl : ""}, {disabilityType : "OTHER_DISABILITY", videoUrl : ""}],
          genericVideoForVariant : "https://youtu.be/5s21p2rI58c"
        }
      }
    , rideStartAudio = {
              acCab : defaultStartAudioUrls,
              nonAcCab : defaultStartAudioUrls,
              auto : defaultStartAudioUrls,
              bike : defaultStartAudioUrls
            } 
  }