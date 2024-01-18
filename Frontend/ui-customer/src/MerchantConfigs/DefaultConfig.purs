module MerchantConfig.DefaultConfig where

import MerchantConfig.Types
import Common.DefaultConfig

config :: AppConfig 
config =
  { primaryTextColor: "#FCC32C"
  , primaryBackground: "#2C2F3A"
  , estimateConfirmText: "Request a NammaYatri Ride"
  , autoConfirmingLoaderColor: "#80B2FF"
  , quoteListModelBackground: "#2C2F3A"
  , defaultLanguage : "EN_US"
  , currency: "₹"
  , primaryButtonCornerRadius: 8.0
  , showPickUpandDrop: true
  , alertDialogPrimaryColor: "#2194FF"
  , cancelSearchTextColor: "#E55454"
  , showHamMenu : true
  , showQuoteFindingText : false
  , quoteListItemConfig: 
    { primaryButtonCorner: 8.0
    , expiresColor: "#E55454"
    , driverImagebg: "#F1F1F1"
    , vehicleHeight: 37
    , vehicleWidth: 40
    }
  , quoteListModel:
    { backgroundColor: "#2C2F3A"
    , textColor: "#FFFFFF"
    , loaderColor: "#80B2FF"
    , otpTextBackground : "#2C2F3A"
    , otpBackground: "#F1F1F1"
    , otpTextColor: "#FFFFFF"
    , otpTitleColor : "#6D7280"
    , selectRideTextColor: "#2C2F3A"
    , lineImage : "ic_line"
    , lottieHeight : 300
    , lottieWidth : 300
    , topMargin : 0
    , noQuotesImageHeight: 115
    , noQuotesImageWidth : 137
    , separatorColor : "#00FFFFFF"
    , showSeparator : false
    , closeIcon : "ny_ic_close_white,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_close_white.png"
    }
  , searchLocationConfig : 
    { searchLocationTheme: "#2C2F3A"
    , setLocationOnMapColor:"#6D7280"
    , editTextBackground : "#313440"
    , editTextDefaultColor : "#A7A7A7"
    , strokeColor: "1,#E5E7EB"
    , backgroundColor : "#2C2F3A"
    , editTextColor : "#FFFFFF"
    , separatorColor : "#00FFFFFF"
    , enableLocationTagbar : "true"
    , resultsCardCornerRadius : 20.0
    , showRateCardDetails : true
    , showAdditionalChargesText : false
    , lottieHeight : 96
    , lottieWidth : 96
    , primaryButtonHeight : 60
    , hintColor : "#A7A7A7"
    , showSeparator : false
    , showChargeDesc: false
    , backArrow : "ny_ic_chevron_left_white,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_chevron_left_white.png"
    , enableRateCard: true
    , clearTextImage: "ny_ic_close_grey"
    }
  , driverInfoConfig : 
    { ratingTextColor: "#454545"
    , ratingBackground: "#F1F1F1"
    , ratingStroke: "0,#717171"
    , ratingCornerRadius: 10.0
    , callBackground: "#2053BB6F"
    , callButtonStroke: "0,#EB0055" 
    , cardStroke: "1,#E5E7EB"
    , otpStroke: "0,#717171"
    , showNumberPlatePrefix : true
    , showNumberPlateSuffix : false
    , callHeight: 32
    , callWidth: 32
    , numberPlateBackground : "#E9BE4D"
    , showCancelPrevention : true
    , showTrackingButton : true
    , specialZoneQuoteExpirySeconds : 3600
    , footerVisibility : false
    , footerImageUrl : "ic_namma_yatri_logo,https://assets.juspay.in/beckn/nammayatri/user/images/ic_namma_yatri_logo.png"
    , footerBackgroundColor : "#FFFFFF"
    }
  , ratingConfig : 
    { secondaryButtonTextColor : "#2C2F3A"
    , secondaryButtonStroke : "1,#2C2F3A"
    , buttonCornerRadius : 8.0
    }
  , cancelReasonConfig : 
    { secondaryButtonTextColor : "#2C2F3A"
    , secondaryButtonStroke : "1,#2C2F3A"
    , buttonCornerRadius : 8.0
    }
  , profileBackground: "#2C2F3A"
  , profileName: "#FFFFFF"
  , profileImage: "#012A72"
  , feedbackBackground: "#2C2F3A"
  , sideBarList: [ "MyRides", "Favorites", "EmergencyContacts", "HelpAndSupport", "Language", "Separator", "ShareApp", "LiveStatsDashboard", "About", "Logout" ]
  , rateCardColor: "#2C2F3A"
  , nyBrandingVisibility: false
  , fontType: "Assets"
  , languageList : []
  , confirmPickUpLocationBorder: "#E5E7EB"
  , bannerConfig : {
        backgroundColor : "#F0FAF0"
      , title : "Complete your profile for a personalised ride experience"
      , titleColor :"#21C179"
      , actionText : "Update now"
      , actionTextColor : "#27AE5F"
      , imageUrl : "ny_ic_banner_gender_feat,https://assets.juspay.in/beckn/merchantcommon/images/ny_ic_banner_gender_feat.png" 
      }
  , popupBackground : "#FFFFFF"
  , profileCompletion : "#FCC32C"
  , showProfileStatus: true
  , profileArrowImage: "ny_ic_chevron_right_white,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_chevron_right_white.png"
  , cancelRideColor : "#E55454"
  , infoIconUrl : "ny_ic_info,https://assets.juspay.in/nammayatri/images/user/ny_ic_information_grey.png"
  , profileEditGravity : "center"
  , merchantLogo : "ic_launcher,https://assets.juspay.in/nammayatri/images/user/ny_ic_launcher.png"
  , logs: [ "JUSPAY" ]
  , showCorporateAddress : true
  , terminateBtnConfig : {
          visibility: false, 
          title : "",
          backgroundColor : "#00FFFFFF",
          imageUrl : "ny_ic_chevron_left_double,https://assets.juspay.in/beckn/mobilitypaytm/mobilitypaytmcommon/ny_ic_chevron_left_double.png"
      }
  , suggestedTripsAndLocationConfig : {
        geohashLimitForMap : 60,
        geohashPrecision : 7,
        maxLocationsToBeShown : 5,
        minLocationsToBeShown : 2,
        maxTripsToBeShown : 5,
        minTripsToBeShown : 2,
        locationsToBeStored : 30,
        tripsToBeStored : 30,
        frequencyWeight : 0.85,
        tripDistanceThreshold : 0.021,
        repeatRideTime : 8,
        autoScrollTime : 5,
        tripWithinXDist : 0.05,
        locationWithinXDist : 100.0
      }
  , showDeleteAccount : false
  , autoSelectBackground : "#53BB6F"
  , showGenderBanner : true
  , enableMockLocation : false
  , specialLocationView : false
  , internationalNumberEnabled : false
  , callOptions : ["ANONYMOUS"]
  , autoVariantEnabled : true
  , showDisabilityBanner : false
  , geoCoder: {
      enableLLtoAddress : true
    , enableAddressToLL : true 
    }
  , enableWhatsappOTP : ["BD"]
  , notifyRideConfirmationConfig : 
      { notify : false 
      , autoGeneratedText : ""
      }
  , estimateAndQuoteConfig : 
      { variantTypes : [ ["SUV"], ["HATCHBACK", "TAXI_PLUS", "SEDAN"], ["TAXI"], ["AUTO_RICKSHAW"] ]
      , variantOrder : ["HATCHBACK", "TAXI_PLUS", "SEDAN", "TAXI", "SUV", "AUTO_RICKSHAW"]
      , variantInfo : {
        hatchback : {
          name : "Hatchback",
          image : "ny_ic_hatchback,https://assets.juspay.in/beckn/nammayatri/user/ny_ic_hatchback.png"
          },
        taxiPlus : {
          name : "AC Taxi",
          image : "ny_ic_sedan_ac,https://assets.juspay.in/beckn/nammayatri/user/ny_ic_sedan_ac.png"
        },
        sedan : {
          name : "Sedan",
          image : "ny_ic_sedan,https://assets.juspay.in/beckn/nammayatri/user/ny_ic_sedan.png"
        },
        taxi : {
          name : "Non-AC Taxi",
          image : "ny_ic_sedan,https://assets.juspay.in/beckn/nammayatri/user/ny_ic_sedan.png"
        },
        suv : {
          name : "SUV",
          image : "ny_ic_suv,https://assets.juspay.in/beckn/nammayatri/user/ny_ic_suv.png"
        },
        autoRickshaw : {
          name : "Auto Rickshaw",
          image : "ny_ic_auto_quote_list,https://assets.juspay.in/beckn/nammayatri/user/ny_ic_auto_quote_list.png"
        }

      }
      , enableOnlyAuto : false
      , showNearByDrivers: false
      , enableBookingPreference: true
      , textColor: "#6D7280"
      , showInfoIcon : true 
      }
  , customerTip : {
      auto : true,
      cabs : false
    }
  , feature : {
    enableAutoReadOtp : true,
    enableZooTicketBookingFlow : false,
    enableLiveDashboard : true,
    enableSuggestions : true,
    enableShareRide : true,
    enableChat: true,
    enableEmergencyContacts: true,
    enableReferral: true,
    enableSupport: true,
    enableShareApp: false,
    enableReAllocation : true,
    forceLogReferrerUrl : true,
    enableSelfServe : true
  }

  , rideCompletedCardConfig : {
      topCard : {
        gradient : "#29334A"
      , enableGradient : true
      , background : "#2C2F3A"
      , titleColor : "#E5E7EB"
      , rideDescription : { 
          background : "#00FFFFFF"
        , textColor : "#A7A7A7"
        }                     
      }
     , showCallSupport : false
    }
  , mapConfig : 
      { locateOnMapConfig : 
          { dottedLineConfig : 
              { visible : false
              , range : 100
              , color : "#323643"
              },
            apiTriggerRadius : 10.0,
            pickUpToSourceThreshold : 1.0
          }
      , labelTextSize : 30
      , animationDuration : 500
      , vehicleMarkerSize: 90
      }
  , purpleRideConfig : {
      genericVideoUrl : "" ,
      visualImpairmentVideo : "" ,
      physicalImpairmentVideo : "",
      hearingImpairmentVideo : ""
    }
  , homeScreen: {
      primaryBackground : "#2C2F3A",
      isServiceablePopupFullScreen : false,
      pickUpViewColor : "#303440",
      header : {
        menuButtonBackground : "#00FFFFFF",
        showLogo : true,
        titleColor : "#1D1D1D",
        showSeparator : false
      },
      bannerViewVisibility : true,
      pickupLocationTextColor : "#A7A7A7",
      whereToButton : {
        margin : {
          top : 0,
          left : 16,
          right : 16,
          bottom : 16
        },
        shadow : {
          color : "#2C2F3A",
          x: 0.0,
          y: 0.9,
          blur: 10.0,
          spread: 24.0,
          opacity : 0.14
        }
      }
    }
  , appData : defaultAppData
  , navigationAppConfig : defaultNavigationAppConfig
  , genericHeaderConfig : defaultGenericHeader
  , colors : defaultColors
  , primaryButtonConfig : defaultPrimaryButtonConfig
  , fontConfig : defaultFontConfig
  , loaderConfig : defaultLoaderConfig
  , otpRegex :  "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
  , termsLink : "https://docs.google.com/document/d/1-oRR_oI8ncZRPZvFZEJZeCVQjTmXTmHA"
  , privacyLink : "https://docs.google.com/document/d/128VU80K5E1iz-x6QnP1R127m_lwmDO3F"
  , shareAppConfig : {
      title : "Share Namma Yatri!"
    , description : "Hey there!\n\nCheck India's first Zero Commission auto booking app.\n100% Open source | 100% Open Data\n\nDownload Namma Yatri now! \nhttps://nammayatri.in/link/rider/SJ8D \n\n #beOpen #chooseOpen"
  }
  , dashboard :{
      url : "https://nammayatri.in/open?source=in-app"
    , enable : false
  }
  , logFunctionCalls : false
  , locationTagBar : {
    cornerRadius : 8.0
  , textColor : "#454545"
  , stroke: "1,#E5E7EB"
  }
  , cityConfig : {
      banglore : {
                    cityName : "Bangalore",
                    selectMultipleQuote : true,
                    cityCode : "std:080",
                    d2cReferral : true,
                    customerTip : true,
                    supportNumber : "+918069724848",
                    customerTipArray : "",
                    customerTipArrayWithValues : [0,10, 20, 30],
                    searchText: ["Search for a location", "Search for a location", "Search for a location", "Search for a location"],
                    searchTextDuration : 30, -- in seconds,
                    reallocation : true
                    },
      hyderabad : {
                    cityName : "Hyderabad",
                    selectMultipleQuote : true,
                    cityCode : "std:040",
                    d2cReferral : true,
                    customerTip : true,
                    supportNumber : "+918069724848",
                    customerTipArray : "",
                    customerTipArrayWithValues : [0,10, 20, 30],
                    searchText: ["Search for a location", "Search for a location", "Search for a location", "Search for a location"],
                    searchTextDuration : 30, -- in seconds,
                    reallocation : true
                   },
      mysore : {
                  cityName : "Mysore",
                  selectMultipleQuote : true,
                  cityCode : "std:0821",
                  d2cReferral : true,
                  customerTip : true,
                  supportNumber : "+918069724848",
                  customerTipArray : "",
                  customerTipArrayWithValues : [0,10, 20, 30],
                  searchText: ["Search for a location", "Search for a location", "Search for a location", "Search for a location"],
                  searchTextDuration : 30, -- in seconds,
                  reallocation : true
                  },
      delhi : {
                  cityName : "Delhi",
                  selectMultipleQuote : true,
                  cityCode : "std:011",
                  d2cReferral : true,
                  customerTip : true,
                  supportNumber : "+918069724848",
                  customerTipArray : "",
                  customerTipArrayWithValues : [0,10, 20, 30],
                  searchText: ["Search for a location", "Search for a location", "Search for a location", "Search for a location"],
                  searchTextDuration : 30, -- in seconds,
                  reallocation : true
                  },
      chennai : {
                  cityName : "Chennai",
                  selectMultipleQuote : true,
                  cityCode : "std:044",
                  d2cReferral : true,
                  customerTip : true,
                  supportNumber : "+918069724848",
                  customerTipArray : "",
                  customerTipArrayWithValues : [0,10, 20, 30],
                  searchText: ["Search for a location", "Search for a location", "Search for a location", "Search for a location"],
                  searchTextDuration : 30, -- in seconds,
                  reallocation : true
                  },
      coimbatore : {
                  cityName : "Coimbatore",
                  selectMultipleQuote : true,
                  cityCode : "std:0422",
                  d2cReferral : true,
                  customerTip : true,
                  supportNumber : "+918069724848",
                  customerTipArray : "",
                  customerTipArrayWithValues : [0,10, 20, 30],
                  searchText: ["Search for a location", "Search for a location", "Search for a location", "Search for a location"],
                  searchTextDuration : 30, -- in seconds,
                  reallocation : true
                  },
      puducherry : {
                  cityName : "Puducherry",
                  selectMultipleQuote : true,
                  cityCode : "std:0413",
                  d2cReferral : true,
                  customerTip : true,
                  supportNumber : "+918069724848",
                  customerTipArray : "",
                  customerTipArrayWithValues : [0,10, 20, 30],
                  searchText: ["Search for a location", "Search for a location", "Search for a location", "Search for a location"],
                  searchTextDuration : 30, -- in seconds,
                  reallocation : true
                          },
      tumakuru : {
                cityName : "Tumakuru",
                selectMultipleQuote : true,
                cityCode : "std:0816",
                d2cReferral : true,
                customerTip : true,
                supportNumber : "+918069724848",
                customerTipArray : "",
                customerTipArrayWithValues : [0,10, 20, 30],
                searchText: ["Search for a location", "Search for a location", "Search for a location", "Search for a location"],
                searchTextDuration : 30, -- in seconds,
                reallocation : true
                  }
                }
}
