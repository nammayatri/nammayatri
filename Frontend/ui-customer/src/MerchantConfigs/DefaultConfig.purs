module MerchantConfig.DefaultConfig where

import MerchantConfig.Types
import Common.DefaultConfig
import Engineering.Helpers.Commons as EHC

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
    , closeIcon : "ny_ic_close_white,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_close_white.png"
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
    , backArrow : "ny_ic_chevron_left_white,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_chevron_left_white.png"
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
    , footerImageUrl : "ic_namma_yatri_logo,https://assets.moving.tech/beckn/nammayatri/user/images/ic_namma_yatri_logo.png"
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
  , sideBarList: [ "MyRides", "Favorites", "NammaSafety", "HelpAndSupport", "Language", "Separator", "ShareApp", "LiveStatsDashboard", "About", "Logout", "MetroTickets"]
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
      , imageUrl : "ny_ic_banner_gender_feat,https://assets.moving.tech/beckn/merchantcommon/images/ny_ic_banner_gender_feat.png" 
      }
  , popupBackground : "#FFFFFF"
  , profileCompletion : "#FCC32C"
  , showProfileStatus: true
  , profileArrowImage: "ny_ic_chevron_right_white,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_chevron_right_white.png"
  , cancelRideColor : "#E55454"
  , infoIconUrl : "ny_ic_info,https://assets.moving.tech/beckn/common/user/ny_ic_information_grey.png"
  , profileEditGravity : "center"
  , merchantLogo : "ic_launcher,https://assets.moving.tech/beckn/common/user/ny_ic_launcher.png"
  , logs: [ "JUSPAY" ]
  , showCorporateAddress : true
  , terminateBtnConfig : {
          visibility: false, 
          title : "",
          backgroundColor : "#00FFFFFF",
          imageUrl : "ny_ic_chevron_left_double,https://assets.moving.tech/beckn/mobilitypaytm/mobilitypaytmcommon/ny_ic_chevron_left_double.png"
      }
  , suggestedTripsAndLocationConfig : {
        geohashLimitForMap : 60,
        geohashPrecision : 7,
        maxLocationsToBeShown : 10,
        minLocationsToBeShown : 3,
        maxTripsToBeShown : 10,
        minTripsToBeShown : 3,
        locationsToBeStored : 30,
        tripsToBeStored : 30,
        frequencyWeight : 0.85,
        tripDistanceThreshold : 0.021,
        repeatRideTime : 8,
        autoScrollTime : 5,
        tripWithinXDist : 0.05,
        locationWithinXDist : 100.0,
        destinationGeohashPrecision : 9 
      }
  , showDeleteAccount : false
  , autoSelectBackground : "#53BB6F"
  , showGenderBanner : true
  , enableMockLocation : false
  , specialLocationView : false
  , internationalNumberEnabled : true
  , callOptions : ["ANONYMOUS"]
  , autoVariantEnabled : true
  , showDisabilityBanner : false
  , showCheckoutRentalBanner : false
  , geoCoder: {
      enableLLtoAddress : true
    , enableAddressToLL : true 
    }
  , enableWhatsappOTP : ["BD", "US"]
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
          image : "ny_ic_hatchback,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_hatchback-2.png",
          leftViewImage : "ny_ic_hatchback_left_view,"
          },
        taxiPlus : {
          name : "AC Taxi",
          image : "ny_ic_sedan_ac,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_sedan_ac.png",
          leftViewImage : "ny_ic_sedan_left_view,"
        },
        sedan : {
          name : "Sedan",
          image : "ny_ic_sedan,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_sedan.png",
          leftViewImage : "ny_ic_sedan_left_view,"
        },
        taxi : {
          name : "Non-AC Taxi",
          image : "ny_ic_sedan,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_sedan.png",
          leftViewImage : "ny_ic_sedan_left_view,"
        },
        suv : {
          name : "SUV",
          image : "ny_ic_suv,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_suv.png",
          leftViewImage : "ny_ic_suv_left_view,"
        },
        autoRickshaw : {
          name : "Auto Rickshaw",
          image : "ny_ic_auto_quote_list,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_auto_quote_list.png",
          leftViewImage : "ny_ic_auto_left_view,"
        },
        bookAny : {
          name : "Book Any",
          image : "ny_ic_auto_cab_green,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_auto_cab_green.png",
          leftViewImage : ","
        }
      }
      , enableOnlyAuto : false
      , showNearByDrivers: false
      , enableBookingPreference: true
      , textColor: "#6D7280"
      , showInfoIcon : true 
      , genericLoaderLottie : "https://assets.moving.tech/beckn/nammayatri/user/lottie/ny_ic_generic_loader.json"
      }
  , customerTip : {
      auto : true,
      cabs : true
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
    enableRepeatTripBackfilling : true,
    forceLogReferrerUrl : true,
    enableSelfServe : true,
    enableAdditionalServices : false,
    enableSafetyFlow : true, 
    shareWithEmergencyContacts: true,
    enableAutoReferral : true,
    enableCustomerSupportForSafety : false,
    enableSpecialPickup : EHC.jBridgeMethodExists "locateOnMapV2",
    enableAcPopup : false,
    enableRentalReallocation : true
  }

  , rideCompletedCardConfig : {
      topCard : {
        gradient : "#29334A"
      , enableGradient : true
      , background : "#2C2F3A"
      , titleColor : "#E5E7EB"
      , rideDescription : { 
          background : "#08FFFFFF"
        , textColor : "#A7A7A7"
        }        
      , horizontalLineColor : "#E5E7EB"
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
            pickUpToSourceThreshold : 1.0,
            hotSpotConfig :
              { goToNearestPointWithinRadius : 12.0
              , showHotSpotsWithinRadius : 150.0
              , enableHotSpot : EHC.jBridgeMethodExists "locateOnMapV2"
              , updateHotSpotOutSideRange : 200.0
              }
          }
      , labelTextSize : 30
      , animationDuration : 500
      , vehicleMarkerSize: 150
      }
  , metroTicketingConfig : [
    { 
      cityName : "chennai"
    , cityCode : "std:040"
    , customEndTime : "01:00:00" 
    , customDates : ["23/04/2024","28/04/2024","01/05/2024","12/05/2024"] 
    , metroStationTtl : 10080 -- in Minutes
    , bookingStartTime : "04:30:00"
    , bookingEndTime : "22:30:00"
    , ticketLimit : {
        roundTrip : 6
      , oneWay : 6
    }
    },
    {
      cityName : "kochi"
    , cityCode : "std:0484"
    , customEndTime : "23:59:59" 
    , customDates : ["03/05/2024", "04/05/2024", "05/05/2024", "06/05/2024", "07/05/2024", "08/05/2024", "09/05/2024", "10/05/2024","11/05/2024"]
    , metroStationTtl : 10080
    , bookingStartTime : "00:00:00"
    , bookingEndTime : "23:59:59"
    , ticketLimit : {
        roundTrip : 1
      , oneWay : 6
    }
    }
  ]
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
          color : "#000000",
          x: 0.0,
          y: 0.8,
          blur: 4.0,
          spread: 2.0,
          opacity : 0.3
        }
      },
      showAdditionalServicesNew : true
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
  , termsVersion : 1.0
  , privacyLink : "https://docs.google.com/document/d/128VU80K5E1iz-x6QnP1R127m_lwmDO3F"
  , shareAppConfig : {
      title : "Share Namma Yatri!"
    , description : "Hey there!\n\nCheck India's first Zero Commission auto booking app.\n100% Open source | 100% Open Data\n#beOpen #chooseOpen \n\n"--https://nammayatri.in/link/rider/SJ8D"
  }
  , dashboard :{
      url : "https://nammayatri.in/open?source=in-app"
    , enable : false
  }
  , logFunctionCalls : false
  , locationTagBar : {
    cornerRadius : 18.0
  , textColor : "#454545"
  , stroke: "1,#E5E7EB"
  }
  , countryCodeConfig : [
      {
        countryName : "US" 
      , countryCode  : "+1" 
      , countryShortCode : "US"
      },
      {
        countryName : "India" 
      , countryCode  : "+91" 
      , countryShortCode : "IN"
      }
    ]
  , cityConfig :
      [ defaultCityConfig 
          { cityName = "Bangalore",
            cityCode = "std:080"
            , geoCodeConfig
              { radius = 700000
              , strictBounds = true
              }
            , enableRentals = true 
            , featureConfig {
                enableCabBanner = true,
                enableChangeRideVariant = true
              }
            , enableIntercity = false
            , enableCabs = false
            , estimateAndQuoteConfig {
              showInfoIcon = true
              }
          },
        defaultCityConfig 
          {   cityName = "Chennai"
            , cityCode = "std:044"
            , geoCodeConfig
                { radius = 700000
                , strictBounds = true
                }
            , enableCabs = true
            , enableRentals = true 
            , enableIntercity = false
            , estimateAndQuoteConfig {
              showInfoIcon = false
              }
            , featureConfig {
                enableCabBanner = false,
                enableChangeRideVariant = false
              }
          },
        defaultCityConfig
          { cityName = "Hyderabad",
            cityCode = "std:040",
            referral
              { domain = "https://www.manayatri.in",
                customerAppId = "in.mobility.manayatri"
              },
            dashboardUrl = "https://www.manayatri.in/open?source=in-app",
            appLogoLight = "ny_ic_logo_light_my,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_logo_light_my.png",
            appLogo = "ic_namma_yatri_logo,https://assets.moving.tech/beckn/nammayatri/user/images/ic_namma_yatri_logo.png"
          },
        defaultCityConfig
          { cityName = "Kolkata",
            cityCode = "std:033",
            referral
              { domain = "https://www.yatrisathi.in",
                customerAppId = "in.juspay.jatrisaathi"
              }
            , geoCodeConfig
              { radius = 700000
              , strictBounds = true
              }
            , enableRentals = true 
            , enableIntercity = true
            , enableCabs = true
            , estimateAndQuoteConfig {
              showInfoIcon = true
              }
            , featureConfig {
                enableCabBanner = true,
                enableChangeRideVariant = false
              }
          }
      ]
  , bannerCarousel : defaultBannerCarousel
  , driverLocationPolling : {
    retryExpFactor : 3
  },
  banners : {
    homeScreenSafety : false,
    homeScreenCabLaunch : true
  }
  , tipDisplayDuration : 30
  , tipsEnabled : true
  , tipEnabledCities : ["Bangalore", "Hyderabad"]
  , referral : {
          domain : "https://nammayatri.in/"
        , customerAppId : "in.juspay.nammayatri"
      }
  , safety : {
      pastRideInterval : 15
  }
  , enableBookAny : true
  , acPopupConfig : {
      enableAcPopup : false,
      enableNonAcPopup : false,
      showAfterTime : 5
    }
}

defaultCityConfig :: CityConfig
defaultCityConfig =
  { cityName : "",
    cityCode : "",
    iopConfig : {
            enable : false,
            autoSelectTime : 8
          },
    estimateAndQuoteConfig : {
          showInfoIcon : true},
    geoCodeConfig :
      { radius : 100000
      , strictBounds : false
      },
    enableCabs : false,
    featureConfig : {
      enableCabBanner : false,
      enableChangeRideVariant : false
    },
    referral : {
      domain : "https://nammayatri.in",
      customerAppId : "in.juspay.nammayatri"
    },
    appLogo : "",
    dashboardUrl : "",
    appLogoLight : "",
    enableRentals : false,
    enableIntercity : false
  }
