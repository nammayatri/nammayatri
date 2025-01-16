module MerchantConfig.DefaultConfig where

import MerchantConfig.Types
import Common.DefaultConfig
import Engineering.Helpers.Commons as EHC
import Prelude ((/=))
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Common.Types.App (LazyCheck(..))

config :: AppConfig 
config =
  { primaryTextColor: "#FCC32C"
  , primaryBackground: "#2C2F3A"
  , submitIssueBtnColor : "#0066FF"
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
    , showAdditionalChargesText : true
    , showDriverAdditions : true
    , lottieHeight : 96
    , lottieWidth : 96
    , primaryButtonHeight : 60
    , hintColor : "#A7A7A7"
    , showSeparator : false
    , showChargeDesc: false
    , backArrow : "ny_ic_chevron_left_white,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_chevron_left_white.png"
    , crossIcon : "ny_ic_close_white,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_close_white.png"
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
    , shuffleCancelReasons : true
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
  , riderRideCompletedCard : {
      showDriverProfile : true
  }
  , geoCoder: {
      enableLLtoAddress : true
    , enableAddressToLL : false 
    }
  , isAdvancedBookingEnabled : false
  , enableWhatsappOTP : ["BD", "US"]
  , notifyRideConfirmationConfig : 
      { notify : false 
      , autoGeneratedText : ""
      }
  , estimateAndQuoteConfig : 
      { variantTypes : [ ["AUTO_RICKSHAW"], ["SUV"], ["SEDAN", "TAXI_PLUS"], ["HATCHBACK"], ["TAXI"], ["BOOK_ANY"], ["BIKE"], ["SUV_PLUS"], ["DELIVERY_BIKE"],["AMBULANCE_VENTILATOR"],["AMBULANCE_AC_OXY"], ["AMBULANCE_AC"],["AMBULANCE_TAXI_OXY"] ,["AMBULANCE_TAXI"]]
      , variantOrder : ["AUTO_RICKSHAW", "BIKE", "BOOK_ANY", "HATCHBACK", "TAXI", "SEDAN", "TAXI_PLUS", "SUV", "SUV_PLUS", "DELIVERY_BIKE" , "AMBULANCE_VENTILATOR","AMBULANCE_AC_OXY", "AMBULANCE_AC","AMBULANCE_TAXI_OXY" ,"AMBULANCE_TAXI"]
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
        evAutoRickshaw : {
          name : "EV Auto Rickshaw",
          image : "ny_ic_auto_quote_list,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_auto_quote_list.png",
          leftViewImage : "ny_ic_auto_left_view,"
        },
        bookAny : {
          name : "Book Any",
          image : "ny_ic_cab_auto_green,https://assets.moving.tech/beckn/common/user/images/ny_ic_cab_auto_green.png",
          leftViewImage : ","
        },
        bike : {
          name : "Bike",
          image : "ny_ic_bike_right_side,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_bike_right_side.png",
          leftViewImage : "ny_ic_bike_side,"
        },
        suvPlus : {
          name: "XL Plus",
          image: "ny_ic_suv_plus_side,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_suv_plus_side.png",
          leftViewImage: "ny_ic_suv_plus_left_side,"
        },
        deliveryBike : {
          name : "Bike Delivery",
          image : "ny_ic_bike_delivery,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_bike_delivery.png",
          leftViewImage : "ny_ic_bike_delivery_left_view,"
        },
        ambulanceTaxi : {
          name : "Ambulance Taxi",
          image : "ny_ic_ambulance_noac_nooxy,https://assets.moving.tech/beckn/jatrisaathi/user/images/ny_ic_ambulance_noac_nooxy.png",
          leftViewImage : "ny_ic_ambulance_noac_nooxy,"
        },
        ambulanceTaxiOxy : {
          name : "Ambulance Taxi with Oxygen",
          image : "ny_ic_ambulance_noac_oxy,https://assets.moving.tech/beckn/jatrisaathi/user/images/ny_ic_ambulance_noac_oxy.png",
          leftViewImage : "ny_ic_ambulance_noac_oxy,"
        },
        ambulanceAc : {
          name : "Ambulance AC",
          image : "ny_ic_ambulance_ac_nooxy,https://assets.moving.tech/beckn/jatrisaathi/user/images/ny_ic_ambulance_ac_nooxy.png",
          leftViewImage : "ny_ic_ambulance_ac_nooxy,"
        },
        ambulanceAcOxy : {
          name : "Ambulance AC with Oxygen",
          image  : "ny_ic_ambulance_ac_oxy,https://assets.moving.tech/beckn/jatrisaathi/user/images/ny_ic_ambulance_ac_oxy.png",
          leftViewImage : "ny_ic_ambulance_ac_oxy,"
        },
        ambulanceVentilator : {
          name : "Ambulance with Ventilator",
          image : "ny_ic_ambulance_ventilator,https://assets.moving.tech/beckn/jatrisaathi/user/images/ny_ic_ambulance_ventilator.png",
          leftViewImage : "ny_ic_ambulance_ventilator,"
        },
        heritageCab : {
          name : "Heritage Cab",
          image : "ny_ic_heritage_cab_side,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_heritage_cab_side.png",
          leftViewImage : "ny_ic_heritage_cab_left_side,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_heritage_cab_left_side.png"
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
    enableEditPickupLocation : true,
    enableCustomerSupportForSafety : false,
    enableSpecialPickup : EHC.jBridgeMethodExists "locateOnMapV2",
    enableAcPopup : false,
    enableRentalReallocation : true,
    enableEditDestination : false,
    enableHelpAndSupport : true,
    enableBusBooking : false
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
              },
            editPickUpThreshold : 100.0
          }
      , labelTextSize : 30
      , animationDuration : 500
      , vehicleMarkerSize: 150
      , labelTheme : "LIGHT"
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
    , metroHomeBannerImage : "ny_ic_chennai_metro_discount_banner"
    , metroBookingBannerImage : "ny_ic_chennai_metro_banner"
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
    , metroHomeBannerImage : "ny_ic_kochi_metro_banner"
    , metroBookingBannerImage : "ny_ic_kochi_metro_banner"
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
        menuButtonBackground : "#FFFFFF",
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
      showAdditionalServicesNew : true,
      shimmerTimer : 3
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
        countryName : "India" 
      , countryCode  : "+91" 
      , countryShortCode : "IN"
      }
    ]
  , cityConfig :
      [ defaultCityConfig 
          { cityName = "Bangalore",
            cityCode = "std:080",
            featureConfig {
              enableCabBanner = true
            }
            , enableAcViews = true
            , enableIntercity= true
            , enableScheduling = true
            , enableCabs = false
            , enableRentals = true
            , estimateAndQuoteConfig {
              showInfoIcon = true
              }
            , waitingChargeConfig  {
                cabs  {
                  freeMinutes = 5.0
                , perMinCharges = 1.0
                }
              }
          },
        defaultCityConfig 
          { cityName = "Mysore",
            cityCode = "std:0821",
            featureConfig {
              enableCabBanner = true
            },
            enableAcViews = true,
            enableRentals = true,
            enableIntercity = true,
            enableScheduling = true,
            waitingChargeConfig  {
              cabs  {
                freeMinutes = 5.0
              , perMinCharges = 1.0
              }
            },
            rentalWaitingChargeConfig {
              auto {
                freeMinutes = 3.0
              , perMinCharges = 1.5
              },
              cabs {
                freeMinutes = 3.0
              , perMinCharges = 2.0
              }
            }
          },
        defaultCityConfig 
          { cityName = "Tumakuru",
            cityCode = "std:0816",
            featureConfig {
              enableCabBanner = true
            },
            enableAcViews = true,
            enableRentals = true,
            enableIntercity = true,
            enableScheduling = true,
            waitingChargeConfig  {
              cabs  {
                freeMinutes = 5.0
              , perMinCharges = 1.0
              }
            }
          },
        defaultCityConfig 
          {   cityName = "Chennai"
            , cityCode = "std:044"
            , enableCabs = true
            , enableRentals = true 
            , enableIntercity = true
            , enableScheduling = true
            , estimateAndQuoteConfig {
              showInfoIcon = false
              }
            , featureConfig {
                enableCabBanner = false
              }
          },

           defaultCityConfig 
          {   cityName = "Vellore"
            , cityCode = "std:0452"
            , enableCabs = true
            , enableRentals = true 
            , enableIntercity = true
            , enableScheduling = true
            , estimateAndQuoteConfig {
              showInfoIcon = false
              }
            , featureConfig {
                enableCabBanner = false
              }
          },

           defaultCityConfig 
          {   cityName = "Tirunelveli"
            , cityCode = "std:0462"
            , enableCabs = true
            , enableRentals = true 
            , enableIntercity = true
            , enableScheduling = true
            , estimateAndQuoteConfig {
              showInfoIcon = false
              }
            , featureConfig {
                enableCabBanner = false
              }
          },

          defaultCityConfig 
          {   cityName = "Pudukkottai"
            , cityCode = "std:04322"
            , enableCabs = true
            , enableRentals = true 
            , enableIntercity = true
            , enableScheduling = true
            , estimateAndQuoteConfig {
              showInfoIcon = false
              }
            , featureConfig {
                enableCabBanner = false
              }
          },
          defaultCityConfig 
          {   cityName = "Bidar"
            , cityCode = "std:8482"
            , enableCabs = true
            , enableRentals = true 
            , enableIntercity = true
            , enableScheduling = true
            , estimateAndQuoteConfig {
              showInfoIcon = false
              }
            , featureConfig {
                enableCabBanner = false
              }
          },

           defaultCityConfig 
          {   cityName = "Thanjavur"
            , cityCode = "std:04362"
            , enableCabs = true
            , enableRentals = true 
            , enableIntercity = true
            , enableScheduling = true
            , estimateAndQuoteConfig {
              showInfoIcon = false
              }
            , featureConfig {
                enableCabBanner = false
              }
          },

           defaultCityConfig 
          {   cityName = "Madurai"
            , cityCode = "std:0452"
            , enableCabs = true
            , enableRentals = true 
            , enableIntercity = true
            , enableScheduling = true
            , estimateAndQuoteConfig {
              showInfoIcon = false
              }
            , featureConfig {
                enableCabBanner = false
              }
          },

           defaultCityConfig 
          {   cityName = "Salem"
            , cityCode = "std:0427"
            , enableCabs = true
            , enableRentals = true 
            , enableIntercity = true
            , enableScheduling = true
            , estimateAndQuoteConfig {
              showInfoIcon = false
              }
            , featureConfig {
                enableCabBanner = false
              }
          },

           defaultCityConfig 
          {   cityName = "Hosur"
            , cityCode = "std:04344"
            , enableCabs = true
            , enableRentals = true 
            , enableIntercity = true
            , enableScheduling = true
            , estimateAndQuoteConfig {
              showInfoIcon = false
              }
            , featureConfig {
                enableCabBanner = false
              }
          },

           defaultCityConfig 
          {   cityName = "Trichy"
            , cityCode = "std:0431"
            , enableCabs = true
            , enableRentals = true 
            , enableIntercity = true
            , enableScheduling = true
            , estimateAndQuoteConfig {
              showInfoIcon = false
              }
            , featureConfig {
                enableCabBanner = false
              }
          },


           defaultCityConfig 
          {   cityName = "TamilNaduCities"
            , cityCode = "std:0422"
            , enableCabs = true
            , enableRentals = true 
            , enableIntercity = true
            , enableScheduling = true
            , estimateAndQuoteConfig {
              showInfoIcon = false
              }
            , featureConfig {
                enableCabBanner = false
              }
          },


        defaultCityConfig
          { cityName = "Hyderabad",
            cityCode = "std:040",
            enableIntercity = true,
            enableScheduling = true,
            iopConfig = {
            enable :EHC.os /= "IOS",
            autoSelectTime : 8
          },
            referral
              { domain = "https://www.manayatri.in",
                customerAppId = "in.mobility.manayatri"
              },
            dashboardUrl = "https://www.manayatri.in/open?source=in-app",
            appLogoLight = "ny_ic_logo_dark_my,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_logo_dark_my.png",
            appLogo = "ic_namma_yatri_logo,https://assets.moving.tech/beckn/nammayatri/user/images/ic_namma_yatri_logo.png",
            waitingChargeConfig {
              auto {
                freeMinutes = 3.0
              , perMinCharges = 2.0
              },
              cabs  {
                freeMinutes = 3.0
              , perMinCharges = 2.0
              }
            },
            rentalWaitingChargeConfig  {
              auto{
                freeMinutes = 5.0
              , perMinCharges = 1.0
              },
              cabs  {
                freeMinutes = 5.0
              , perMinCharges = 1.0
      }
    }
          },
        defaultCityConfig
          { cityName = "Kolkata",
            cityCode = "std:033",
            referral
              { domain = "https://www.yatrisathi.in",
                customerAppId = "in.juspay.jatrisaathi"
              }
            , enableRentals = true
            , enableIntercity = true
            , enableCabs = true
            , enableAcViews = true
            , estimateAndQuoteConfig {
              showInfoIcon = true
              }
            , featureConfig {
                enableCabBanner = true
              }
            , waitingChargeConfig {
                cabs {
                  freeMinutes = 3.0
                , perMinCharges = 2.0
                },
                ambulance {
                  freeMinutes = 8.0
                , perMinCharges = 2.0
               }
            }
            , rentalWaitingChargeConfig {
                cabs {
                  freeMinutes = 3.0
                , perMinCharges = 2.0
                }
              }
            , intercityWaitingChargeConfig {
              cabs {
                freeMinutes = 3.0
              , perMinCharges = 2.0
              }
            }
          },
        defaultCityConfig
          { cityName = "Siliguri",
            cityCode = "std:0353",
            referral
              { domain = "https://www.yatrisathi.in",
                customerAppId = "in.juspay.jatrisaathi"
              }
            , enableRentals = true 
            , enableIntercity = true
            , enableAcViews = true
            , enableCabs = true
            , estimateAndQuoteConfig {
              showInfoIcon = true
              }
            , featureConfig {
                enableCabBanner = true
              }
            , waitingChargeConfig {
                cabs {
                  freeMinutes = 5.0
                , perMinCharges = 2.0
              },
                ambulance {
                  freeMinutes = 8.0
                , perMinCharges = 2.0
               }
            }
            , rentalWaitingChargeConfig {
                cabs {
                  freeMinutes = 5.0
                , perMinCharges = 2.0
                }
              }
            , intercityWaitingChargeConfig {
              cabs {
                freeMinutes = 5.0
              , perMinCharges = 2.0
              }
            }
          },
        defaultCityConfig
          { cityName = "Delhi",
            cityCode = "std:011",
            waitingChargeConfig {
              auto {
                freeMinutes = 3.0
              , perMinCharges = 0.75
              } ,
              cabs  {
                freeMinutes = 5.0
              , perMinCharges = 1.5
              }
            }
          },
        defaultCityConfig
          { cityName = "Kochi",
            cityCode = "std:0484",
            enableIntercity = true,
            waitingChargeConfig = keralaWTC
          },
        defaultCityConfig
          { cityName = "Trivandrum",
            cityCode = "std:0471",
            waitingChargeConfig = keralaWTC,
            enableIntercity = true
          },
        defaultCityConfig
          { cityName = "Thrissur",
            cityCode = "std:0487",
            waitingChargeConfig = keralaWTC,
            enableIntercity = true
          },

        defaultCityConfig
          { cityName = "Kozhikode",
            cityCode = "std:0495",
            waitingChargeConfig = keralaWTC,
            enableIntercity = true
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
  , maxVehicleIconsToShowOnMap : 10
  , referral : {
          domain : "https://nammayatri.in/"
        , customerAppId : "in.juspay.nammayatri"
      }
  , safety : {
      pastRideInterval : 60,
      followingInterval : 5,
      safetyTeamNumber : "08069724911"
  }
  , enableBookAny : true
  , acPopupConfig : {
      enableAcPopup : false,
      enableNonAcPopup : false,
      showAfterTime : 5
    }
  , enableDeliveryService : false
  , ambulanceConfig : { radius : 50000
      , strictBounds : false
      }
  , showRecommendedText : false
  , showFasterText : true
}

defaultCityConfig :: CityConfig
defaultCityConfig =
  case (getMerchant FunctionCall) of
    YATRISATHI -> ysDefaultCityConfig
    _ -> allCitiesDefCityConfig


allCitiesDefCityConfig :: CityConfig
allCitiesDefCityConfig = 
  { cityName : "",
    cityCode : "",
    iopConfig : {
            enable : false,
            autoSelectTime : 8
          },
    estimateAndQuoteConfig : {
          showInfoIcon : true},
    geoCodeConfig :
      { radius : 50000
      , strictBounds : false
      },
    enableCabs : false,
    featureConfig : {
      enableCabBanner : false
    },
    referral : {
      domain : "https://nammayatri.in",
      customerAppId : "in.juspay.nammayatri"
    },
    appLogo : "ic_namma_yatri_logo",
    dashboardUrl : "",
    appLogoLight : "",
    enableAcViews : false,
    enableRentals : false,
    enableIntercity : false, 
    enableScheduling : false,
    enableIntercityBus : true,
    waitingChargeConfig : {
      auto : {
        freeMinutes : 3.0
      , perMinCharges : 1.5
      },
      cabs : {
        freeMinutes : 5.0
      , perMinCharges : 1.5
      },
      bike : {
        freeMinutes : 3.0
      , perMinCharges : 2.0
      },
      ambulance : {
        freeMinutes : 480.0
      , perMinCharges : 2.0
      }
    },
    rentalWaitingChargeConfig : {
      auto : {
        freeMinutes : 3.0
      , perMinCharges : 1.0
      },
      cabs : {
        freeMinutes : 3.0
      , perMinCharges : 1.0
      },
      bike : {
        freeMinutes : 3.0
      , perMinCharges : 2.0
      },
      ambulance : {
        freeMinutes : 480.0
      , perMinCharges : 2.0
      }
    },
    intercityWaitingChargeConfig :{
      auto : {
        freeMinutes : 3.0
      , perMinCharges : 1.0
      } ,
      cabs : {
        freeMinutes : 5.0
      , perMinCharges : 1.0
      } ,
      bike : {
        freeMinutes : 3.0
      , perMinCharges : 2.0
      },
      ambulance : {
        freeMinutes : 480.0
      , perMinCharges : 2.0
      }
    },
    enableWaitingConfig : true,
    allowBlockedUserLogin : true
  }

ysDefaultCityConfig :: CityConfig
ysDefaultCityConfig = allCitiesDefCityConfig {
    enableCabs = true,
    featureConfig {
      enableCabBanner = true
    },
    referral {
      domain = "https://www.yatrisathi.in",
      customerAppId = "in.juspay.jatrisaathi"
    },
    enableAcViews = true,
    enableRentals = true,
    enableIntercity = true, 
    waitingChargeConfig {
      auto {
        freeMinutes = 5.0
      , perMinCharges = 1.5
      },
      cabs {
        freeMinutes = 3.0
      , perMinCharges = 2.0
      }
    },
    rentalWaitingChargeConfig {
      auto {
        freeMinutes = 3.0
      , perMinCharges = 1.0
      },
      cabs {
        freeMinutes = 3.0
      , perMinCharges = 1.0
      }
    },
    intercityWaitingChargeConfig {
      auto {
         freeMinutes = 3.0
       , perMinCharges = 1.0
      } ,
      cabs {
        freeMinutes = 5.0
      , perMinCharges = 1.0
      }
    },
    enableWaitingConfig = false
  }

keralaWTC :: WaitingChargeConfig
keralaWTC = defaultCityConfig.waitingChargeConfig {
              auto {
                freeMinutes = 3.0
              , perMinCharges = 1.0
              },
              cabs {
                freeMinutes = 5.0
              , perMinCharges = 1.0
              }
      }