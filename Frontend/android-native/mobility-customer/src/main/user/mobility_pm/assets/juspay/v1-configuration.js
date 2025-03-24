window.version = window.version || {};
let version = "1.0.0";
if (typeof __VERSION__ !== "undefined") {
  version = __VERSION__
}

window.version["configuration"]= version;
window.getMerchantConfig = function () {
  return JSON.stringify({
    "StringKeys": ["YOUR_LOCATION_HELPS_OUR_SYSTEM"],
    "isReferralEnabled": "true",
    "showBookingPreference": "true",
    "showRateCard": "true",
    "showDashboard": "false",
    "enableShareRide": "false",
    "autoVariantEnabled": true,
    "shareAppTitle": "Share Namma Yatri!",
    "shareAppContent": "Hey there!\n\nCheck India's first Zero Commission auto booking app.\n100% Open source | 100% Open Data\n\nDownload Namma Yatri now! \nhttps://nammayatri.in/link/rider/SJ8D \n\n #beOpen #chooseOpen",
    "DOCUMENT_LINK": "https://docs.google.com/document/d/1-oRR_oI8ncZRPZvFZEJZeCVQjTmXTmHA",
    "appLink": "",
    "PRIVACY_POLICY_LINK": "https://docs.google.com/document/d/1crETbSJ7QDqh1BCBjOnDUvJzwV5zb3gC",
    "customerTip" : {
      "auto" : true,
      "cabs" : true
    },
    "showChargeDesc" : false,
    "isShareAppEnabled": "false",
    "showPickUpandDrop": true,
    "loaderColor": "",
    "apiLoaderLottie": "Payments-Loader.json",
    "isEmergencyContacts": "false",
    "isChatEnabled": "false",
    "showHamMenu": true,
    "showQuoteFindingText": false,
    "showCorporateAddress": true,
    "primaryTextColor": "#FFFFFF",
    "isGradient": "false",
    "gradient": [],
    "primaryBackground": "#03B9F5",
    "currency": "₹",
    "alertDialogPrimaryColor": "#00B8F5",
    "primaryButtonCornerRadius": 8.0,
    "cancelSearchTextColor": "#FD5154",
    "estimateConfirmText": "Request a NammaYatri Ride",
    "autoConfirmingLoaderColor": "#00B8F5",
    "quoteListModelBackground": "#F5F9FE",
    "dashboardUrl" : "https://www.getyatri.com/open/?source=in-app",
    "quoteListItemConfig": {
      "primaryButtonCorner": 8.0,
      "expiresColor": "#EB5757",
      "driverImagebg": "#F1F1F1",
      "vehicleHeight": 37,
      "vehicleWidth": 40
    },
    "searchLocationConfig": {
      "searchLocationTheme": "#012A72",
      "setLocationOnMapColor": "#101010",
      "strokeColor": "1,#13101010",
      "backgroundColor" : "#012A72",
      "enableLocationTagbar": "true",
      "editTextBackground" : "#07101010",
      "editTextColor" : "#FFFFFF",
      "hintColor" : "#50FFFFFF",
      "editTextDefaultColor" : "#FFFFFF",
      "resultsCardCornerRadius": 8.0,
      "showRateCardDetails": true,
      "showAdditionalChargesText" : false,
      "lottieHeight": 96,
      "enableRateCard" : true,
          
      "lottieWidth": 96,
      "primaryButtonHeight": 60,
      "backArrow": "ny_ic_chevron_left_white,https://assets.juspay.in/beckn/mobilitypaytm/user/ny_ic_chevron_left_white.png"
    },
    "ratingConfig": {
      "secondaryButtonTextColor": "#00B8F5",
      "secondaryButtonStroke": "1,#00B8F5",
      "buttonCornerRadius": 8.0
    },
    "cancelReasonConfig": {
      "secondaryButtonTextColor": "#00B8F5",
      "secondaryButtonStroke": "1,#00B8F5",
      "buttonCornerRadius": 8.0
    },
    "driverInfoConfig": {
      "ratingTextColor": "#101010",
      "ratingBackground": "#FFFFFF",
      "ratingStroke": "1,#10101012",
      "ratingCornerRadius": 6.0,
      "callBackground": "#2053BB6F",
      "callButtonStroke": "0,#EB0055",
      "cardStroke": "1,#E5E7EB",
      "otpStroke": "1,#10101021",
      "numberPlateBackground" : "#E9BE4D",
      "showCancelPrevention" : true,
      "showNumberPlatePrefix": true,
      "showNumberPlateSuffix": false,
      "showTrackingButton": false,
      "callHeight": 24,
      "callWidth": 24,
      "closeIcon" : "ny_ic_close_white,https://assets.juspay.in/beckn/mobilitypaytm/user/ny_ic_close_white.png",
      "footerVisibility" : true,
      "footerImageUrl" : "ic_namma_yatri_logo,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_namma_yatri_logo.png",
      "footerBackgroundColor" : "#FFFFFF"
    },
    "quoteListModel": {
      "backgroundColor": "#012A72",
      "textColor": "#FFFFFF",
      "loaderColor": "#00B8F5",
      "otpBackground": "#F5F9FE",
      "otpTextBackground": "#FFFFFF",
      "otpTextColor": "#101010",
      "otpTitleColor": "#101010",
      "selectRideTextColor": "#101010",
      "lineImage": "ic_line",
      "lottieHeight": 300,
      "lottieWidth": 300,
      "topMargin": 100,
      "noQuotesImageHeight": 115,
      "noQuotesImageWidth": 137
    },
    "profileBackground": "#012A72",
    "profileName": "#FFFFFF",
    "profileCompletion": "#FFFFFF",
    "profileImage": "#012A72",
    "feedbackBackground": "#D3D3D3",
    "sideBarList": ["MyRides", "Favorites", "HelpAndSupport", "Language", "About"],
    "rateCardColor": "#00B8F5",
    "nyBrandingVisibility": true,
    "fontType": "System",
    "black900": "#101010",
    "black800": "#101010",
    "red": "#FD5154",
    "popupBackground": "#FFFFFF",
    "addFavouriteScreenBackArrow": "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_left.png",
    "cancelRideColor": "#101010",
    "infoIconUrl": "ny_ic_info_blue,https://assets.juspay.in/beckn/mobilitypaytm/mobilitypaytmcommon/ny_ic_info_blue.png",
    "profileEditGravity": "bottom",
    "merchantLogo": "ny_ic_paytm_logo,https://assets.juspay.in/beckn/mobilitypaytm/user/ny_ic_paytm_logo.png",
    "confirmPickUpLocationBorder": "#13101010",
    "logs": ["JUSPAY"],
    "englishStrings": {
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "Your location helps our system to map down all the near by autos and get you the quickest ride possible."
    },
    "hindiStrings": {
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "आपका स्थान हमारे सिस्टम को आस पास के सभी ऑटो को मैप करने में सहायता कर्ता है और आपको सबसे तेज सवारी प्रदान करता है"
    },
    "kannadaStrings": {
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "ನಿಮ್ಮ ಸ್ಥಳವು ನಮ್ಮ ಸಿಸ್ಟಂ ಅನ್ನು ಆಟೋಗಳ ಮೂಲಕ ನಕ್ಷೆ ಮಾಡಲು ಸಹಾಯ ಮಾಡುತ್ತದೆ ಮತ್ತು ನಿಮಗೆ ಸಾಧ್ಯವಿರುವ ವೇಗದ ಸವಾರಿಯನ್ನು ನೀಡುತ್ತದೆ."
    },
    "languageList": [{
      "name": "English",
      "value": "EN_US",
      "subTitle": ""
    }, {
      "name": "ಕನ್ನಡ",
      "value": "KN_IN",
      "subTitle": "Kannada"
    }, {
      "name": "हिंदी",
      "value": "HI_IN",
      "subTitle": "Hindi"
    }],
    "bannerConfig": {
      "backgroundColor": "#F0FAF0",
      "title": "Complete your profile for a personalised ride experience",
      "titleColor": "#21C179",
      "actionText": "Update now",
      "actionTextColor": "#27AE5F",
      "imageUrl": "ny_ic_banner_gender_feat,https://assets.juspay.in/beckn/merchantcommon/images/ny_ic_banner_gender_feat.png"
    },
    "terminateBtnConfig": {
      "visibility": false,
      "title": "Paytm",
      "imageUrl": "ny_ic_chevron_left_double,https://assets.juspay.in/beckn/mobilitypaytm/mobilitypaytmcommon/ny_ic_chevron_left_double.png"
    },
    "showDeleteAccount": false,
    "showGenderBanner": false,
    "autoSelectBackground": "#53BB6F",
    "enableMockLocation": false,
    "isAdvancedBookingEnabled" : true,
    "defaultLanguage": "EN_US",
    "fontKannada": "Roboto",
    "tipEnabledCities" : ["Bangalore", "Hyderabad"],
    "fontName": "Roboto"
    , "navigationAppConfig" : {
      "query" : "google.navigation:q=%f,%f"
      , "packageName" : "com.google.android.apps.maps"
    }
    ,  "rideCompletedCardConfig" : {
      "topCard" : {
        "gradient" : "#012A72"
        , "enableGradient" : false
        , "background" : "#012A72"
        , "titleColor" : "#60FFFFFF"
        , "rideDescription" : {
          "background" : "#22101010"
          , "textColor" : "#FFFFFF"
        }
        , "horizontalLineColor" : "#60FFFFFF"
      }
    }
    , "feature" : {
      "enableAutoReadOtp" : false,
      "enableShareRide" : false,
      "enableChat": false,
      "enableEmergencyContacts": false,
      "enableReferral": false,
      "enableSupport": false,
    }
    , "dashboard" : {
      "enable" : false
    }
    , "fontConfig" : {
      "type" : "System"
    }
    , "estimateAndQuoteConfig" :
      { "enableOnlyAuto" : true
        , "textColor" :  "#101010"
      }
    , "homeScreen" : {
      "primaryBackground" : "#012A72",
      "pickUpViewColor" : "#22101010",
      "pickupLocationTextColor" : "#FFFFFF",
      "bannerViewVisibility" : false,
      "header" : {
        "menuButtonBackground" : "#22101010",
        "showLogo" : false,
        "titleColor" : "#FFFFFF",
        "showSeparator" : false,
      },
      "whereToButton" : {
        "margin" : {
          "top" : 3
        },
        "shadow" : {
          "x": 0.3,
          "y": 0.3,
          "blur": 3.0,
          "spread": 10.0
        }
      }
    }
    , "locationTagBar" : {
      "cornerRadius" : 32.0
      ,  "textColor" : "#54101010"
      ,   "stroke" : "1,#13101010"
    }
    , "loaderConfig" : {
      "color" : "#00B8F5"
    }
    , "primaryButtonConfig" : {
      "isGradient" : false
      , "gradient" : []
      , "loaderUrl" : "https://assets.juspay.in/beckn/mobilitypaytm/user/lottie/Payments-Loader.json"
    }
  })
}