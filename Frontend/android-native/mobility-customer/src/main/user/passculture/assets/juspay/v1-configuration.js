window.version = window.version || {};
let version = "1.0.0";
if (typeof __VERSION__ !== 'undefined') {
  version = __VERSION__
}
window.version["configuration"]= version;
window.getMerchantConfig = function () {
  return JSON.stringify({
    "StringKeys": ["REQUEST_AUTO_RIDE" ,
                         "CONFIRM_RIDE_",
                         "PAYMENT_METHOD_STRING_",
                         "PAYMENT_METHOD_STRING",
                         "PAY_DRIVER_USING_CASH_OR_UPI",
                         "PAY_DRIVER_USING_WALLET",
                         "PAY_DRIVER_USING_CASH_OR_UPI_",
                         "LOCATION_PERMISSION_SUBTITLE_NEW_USER"],
    "isReferralEnabled": "false",
    "showBookingPreference": "true",
    "showRateCard": "true",
    "showDashboard": "false",
    "enableShareRide": "false",
    "autoVariantEnabled": false,
    "shareAppTitle": "Share Pass Culture!",
    "shareAppContent": "Hey there!\n\nCheck India's first Zero Commission auto booking app.\n100% Open source | 100% Open Data\n\nDownload Namma Yatri now! \nhttps://nammayatri.in/link/rider/SJ8D \n\n #beOpen #chooseOpen",
    "DOCUMENT_LINK": "https://docs.google.com/document/d/1-oRR_oI8ncZRPZvFZEJZeCVQjTmXTmHA",
    "appLink" : "",
    "PRIVACY_POLICY_LINK": "https://docs.google.com/document/d/128VU80K5E1iz-x6QnP1R127m_lwmDO3F",
    "showChargeDesc" : false,
    "isShareAppEnabled": "false",
    "addFavouriteScreenBackArrow" : "ny_ic_chevron_left_white,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_left_white.png",
    "showPickUpandDrop": false,
    "apiLoaderLottie": "",
    "isEmergencyContacts": "false",
    "showCorporateAddress" : false,
    "isChatEnabled": "false",
    "loaderColor": "#EB0055",
    "isGradient" : "true",
    "gradient": ["#EB0055", "#320096"],
    "primaryTextColor": "#FFFFFF",
    "primaryBackground": "#EB0055",
    "estimateConfirmText": "Request a PassCulture Ride",
    "autoConfirmingLoaderColor": "#80B2FF",
    "quoteListModelBackground": "#2C2F3A",
    "confirmPickUpLocationBorder": "#E5E7EB",
    "currency": "€",
    "alertDialogPrimaryColor": "#EB0055",
    "logs": ["JUSPAY"],
    "primaryButtonCornerRadius" : 24.0,
    "cancelSearchTextColor": "#EB0055",
    "quoteListItemConfig": {
      "primaryButtonCorner": 24.0,
      "expiresColor" : "#EB0055",
      "driverImagebg": "#FED0E3",
      "vehicleHeight" : 57,
      "vehicleWidth": 57
    },
    "searchLocationConfig": {
      "searchLocationTheme": "#EB0055",
      "setLocationOnMapColor" : "#EB0055",
      "strokeColor": "1,#717171",
      "enableLocationTagbar" : "false",
      "resultsCardCornerRadius" : 8.0,
      "showRateCardDetails" : false,
      "showAdditionalChargesText" : false,
      "lottieHeight": 40,
      "lottieWidth": 40,
      "primaryButtonHeight": 48
     , "backArrow" : "ny_ic_chevron_left_white,https://assets.juspay.in/beckn/passculture/user/images/ny_ic_chevron_left_white.png"
     },
     "ratingConfig": {
      "secondaryButtonTextColor": "#EB0055",
      "secondaryButtonStroke": "1,#EB0055",
      "buttonCornerRadius": 24.0
     },
     "cancelReasonConfig": {
      "secondaryButtonTextColor": "#EB0055",
      "secondaryButtonStroke": "1,#EB0055",
      "buttonCornerRadius": 24.0
     },
    "quoteListModel": {
      "backgroundColor": "#EB0055",
      "textColor": "#FFFFFF",
      "loaderColor": "#80B2FF",
      "otpTextBackground"  : "#FFFFFF",
      "otpBackground" : "#FFF0F6",
      "otpTextColor" : "#3C2062",
      "otpTitleColor"  : "#6D7280",
      "selectRideTextColor": "#454545",
      "lineImage": "ic_line_white",
      "lottieHeight": 40,
      "lottieWidth": 40,
      "topMargin": 0,
      "noQuotesImageHeight": 225,
      "noQuotesImageWidth": 340,
      "closeIcon" : "ny_ic_close_white,https://assets.juspay.in/beckn/passculture/user/images/ny_ic_close_white.png"
    },
    "driverInfoConfig": {
      "ratingTextColor": "#EB0055",
      "ratingBackground": "#FFF0F6",
      "ratingStroke": "1,#717171",
      "ratingCornerRadius": 24.0,
      "callBackground": "#FFFFFF",
      "callButtonStroke": "1,#EB0055",
      "cardStroke": "1,#CECECE",
      "otpStroke": "1,#717171",
      "callHeight": 16,
      "callWidth": 16,
      "showNumberPlatePrefix": true,
      "showNumberPlateSuffix" : true,
      "numberPlateBackground" : "#FFFFFF",
      "showCancelPrevention" : false
    },
    "profileBackground": "#EB0055",
    "profileName": "#FFFFFF",
    "profileImage": "#012A72",
    "feedbackBackground": "#2C2F3A",
    "profileCompletion" : "#FFFFFF",
    "cancelRideColor" : "#EB0055",
    "merchantLogo" : "ic_launcher,https://assets.juspay.in/nammayatri/images/user/ny_ic_launcher.png",
    "infoIconUrl" : "ny_ic_info,https://assets.juspay.in/nammayatri/images/user/ny_ic_information_grey.png",
    "sideBarList": ["MyRides", "Favorites", "HelpAndSupport", "Language", "About"],
    "rateCardColor": "#2C2F3A",
    "profileEditGravity" : "center",
    "nyBrandingVisibility": false,
    "fontType": "Assets",
    "black900": "#2C2F3A",
    "black800": "#454545",
    "showHamMenu" : true,
    "showQuoteFindingText" : true,
    "red" : "#E55454",
    "popupBackground" : "#FFFFFF",
    "englishStrings": {
      "REQUEST_AUTO_RIDE": "Request Taxi Ride" ,
      "CONFIRM_RIDE_": "Confirm Ride",
      "PAYMENT_METHOD_STRING_": "Pass Culture Wallet",
      "PAYMENT_METHOD_STRING": "Cash",
      "PAY_DRIVER_USING_CASH_OR_UPI": "Pay driver by Cash",
      "PAY_DRIVER_USING_WALLET": "Pay driver using Pass Culture Wallet",
      "PAY_DRIVER_USING_CASH_OR_UPI_": "Already paid using Pass Culture Wallet",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "Welcome to Pass Culture \n To start booking ride, please allow us to \n find you!"
    },
    "frenchStrings": {
      "REQUEST_AUTO_RIDE": "Demande de trajet",
      "CONFIRM_RIDE_" : "Confirmer Ride",
      "PAYMENT_METHOD_STRING": "Espèces",
      "PAYMENT_METHOD_STRING_": "Porte-monnaie Pass Culture",
      "PAY_DRIVER_USING_CASH_OR_UPI": "Payer le chauffeur en espèces",
      "PAY_DRIVER_USING_WALLET" : "Payer le chauffeur avec Pass Culture Wallet",
      "PAY_DRIVER_USING_CASH_OR_UPI_" : "Déjà payé avec Pass Culture Wallet",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "Bienvenue au Pass Culture \n Pour commencer à réserver un trajet, veuillez nous permettre \n de vous trouver !"
    } ,
    "languageList": [{
      "name": "Français",
      "value": "FR_FR",
      "subTitle": "French"
    }],
    "bannerConfig" : {
      "backgroundColor" : "#F0FAF0"
    , "title" : "Complete your profile for a personalised ride experience"
    , "titleColor" : "#21C179"
    , "actionText" : "Update now"
    , "actionTextColor" : "#27AE5F"
    , "imageUrl" : "ny_ic_banner_gender_feat,https://assets.juspay.in/beckn/merchantcommon/images/ny_ic_banner_gender_feat.png"
    },
    "terminateBtnConfig" : {
      "visibility" : true,
      "title" : "",
      "imageUrl" : "ny_ic_go_home,https://assets.juspay.in/beckn/passculture/user/images/ny_ic_go_home.png"
  },
  "showDeleteAccount" : false
  , "fontName" : "Montserrat"
  , "fontKannada" : "Montserrat"
  , "showGenderBanner" : false
  , "autoSelectBackground" : "#320096"
  , "defaultLanguage" : "FR_FR"
  , "enableMockLocation" : false
  , "navigationAppConfig" : {
      android : {
        "query" : "https://waze.com/ul?ll=%f,%f"
      , "packageName" : "com.waze"
      }
    , ios : {
        "query" : "https://www.waze.com/ul?ll=%@,%@&navigate=yes&zoom=17"
      , "walkQuery" : "https://www.waze.com/ul?ll=%@,%@&navigate=yes&zoom=17" // Waze not supports walk mode
      }
    }
  , "dashboardUrl" : "https://www.getyatri.com/open/?source=in-app"
  })
}