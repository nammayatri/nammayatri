window.version = window.version || {};
let version = "1.0.0";
if (typeof __VERSION__ !== "undefined") {
  version = __VERSION__
}

window.version["configuration"]= version;
window.getMerchantConfig = function () {
  return JSON.stringify({
    "StringKeys": ["YOUR_LOCATION_HELPS_OUR_SYSTEM", "ABOUT_APP_DESCRIPTION", "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL", "YOUR_FEEDBACK_HELPS_US", "LOCATION_PERMISSION_SUBTITLE_NEW_USER"],
    "showPickUpandDrop": false,
    "primaryButtonConfig" : {
      "loaderUrl": "https://assets.juspay.in/beckn/mobilityredbus/user/lottie/primary_button_loader.json"
    },
    "showCorporateAddress": false,
    "loaderColor": "#03B9F5",
    "primaryTextColor": "#FFFFFF",
    "primaryBackground": "#03B9F5",
    "autoConfirmingLoaderColor": "#80B2FF",
    "alertDialogPrimaryColor": "#BD5500",
    "logs": ["JUSPAY"],
    "cancelSearchTextColor": "#D84E55",
    "quoteListItemConfig": {
      "primaryButtonCorner": 8.0,
      "expiresColor": "#EB5757",
      "driverImagebg": "#F1F1F1",
      "vehicleHeight": 57,
      "vehicleWidth": 57
    },
    "searchLocationConfig": {
      "searchLocationTheme": "#F5F9FE",
      "setLocationOnMapColor": "#101010",
      "strokeColor": "1,#13101010",
      "resultsCardCornerRadius": 8.0,
      "showRateCardDetails": true,
      "editTextBackground" : "#DA4C54",
      "editTextDefaultColor" : "#E89398",
      "backArrow": "ny_ic_chevron_left_white,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_left_white.png"
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
    "quoteListModel": {
      "backgroundColor": "#F5F9FE",
      "textColor": "#101010",
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
    "driverInfoConfig": {
      "ratingTextColor": "#D84E55",
      "ratingBackground": "#FFF0F6",
      "ratingStroke": "1,#717171",
      "ratingCornerRadius": 8.0,
      "callBackground": "#2053BB6F",
      "callButtonStroke": "0,#EB0055",
      "cardStroke": "1,#E5E7EB",
      "otpStroke": "1,#10101021",
      "callHeight": 24,
      "callWidth": 24,
      "showNumberPlatePrefix": true,
      "showNumberPlateSuffix": false,
      "showCancelPrevention": false,
      "showTrackingButton": false
    },
    "profileBackground": "#03B9F5",
    "profileName": "#FFFFFF",
    "profileImage": "#012A72",
    "feedbackBackground": "#03B9F5",
    "profileCompletion": "#FFFFFF",
    "cancelRideColor": "#BD5500",
    "merchantLogo": "ic_launcher,https://assets.juspay.in/beckn/mobilityredbus/user/images/ny_ic_launcher.png",
    "infoIconUrl": "ny_ic_info,https://assets.juspay.in/nammayatri/images/user/ny_ic_information_grey.png",
    "sideBarList": ["MyRides", "Favorites", "HelpAndSupport", "Language", "About"],
    "rateCardColor": "#D84E55",
    "nyBrandingVisibility": true,
    "showHamMenu": true,
    "showQuoteFindingText": false,
    "popupBackground": "#FFFFFF",
    "englishStrings": {
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "Your location helps our system to map down all the near by autos and get you the quickest ride possible.",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER": "Welcome to Redbus Mobility! \nTo start booking rides, please allow us to find you!",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL": "Website: <u>https://nammayatri.in/</u>",
      "YOUR_FEEDBACK_HELPS_US": "Your feedback helps us improve the Redbus Mobility experience",
      "ABOUT_APP_DESCRIPTION": "Redbus Mobility is an open platform to connect commuters with transport providers. The app makes it convenient for travellers to find available means of transport, and avail these options by connecting them with service providers "
    },
    "hindiStrings": {
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "आपका स्थान हमारे सिस्टम को आस-पास के सभी ऑटो को मैप करने और आपको सबसे तेज़ संभव सवारी दिलाने में मदद करता है।",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER": "रेडबस मोबिलिटी में आपका स्वागत है! \nसवारी की बुकिंग शुरू करने के लिए, कृपया हमें आपको ढूंढने की अनुमति दें!",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL": "वेबसाइट: <u>https://nammayatri.in/</u>",
      "YOUR_FEEDBACK_HELPS_US": "आपका फीडबैक हमें रेडबस मोबिलिटी अनुभव को बेहतर बनाने में मदद करता है",
      "ABOUT_APP_DESCRIPTION": "रेडबस मोबिलिटी यात्रियों को परिवहन प्रदाताओं के साथ जोड़ने के लिए एक खुला मंच है। ऐप यात्रियों के लिए परिवहन के उपलब्ध साधनों को ढूंढना सुविधाजनक बनाता है, और उन्हें सेवा प्रदाताओं के साथ जोड़कर इन विकल्पों का लाभ उठाता है"
    },
    "kannadaStrings": {
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "ನಿಮ್ಮ ಸ್ಥಳವು ನಮ್ಮ ಸಿಸ್ಟಂ ಅನ್ನು ಆಟೋಗಳ ಮೂಲಕ ಮ್ಯಾಪ್ ಮಾಡಲು ಸಹಾಯ ಮಾಡುತ್ತದೆ ಮತ್ತು ನಿಮಗೆ ತ್ವರಿತವಾದ ಸವಾರಿ ಸಾಧ್ಯ.",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER": "ರೆಡ್‌ಬಸ್ ಮೊಬಿಲಿಟಿಗೆ ಸುಸ್ವಾಗತ! \nಬುಕಿಂಗ್ ರೈಡ್‌ಗಳನ್ನು ಪ್ರಾರಂಭಿಸಲು, ದಯವಿಟ್ಟು ನಿಮ್ಮನ್ನು ಹುಡುಕಲು ನಮಗೆ ಅನುಮತಿಸಿ!",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL": "ವೆಬ್‌ಸೈಟ್: <u>https://nammayatri.in/</u>",
      "YOUR_FEEDBACK_HELPS_US": "ನಿಮ್ಮ ಪ್ರತಿಕ್ರಿಯೆ ನಮಗೆ Redbus Mobility ಅನುಭವವನ್ನು ಸುಧಾರಿಸಲು ಸಹಾಯ ಮಾಡುತ್ತದೆ",
      "ABOUT_APP_DESCRIPTION": "ರೆಡ್‌ಬಸ್ ಮೊಬಿಲಿಟಿಯು ಪ್ರಯಾಣಿಕರನ್ನು ಸಾರಿಗೆ ಪೂರೈಕೆದಾರರೊಂದಿಗೆ ಸಂಪರ್ಕಿಸಲು ಮುಕ್ತ ವೇದಿಕೆಯಾಗಿದೆ. ಈ ಅಪ್ಲಿಕೇಶನ್ ಪ್ರಯಾಣಿಕರಿಗೆ ಲಭ್ಯವಿರುವ ಸಾರಿಗೆ ಸಾಧನಗಳನ್ನು ಹುಡುಕಲು ಅನುಕೂಲಕರವಾಗಿಸುತ್ತದೆ ಮತ್ತು ಸೇವೆ ಒದಗಿಸುವವರೊಂದಿಗೆ ಅವರನ್ನು ಸಂಪರ್ಕಿಸುವ ಮೂಲಕ ಈ ಆಯ್ಕೆಗಳನ್ನು ಪಡೆದುಕೊಳ್ಳಿ "
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
    "terminateBtnConfig": {
      "visibility": true,
      "title": "Paytm",
      "imageUrl": "ny_ic_chevron_left_double,https://assets.juspay.in/beckn/mobilitypaytm/mobilitypaytmcommon/ny_ic_chevron_left_double.png"
    },
    "showDeleteAccount": false,
    "showGenderBanner": false,
    "SUPPORT_EMAIL": "nammayatri.support@juspay.in",
    "rideCompletedGradient": "#F2A2A2"
    , "estimateAndQuoteConfig" : { 
      "enableOnlyAuto" : true 
    }
    , "rideCompletedCardConfig" : {
      "topCard" : {
        "gradient" : "#F2A2A2"
      }
    }
    , "features" : {
      "enableAutoReadOtp" : false,
      "enableShareRide" : false,
      "enableChat": false,
      "enableEmergencyContacts": false,
      "enableReferral": false,
      "enableSupport": false,
      "enableShareApp": false
    }
    , "dashboard" : {
      "enable" : false
    }
    , "fontConfig" : {
      "type" : "System"
    }
  })
}