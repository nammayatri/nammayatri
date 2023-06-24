window.getMerchantConfig = function () {
  return JSON.stringify({
    "StringKeys": ["YOUR_LOCATION_HELPS_OUR_SYSTEM", "LOCATION_PERMISSION_SUBTITLE_NEW_USER"],
    "isReferralEnabled": "true",
    "showBookingPreference": "true",
    "showRateCard": "true",
    "showDashboard": "true",
    "enableShareRide": "true",
    "shareAppTitle": "Share Namma Yatri!",
    "shareAppContent": "Hey there!\n\nCheck India's first Zero Commission auto booking app.\n100% Open source | 100% Open Data\n\nDownload Namma Yatri now! \nhttps://nammayatri.in/link/rider/SJ8D \n\n #beOpen #chooseOpen",
    "DOCUMENT_LINK": "https://docs.google.com/document/d/1-oRR_oI8ncZRPZvFZEJZeCVQjTmXTmHA",
    "APP_LINK": getAppLink(window.__OS),
    "PRIVACY_POLICY_LINK": "https://docs.google.com/document/d/128VU80K5E1iz-x6QnP1R127m_lwmDO3F",
    "CUSTOMER_TIP": "true",
    "isShareAppEnabled": "true",
    "apiLoaderLottie": "primary_button_loader.json",
    "isEmergencyContacts": "true",
    "isChatEnabled": "true",
    "addFavouriteScreenBackArrow" : "ny_ic_chevron_left_white,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_left_white.png",
    "primaryTextColor": "#FCC32C",
    "primaryBackground": "#2C2F3A",
    "searchLocationTheme": "#2C2F3A",
    "estimateConfirmText": "Request a NammaYatri Ride",
    "autoConfirmingLoaderColor": "#80B2FF",
    "quoteListModelBackground": "#2C2F3A",
    "confirmPickUpLocationBorder": "#E5E7EB",
    "logs": ["JUSPAY","FIREBASE"],
    "quoteListModel": {
      "backgroundColor": "#2C2F3A",
      "textColor": "#FFFFFF",
      "loaderColor": "#80B2FF", 
      "otpTextBackground"  : "#2C2F3A",
      "otpBackground" : "#F1F1F1",
      "otpTextColor" : "#FFFFFF",
      "otpTitleColor"  : "#6D7280"
    },
    "profileBackground": "#2C2F3A",
    "profileName": "#FFFFFF",
    "profileImage": "#012A72",
    "feedbackBackground": "#2C2F3A",
    "profileCompletion" : "#FCC32C",
    "cancelRideColor" : "#E55454",
    "merchantLogo" : "ic_launcher,https://assets.juspay.in/nammayatri/images/user/ny_ic_launcher.png",
    "infoIconUrl" : "ny_ic_info,https://assets.juspay.in/nammayatri/images/user/ny_ic_information_grey.png",
    "sideBarList": ["MyRides", "Favorites", "EmergencyContacts", "HelpAndSupport", "Language", "Separator", "ShareApp", "LiveStatsDashboard", "About", "Logout"],
    "rateCardColor": "#2C2F3A",
    "profileEditGravity" : "center",
    "nyBrandingVisibility": false,
    "fontType": "Assets",
    "black900": "#2C2F3A",
    "black800": "#454545",
    "red" : "#E55454",
    "popupBackground" : "#FFFFFF",
    "englishStrings": {
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "Your location helps our system to map down all the near by autos and get you the quickest ride possible.",
      "REQUEST_RIDE": "Request a NammaYatri Ride",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "Welcome to Namma Yatri! \nTo start booking rides, we require your device location."
    },
    "hindiStrings": {
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "आपका स्थान हमारे सिस्टम को आस पास के सभी ऑटो को मैप करने में सहायता कर्ता है और आपको सबसे तेज सवारी प्रदान करता है",
      "REQUEST_RIDE": "नम्मायात्री राइड का अनुरोध करें",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "नम्मा यात्री में आपका स्वागत है! \nराइड बुक करना शुरू करने के लिए, हमें आपके डिवाइस की लोकेशन की आवश्यकता होती है।"
    },
    "kannadaStrings": {
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "ನಿಮ್ಮ ಸ್ಥಳವು ನಮ್ಮ ಸಿಸ್ಟಂ ಅನ್ನು ಆಟೋಗಳ ಮೂಲಕ ನಕ್ಷೆ ಮಾಡಲು ಸಹಾಯ ಮಾಡುತ್ತದೆ ಮತ್ತು ನಿಮಗೆ ಸಾಧ್ಯವಿರುವ ವೇಗದ ಸವಾರಿಯನ್ನು ನೀಡುತ್ತದೆ.",
      "REQUEST_RIDE": "ನಮ್ಮಯಾತ್ರಿ ಸವಾರಿಗೆ ವಿನಂತಿಸಿ",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "ನಮ್ಮ ಯಾತ್ರಿಗೆ ಸುಸ್ವಾಗತ! \nಬುಕಿಂಗ್ ರೈಡ್‌ಗಳನ್ನು ಪ್ರಾರಂಭಿಸಲು, ನಮಗೆ ನಿಮ್ಮ ಸಾಧನದ ಸ್ಥಳದ ಅಗತ್ಯವಿದೆ."
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
    "bannerConfig" : {
      "backgroundColor" : "#F0FAF0"
    , "title" : "Complete your profile for a personalised ride experience"
    , "titleColor" : "#21C179"
    , "actionText" : "Update now"
    , "actionTextColor" : "#27AE5F"
    , "imageUrl" : "ny_ic_banner_gender_feat,https://assets.juspay.in/beckn/merchantcommon/images/ny_ic_banner_gender_feat.png" 
    },
    "showCorporateAddress" : true
  })
}

function getAppLink(os) {
  if (os == "ANDROID") {
    return "https://play.google.com/store/apps/details?id=in.juspay.nammayatri"
  } else {
    return "https://apps.apple.com/in/app/namma-yatri/id1637429831"
  }
}