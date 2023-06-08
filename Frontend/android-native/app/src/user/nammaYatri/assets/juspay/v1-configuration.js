window.getMerchantConfig = function () {
  return JSON.stringify({
    "StringKeys": ["YOUR_LOCATION_HELPS_OUR_SYSTEM"],
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
    "apiLoaderLottie": "primary_button_loader",
    "isEmergencyContacts": "true",
    "isChatEnabled": "true",
    "primaryTextColor": "#FCC32C",
    "primaryBackground": "#2C2F3A",
    "searchLocationTheme": "#2C2F3A",
    "estimateConfirmText": "Request a NammaYatri Ride",
    "autoConfirmingLoaderColor": "#80B2FF",
    "quoteListModelBackground": "#2C2F3A",
    "quoteListModel": {
      "backgroundColor": "#2C2F3A",
      "textColor": "#FFFFFF",
      "loaderColor": "#80B2FF"
    },
    "profileBackground": "#2C2F3A",
    "profileName": "#FFFFFF",
    "profileImage": "#012A72",
    "feedbackBackground": "#2C2F3A",
    "sideBarList": ["MyRides", "Favorites", "EmergencyContacts", "HelpAndSupport", "Language", "Separator", "ShareApp", "LiveStatsDashboard", "Logout"],
    "otpBackground": "#F1F1F1",
    "otpTextColor": "#FFFFFF",
    "rateCardColor": "#2C2F3A",
    "nyBrandingVisibility": false,
    "fontType": "Assets",
    "englishStrings": {
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "Your location helps our system to map down all the near by autos and get you the quickest ride possible.",
      "REQUEST_RIDE": "Request a NammaYatri Ride"
    },
    "hindiStrings": {
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "आपका स्थान हमारे सिस्टम को आस पास के सभी ऑटो को मैप करने में सहायता कर्ता है और आपको सबसे तेज सवारी प्रदान करता है",
      "REQUEST_RIDE": "नम्मायात्री राइड का अनुरोध करें"
    },
    "kannadaStrings": {
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "ನಿಮ್ಮ ಸ್ಥಳವು ನಮ್ಮ ಸಿಸ್ಟಂ ಅನ್ನು ಆಟೋಗಳ ಮೂಲಕ ನಕ್ಷೆ ಮಾಡಲು ಸಹಾಯ ಮಾಡುತ್ತದೆ ಮತ್ತು ನಿಮಗೆ ಸಾಧ್ಯವಿರುವ ವೇಗದ ಸವಾರಿಯನ್ನು ನೀಡುತ್ತದೆ.",
      "REQUEST_RIDE": "ನಮ್ಮಯಾತ್ರಿ ಸವಾರಿಗೆ ವಿನಂತಿಸಿ"
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
    }]
  })
}

function getAppLink(os) {
  if (os == "ANDROID") {
    return "https://play.google.com/store/apps/details?id=in.juspay.nammayatri"
  } else {
    return "https://apps.apple.com/in/app/namma-yatri/id1637429831"
  }
}