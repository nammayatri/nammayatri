export function getMerchantString(key) {
    var selectedLanguage = JBridge.getKeysInSharedPref("LANGUAGE_KEY");
    switch (selectedLanguage) {
      case "HI_IN":
        return getStringHIValue(key);
      case "KN_IN":
        return getStringKNValue(key);
      default:
        return getStringENValue(key);
    }
  }
  
  export function isMerchantString(key) {
    return config.StringKeys.includes(key);
  }
  
  export function getMerchantConfig(key){
    if (key in config){
        return config[key];
    }
    console.error("no value found for key "+ key);
    return "";
  }
  
  export function getStringENValue(key) {
    if (key in englishStrings) {
      return englishStrings[key];
    }
    return "error in getCommonEN";
  }
  
  export function getStringHIValue(key) {
    if (key in hindiStrings) {
      return hindiStrings[key];
    }
    return "error in getCommonHI";
  }
  
  export function getStringKNValue(key) {
    if (key in kannadaStrings) {
      return kannadaStrings[key];
    }
    return "error in getCommonKN";
  }
  
  export function getAppLink(os) {
    if (os == "ANDROID"){
        return "https://play.google.com/store/apps/details?id=in.juspay.nammayatri"
    }else {return "https://apps.apple.com/in/app/namma-yatri/id1637429831"}
  }
  
  const config = {
    "StringKeys": ["YOUR_LOCATION_HELPS_OUR_SYSTEM", "REQUEST_RIDE"],
    "isReferralEnabled" : "true",
    "showBookingPreference" : "true",
    "showRateCard" : "true",
    "showDashboard" : "false",
    "shareAppTitle" : "Share Namma Yatri!",
    "shareAppContent" : "Hey there!\n\nCheck India's first Zero Commission auto booking app.\n100% Open source | 100% Open Data\n\nDownload Namma Yatri now! \nhttps://nammayatri.in/link/rider/SJ8D \n\n #beOpen #chooseOpen",
    "DOCUMENT_LINK" : "https://docs.google.com/document/d/1-oRR_oI8ncZRPZvFZEJZeCVQjTmXTmHA",
    "APP_LINK" : getAppLink(window.__OS),
    "PRIVACY_POLICY_LINK" : "https://docs.google.com/document/d/128VU80K5E1iz-x6QnP1R127m_lwmDO3F",
    "CUSTOMER_TIP" : "true",
    "isShareAppEnabled" : "false",
    "apiLoaderLottie" : "Payments-Loader",
    "isEmergencyContacts" : "false",
    "isChatEnabled" : "false"
  }
  
  const kannadaStrings = {
    "YOUR_LOCATION_HELPS_OUR_SYSTEM" : "ನಿಮ್ಮ ಸ್ಥಳವು ನಮ್ಮ ಸಿಸ್ಟಂ ಅನ್ನು ಆಟೋಗಳ ಮೂಲಕ ನಕ್ಷೆ ಮಾಡಲು ಸಹಾಯ ಮಾಡುತ್ತದೆ ಮತ್ತು ನಿಮಗೆ ಸಾಧ್ಯವಿರುವ ವೇಗದ ಸವಾರಿಯನ್ನು ನೀಡುತ್ತದೆ.",
    "REQUEST_RIDE" : "ನಮ್ಮಯಾತ್ರಿ ಸವಾರಿಗೆ ವಿನಂತಿಸಿ"
  }
  
  const hindiStrings = {
    "YOUR_LOCATION_HELPS_OUR_SYSTEM" : "आपका स्थान हमारे सिस्टम को आस पास के सभी ऑटो को मैप करने में सहायता कर्ता है और आपको सबसे तेज सवारी प्रदान करता है",
    "REQUEST_RIDE" : "नम्मायात्री राइड का अनुरोध करें"
  }
  
  const englishStrings = {
    "YOUR_LOCATION_HELPS_OUR_SYSTEM": "Your location helps our system to map down all the near by autos and get you the quickest ride possible.",
    "REQUEST_RIDE" : "Request a NammaYatri Ride"
  }
  
  