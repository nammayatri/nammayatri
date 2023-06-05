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
    "StringKeys": ["YOUR_LOCATION_HELPS_OUR_SYSTEM", "REQUEST_RIDE", "ABOUT_APP_DESCRIPTION", "GETTING_ESTIMATES_FOR_YOU"],
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
    "REQUEST_RIDE" : "ನಮ್ಮಯಾತ್ರಿ ಸವಾರಿಗೆ ವಿನಂತಿಸಿ",
    "ABOUT_APP_DESCRIPTION" : "ಒಂದು ಕ್ಲಿಕ್‌ನ ಅನುಕೂಲಕ್ಕಾಗಿ ನಿಮ್ಮ ಆಟೋ ರೈಡ್‌ಗಳನ್ನು ಬುಕ್ ಮಾಡಲು Paytm ನಿಮಗೆ ವೈಶಿಷ್ಟ್ಯವನ್ನು ಒದಗಿಸುತ್ತದೆ. ಪ್ಲಾಟ್‌ಫಾರ್ಮ್ ಸ್ವಯಂ ಬುಕಿಂಗ್‌ಗಾಗಿ ನಮ್ಮ ಯಾತ್ರಿ ಓಪನ್ ಸೋರ್ಸ್ ಕೋಡ್ ಅನ್ನು ಆಧರಿಸಿದೆ ಮತ್ತು ಇದು ವೆಚ್ಚ ಪರಿಣಾಮಕಾರಿ ಮತ್ತು ಬಳಸಲು ಸುಲಭವಾಗಿದೆ. ನಿಮ್ಮ ಹತ್ತಿರವಿರುವ ಡ್ರೈವರ್‌ಗಳೊಂದಿಗೆ ಸಂಪರ್ಕಿಸಲು ನಿಮ್ಮ ಪಿಕಪ್ ಮತ್ತು ಡ್ರಾಪ್ ಸ್ಥಳಗಳನ್ನು ಸರಳವಾಗಿ ಇರಿಸಿ. ಬುಕ್ ಮಾಡಿ ಮತ್ತು ಈಗಲೇ ಹೋಗಿ!",
    "GETTING_ESTIMATES_FOR_YOU" : "ನಮ್ಮ ಯಾತ್ರಿ ನಿಮ್ಮ ಬಳಿ ಸವಾರಿಗಳನ್ನು ಹುಡುಕುತ್ತಿದೆ...Namma Yatri is finding rides near you..."
  }
  
  const hindiStrings = {
    "YOUR_LOCATION_HELPS_OUR_SYSTEM" : "आपका स्थान हमारे सिस्टम को आस पास के सभी ऑटो को मैप करने में सहायता कर्ता है और आपको सबसे तेज सवारी प्रदान करता है",
    "REQUEST_RIDE" : "नम्मायात्री राइड का अनुरोध करें",
    "ABOUT_APP_DESCRIPTION" : "पेटीएम आपको एक क्लिक की सुविधा के साथ अपनी ऑटो सवारी बुक करने की सुविधा प्रस्तुत करता है। प्लेटफॉर्म ऑटो बुकिंग के लिए नम्मा यात्री ओपन सोर्स कोड पर आधारित है और यह लागत प्रभावी और उपयोग में आसान दोनों है। अपने आस-पास के ड्राइवरों से जुड़ने के लिए बस अपना पिकअप और ड्रॉप स्थान डालें। बुक करें और अभी जाएं!",
    "GETTING_ESTIMATES_FOR_YOU" : "नम्मा यात्री आपके आस-पास सवारी ढूंढ रहा है..."
  }
  
  const englishStrings = {
    "YOUR_LOCATION_HELPS_OUR_SYSTEM": "Your location helps our system to map down all the near by autos and get you the quickest ride possible.",
    "REQUEST_RIDE" : "Request a NammaYatri Ride",
    "ABOUT_APP_DESCRIPTION" : "Paytm presents you a feature to Book your Auto rides with the convenience of a click. The platform is based on the Namma Yatri Open source code for Auto bookings and is both cost effective and easy to use. Simply put in your pickup & drop locations to connect with drivers near you. Book & go now!",
    "GETTING_ESTIMATES_FOR_YOU" : "Namma Yatri is finding rides near you..."
  }
  
  