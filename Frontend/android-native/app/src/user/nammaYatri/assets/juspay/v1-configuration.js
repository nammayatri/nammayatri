window.version = window.version || {};
let version = "1.0.0";
if (typeof __VERSION__ !== "undefined") {
  version = __VERSION__
}
window.version["configuration"]= version;

function getAppLink(os) {
  if (os == "ANDROID") {
    return "https://play.google.com/store/apps/details?id=in.juspay.nammayatri"
  } else {
    return "https://apps.apple.com/in/app/namma-yatri/id1637429831"
  }
}

window.getMerchantConfig = function () {
  return JSON.stringify({
    "StringKeys": ["YOUR_LOCATION_HELPS_OUR_SYSTEM","LOCATION_PERMISSION_SUBTITLE_NEW_USER","CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL", "YOUR_FEEDBACK_HELPS_US", "LEARN_HOW_TEXT", "FARE_INFO_TEXT", "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE", "PLEASE_ENABLE_LOCATION_PERMISSION", "TAXI_FROM_ZONE", "GO_TO_ZONE", "OTP_FOR_THE_JATRI_SATHI_ZONE_HAS_BEEN_EXPIRED_PLEASE_TRY_LOOKING_AGAIN"],
    "logs": ["JUSPAY","FIREBASE","CLEVERTAP"],
    "driverInfoConfig": {
      "showTrackingButton" : false
    },
    "merchantLogo" : "ic_invoice_logo,https://assets.juspay.in/beckn/nammayatri/user/images/ic_invoice_logo.png",
    "englishStrings": {
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "Your location helps our system to map down all the near by autos and get you the quickest ride possible.",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "Welcome to Namma Yatri! \nTo start booking rides, please allow us to find you!",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" : "Website: <u>https://nammayatri.in/</u>",
      "YOUR_FEEDBACK_HELPS_US" : "Your feedback helps us improve the Namma Yatri experience",
      "LEARN_HOW_TEXT" : "Learn how NammaYatri caters to your needs",
      "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE" : "Driver addition limits are calculated at 10% of the base fare rounded off to the nearest ₹10",
      "FARE_INFO_TEXT" : "The fare is based on the Karnataka State Government Rules factoring auto driver's additions and is not determined by Namma Yatri.",
      "PLEASE_ENABLE_LOCATION_PERMISSION" : "Please enable location permissions for Namma Yatri from the settings app to start looking for rides.",
      "TAXI_FROM_ZONE" : "from Namma Yatri Zone",
      "GO_TO_ZONE" : "Go to Namma Yatri Zone",
      "OTP_FOR_THE_JATRI_SATHI_ZONE_HAS_BEEN_EXPIRED_PLEASE_TRY_LOOKING_AGAIN" : "OTP for the Namma Yatri zone has been expired, please try booking again"
    },
    "hindiStrings": {
      "YOUR_LOCATION_HELPS_OUR_SYSTEM" : "आपका स्थान हमारे सिस्टम को आस पास के सभी ऑटो को मैप करने में सहायता कर्ता है और आपको सबसे तेज सवारी प्रदान करता है",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "नम्मा यात्री में आपका स्वागत है! \nसवारी की बुकिंग शुरू करने के लिए, कृपया हमें आपको ढूंढने की अनुमति दें!",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" : "वेबसाइट: <u>https://nammayatri.in/</u>",
      "YOUR_FEEDBACK_HELPS_US" : "आपकी प्रतिक्रिया हमें नम्मा यात्री अनुभव को बेहतर बनाने में मदद करती है",
      "LEARN_HOW_TEXT" : "जानें कि नम्मा यात्री आपकी आवश्यकताओं को कैसे पूरा करता है",
      "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE": "ड्राइवर द्वारा अतिरिक्त सीमा की गणना बेस फेयर के 10% पर की जाती है, जिसे निकटतम ₹10 में राउंड किया जाता है",
      "FARE_INFO_TEXT" : "किराया कर्नाटक राज्य सरकार के नियमों पर आधारित है जिसमें ऑटो चालक की संख्या को शामिल किया गया है और यह नम्मा यात्री द्वारा निर्धारित नहीं किया जाता है।",
      "PLEASE_ENABLE_LOCATION_PERMISSION" : "कृपया सवारी की तलाश शुरू करने के लिए सेटिंग ऐप से नम्मा यात्री के लिए स्थान अनुमतियाँ सक्षम करें।",
      "TAXI_FROM_ZONE" : "नम्मा यात्री जोन से",
      "GO_TO_ZONE" : "नम्मा यात्री जोन पर जाएं",
      "OTP_FOR_THE_JATRI_SATHI_ZONE_HAS_BEEN_EXPIRED_PLEASE_TRY_LOOKING_AGAIN" : "नम्मा यात्री क्षेत्र के लिए OTP समाप्त हो गया है, कृपया पुनः बुकिंग का प्रयास करें"
    },
    "kannadaStrings": {
      "YOUR_LOCATION_HELPS_OUR_SYSTEM" : "ನಿಮ್ಮ ಸ್ಥಳವು ನಮ್ಮ ಸಿಸ್ಟಂ ಅನ್ನು ಆಟೋಗಳ ಮೂಲಕ ನಕ್ಷೆ ಮಾಡಲು ಸಹಾಯ ಮಾಡುತ್ತದೆ ಮತ್ತು ನಿಮಗೆ ಸಾಧ್ಯವಿರುವ ವೇಗದ ಸವಾರಿಯನ್ನು ನೀಡುತ್ತದೆ.",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "ನಮ್ಮ ಯಾತ್ರಿಗೆ ಸುಸ್ವಾಗತ! \nಬುಕಿಂಗ್ ರೈಡ್‌ಗಳನ್ನು ಪ್ರಾರಂಭಿಸಲು, ದಯವಿಟ್ಟು ನಿಮ್ಮನ್ನು ಹುಡುಕಲು ನಮಗೆ ಅನುಮತಿಸಿ!",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" : "Website: <u>https://nammayatri.in/</u>",
      "YOUR_FEEDBACK_HELPS_US" : "ನಿಮ್ಮ ಪ್ರತಿಕ್ರಿಯೆ ನಮ್ಮ ಯಾತ್ರಿ ಅನುಭವವನ್ನು ಸುಧಾರಿಸಲು ನಮಗೆ ಸಹಾಯ ಮಾಡುತ್ತದೆ",
      "LEARN_HOW_TEXT" : "ನಮ್ಮ ಯಾತ್ರಿ ನಿಮ್ಮ ಅಗತ್ಯಗಳನ್ನು ಹೇಗೆ ಪೂರೈಸುತ್ತದೆ ಎಂಬುದನ್ನು ತಿಳಿಯಿರಿ",
      "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE": "ಚಾಲಕ ಸೇರ್ಪಡೆ ಮಿತಿಯನ್ನು ಮೂಲ ದರದ 10% ರಷ್ಟನ್ನು ಸುಮಾರು ₹10 ಕ್ಕೆ ಲೆಕ್ಕಹಾಕಲಾಗುತ್ತದೆ",
      "FARE_INFO_TEXT" : "ದರವು ಕರ್ನಾಟಕ ರಾಜ್ಯ ಸರ್ಕಾರದ ನಿಯಮಗಳ ಅಂಶವನ್ನು ಆಧರಿಸಿದ ಆಟೋ ಚಾಲಕರ ಸೇರ್ಪಡೆಗಳನ್ನು ಆಧರಿಸಿದೆ ಮತ್ತು ನಮ್ಮ ಯಾತ್ರಿ ನಿರ್ಧರಿಸುವುದಿಲ್ಲ",
      "PLEASE_ENABLE_LOCATION_PERMISSION" : "ಸವಾರಿಗಳಿಗಾಗಿ ಹುಡುಕುವುದನ್ನು ಪ್ರಾರಂಭಿಸಲು ದಯವಿಟ್ಟು ಸೆಟ್ಟಿಂಗ್‌ಗಳ ಅಪ್ಲಿಕೇಶನ್‌ನಿಂದ ನಮ್ಮ ಯಾತ್ರಿಗೆ ಸ್ಥಳ ಅನುಮತಿಗಳನ್ನು ಸಕ್ರಿಯಗೊಳಿಸಿ.",
      "TAXI_FROM_ZONE" : " ಹತ್ತಿ",
      "GO_TO_ZONE" : "ನಮ್ಮ ಯಾತ್ರಿ ವಲಯಕ್ಕೆ ಹೋಗಿ",
      "OTP_FOR_THE_JATRI_SATHI_ZONE_HAS_BEEN_EXPIRED_PLEASE_TRY_LOOKING_AGAIN" : "ನಮ್ಮ ಯಾತ್ರಿ ವಲಯದ OTP ಅವಧಿ ಮುಗಿದಿದೆ, ದಯವಿಟ್ಟು ಮತ್ತೆ ಬುಕ್ ಮಾಡಲು ಪ್ರಯತ್ನಿಸಿ"
    },
    "languageList": [{
      "name": "English",
      "value": "EN_US",
      "subTitle": "ಆಂಗ್ಲ"
    }, {
      "name": "ಕನ್ನಡ",
      "value": "KN_IN",
      "subTitle": "Kannada"
    }, {
      "name": "हिंदी",
      "value": "HI_IN",
      "subTitle": "Hindi"
    }]
    , "showDeleteAccount" : true
    , "enableMockLocation" : true
    , "navigationAppConfig" : {
      "query" : "google.navigation:q=%f,%f"
      , "packageName" : "com.google.android.apps.maps"
    }
    , "purpleRideConfig" : {
      "genericVideoUrl" : "https://www.youtube.com/watch?v=tzrf2Rdpkc4",
      "visualImpairmentVideo" : "https://www.youtube.com/watch?v=GllvoYpOUpc" ,
      "physicalImpairmentVideo" : "https://youtu.be/-ku9Gc8U5B8",
      "hearingImpairmentVideo" : "https://www.youtube.com/watch?v=QcKeSF9uiJ4"
    }
    , "callOptions" : ["ANONYMOUS", "DIRECT"]
    , "showDisabilityBanner" : true
    , "features" : {
      "enableSupport" : false
    }
    , "estimateAndQuoteConfig" :
      { "enableOnlyAuto" : true
      }
    , "appDatas" : {
      "link" : getAppLink(window.__OS)
      , "supportMail" :"nammayatri.support@juspay.in"
      , "name" : "Namma Yatri"
    }
    , 
  })
}