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
    "StringKeys": ["YOUR_LOCATION_HELPS_OUR_SYSTEM","LOCATION_PERMISSION_SUBTITLE_NEW_USER","CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL", "YOUR_FEEDBACK_HELPS_US", "LEARN_HOW_TEXT", "REPORT_ISSUE_CHAT_PLACEHOLDER", "CALL_SUPPORT_DESCRIPTION", "FARE_INFO_TEXT", "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE", "PLEASE_ENABLE_LOCATION_PERMISSION", "TAXI_FROM_ZONE", "OTP_FOR_THE_JATRI_SATHI_ZONE_HAS_BEEN_EXPIRED_PLEASE_TRY_LOOKING_AGAIN", "WE_HOPE_THE_ISSUE_IS_RESOLVED"],
    "isReferralEnabled": "true",
    "showBookingPreference": "true",
    "showRateCard": "true",
    "showDashboard": "true",
    "enableShareRide": "true",
    "autoVariantEnabled": true,
    "shareAppTitle": "Share Namma Yatri!",
    "shareAppContent": "Hey there!\n\nCheck India's first Zero Commission auto booking app.\n100% Open source | 100% Open Data\n\nDownload Namma Yatri now! \nhttps://nammayatri.in/link/rider/SJ8D \n\n #beOpen #chooseOpen",
    "DOCUMENT_LINK": "https://docs.google.com/document/d/1-oRR_oI8ncZRPZvFZEJZeCVQjTmXTmHA",
    "appLink": getAppLink(window.__OS),
    "PRIVACY_POLICY_LINK": "https://docs.google.com/document/d/128VU80K5E1iz-x6QnP1R127m_lwmDO3F",
    "showChargeDesc" : false,
    "isShareAppEnabled": "true",
    "addFavouriteScreenBackArrow" : "ny_ic_chevron_left_white,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_chevron_left_white.png",
    "showPickUpandDrop": true,
    "apiLoaderLottie": "primary_button_loader.json",
    "isEmergencyContacts": "true",
    "showCorporateAddress" : true,
    "loaderColor": "",
    "showHamMenu" : true,
    "showQuoteFindingText" : false,
    "isChatEnabled": "true",
    "isGradient" : "false",
    "gradient": [],
    "primaryTextColor": "#FCC32C",
    "primaryBackground": "#2C2F3A",
    "estimateConfirmText": "Request a NammaYatri Ride",
    "autoConfirmingLoaderColor": "#80B2FF",
    "quoteListModelBackground": "#2C2F3A",
    "confirmPickUpLocationBorder": "#E5E7EB",
    "currency": "₹",
    "alertDialogPrimaryColor": "#2194FF",
    "logs": ["JUSPAY","FIREBASE","CLEVERTAP"],
    "primaryButtonCornerRadius" : 8.0,
    "cancelSearchTextColor": "#E55454",
    "dashboardUrl" : "https://nammayatri.in/open?source=in-app",
    "driverInfoConfig": {
      "showTrackingButton" : false
    },
    "dashboard" : {
      "url" : "https://nammayatri.in/open?source=in-app"
    , "enable" : true
    },
    "profileBackground": "#2C2F3A",
    "profileName": "#FFFFFF",
    "profileImage": "#012A72",
    "feedbackBackground": "#2C2F3A",
    "profileCompletion" : "#FCC32C",
    "cancelRideColor" : "#E55454",
    "merchantLogo" : "ic_invoice_logo,https://assets.juspay.in/beckn/nammayatri/user/images/ic_invoice_logo.png",
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
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "Welcome to Namma Yatri! \nTo start booking rides, please allow us to find you!",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" : "Website: <u>https://nammayatri.in/</u>",
      "YOUR_FEEDBACK_HELPS_US" : "Your feedback helps us improve the Namma Yatri experience",
      "LEARN_HOW_TEXT" : "Learn how NammaYatri caters to your needs",
      "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE" : "Driver addition limits are calculated at 10% of the base fare rounded off to the nearest ₹10",
      "FARE_INFO_TEXT" : "The fare is based on the Karnataka State Government Rules factoring auto driver's additions and is not determined by Namma Yatri.",
      "PLEASE_ENABLE_LOCATION_PERMISSION" : "Please enable location permissions for Namma Yatri from the settings app to start looking for rides.",
      "TAXI_FROM_ZONE" : "from Namma Yatri Zone",
      "OTP_FOR_THE_JATRI_SATHI_ZONE_HAS_BEEN_EXPIRED_PLEASE_TRY_LOOKING_AGAIN" : "OTP for the Namma Yatri zone has been expired, please try booking again",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "Describe your issue. Namma Yatri will try to resolve it in under 24 hours.",
      "CALL_SUPPORT_DESCRIPTION" : "You are about to place a call to Namma Yatri Support. Do you want to proceed?",
      "WE_HOPE_THE_ISSUE_IS_RESOLVED" : "We hope the issue is resolved, feel free to write to us at nammayatri.support@juspay.in for any queries."
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
      "OTP_FOR_THE_JATRI_SATHI_ZONE_HAS_BEEN_EXPIRED_PLEASE_TRY_LOOKING_AGAIN" : "नम्मा यात्री क्षेत्र के लिए OTP समाप्त हो गया है, कृपया पुनः बुकिंग का प्रयास करें",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "अपने मामले का वर्णन करें। नम्मा यात्री 24 घंटे के अंदर इसका समाधान करने का प्रयास करेगा।",
      "CALL_SUPPORT_DESCRIPTION" : "आप नम्मा यात्री सहायता को कॉल करने वाले हैं। क्या आपकी आगे बढ़ने की इच्छा है?",
      "WE_HOPE_THE_ISSUE_IS_RESOLVED" : "हमें उम्मीद है कि समस्या का समाधान हो गया है, किसी भी प्रश्न के लिए बेझिझक हमें nammayatri.support@juspay.in पर लिखें।"
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
      "OTP_FOR_THE_JATRI_SATHI_ZONE_HAS_BEEN_EXPIRED_PLEASE_TRY_LOOKING_AGAIN" : "ನಮ್ಮ ಯಾತ್ರಿ ವಲಯದ OTP ಅವಧಿ ಮುಗಿದಿದೆ, ದಯವಿಟ್ಟು ಮತ್ತೆ ಬುಕ್ ಮಾಡಲು ಪ್ರಯತ್ನಿಸಿ",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "ನಿಮ್ಮ ಸಮಸ್ಯೆಯನ್ನು ವಿವರಿಸಿ. ನಮ್ಮ ಯಾತ್ರಿ ಅದನ್ನು 24 ಗಂಟೆಗಳಲ್ಲಿ ಪರಿಹರಿಸಲು ಪ್ರಯತ್ನಿಸುತ್ತದೆ.",
      "CALL_SUPPORT_DESCRIPTION" : "আপনি নম্মা যাত্রী সমর্থনে একটি কল করতে চলেছেন৷ আপনি কি এগিয়ে যেতে চান?",
      "WE_HOPE_THE_ISSUE_IS_RESOLVED" : "ಸಮಸ್ಯೆಯನ್ನು ಪರಿಹರಿಸಲಾಗಿದೆ ಎಂದು ನಾವು ಭಾವಿಸುತ್ತೇವೆ, ಯಾವುದೇ ಪ್ರಶ್ನೆಗಳಿಗೆ ನಮಗೆ nammayatri.support@juspay.in ನಲ್ಲಿ ಬರೆಯಲು ಮುಕ್ತವಾಗಿರಿ."
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
      "visibility" : false,
      "title" : "Namma Yatri",
      "imageUrl" : "ny_ic_chevron_left_double,https://assets.juspay.in/beckn/mobilitypaytm/mobilitypaytmcommon/ny_ic_chevron_left_double.png"
    }
    , "showDeleteAccount" : true
    , "fontName" : "PlusJakartaSans"
    , "fontKannada" : "NotoSansKannada"
    , "showGenderBanner" : true
    , "homeScreen" : {
      "isServiceablePopupFullScreen" : true,
    }
    , "autoSelectBackground" : "#53BB6F"
    , "enableMockLocation" : true
    , "defaultLanguage" : "EN_US"
    , "specialLocationView" : false
    , "geoCoder": {
      "enableLLtoAddress" : true
      , "enableAddressToLL" : true 
    }
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
    , "SUPPORT_EMAIL" : "nammayatri.support@juspay.in"
    , "OTP_MESSAGE_REGEX" : "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
    , "callOptions" : ["ANONYMOUS", "DIRECT"]
    , "showDisabilityBanner" : false
    , "enableContactSupport" : false
    , "enableGeocoder" : true
    , "estimateAndQuoteConfig" :
                { "enableOnlyAuto" : true
                }
    , "feature" : {
      "enableSupport" : false
      , "enableSuggestions" : true
    }
    , "appData" : {
      "link" : getAppLink(window.__OS)
      , "supportMail" :"nammayatri.support@juspay.in"
      , "name" : "Namma Yatri"
      , "website" : "https://nammayatri.in/"
    }
    , "cityConfig" : {
      "Banglore" : {
                    "cityName" : "Bangalore",
                    "selectMultipleQuote" : true,
                    "cityCode" : "std:080",
                    "d2cReferral" : true,
                    "customerTip" : true,
                    "supportNumber " : "+918069724848",
                    "customerTipArray" : "[(getString NO_TIP), "₹10 🙂", "₹20 😄", "₹30 🤩"]",
                    "customerTipArrayWithValues" : [0,10, 20, 30],
                    "searchText": ["Search for a location", "Search for a location", "Search for a location", "Search for a location"],
                    "searchTextDuration" : 30 -- in seconds,
                    "reallocation" : true,
                    },
      "Hyderabad" : {
                    "cityName" : "Hyderabad",
                    "selectMultipleQuote" : true,
                    "cityCode" : "std:040",
                    "d2cReferral" : true,
                    "customerTip" : true,
                    "supportNumber " : "+918069724848",
                    "customerTipArray" : "[(getString NO_TIP), "₹10 🙂", "₹20 😄", "₹30 🤩"]",
                    "customerTipArrayWithValues" : [0,10, 20, 30],
                    "searchText": ["Search for a location", "Search for a location", "Search for a location", "Search for a location"],
                    "searchTextDuration" : 30 -- in seconds,
                    "reallocation" : true,
                   },
      "Mysore" : {
                  "cityName" : "Mysore",
                  "selectMultipleQuote" : true,
                  "cityCode" : "std:0821",
                  "d2cReferral" : true,
                  "customerTip" : true,
                  "supportNumber " : "+918069724848",
                  "customerTipArray" : "[(getString NO_TIP), "₹10 🙂", "₹20 😄", "₹30 🤩"]",
                  "customerTipArrayWithValues" : [0,10, 20, 30],
                  "searchText": ["Search for a location", "Search for a location", "Search for a location", "Search for a location"],
                  "searchTextDuration" : 30 -- in seconds,
                  "reallocation" : true,
                  },
      "Delhi" : {
                  "cityName" : "Delhi",
                  "selectMultipleQuote" : true,
                  "cityCode" : "std:011",
                  "d2cReferral" : true,
                  "customerTip" : true,
                  "supportNumber " : "+918069724848",
                  "customerTipArray" : "[(getString NO_TIP), "₹10 🙂", "₹20 😄", "₹30 🤩"]",
                  "customerTipArrayWithValues" : [0,10, 20, 30],
                  "searchText": ["Search for a location", "Search for a location", "Search for a location", "Search for a location"],
                  "searchTextDuration" : 30 -- in seconds,
                  "reallocation" : true,
                  },
      "Chennai" : {
                  "cityName" : "Chennai",
                  "selectMultipleQuote" : true,
                  "cityCode" : "std:044",
                  "d2cReferral" : true,
                  "customerTip" : true,
                  "supportNumber " : "+918069724848",
                  "customerTipArray" : "[(getString NO_TIP), "₹10 🙂", "₹20 😄", "₹30 🤩"]",
                  "customerTipArrayWithValues" : [0,10, 20, 30],
                  "searchText": ["Search for a location", "Search for a location", "Search for a location", "Search for a location"],
                  "searchTextDuration" : 30 -- in seconds,
                  "reallocation" : true,
                  },
      "Coimbatore" : {
                  "cityName" : "Coimbatore",
                  "selectMultipleQuote" : true,
                  "cityCode" : "std:0422",
                  "d2cReferral" : true,
                  "customerTip" : true,
                  "supportNumber " : "+918069724848",
                  "customerTipArray" : "[(getString NO_TIP), "₹10 🙂", "₹20 😄", "₹30 🤩"]",
                  "customerTipArrayWithValues" : [0,10, 20, 30],
                  "searchText": ["Search for a location", "Search for a location", "Search for a location", "Search for a location"],
                  "searchTextDuration" : 30 -- in seconds,
                  "reallocation" : true,
                  },
      "Puducherry" : {
                  "cityName" : "Puducherry",
                  "selectMultipleQuote" : true,
                  "cityCode" : "std:0413",
                  "d2cReferral" : true,
                  "customerTip" : true,
                  "supportNumber " : "+918069724848",
                  "customerTipArray" : "[(getString NO_TIP), "₹10 🙂", "₹20 😄", "₹30 🤩"]",
                  "customerTipArrayWithValues" : [0,10, 20, 30],
                  "searchText": ["Search for a location", "Search for a location", "Search for a location", "Search for a location"],
                  "searchTextDuration" : 30 -- in seconds,
                  "reallocation" : true,
                          },
      "Tumakuru" : {
                "cityName" : "Tumakuru",
                "selectMultipleQuote" : true,
                "cityCode" : "std:0816",
                "d2cReferral" : true,
                "customerTip" : true,
                "supportNumber " : "+918069724848",
                "customerTipArray" : "[(getString NO_TIP), "₹10 🙂", "₹20 😄", "₹30 🤩"]",
                "customerTipArrayWithValues" : [0,10, 20, 30],
                "searchText": ["Search for a location", "Search for a location", "Search for a location", "Search for a location"],
                "searchTextDuration" : 30 -- in seconds,
                "reallocation" : true,
                  }
                }
  })
}