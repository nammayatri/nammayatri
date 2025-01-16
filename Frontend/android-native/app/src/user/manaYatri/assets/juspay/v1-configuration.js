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
    "sideBarList": ["MyRides", "Favorites", "NammaSafety", "MetroTickets", "HelpAndSupport", "Language", "Separator", "ShareApp", "LiveStatsDashboard", "About", "Logout"],
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
      "WE_HOPE_THE_ISSUE_IS_RESOLVED" : "We hope the issue is resolved, feel free to write to us at support@nammayatri.in for any queries."
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
      "WE_HOPE_THE_ISSUE_IS_RESOLVED" : "हमें उम्मीद है कि समस्या का समाधान हो गया है, किसी भी प्रश्न के लिए बेझिझक हमें support@nammayatri.in पर लिखें।"
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
      "CALL_SUPPORT_DESCRIPTION" : "ನೀವು ನಮ್ಮ ಯಾತ್ರಿ ಬೆಂಬಲಕ್ಕೆ ಕರೆ ಮಾಡಲಿರುವಿರಿ. ನೀವು ಮುಂದುವರೆಯಲು ಬಯಸುವಿರಾ?",
      "WE_HOPE_THE_ISSUE_IS_RESOLVED" : "ಸಮಸ್ಯೆಯನ್ನು ಪರಿಹರಿಸಲಾಗಿದೆ ಎಂದು ನಾವು ಭಾವಿಸುತ್ತೇವೆ, ಯಾವುದೇ ಪ್ರಶ್ನೆಗಳಿಗೆ ನಮಗೆ support@nammayatri.in ನಲ್ಲಿ ಬರೆಯಲು ಮುಕ್ತವಾಗಿರಿ."
    },
    "tamilStrings": {
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "உங்கள் இருப்பிடம் எங்கள் கணினிக்கு அருகில் உள்ள அனைத்தையும் ஆட்டோக்கள் மூலம் வரைபடமாக்கி, உங்களுக்கு விரைவான பயணத்தை சாத்தியமாக்க உதவுகிறது.",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "நம்ம யாத்ரிக்கு வரவேற்கிறோம்! \nசவாரிகளை முன்பதிவு செய்யத் தொடங்க, உங்களைக் கண்டறிய எங்களை அனுமதிக்கவும்!",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" : "இணையதளம்: <u>https://nammayatri.in/</u>",
      "YOUR_FEEDBACK_HELPS_US" : "உங்கள் கருத்து நம் யாத்ரி அனுபவத்தை மேம்படுத்த உதவுகிறது",
      "LEARN_HOW_TEXT" : "நம்மயாத்ரி உங்கள் தேவைகளை எப்படிப் பூர்த்தி செய்கிறது",
      "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE" : "டிரைவர் சேர்க்கை வரம்புகள் அடிப்படைக் கட்டணத்தில் 10% கணக்கிடப்பட்டு, அருகிலுள்ள ₹10 வரை கணக்கிடப்படும்",
      "FARE_INFO_TEXT" : "கர்நாடக மாநில அரசு விதிகளின் அடிப்படையிலான ஆட்டோ ஓட்டுநர் சேர்க்கையின் அடிப்படையில் கட்டணம் நிர்ணயிக்கப்பட்டுள்ளது, மேலும் இது நம்ம யாத்ரியால் நிர்ணயிக்கப்படவில்லை.",
      "PLEASE_ENABLE_LOCATION_PERMISSION" : "சவாரிகளைத் தேடத் தொடங்க, அமைப்புகள் பயன்பாட்டிலிருந்து நம்ம யாத்ரிக்கான இருப்பிட அனுமதிகளை இயக்கவும்.",
      "TAXI_FROM_ZONE" : "நம்ம யாத்ரி மண்டலத்திலிருந்து",
      "OTP_FOR_THE_JATRI_SATHI_ZONE_HAS_BEEN_EXPIRED_PLEASE_TRY_LOOKING_AGAIN" : "நம்ம யாத்ரி மண்டலத்திற்கான OTP காலாவதியானது, மீண்டும் முன்பதிவு செய்ய முயற்சிக்கவும்",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "உங்கள் சிக்கலை விவரிக்கவும். நம்ம யாத்ரி 24 மணி நேரத்திற்குள் அதைத் தீர்க்க முயற்சிக்கும்.",
      "CALL_SUPPORT_DESCRIPTION" : "நம்ம யாத்ரி ஆதரவிற்கு அழைப்பை மேற்கொள்ள உள்ளீர்கள். தொடர விரும்புகிறீர்களா?",
      "WE_HOPE_THE_ISSUE_IS_RESOLVED" : "சிக்கல் தீர்ந்துவிட்டது என்று நம்புகிறோம், ஏதேனும் கேள்விகளுக்கு தயங்காமல் எங்களுக்கு nammayatri.support@juspay.in இல் எழுதவும்."
    },
    "teluguStrings": {
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "మీ లొకేషన్ మా సిస్టమ్‌కు సమీపంలోని అన్ని ఆటోల ద్వారా మ్యాప్ చేయడానికి మరియు మీకు వీలైనంత త్వరగా ప్రయాణించేలా చేయడంలో సహాయపడుతుంది.",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER": "నమ్మ యాత్రికి స్వాగతం! \nరైడులు బుక్ చేయడానికి, దయచేసి మిమ్మల్ని కనుగొనడానికి మమ్మల్ని అనుమతించండి!",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL": "వెబ్‌సైట్: <u>https://nammayatri.in/</u>",
      "YOUR_FEEDBACK_HELPS_US": "మీ అభిప్రాయం యాప్ అనుభవాన్ని మెరుగుపరచడంలో మాకు సహాయపడుతుంది",
      "LEARN_HOW_TEXT": "నమ్మ యాత్రి మీ అవసరాలను ఎలా తీరుస్తుందో తెలుసుకోండి",
      "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE": "డ్రైవర్ జోడింపు పరిమితులు బేస్ ఫేర్‌లో 10%కి సమీప ₹10కి లెక్కించబడతాయి",
      "FARE_INFO_TEXT": "ఈ ఛార్జీ కర్ణాటక రాష్ట్ర ప్రభుత్వ నిబంధనల ప్రకారం ఆటో డ్రైవర్ చేర్పులపై ఆధారపడి ఉంటుంది మరియు ఇది నమ్మ యాత్రి ద్వారా నిర్ణయించబడదు.",
      "PLEASE_ENABLE_LOCATION_PERMISSION": "రైడ్‌ల కోసం వెతకడం ప్రారంభించడానికి దయచేసి యాప్ సెట్టింగ్‌ల నుండి స్థాన అనుమతులను ప్రారంభించండి.",
      "TAXI_FROM_ZONE": "నమ్మాయాత్రి జోన్ నుండి",
      "OTP_FOR_THE_JATRI_SATHI_ZONE_HAS_BEEN_EXPIRED_PLEASE_TRY_LOOKING_AGAIN": "నమ్మ యాత్రి జోన్ కోసం OTP గడువు ముగిసింది, దయచేసి మళ్లీ బుక్ చేసుకోవడానికి ప్రయత్నించండి",
      "REPORT_ISSUE_CHAT_PLACEHOLDER": "మీ సమస్య వివరించండి. నమ్మాయాత్రి 24 గంటల లో పరిష్కరించటం లేదా సహాయం చేయడం చేస్తుంది.",
      "CALL_SUPPORT_DESCRIPTION": "మీరు మద్దతు బృందానికి కాల్ చేయబోతున్నారు. మీరు కొనసాగించాలనుకుంటున్నారా?",
      "WE_HOPE_THE_ISSUE_IS_RESOLVED": "సమస్య పరిష్కరించబడిందని మేము ఆశిస్తున్నాము!, ఏదైనా ప్రశ్నల కోసం support@nammayatri.in కి వ్రాయండి."
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
    }, {
      "name": "தமிழ்",
      "value": "TA_IN",
      "subTitle": "Tamil"
    }, {
      "name": "తెలుగు",
      "value": "TE_IN",
      "subTitle": "Telugu"
    }
  ],
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
      , "enableAddressToLL" : false 
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
    , "SUPPORT_EMAIL" : "support@nammayatri.in"
    , "OTP_MESSAGE_REGEX" : "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
    , "callOptions" : ["ANONYMOUS", "DIRECT"]
    , "termsVersion" : 2.0
    , "tipEnabledCities" : ["Bangalore", "Hyderabad"]
    , "showDisabilityBanner" : false
    , "enableContactSupport" : false
    , "enableGeocoder" : true
    , "isAdvancedBookingEnabled" : true
    , "estimateAndQuoteConfig" : 
      { "variantTypes" : [ ["SUV"], ["HATCHBACK", "TAXI_PLUS", "SEDAN"], ["TAXI"], ["AUTO_RICKSHAW"], ["DELIVERY_BIKE"],["AMBULANCE_VENTILATOR"],["AMBULANCE_AC_OXY"],[ "AMBULANCE_AC"],["AMBULANCE_TAXI_OXY"] ,["AMBULANCE_TAXI"] ]
      , "variantOrder" : ["AUTO_RICKSHAW", "BOOK_ANY", "DELIVERY_BIKE" ,"AMBULANCE_VENTILATOR","AMBULANCE_AC_OXY", "AMBULANCE_AC","AMBULANCE_TAXI_OXY" ,"AMBULANCE_TAXI"]
      , "variantInfo" : {
        "hatchback" : {
          "name" : "Hatchback",
          "image" : "ny_ic_hatchback,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_hatchback-2.png",
          "leftViewImage" : "ny_ic_hatchback_left_view,"
          },
        "taxiPlus" : {
          "name" : "AC Taxi",
          "image" : "ny_ic_sedan_ac,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_sedan_ac.png",
          "leftViewImage" : "ny_ic_sedan_left_view,"
        },
        "sedan" : {
          "name" : "Sedan",
          "image" : "ny_ic_sedan,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_sedan.png",
          "leftViewImage" : "ny_ic_sedan_left_view,"
        },
        "taxi" : {
          "name" : "Non-AC Taxi",
          "image" : "ny_ic_sedan,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_sedan.png",
          "leftViewImage" : "ny_ic_sedan_left_view,"
        },
        "suv" : {
          "name" : "SUV",
          "image" : "ny_ic_suv,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_suv.png",
          "leftViewImage" : "ny_ic_suv_left_view,"
        },
        "autoRickshaw" : {
          "name" : "Auto Rickshaw",
          "image" : "ny_ic_single_estimate_auto,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_single_estimate_auto.png",
          "leftViewImage" : "ny_ic_auto_left_view,"
        },
        "evAutoRickshaw" : {
          "name" : "EV Auto Rickshaw",
          "image" : "ny_ic_single_estimate_auto,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_single_estimate_auto.png",
          "leftViewImage" : "ny_ic_auto_left_view,"
        },
        "bookAny" : {
          "name" : "Book Any",
          "image" : "ny_ic_cab_auto_yellow,https://assets.moving.tech/beckn/common/user/images/ny_ic_cab_auto_yellow.png",
          "leftViewImage" : ","
        },
        "bike" : {
          "name" : "Bike",
          "image": "ny_ic_bike_side,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_bike_side.png",
          "leftViewImage" : "ny_ic_bike_left_side,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_bike_left_side.png"
        },
        "suvPlus" : {
          "name" : "XL Plus",
          "image" : "ny_ic_suv_plus_side,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_suv_plus_side.png",
          "leftViewImage" : "ny_ic_suv_plus_left_side,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_suv_plus_left_side.png"
        },
        "deliveryBike" : {
            "name" : "2 Wheeler",
            "image" : "ny_ic_bike_side,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_bike_side.png",
            "leftViewImage" : "ny_ic_bike_left_side,"
          },
          "ambulanceTaxi" : {
            "name" : "Ambulance Taxi",
            "image" : "ny_ic_ambulance_noac_nooxy,https://assets.moving.tech/beckn/jatrisaathi/user/images/ny_ic_ambulance_noac_nooxy.png",
            "leftViewImage" : "ny_ic_ambulance_noac_nooxy,"
          },
          "ambulanceTaxiOxy" : {
            "name" : "Ambulance Taxi with Oxygen",
            "image" : "ny_ic_ambulance_noac_oxy,https://assets.moving.tech/beckn/jatrisaathi/user/images/ny_ic_ambulance_noac_oxy.png",
            "leftViewImage" : "ny_ic_ambulance_noac_oxy,"
          },
          "ambulanceAc" : {
            "name" : "Ambulance AC",
            "image" : "ny_ic_ambulance_ac_nooxy,https://assets.moving.tech/beckn/jatrisaathi/user/images/ny_ic_ambulance_ac_nooxy.png",
            "leftViewImage" : "ny_ic_ambulance_ac_nooxy,"
          },
          "ambulanceAcOxy" : {
            "name" : "Ambulance AC with Oxygen",
            "image" : "ny_ic_ambulance_ac_oxy,https://assets.moving.tech/beckn/jatrisaathi/user/images/ny_ic_ambulance_ac_oxy.png",
            "leftViewImage" : "ny_ic_ambulance_ac_oxy,"
          },
          "ambulanceVentilator" : {
            "name" : "Ambulance with Ventilator",
            "image" : "ny_ic_ambulance_ventilator,https://assets.moving.tech/beckn/jatrisaathi/user/images/ny_ic_ambulance_ventilator.png",
            "leftViewImage" : "ny_ic_ambulance_ventilator,"
          },
        "heritageCab" : {
          "name" : "Heritage Cab",
          "image" : "ny_ic_heritage_cab_side,https://assets.moving.tech/beckn/nammayatri/nammayatricommon/images/ny_ic_heritage_cab_side.png",
          "leftViewImage" : "ny_ic_heritage_cab_left_side,https://assets.moving.tech/beckn/nammayatri/nammayatricommon/images/ny_ic_heritage_cab_left_side.png"
        }
      }
      , "enableOnlyAuto" : true
      , "showNearByDrivers": false
      , "enableBookingPreference": false
      , "textColor": "#6D7280"
      , "showInfoIcon" : true 
      }
    , "feature" : {
        "enableSupport" : false
      , "enableSuggestions" : true
      , "enableEditDestination" : true
      , "enableEditPickupLocation" : true
    }
    , "appData" : {
      "link" : getAppLink(window.__OS)
      , "supportMail" :"support@nammayatri.in"
      , "name" : "Namma Yatri"
      , "website" : "https://nammayatri.in/"
    }
    , 
  })
}