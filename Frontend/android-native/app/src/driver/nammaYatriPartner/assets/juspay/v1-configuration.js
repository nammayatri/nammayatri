window.version = window.version || {};
let version = "1.0.0";
if (typeof __VERSION__ !== 'undefined') {
  version = __VERSION__
}
window.version["configuration"]= version;
window.getMerchantConfig = function () {
  return JSON.stringify({
    "RC_VALIDATION_TEXT": "KA|AP|TS|DL|TN|PY",
    "DOCUMENT_LINK": "https://drive.google.com/file/d/1qYXbQUF4DVo2xNOawkHNTR_VVe46nggc",
    "APP_LINK": "https://play.google.com/store/apps/details?id=in.juspay.nammayatripartner",
    "USER_APP_LINK": "https://nammayatri.in/link/rider/mvnw",
    "PRIVACY_POLICY_LINK": "https://docs.google.com/document/d/1tF96MwtaEiq70y_P40E29Sy3X61moTc9",
    "SPECIAL_ZONE_OTP_VIEW": "false",
    "StringKeys": ["WELCOME_TEXT", "ABOUT_TEXT", "NEED_IT_TO_ENABLE_LOCATION", "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER", "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT", "YOUR_LOCATION_HELPS_OUR_SYSTEM", "WHAT_IS_NAMMA_YATRI_BONUS", "EARNED_ON_APP", "TRAVELLED_ON_APP", "GUARANTEED_FIXED_PRICE", "GET_READY_FOR_YS_SUBSCRIPTION"],
    "fontType": "Assets",
    "currency": "₹",
    "isGradient" : "false",
    "gradient": [],
    "addFavouriteScreenBackArrow" : "ny_ic_chevron_left_white,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_left_white.png",
    "popupBackground" : "#FFFFFF",
    "apiLoaderLottie": "primary_button_loader.json",
    "primaryTextColor": "#FCC32C",
    "primaryBackground": "#2C2F3A",
    "showCorporateAddress" : true,
    "imageUploadOptional" : false,
    "BONUS_EARNED" : "true",
    "clientName" : "Namma Yatri",
    "languageList" : [
      {"name":"English","value":"EN_US", "subtitle": "ಆಂಗ್ಲ"}, 
      {"name":"ಕನ್ನಡ","value":"KN_IN", "subtitle": "Kannada"},
      {"name":"हिंदी","value":"HI_IN", "subtitle": "Hindi"},
      {"name":"தமிழ்","value":"TA_IN", "subtitle": "Tamil"},
      {"name":"తెలుగు","value":"TE_IN", "subtitle": "Telugu"}
    ],
    "engilshInNative" : "ಆಂಗ್ಲ",
    "englishStrings": {
      "WELCOME_TEXT": "Welcome to the Namma Yatri Driver",
      "ABOUT_TEXT": "Namma Yatri partner is an open platform to connect drivers with riders. The app makes it convenient for drivers to find riders with proposed desired rates. No ride based commission, just pay small amount in the form of monthly subscription",
      "NEED_IT_TO_ENABLE_LOCATION": "Namma Yatri Partner collect location data to enable share your location to monitor driver current location, even when the app is closed or not in use.",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "Currently, We aren't operating with this vehicle registration number",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "You are about to place a call to the Namma Yatri Support Team. Do you want to proceed?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "Your location helps our system to map down all the near by autos and get you the quickest ride possible.",
      "WHAT_IS_NAMMA_YATRI_BONUS" : "What is Namma Yatri Bonus?",
      "EARNED_ON_APP" : "Earned on NY",
      "TRAVELLED_ON_APP" : "Travelled On Namma Yatri",
      "GUARANTEED_FIXED_PRICE" : "Guaranteed fixed price until \nApr 1, 2025",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "Get ready for\nNamma Yatri Plans!"
    },
    "hindiStrings": {
      "WELCOME_TEXT": "Welcome to the Namma Yatri Driver",
      "ABOUT_TEXT": "Namma Yatri partner चालकों को सवारियों से जोड़ने का एक खुला मंच है। ऐप ड्राइवरों के लिए प्रस्तावित वांछित दरों के साथ सवारियों को ढूंढना सुविधाजनक बनाता है। कोई सवारी आधारित कमीशन नहीं, बस मासिक सदस्यता के रूप में छोटी राशि का भुगतान करें",
      "NEED_IT_TO_ENABLE_LOCATION": "नम्मा यात्री पार्टनर ड्राइवर के लोकेशन की निगरानी के लिए अपना स्थान साझा करने के लिए लोकेशन डेटा एकत्र करता है, तब भी जब ऐप बंद हो या उपयोग में न हो।",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "वर्तमान में, हम इन वाहन पंजीकरण संख्या के साथ नहीं चल रहे हैं",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "आप नम्मा यात्री सपोर्ट टीम को कॉल करने वाले हैं। क्या आप आगे बढ़ना चाहते हैं?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "आपका स्थान हमारे सिस्टम को आस पास के सभी ऑटो को मैप करने में सहायता कर्ता है और आपको सबसे तेज सवारी प्रदान करता है",
      "WHAT_IS_NAMMA_YATRI_BONUS" : "नम्मा यात्री बोनस क्या है?",
      "EARNED_ON_APP" : "NY पर अर्जित मूल्य",
      "TRAVELLED_ON_APP" : "नम्मा यात्री पर तय की गई दूरी",
      "GUARANTEED_FIXED_PRICE" : "अप्रैल 1, 2025 तक निश्चित मूल्य गारंटी",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "नम्मा यात्री योजनाओं के लिए तैयार हो जाइए!"
    },
    "kannadaStrings": {
      "WELCOME_TEXT": "Welcome to the Namma Yatri Driver",
      "ABOUT_TEXT": "Namma Yatri partner ಪ್ರಯಾಣಿಕರೊಂದಿಗೆ ಚಾಲಕರನ್ನು ಸಂಪರ್ಕಿಸಲು ಮುಕ್ತ ವೇದಿಕೆಯಾಗಿದೆ. ಅಪ್ಲಿಕೇಶನ್ ಚಾಲಕರು ಪ್ರಯಾಣಿಕರನ್ನು ಹುಡುಕಲು ಅನುಕೂಲವಾಗುವಂತೆ ಮಾಡುತ್ತದೆ. ಮತ್ತು ಅವುಗಳನ್ನು ಸೇವಾ ಪೂರೈಕೆದಾರರೊಂದಿಗೆ ಸಂಪರ್ಕಿಸುವ ಮೂಲಕ ಈ ಆಯ್ಕೆಗಳನ್ನು ಪಡೆದುಕೊಳ್ಳಿ",
      "NEED_IT_TO_ENABLE_LOCATION": "ಅಪ್ಲಿಕೇಶನ್ ಮುಚ್ಚಿದಾಗ ಅಥವಾ ಬಳಕೆಯಲ್ಲಿಲ್ಲದಿದ್ದರೂ ಸಹ ಚಾಲಕ ಪ್ರಸ್ತುತ ಸ್ಥಳವನ್ನು ಮೇಲ್ವಿಚಾರಣೆ ಮಾಡಲು ನಿಮ್ಮ ಸ್ಥಳವನ್ನು ಹಂಚಿಕೊಳ್ಳಲು ಸಕ್ರಿಯಗೊಳಿಸಲು ನಮ್ಮ ಯಾತ್ರಿ ಪಾಲುದಾರರು ಸ್ಥಳ ಡೇಟಾವನ್ನು ಸಂಗ್ರಹಿಸುತ್ತಾರೆ.",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "ಪ್ರಸ್ತುತ, ನಾವು ಈ ವಾಹನ ನೋಂದಣಿ ಸಂಖ್ಯೆಯೊಂದಿಗೆ ಕಾರ್ಯನಿರ್ವಹಿಸುತ್ತಿಲ್ಲ",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "ನೀವು ನಮ್ಮ ಯಾತ್ರಿ ಬೆಂಬಲ ತಂಡಕ್ಕೆ ಕರೆ ಮಾಡಲಿರುವಿರಿ. ನೀವು ಮುಂದುವರೆಯಲು ಬಯಸುವಿರಾ?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "ನಿಮ್ಮ ಸ್ಥಳವು ನಮ್ಮ ಸಿಸ್ಟಂ ಅನ್ನು ಆಟೋಗಳ ಮೂಲಕ ನಕ್ಷೆ ಮಾಡಲು ಸಹಾಯ ಮಾಡುತ್ತದೆ ಮತ್ತು ನಿಮಗೆ ಸಾಧ್ಯವಿರುವ ವೇಗದ ಸವಾರಿಯನ್ನು ನೀಡುತ್ತದೆ.",
      "WHAT_IS_NAMMA_YATRI_BONUS" : "ನಮ್ಮ ಯಾತ್ರಿ ಬೋನಸ್ ಎಂದರೇನು?",
      "EARNED_ON_APP" : "NY ನಲ್ಲಿ ಗಳಿಸಿದ ಬೆಲೆ",
      "TRAVELLED_ON_APP" : "ನಮ್ಮ ಯಾತ್ರಿಯಲ್ಲಿ ಪ್ರಯಾಣಿಸಿದೆ",
      "GUARANTEED_FIXED_PRICE" : "ಅಪ್ರೈಲ್ 1, 2025 ವರೆಗೆ ಖಚಿತ ಬೆಲೆಯ ಗ್ಯಾರಂಟಿ",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "ನಮ್ಮ ಯಾತ್ರಿ ಯೋಜನೆಗಳಿಗೆ ಸಿದ್ಧತೆ ಪಡೆಯಲು ಸಿದ್ಧವಾಗಿರಿ!"
    },
    "tamilStrings": {
      "WELCOME_TEXT": "Welcome to the Namma Yatri Driver",
      "ABOUT_TEXT": "நம்மா யாத்திரி கூட்டாளர் இயக்கிகளை ரைடர்ஸுடன் இணைக்க ஒரு திறந்த தளமாகும். முன்மொழியப்பட்ட விரும்பிய விகிதங்களைக் கொண்ட ரைடர்ஸைக் கண்டுபிடிப்பதை ஓட்டுநர்களுக்கு பயன்பாடு வசதியாக ஆக்குகிறது. சவாரி அடிப்படையிலான கமிஷன் இல்லை, மாதாந்திர சந்தா வடிவத்தில் சிறிய தொகையை செலுத்துங்கள்",
      "NEED_IT_TO_ENABLE_LOCATION": "பயன்பாடு மூடப்பட்டிருந்தாலும் கூட, இயக்கி தற்போதைய இருப்பிடத்தை கண்காணிக்க உங்கள் இருப்பிடத்தைப் பகிர்வதற்கு நம்மா யாத்ரி கூட்டாளர் இருப்பிடத் தரவை சேகரிக்கவும்",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "தற்போது நாங்கள் இந்த வாகன பதிவு எண்களுடன் செயல்படவில்லை",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "நீங்கள் நம்ம யாத்ரி ஆதரவுக் குழுவிற்கு அழைப்பு விடுக்க உள்ளீர்கள். தொடர வேண்டுமா?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "உங்கள் இருப்பிடம் எங்கள் கணினியை ஆட்டோக்கள் மூலம் அருகிலுள்ள அனைத்து வரைபடத்தையும், விரைவாக சவாரி செய்ய உதவுகிறது.",
      "WHAT_IS_NAMMA_YATRI_BONUS" : "நம்ம யாத்ரி போனஸ் என்றால் என்ன?",
      "EARNED_ON_APP" : "NY இல் பெறப்பட்ட விலை",
      "TRAVELLED_ON_APP" : "நம்ம யாத்ரியில் நடந்த தூரம்",
      "GUARANTEED_FIXED_PRICE" : "2025 ஆம் ஆண்டு ஏப்ரல் 1 வரை உறுதியான விலை",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "நம்ம யாத்ரி திட்டங்களுக்கு தயாராக இருங்கள்!"
    },
    "teluguStrings": {
      "WELCOME_TEXT": "నమ్మ యాత్రి డ్రైవర్‌కు స్వాగతం",
      "ABOUT_TEXT": "నమ్మ యాత్రి పార్ట్‌నెర్ ఒక డ్రైవర్లను రైడర్లతో కనెక్ట్ చేయడానికి ప్రారంభించిన ఒక మౌఖిక దర్పణం. యాప్‌ని డ్రైవర్లకు సూచించిన కొనుగోలుతో రైడర్లను కనుగొలుచేస్తుంది. రైడు ఆధారిత కమిషన్ లేదా వారికి చెల్లించిన డేయిలీ సబ్‌స్క్రిప్షన్ రూపంలో చిన్నమైన మొత్తం చెల్లించండి",
      "NEED_IT_TO_ENABLE_LOCATION": "నమ్మ యాత్రి పార్ట్‌నెర్ లొకేషన్ డేటాను కలెక్ట్ చేస్తుంది, డ్రైవర్ ప్రస్తుత స్థానాన్ని మానిటర్ చేసేందుకు, ఆప్ మూసివేయబడినా లేదా అనావరణలో ఉండకుండా.",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "ప్రస్తుతం, ఈ వాహన నమ్బర్‌తో మాకు పనిచేయడం లేదు",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "మీరు నమ్మ యాత్రి సపోర్ట్ టీమ్‌కు ఫోన్ చేయాలనుకుంటున్నారు. మీరు కొనసాగాలా?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "మీ స్థానం మా సిస్టమ్‌ను అన్వయించడంతో అన్ని చికిత్సాలను నక్షారూపం చేసుకోవడం సాధారణం.",
      "WHAT_IS_NAMMA_YATRI_BONUS" : "నమ్మ యాత్రి బోనస్ అంటే ఏమిటి?",
      "EARNED_ON_APP" : "యొక్క ఆర్జి",
      "TRAVELLED_ON_APP" : "నమ్మ యాత్రిలో పయణించినది",
      "GUARANTEED_FIXED_PRICE" : "ఏప్రిల్ 1, 2025 వరకు స్థిర ధరకు హామీ ఇవ్వబడుతుంది",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "నమ్మ యాత్రి ప్రణాళికల కోసం సిద్ధంగా ఉండండి!"
    },
    "logs": ["JUSPAY","FIREBASE","CLEVERTAP"]
    , "fontName" : "PlusJakartaSans"
    , "fontKannada" : "NotoSansKannada"
    , "allowAllMobileNumber" : false
    , "showGenderBanner" : true
    , "defaultLanguage" : "EN_US"
    , "OTP_MESSAGE_REGEX" : "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
    , "leaderBoard": {
      "isMaskedName": false
    }
    , "subscriptionConfig" : {
      "enableBlocking" : true,
      "onBoardingSubscription" : true,
      "showDUOfferBanner" : true,
      "offerBannerValidTill" : "2024-01-01T00:00:00",
      "offerBannerDeadline" : "December 31-*$*-ಡಿಸೆಂಬರ್ 31-*$*-31 दिसंबर-*$*-டிசம்பர் 31",
      "offerBannerConfig" : {
        "showDUOfferBanner" : true,
        "offerBannerValidTill" : "2024-01-01T00:00:00",
        "offerBannerDeadline" : "December 31-*$*-ಡಿಸೆಂಬರ್ 31-*$*-31 दिसंबर-*$*-டிசம்பர் 31",
        "offerBannerPlans" : ["a35ffc7c-de0d-4dcc-83a8-e36a5a29cc1d"],
      },
      "lowDuesLimit" : 25.0,
      "maxDuesLimit" : 100.0,
      "highDueWarningLimit" : 75.0,
      "moveDriverToOfflineInHighDueDaily" : true,
      "enableSubscriptionPopups" : true,
      "faqLink" : "https://nammayatri.in/plans/",
      "supportNumber" : "08069724800",
      "whatsappSupportLink" : "https://wa.me/917483117936?text=Hello%2C%20I%20need%20help%20with%20setting%20up%20Autopay%20Subscription%0A%E0%B2%B8%E0%B3%8D%E0%B2%B5%E0%B2%AF%E0%B2%82%20%E0%B2%AA%E0%B2%BE%E0%B2%B5%E0%B2%A4%E0%B2%BF%20%E0%B2%9A%E0%B2%82%E0%B2%A6%E0%B2%BE%E0%B2%A6%E0%B2%BE%E0%B2%B0%E0%B2%BF%E0%B2%95%E0%B3%86%E0%B2%AF%E0%B2%A8%E0%B3%8D%E0%B2%A8%E0%B3%81%20%E0%B2%B9%E0%B3%8A%E0%B2%82%E0%B2%A6%E0%B2%BF%E0%B2%B8%E0%B2%B2%E0%B3%81%20%E0%B2%A8%E0%B2%A8%E0%B2%97%E0%B3%86%20%E0%B2%B8%E0%B2%B9%E0%B2%BE%E0%B2%AF%E0%B2%A6%20%E0%B2%85%E0%B2%97%E0%B2%A4%E0%B3%8D%E0%B2%AF%E0%B2%B5%E0%B2%BF%E0%B2%A6%E0%B3%86",
      "myPlanYoutubeLink" : "https://www.youtube.com/playlist?list=PL4AEiRR3V7kHcg2-fgzvDXDqWihZD9mTK",
      "overlayYoutubeLink" : "https://www.youtube.com/shorts/x9cJN78j9V8",
      "optionsMenuItems" : {
        "managePlan" : true,
        "paymentHistory" : true,
        "viewFaqs" : true,
      },
    }
    , "rideActionModelConfig" : {
      "showVehicleVariant" : false
    }
    , "referralType" : "LeaderBoard"
    , "gotoConfig" : {
      "maxGotoLocations" : 5,
      "enableGoto" : true
    }
    , "bottomNavConfig" : {
      "subscription" : 
            { isVisible : true
            }
    }
    , "purpleRideConfig" : {
      "showPurpleVideos" : false,
      "visualImpairmentVideo" : "https://www.youtube.com/watch?v=2qYXl03N6Jg",
      "physicalImpairmentVideo" : "https://www.youtube.com/watch?v=B0C6SZTQO6k",
      "hearingImpairmentVideo" : "https://www.youtube.com/watch?v=udkWOt0serg",
      "genericAccessibilityVideo" : "https://youtu.be/5s21p2rI58c"
    }
    , "enableMockLocation" : false
    , "cityConfig" : [
      {
        "cityName" : "Bangalore",
        "mapImage" : "ny_ic_bengalore_map",
        "cityCode" : "std:080",
        "showSubscriptions" : true,
        "cityLat" : 12.971599,
        "cityLong" : 77.594566,
        "supportNumber" : "",
        "languageKey" : "KN_IN"
      },
      {
        "cityName" : "Hyderabad",
        "mapImage" : "ny_ic_hyderabad_map",
        "cityCode" : "std:040",
        "showSubscriptions" : false,
        "cityLat" : 17.387140,
        "cityLong" : 78.491684,
        "supportNumber" : "+918069724900",
        "languageKey" : "TE_IN"
      },
      {
        "cityName" : "Mysore",
        "mapImage" : "ny_ic_mysuru_map",
        "cityCode" : "std:0821",
        "showSubscriptions" : false,
        "cityLat" : 12.295810,
        "cityLong" : 76.639381,
        "supportNumber" : "",
        "languageKey" : "TA_IN"
      },
      {
        "cityName" : "Delhi",
        "mapImage" : "ny_ic_delhi_map",
        "cityCode" : "std:011",
        "showSubscriptions" : false,
        "cityLat" : 28.644800,
        "cityLong" : 77.216721,
        "supportNumber" : "+918069724848",
        "languageKey" : "HI_IN"
      },
      {
        "cityName" : "Chennai",
        "mapImage" : "ny_ic_chennai_map",
        "cityCode" : "std:044",
        "showSubscriptions" : false,
        "cityLat" : 13.067439,
        "cityLong" : 80.237617,
        "supportNumber" : "08069724899",
        "languageKey" : "TA_IN"
      },
      {
        "cityName" : "Coimbatore",
        "mapImage" : "ny_ic_coimbatore_map",
        "cityCode" : "std:0422",
        "showSubscriptions" : false,
        "cityLat" : 11.004556,
        "cityLong" : 76.961632,
        "supportNumber" : "",
        "languageKey" : "TA_IN"
      },
      {
        "cityName" : "Puducherry",
        "mapImage" : "ny_ic_puducherry_map",
        "cityCode" : "std:0413",
        "showSubscriptions" : false,
        "cityLat" : 11.943852,
        "cityLong" : 79.808292,
        "supportNumber" : "08069724899",
        "languageKey" : "TA_IN"
      }
  ]
  })
}