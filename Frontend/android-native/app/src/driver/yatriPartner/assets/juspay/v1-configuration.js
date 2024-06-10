window.version = window.version || {};
let version = "1.0.0";
if (typeof __VERSION__ !== "undefined") {
  version = __VERSION__
}
window.version["configuration"]= version;
window.getMerchantConfig = function () {
  return JSON.stringify({
    "RC_VALIDATION_TEXT": "KL|KA|AP|TS|DL|TN|PY|UP|HR|TG",
    "DOCUMENT_LINK": "https://docs.google.com/document/d/17fnfcDCd2KNKSJjFQmEwm7TqsBIOt7kA/edit?usp=sharing&ouid=115428839751313950285&rtpof=true&sd=true",
    "APP_LINK": "https://play.google.com/store/apps/details?id=net.openkochi.yatripartner",
    "USER_APP_LINK": "https://yatricustomer.page.link/pcJb",
    "PRIVACY_POLICY_LINK": "https://docs.google.com/document/d/1j7REROF75Rpgx65if5guFpqtEKXqCY9O/edit?usp=sharing&ouid=115428839751313950285&rtpof=true&sd=true",
    "SPECIAL_ZONE_OTP_VIEW": "false",
    "StringKeys": ["NEED_IT_TO_ENABLE_LOCATION", "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER", "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT", "YOUR_LOCATION_HELPS_OUR_SYSTEM", "EARNED_ON_APP", "TRAVELLED_ON_APP", "REPORT_ISSUE_CHAT_PLACEHOLDER", "CORPORATE_ADDRESS", "CORPORATE_ADDRESS_DESCRIPTION", "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL", "REGISTERED_ADDRESS", "REGISTERED_ADDRESS_DESCRIPTION", "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL", "REFERRED_DRIVERS_INFO", "REFERRED_CUSTOMERS_INFO", "DOWNLOAD_NAMMA_YATRI", "SHARE_NAMMA_YATRI"],
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
    "clientName" : "Yatri",
    "languageList": [{
      "name": "English",
      "value": "EN_US",
      "subtitle": "ഇംഗ്ലീഷ്"
    },
    {
      "name": "മലയാളം",
      "value": "ML_IN",
      "subtitle": "Malayalam"
    },
    {"name":"हिंदी","value":"HI_IN", "subtitle": "Hindi"},
    {"name":"ಕನ್ನಡ","value":"KN_IN", "subtitle": "Kannada"},
    {"name":"தமிழ்","value":"TA_IN", "subtitle": "Tamil"},
    {"name":"తెలుగు","value":"TE_IN", "subtitle": "Telugu"}
    ],
    "engilshInNative" : "ಆಂಗ್ಲ",
    "englishStrings": {
      "MERCHANT_NAME" : "Yatri",
      "NEED_IT_TO_ENABLE_LOCATION": "Yatri Driver collect location data to enable share your location to monitor driver current location, even when the app is closed or not in use.",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "Currently, We aren't operating with this vehicle registration number",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "You are about to place a call to the Yatri Support Team. Do you want to proceed?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "Your location helps our system to map down all the near by taxis and get you the quickest ride possible.",
      "EARNED_ON_APP" : "Earned on Yatri",
      "TRAVELLED_ON_APP" : "Travelled On Yatri",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "Describe your issue. Yatri will try to resolve it in under 24 hours.",
      "CORPORATE_ADDRESS" : "Corporate Address",
      "CORPORATE_ADDRESS_DESCRIPTION" : "Juspay Technologies Private Limited <br> Girija Building, Number 817, Ganapathi Temple Rd, 8th Block, Koramangala, Bengaluru, Karnataka 560095, India.",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" :  "Website: <u>https://www.getyatri.com</u>",
      "REGISTERED_ADDRESS" : "Registered Address",
      "REGISTERED_ADDRESS_DESCRIPTION" : "Juspay Technologies Private Limited <br> Stallion Business Centre, No. 444, 3rd & 4th Floor, 18th Main, 6th Block, Koramangala Bengaluru, Karnataka- 560095, India.",
      "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL" : "Website: <u>https://www.getyatri.com</u>",
      "DOWNLOAD_NAMMA_YATRI": "Download Yatri",
      "REFERRED_DRIVERS_INFO" : "Referred Drivers who have registered on Yatri",
      "REFERRED_CUSTOMERS_INFO" : "Referred Customers who have registered on Yatri",
      "SHARE_NAMMA_YATRI" : "Share Yatri",
      "WHAT_IS_NAMMA_YATRI_BONUS" : "What is Yatri Bonus?",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "Get ready for\n Yatri Plans!"
    },
    "malayalamStrings": {
      "MERCHANT_NAME" : "യാത്രി",
      "NEED_IT_TO_ENABLE_LOCATION": "ആപ്പ് അടച്ചിരിക്കുമ്പോഴും ഉപയോഗത്തിലില്ലെങ്കിലും ഡ്രൈവർ നിലവിലെ ലൊക്കേഷൻ നിരീക്ഷിക്കാൻ നിങ്ങളുടെ ലൊക്കേഷൻ പങ്കിടുന്നത് പ്രവർത്തനക്ഷമമാക്കാൻ യാത്രി പങ്കാളി ലൊക്കേഷൻ ഡാറ്റ ശേഖരിക്കുന്നു.",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "നിലവിൽ, ഞങ്ങൾ ഈ വാഹന രജിസ്ട്രേഷൻ നമ്പർ ഉപയോഗിച്ച് പ്രവർത്തിക്കുന്നില്ല",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "നിങ്ങൾ യാത്രി സപ്പോർട്ട് ടീമിലേക്ക് ഒരു കോൾ ചെയ്യാൻ പോകുകയാണ്. നിങ്ങൾക്ക് തുടരണോ?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "നിങ്ങളുടെ ലൊക്കേഷൻ ഞങ്ങളുടെ സിസ്റ്റത്തെ ടാക്സികൾ വഴി മാപ്പ് ചെയ്യാൻ സഹായിക്കുന്നു.",
      "EARNED_ON_APP" : "Y-ൽ നേടിയ വില",
      "TRAVELLED_ON_APP" : "യാത്രയിൽ യാത്ര ചെയ്ത ദൂരം",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "നിങ്ങളുടെ പ്രശ്നം വിവരിക്കുക. 24 മണിക്കൂറിനുള്ളിൽ അത് പരിഹരിക്കാൻ യാത്രി ശ്രമിക്കും.",
      "CORPORATE_ADDRESS" : "കോർപ്പറേറ്റ് വിലാസം",
      "CORPORATE_ADDRESS_DESCRIPTION" : "ജസ്‌പേ ടെക്‌നോളജീസ് പ്രൈവറ്റ് ലിമിറ്റഡ് <br> ഗിരിജ ബിൽഡിംഗ്, നമ്പർ 817, ഗണപതി ടെംപിൾ റോഡ്, എട്ടാം ബ്ലോക്ക്, കോറമംഗല, ബെംഗളൂരു, കർണാടക 560095, ഇന്ത്യ",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" :  "വെബ്സൈറ്റ്: <u>https://www.getyatri.com</u>",
      "REGISTERED_ADDRESS" : "രേഖപ്പെടുത്തിയ വിലാസം",
      "REGISTERED_ADDRESS_DESCRIPTION" : "ജസ്‌പേ ടെക്‌നോളജീസ് പ്രൈവറ്റ് ലിമിറ്റഡ് <br> സ്റ്റാലിയൻ ബിസിനസ് സെന്റർ, നമ്പർ 444, 3rd & 4th നിലകൾ, 18th മെയിൻ, 6th ബ്ലോക്ക്, കോറമംഗല ബെംഗളൂരു, കർണാടക- 560095, ഇന്ത്യ.",
      "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL" : "വെബ്സൈറ്റ്: <u>https://www.getyatri.com</u>",
      "DOWNLOAD_NAMMA_YATRI": "യാത്രാ ഡൗൺലോഡുചെയ്യുക",
      "REFERRED_DRIVERS_INFO": "യാത്രിയിൽ രജിസ്റ്റർ ചെയ്ത പരാമർശിച്ച ഡ്രൈവേഴ്സ്",
      "REFERRED_CUSTOMERS_INFO": "യാത്രിയിൽ രജിസ്റ്റർ ചെയ്ത പരാമർശിച്ച കസ്റ്റമേഴ്സ്",
      "SHARE_NAMMA_YATRI" : "യാത്രാ പങ്കിടുക",
      "WHAT_IS_NAMMA_YATRI_BONUS" : "What is Yatri Bonus?",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "Get ready for\n Yatri Plans!"
    },
    "hindiStrings": {
      "NEED_IT_TO_ENABLE_LOCATION": "यात्री पार्टनर ड्राइवर के लोकेशन की निगरानी के लिए अपना स्थान साझा करने के लिए लोकेशन डेटा एकत्र करता है, तब भी जब ऐप बंद हो या उपयोग में न हो।",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "वर्तमान में, हम इन वाहन पंजीकरण संख्या के साथ नहीं चल रहे हैं",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "आप यात्री सपोर्ट टीम को कॉल करने वाले हैं। क्या आप आगे बढ़ना चाहते हैं?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "आपका स्थान हमारे सिस्टम को आस पास के सभी ऑटो को मैप करने में सहायता कर्ता है और आपको सबसे तेज सवारी प्रदान करता है",
      "WHAT_IS_NAMMA_YATRI_BONUS" : "नम्मा यात्री बोनस क्या है?",
      "EARNED_ON_APP" : "Y पर अर्जित मूल्य",
      "TRAVELLED_ON_APP" : "यात्री पर तय की गई दूरी",
      "GUARANTEED_FIXED_PRICE" : "अप्रैल 1, 2025 तक निश्चित मूल्य गारंटी",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "यात्री योजनाओं के लिए\nतैयार हो जाइए!",
      "DOWNLOAD_NAMMA_YATRI" : "यात्री डाउनलोड करें",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "Describe your issue. Yatri will try to resolve it in under 24 hours.",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "Get ready for\n Yatri Plans!"
    },
    "kannadaStrings": {
      "NEED_IT_TO_ENABLE_LOCATION": "ಅಪ್ಲಿಕೇಶನ್ ಮುಚ್ಚಿದಾಗ ಅಥವಾ ಬಳಕೆಯಲ್ಲಿಲ್ಲದಿದ್ದರೂ ಸಹ ಚಾಲಕ ಪ್ರಸ್ತುತ ಸ್ಥಳವನ್ನು ಮೇಲ್ವಿಚಾರಣೆ ಮಾಡಲು ನಿಮ್ಮ ಸ್ಥಳವನ್ನು ಹಂಚಿಕೊಳ್ಳಲು ಸಕ್ರಿಯಗೊಳಿಸಲು ಯಾತ್ರಿ ಡ್ರೈವರ್ ಸ್ಥಳ ಡೇಟಾವನ್ನು ಸಂಗ್ರಹಿಸುತ್ತದೆ.",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "ಪ್ರಸ್ತುತ, ನಾವು ಈ ವಾಹನ ನೋಂದಣಿ ಸಂಖ್ಯೆಯೊಂದಿಗೆ ಕಾರ್ಯನಿರ್ವಹಿಸುತ್ತಿಲ್ಲ",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "ನೀವು ಯಾತ್ರಿ ಬೆಂಬಲ ತಂಡಕ್ಕೆ ಕರೆ ಮಾಡಲಿರುವಿರಿ. ನೀವು ಮುಂದುವರಿಯಲು ಬಯಸುವಿರಾ?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "ನಿಮ್ಮ ಸ್ಥಳವು ನಮ್ಮ ಸಿಸ್ಟಂ ಅನ್ನು ಆಟೋಗಳ ಮೂಲಕ ನಕ್ಷೆ ಮಾಡಲು ಸಹಾಯ ಮಾಡುತ್ತದೆ ಮತ್ತು ನಿಮಗೆ ಸಾಧ್ಯವಿರುವ ವೇಗದ ಸವಾರಿಯನ್ನು ನೀಡುತ್ತದೆ.",
      "WHAT_IS_NAMMA_YATRI_BONUS" : "ಯಾತ್ರಿ ಬೋನಸ್ ಎಂದರೇನು?",
      "EARNED_ON_APP" : "Y ನಲ್ಲಿ ಗಳಿಸಿದ ಬೆಲೆ",
      "TRAVELLED_ON_APP" : "ಯಾತ್ರಿಯಲ್ಲಿ ಪ್ರಯಾಣಿಸಿದೆ",
      "GUARANTEED_FIXED_PRICE" : "ಅಪ್ರೈಲ್ 1, 2025 ವರೆಗೆ ಖಚಿತ ಬೆಲೆಯ ಗ್ಯಾರಂಟಿ",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "ನಮ್ಮ ಯಾತ್ರಿ ಯೋಜನೆಗಳಿಗೆ\nಸಿದ್ಧತೆ ಪಡೆಯಲು ಸಿದ್ಧವಾಗಿರಿ!",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "ನಿಮ್ಮ ಸಮಸ್ಯೆಯನ್ನು ವಿವರಿಸಿ. ಯಾತ್ರಿ ಅದನ್ನು 24 ಗಂಟೆಗಳಲ್ಲಿ ಪರಿಹರಿಸಲು ಪ್ರಯತ್ನಿಸುತ್ತಾರೆ.",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "ಯಾತ್ರಿ ಯೋಜನೆಗಳಿಗೆ\n ಸಿದ್ಧರಾಗಿ!"
    },
    "tamilStrings": {
      "NEED_IT_TO_ENABLE_LOCATION": "பயன்பாடு மூடப்பட்டிருந்தாலும் அல்லது பயன்பாட்டில் இல்லாவிட்டாலும் கூட, இயக்கி தற்போதைய இருப்பிடத்தைக் கண்காணிக்க, உங்கள் இருப்பிடத்தைப் பகிர்வதை இயக்க, யாத்ரி டிரைவர் இருப்பிடத் தரவைச் சேகரிக்கிறார்.",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "தற்போது, ​​இந்த வாகனப் பதிவு எண்ணுடன் நாங்கள் செயல்படவில்லை",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "யாத்ரி ஆதரவுக் குழுவிற்கு நீங்கள் அழைக்கப் போகிறீர்கள். தொடர விரும்புகிறீர்களா?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "உங்கள் இருப்பிடம், டாக்சிகள் மூலம் அருகிலுள்ள அனைத்தையும் வரைபடமாக்குவதற்கும், விரைவாகச் சவாரி செய்வதற்கும் எங்கள் அமைப்புக்கு உதவுகிறது.",
      "WHAT_IS_NAMMA_YATRI_BONUS" : "யாத்ரி போனஸ் என்றால் என்ன?",
      "EARNED_ON_APP" : "Y இல் பெறப்பட்ட விலை",
      "TRAVELLED_ON_APP" : "யாத்ரியில் பயணம் செய்தேன்",
      "GUARANTEED_FIXED_PRICE" : "ஏப்ரல் 1, 2025 வரை உத்தரவாதமான நிலையான விலை",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "யாத்ரி திட்டங்களுக்கு\n தயாராகுங்கள்!",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "உங்கள் சிக்கலை விவரிக்கவும். யாத்ரி அதை 24 மணி நேரத்திற்குள் தீர்க்க முயற்சிக்கும்."
    },
    "teluguStrings": {
      "NEED_IT_TO_ENABLE_LOCATION": "యాత్రి డ్రైవర్ యాప్ మూసివేయబడినప్పుడు లేదా ఉపయోగంలో లేనప్పటికీ, డ్రైవర్ ప్రస్తుత స్థానాన్ని పర్యవేక్షించడానికి మీ స్థానాన్ని భాగస్వామ్యం చేయడానికి స్థాన డేటాను సేకరిస్తుంది.",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "ప్రస్తుతం, మేము ఈ వాహన రిజిస్ట్రేషన్ నంబర్‌తో పనిచేయడం లేదు",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "మీరు యాత్రి సపోర్ట్ టీమ్‌కి కాల్ చేయబోతున్నారు. మీరు కొనసాగాలనుకుంటున్నారా?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "మీ లొకేషన్ టాక్సీల ద్వారా సమీపంలోని అన్ని ప్రాంతాలను మ్యాప్ చేయడంలో మా సిస్టమ్‌కు సహాయపడుతుంది మరియు మీకు వీలైనంత వేగంగా ప్రయాణించేలా చేస్తుంది.",
      "WHAT_IS_NAMMA_YATRI_BONUS" : "యాత్రి బోనస్ అంటే ఏమిటి?",
      "EARNED_ON_APP" : "యాత్రిలో సంపాదించారు",
      "TRAVELLED_ON_APP" : "యాత్రిలో ప్రయాణించారు",
      "GUARANTEED_FIXED_PRICE" : "ఏప్రిల్ 1, 2025 వరకు స్థిర ధరకు హామీ ఇవ్వబడుతుంది",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "యాత్రి ప్రణాళికలకు\n సిద్ధంగా ఉండండి!",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "మీ సమస్యను వివరించండి. యాత్రి 24 గంటలలోపు దాన్ని పరిష్కరించడానికి ప్రయత్నిస్తుంది."
    },
    "logs": ["JUSPAY","FIREBASE","CLEVERTAP"]
    , "fontName" : "PlusJakartaSans"
    , "fontKannada" : "NotoSansKannada"
    , "allowAllMobileNumber" : false
    , "acExplanation" : true
    , "rcLimit" : 3
    , "rateCardScreen" :{
        "showYoutubeVideo" : false,
        "showRateCard" : true
      }
    , "showGenderBanner" : false
    , "defaultLanguage" : "EN_US"
    , "otpRegex" :  "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
    , "termsLink" : "https://docs.google.com/document/d/17fnfcDCd2KNKSJjFQmEwm7TqsBIOt7kA/edit?usp=drive_link&ouid=115428839751313950285&rtpof=true&sd=true"
    , "termsVersion" : 1.0
    , "privacyLink" : "https://docs.google.com/document/d/1j7REROF75Rpgx65if5guFpqtEKXqCY9O/edit?usp=sharing&ouid=115428839751313950285&rtpof=true&sd=true"
    , "feature" : {
      "enableBonus" : true
      , "enableImageUpload" : false
      , "enableGender" : true
      , "enableOtpRide" : false
      , "enableYatriCoins" : true
    }
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
        "offerBannerValidTill" : "2024-01-16T00:00:00",
        "offerBannerDeadline" : "January 15-*$*-ಜನವರಿ 15-*$*-15 जनवरी-*$*-ஜனவரி 15",
        "offerBannerPlans" : ["a35ffc7c-de0d-4dcc-83a8-e36a5a29cc1d"],
      },
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

    , "appData" : {
      "link" : "https://play.google.com/store/apps/details?id=net.openkochi.yatripartner"
      , "name" : "Yatri"
    }
    , "OTP_MESSAGE_REGEX" : "is your OTP for login to [A-Za-z]+ [A-Za-z]+"
    , "autoPayBanner" : false
    , "referralType" : "LeaderBoard"
    , "enableMockLocation" : false
    , "vehicle" : {
      "validationPrefix" : "KA|AP|TS|DL|TN|PY|UP|HR|TG"
    }
    , "referral": {
      "type": "LeaderBoard",
      "link" : "https://yatricustomer.page.link/pcJb",
      "customerAppId" : "net.openkochi.yatri",
      "driverAppId" : "net.openkochi.yatripartner"
    }
    , "gotoConfig" : {
      "maxGotoLocations" : 5,
      "enableGoto" : true
    }
    , "bottomNavConfig" : {
      "subscription" :
        { "isVisible" : true
        },
        "referral" : 
        { 
          "showNew" : true
        }
    },
    "purpleRideConfig" : {
      "showPurpleVideos" : false,
      "visualImpairmentVideo" : "https://www.youtube.com/watch?v=2qYXl03N6Jg",
      "physicalImpairmentVideo" : "https://www.youtube.com/watch?v=B0C6SZTQO6k",
      "hearingImpairmentVideo" : "https://www.youtube.com/watch?v=udkWOt0serg",
      "genericAccessibilityVideo" : "https://youtu.be/5s21p2rI58c"
    },
    "cityConfig" : [
      {
        "cityName" : "Bangalore",
        "mapImage" : "ny_ic_bangalore_map",
        "cityCode" : "std:080",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 12.971599,
        "cityLong" : 77.594566,
        "supportNumber" : "",
        "languageKey" : "KN_IN",
        "showDriverReferral" : false,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : true,
        "vehicleNSImg" : "ny_ic_auto_image",
        "registration" : {
            "supportWAN" : "918618963188",
            "callSupport" : true,
            "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : true,
          "variantList" : ["AutoCategory"],
          "enableCabsSubscriptionView" : true,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
            "domain" : "https://www.getyatri.com"
          , "customerAppId" : "net.openkochi.yatri"
          , "driverAppId" : "net.openkochi.yatripartner"
        },
        "waitingCharges" : 1.50,
        "waitingChargesConfig" : defWaitingChargesConfig,
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : { "showLearnMore" : true, "learnMoreVideoLink" : "https://www.youtube.com/shorts/NUTNKPzslpw" },
        "gstPercentage" : "18",
        "currency" : "₹",
        "assets" :{
          "auto_image" : "ic_auto_rickshaw",
          "onboarding_auto_image" : "ny_ic_auto_side",
          "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
        },
        "enableHvSdk" : false,
        "purpleRideConfig" : {
          "purpleRideConfigForAuto" : {
            "vehicleVariant" : "Auto",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          },
          "purpleRideConfigForCabs" : {
            "vehicleVariant" : "Cab",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          },
          "purpleRideConfigForBikes" : {
            "vehicleVariant" : "Bike",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          }
        }
      },
      {
        "cityName" : "Hyderabad",
        "mapImage" : "ny_ic_hyderabad_map",
        "cityCode" : "std:040",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 17.402113,
        "cityLong" : 78.499827,
        "supportNumber" : "+918069724900",
        "languageKey" : "TE_IN",
        "showDriverReferral" : true,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "vehicleNSImg" : "ny_ic_auto_image_old",
        "registration" : {
            "supportWAN" : "919392636637",
            "callSupport" : true,
            "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : true,
          "variantList" : [],
          "enableCabsSubscriptionView" : true,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
            "domain" : "https://www.manayatri.in"
          , "customerAppId" : "in.mobility.manayatri"
          , "driverAppId" : "in.mobility.manayatripartner"
        },
        "waitingCharges" : 2.00,
        "waitingChargesConfig" : {
             "cab" : {
               "freeSeconds" : 300,
               "perMinCharges" : 1.0
             },
             "auto" : {
               "freeSeconds" : 180,
               "perMinCharges" : 2.00
             },
             "bike" : {
               "freeSeconds" : 300,
               "perMinCharges" : 1.0 
             }
           },
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : defRateCardConfig,
        "gstPercentage" :  "18",        
        "currency" : "₹",
        "assets" :{
          "auto_image" :  "ny_ic_black_yellow_auto_side_view",
          "onboarding_auto_image" : "ny_ic_auto_right_side_yellow",
          "empty_referral_auto" : "ny_ic_refer_now_auto_my,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_my.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_my,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_my.png" 
        },
        "enableHvSdk" : false,
        "purpleRideConfig" : {
          "purpleRideConfigForAuto" : {
            "vehicleVariant" : "Auto",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          },
          "purpleRideConfigForCabs" : {
            "vehicleVariant" : "Cab",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          },
          "purpleRideConfigForBikes" : {
            "vehicleVariant" : "Bike",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          }
        }
      },
      {
        "cityName" : "Mysore",
        "mapImage" : "ny_ic_mysuru_map",
        "cityCode" : "std:0821",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 12.295810,
        "cityLong" : 76.639381,
        "supportNumber" : "",
        "languageKey" : "KN_IN",
        "showDriverReferral" : false,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : true,
        "vehicleNSImg" : "ny_ic_auto_image",
        "registration" : {
            "supportWAN" : "918618963188",
            "callSupport" : true,
            "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : true,
          "variantList" : ["AutoCategory"],
          "enableCabsSubscriptionView" : true,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
          "domain" : "https://www.getyatri.com"
        , "customerAppId" : "net.openkochi.yatri"
        , "driverAppId" : "net.openkochi.yatripartner"
      },
      "waitingCharges" : 1.50,
      "waitingChargesConfig" : defWaitingChargesConfig,
      "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
      "rateCardConfig" : { "showLearnMore" : true, "learnMoreVideoLink" : "https://www.youtube.com/shorts/NUTNKPzslpw" },
      "gstPercentage" : "18",
      "currency" : "₹",
      "assets" :{
        "auto_image" : "ic_auto_rickshaw",
        "onboarding_auto_image" : "ny_ic_auto_side",
        "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
        "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
      },
      "enableHvSdk" : false,
      "purpleRideConfig" : {
        "purpleRideConfigForAuto" : {
          "vehicleVariant" : "Auto",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForCabs" : {
          "vehicleVariant" : "Cab",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForBikes" : {
          "vehicleVariant" : "Bike",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        }
      }
      },
      {
        "cityName" : "Delhi", 
        "mapImage" : "ny_ic_delhi_map",
        "cityCode" : "std:011",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 28.618978,
        "cityLong" : 77.207795,
        "supportNumber" : "+918069724848",
        "languageKey" : "HI_IN",
        "showDriverReferral" : true,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "vehicleNSImg" : "ny_ic_auto_image",
        "registration" : {
            "supportWAN" : "919625724848",
            "callSupport" : true,
            "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : true,
          "variantList" : [],
          "enableCabsSubscriptionView" : true,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
          "domain" : "https://www.getyatri.com"
        , "customerAppId" : "net.openkochi.yatri"
        , "driverAppId" : "net.openkochi.yatripartner"
      },
      "waitingCharges" : 0.75,
      "waitingChargesConfig" : {
           "cab" : {
             "freeSeconds" : 300,
             "perMinCharges" : 1.0
           },
           "auto" : {
             "freeSeconds" : 180,
             "perMinCharges" : 0.75
           },
           "bike" : {
             "freeSeconds" : 300,
             "perMinCharges" : 1.0
           }
         },
      "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
      "rateCardConfig" : defRateCardConfig,
      "gstPercentage" :  "18",         
         "assets" :{
           "auto_image" : "ic_auto_rickshaw",
           "onboarding_auto_image" : "ny_ic_auto_side",
           "empty_referral_auto" : "ny_ic_refer_now_auto_yatri_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_yatri_green.png",
           "empty_referral_cab" : "ny_ic_refer_now_cab_yatri,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_yatri.png"
         },
      "enableHvSdk" : false,
      "currency" : "₹",
      "purpleRideConfig" : {
        "purpleRideConfigForAuto" : {
          "vehicleVariant" : "Auto",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForCabs" : {
          "vehicleVariant" : "Cab",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForBikes" : {
          "vehicleVariant" : "Bike",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        }
      }
      },
      {
        "cityName" : "Chennai",
        "mapImage" : "ny_ic_chennai_map",
        "cityCode" : "std:044",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 13.067439,
        "cityLong" : 80.237617,
        "supportNumber" : "08069724899",
        "languageKey" : "TA_IN",
        "showDriverReferral" : true,
        "showCustomerReferral" : true,
        "uploadRCandDL" : false,
        "enableYatriCoins" : false,
        "vehicleNSImg" : "ny_ic_auto_image_old",
        "registration" : {
            "supportWAN" : "917483117936",
            "callSupport" : true,
            "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : true,
          "variantList" : [],
          "enableCabsSubscriptionView" : true,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
          "domain" : "https://www.getyatri.com"
        , "customerAppId" : "net.openkochi.yatri"
        , "driverAppId" : "net.openkochi.yatripartner"
      },
      "waitingCharges" : 1.00,
      "waitingChargesConfig" : {
           "cab" : {
             "freeSeconds" : 300,
             "perMinCharges" : 1.0
           },
           "auto" : {
             "freeSeconds" : 180,
             "perMinCharges" : 1.00
           },
           "bike" : {
             "freeSeconds" : 300,
             "perMinCharges" : 1.0
           }
         },
      "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
      "rateCardConfig" : defRateCardConfig,
      "gstPercentage" :  "18",         
         "assets" :{
           "auto_image" :  "ny_ic_black_yellow_auto_side_view",
           "onboarding_auto_image" : "ny_ic_auto_right_side_yellow",
           "empty_referral_auto" : "ny_ic_refer_now_auto_ny_yellow,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_yellow.png",
           "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
         },
      "enableHvSdk" : false,
      "purpleRideConfig" : {
        "purpleRideConfigForAuto" : {
          "vehicleVariant" : "Auto",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForCabs" : {
          "vehicleVariant" : "Cab",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForBikes" : {
          "vehicleVariant" : "Bike",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        }
      }
      },
      {
        "cityName" : "Coimbatore",
        "mapImage" : "ny_ic_coimbatore_map",
        "cityCode" : "std:0422",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 11.023481,
        "cityLong" : 76.966429,
        "supportNumber" : "",
        "languageKey" : "TA_IN",
        "showDriverReferral" : true,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "vehicleNSImg" : "ny_ic_auto_image",
        "registration" : {
            "supportWAN" : "",
            "callSupport" : true,
            "whatsappSupport" : false
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : true,
          "variantList" : [],
          "enableCabsSubscriptionView" : true,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
          "domain" : "https://www.getyatri.com"
        , "customerAppId" : "net.openkochi.yatri"
        , "driverAppId" : "net.openkochi.yatripartner"
      },
      "waitingCharges" : 1.50,
      "waitingChargesConfig" : defWaitingChargesConfig,
      "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
      "rateCardConfig" : defRateCardConfig,
      "gstPercentage" :  "18",     
      "currency" : "₹",
      "assets" :{
        "auto_image" :  "ny_ic_black_yellow_auto_side_view",
        "onboarding_auto_image" : "ny_ic_auto_right_side_yellow",
        "empty_referral_auto" : "ny_ic_refer_now_auto_ny_yellow,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_yellow.png",
        "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
      },
      "enableHvSdk" : false,
      "purpleRideConfig" : {
        "purpleRideConfigForAuto" : {
          "vehicleVariant" : "Auto",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForCabs" : {
          "vehicleVariant" : "Cab",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForBikes" : {
          "vehicleVariant" : "Bike",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        }
      }
      },
      {
        "cityName" : "Puducherry",
        "mapImage" : "ny_ic_puducherry_map",
        "cityCode" : "std:0413",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 11.943852,
        "cityLong" : 79.808292,
        "supportNumber" : "08069724899",
        "languageKey" : "TA_IN",
        "showDriverReferral" : true,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "vehicleNSImg" : "ny_ic_auto_image",
        "registration" : {
          "supportWAN" : "917483117936",
          "callSupport" : true,
          "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : true,
          "variantList" : [],
          "enableCabsSubscriptionView" : true,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
          "domain" : "https://www.getyatri.com"
        , "customerAppId" : "net.openkochi.yatri"
        , "driverAppId" : "net.openkochi.yatripartner"
      },
      "waitingCharges" : 1.50,
      "waitingChargesConfig" : defWaitingChargesConfig,
      "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
      "rateCardConfig" : defRateCardConfig,
      "gstPercentage" :  "18",     
      "currency" : "₹",
      "assets" :{
        "auto_image" :  "ny_ic_black_yellow_auto_side_view",
        "onboarding_auto_image" : "ny_ic_auto_right_side_yellow",
        "empty_referral_auto" : "ny_ic_refer_now_auto_ny_yellow,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_yellow.png",
        "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
      },
      "enableHvSdk" : false,
      "purpleRideConfig" : {
        "purpleRideConfigForAuto" : {
          "vehicleVariant" : "Auto",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForCabs" : {
          "vehicleVariant" : "Cab",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForBikes" : {
          "vehicleVariant" : "Bike",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        }
      }
      },
      {
        "cityName" : "Tumakuru",
        "mapImage" : "ny_ic_tumakuru_map",
        "cityCode" : "std:0816",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 15.32383804957557,
        "cityLong" : 75.88071672412116,
        "supportNumber" : "",
        "languageKey" : "KN_IN",
        "showDriverReferral" : false,
        "showCustomerReferral" : true,
        "vehicleNSImg" : "ny_ic_auto_image_old",
        "uploadRCandDL" : true,
        "enableYatriCoins" : true,
        "registration" : {
          "supportWAN" : "918618963188",
          "callSupport" : true,
          "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : true,
          "variantList" : ["AutoCategory"],
          "enableCabsSubscriptionView" : true,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
          "domain" : "https://www.getyatri.com"
        , "customerAppId" : "net.openkochi.yatri"
        , "driverAppId" : "net.openkochi.yatripartner"
      },
      "waitingCharges" : 1.50,
      "waitingChargesConfig" : defWaitingChargesConfig,
      "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
      "rateCardConfig" : { "showLearnMore" : true, "learnMoreVideoLink" : "https://www.youtube.com/shorts/NUTNKPzslpw" },
      "gstPercentage" : "18",
      "currency" : "₹",
      "assets" :{
        "auto_image" : "ic_auto_rickshaw",
        "onboarding_auto_image" : "ny_ic_auto_side",
        "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
        "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
      },
      "enableHvSdk" : false,
      "purpleRideConfig" : {
        "purpleRideConfigForAuto" : {
          "vehicleVariant" : "Auto",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForCabs" : {
          "vehicleVariant" : "Cab",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForBikes" : {
          "vehicleVariant" : "Bike",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        }
      }
      },
      {
        "cityName" : "Gurugram",
        "mapImage" : "ny_ic_gurugram_map",
        "cityCode" : "std:0124",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 28.457523,
        "cityLong" : 77.026344,
        "supportNumber" : "+918069724848",
        "languageKey" : "HI_IN",
        "showDriverReferral" : true,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "vehicleNSImg" : "ny_ic_auto_image",
        "registration" : {
          "supportWAN" : "919625724848",
          "callSupport" : true,
          "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : true,
          "variantList" : [],
          "enableCabsSubscriptionView" : true,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
          "domain" : "https://www.getyatri.com"
        , "customerAppId" : "net.openkochi.yatri"
        , "driverAppId" : "net.openkochi.yatripartner"
      },
      "waitingCharges" : 1.50,
      "waitingChargesConfig" : defWaitingChargesConfig,
      "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
      "rateCardConfig" : defRateCardConfig,
      "gstPercentage" :  "18",     
      "currency" : "₹",
      "assets" :{
        "auto_image" : "ic_auto_rickshaw",
        "onboarding_auto_image" : "ny_ic_auto_side",
        "empty_referral_auto" : "ny_ic_refer_now_auto_yatri_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_yatri_green.png",
        "empty_referral_cab" : "ny_ic_refer_now_cab_yatri,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_yatri.png"
      },
      "enableHvSdk" : false,
      "purpleRideConfig" : {
        "purpleRideConfigForAuto" : {
          "vehicleVariant" : "Auto",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForCabs" : {
          "vehicleVariant" : "Cab",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForBikes" : {
          "vehicleVariant" : "Bike",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        }
      }
      },
      {
        "cityName" : "Noida",
        "mapImage" : "ny_ic_noida_map",
        "cityCode" : "std:01189",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 28.535517,
        "cityLong" : 77.391029,
        "supportNumber" : "+918069724848",
        "languageKey" : "HI_IN",
        "showDriverReferral" : true,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "registration" : {
          "supportWAN" : "919625724848",
          "callSupport" : true,
          "whatsappSupport" : true
        },
        "vehicleNSImg" : "ny_ic_auto_image_old",
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : true,
          "variantList" : [],
          "enableCabsSubscriptionView" : true,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
          "domain" : "https://www.getyatri.com"
        , "customerAppId" : "net.openkochi.yatri"
        , "driverAppId" : "net.openkochi.yatripartner"
      },
      "waitingCharges" : 1.50,
      "waitingChargesConfig" : defWaitingChargesConfig,
      "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
      "rateCardConfig" : defRateCardConfig,
      "gstPercentage" :  "18",      
      "currency" : "₹",
      "assets" :{
        "auto_image" : "ic_auto_rickshaw",
        "onboarding_auto_image" : "ny_ic_auto_side",
        "empty_referral_auto" : "ny_ic_refer_now_auto_yatri_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_yatri_green.png",
        "empty_referral_cab" : "ny_ic_refer_now_cab_yatri,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_yatri.png"
      },
      "enableHvSdk" : false,
      "purpleRideConfig" : {
        "purpleRideConfigForAuto" : {
          "vehicleVariant" : "Auto",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForCabs" : {
          "vehicleVariant" : "Cab",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForBikes" : {
          "vehicleVariant" : "Bike",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        }
      }
      },
      {
        "cityName" : "TamilNaduCities",
        "mapImage" : "ny_ic_tamilnadu_map",
        "cityCode" :  "std:0422",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 11.1271,
        "cityLong" : 78.6569,
        "supportNumber" : "08069724899",
        "languageKey" : "TA_IN",
        "showDriverReferral" : true,
        "showCustomerReferral" : true,
        "uploadRCandDL" : false,
        "enableYatriCoins" : false,
        "vehicleNSImg" : "ny_ic_auto_image_old",
        "registration" : {
          "supportWAN" : "917483117936",
          "callSupport" : true,
          "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : true,
          "variantList" : [],
          "enableCabsSubscriptionView" : true,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
          "domain" : "https://www.getyatri.com"
        , "customerAppId" : "net.openkochi.yatri"
        , "driverAppId" : "net.openkochi.yatripartner"
      },
      "waitingCharges" : 1.50,
      "waitingChargesConfig" : defWaitingChargesConfig,
      "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
      "currency" : "₹",
      "rateCardConfig" : defRateCardConfig,
      "gstPercentage" :  "18",     
      "assets" :{
        "auto_image" :  "ny_ic_black_yellow_auto_side_view",
        "onboarding_auto_image" : "ny_ic_auto_right_side_yellow",
        "empty_referral_auto" : "ny_ic_refer_now_auto_ny_yellow,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_yellow.png",
        "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
      },
      "enableHvSdk" : false,
      "purpleRideConfig" : {
        "purpleRideConfigForAuto" : {
          "vehicleVariant" : "Auto",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForCabs" : {
          "vehicleVariant" : "Cab",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForBikes" : {
          "vehicleVariant" : "Bike",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        }
      }
      },
      {
        "cityName" : "Minneapolis",
        "mapImage" : "ny_ic_minneapolis_map",
        "cityCode" : "std:01189",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "",
        "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 44.977753,
        "cityLong" : -93.2650108,
        "supportNumber" : "+918069724848",
        "vehicleNSImg" : "ny_ic_auto_image_old",
        "languageKey" : "EN_US",
        "showDriverReferral" : true,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "registration" : {
          "supportWAN" : "919625724848",
          "callSupport" : true,
          "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : true,
          "variantList" : [],
          "enableCabsSubscriptionView" : true,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
          "domain" : "https://www.getyatri.com",
          "customerAppId" : "net.openkochi.yatri",
          "driverAppId" : "net.openkochi.yatripartner"
        },
        "waitingCharges" : 1.50,
        "waitingChargesConfig" : defWaitingChargesConfig,
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : defRateCardConfig,
        "gstPercentage" :  "18",       
        "currency" : "$",
        "assets" : {
          "auto_image" : "ny_ic_black_yellow_auto_side_view",
          "onboarding_auto_image" : "ny_ic_auto_right_side_yellow",
          "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
        },
        "enableHvSdk" : false,
        "purpleRideConfig" : {
          "purpleRideConfigForAuto" : {
            "vehicleVariant" : "Auto",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          },
          "purpleRideConfigForCabs" : {
            "vehicleVariant" : "Cab",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          },
          "purpleRideConfigForBikes" : {
            "vehicleVariant" : "Bike",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          }
        }
      },
      {
        "cityName" : "Kochi",
        "mapImage" : "ny_ic_kochi_map",
        "cityCode" : "std:0484",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "",
        "callDriverInfoPost": false,
        "showSubscriptions" : false,
        "cityLat" : 9.931233,
        "cityLong" : 76.267303,
        "supportNumber" : "08069724930",
        "languageKey" : "ML_IN",
        "showDriverReferral" : true,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "vehicleNSImg" : "ny_ic_auto_image",
        "registration" : {
            "supportWAN" : "918618963188",
            "callSupport" : true,
            "whatsappSupport" : false
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : false,
          "variantList" : [],
          "enableCabsSubscriptionView" : false,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
          "domain" : "https://www.getyatri.com"
        , "customerAppId" : "net.openkochi.yatri"
        , "driverAppId" : "net.openkochi.yatripartner"
      },
      "waitingCharges" : 1.50,
      "waitingChargesConfig" : defWaitingChargesConfig,
      "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
      "currency" : "₹",
      "rateCardConfig" : defRateCardConfig,
      "gstPercentage" :  "18",     
      "assets" :{
        "auto_image" : "ny_ic_auto_side_view",
        "onboarding_auto_image" : "ny_ic_auto_right_side_black",
        "empty_referral_auto" : "ny_ic_refer_now_auto_yatri_black,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_yatri_black.png",
        "empty_referral_cab" : "ny_ic_refer_now_cab_yatri,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_yatri.png"
      },
      "enableHvSdk" : true,
      "purpleRideConfig" : {
        "purpleRideConfigForAuto" : {
          "vehicleVariant" : "Auto",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForCabs" : {
          "vehicleVariant" : "Cab",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForBikes" : {
          "vehicleVariant" : "Bike",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        }
      }
    },
      {
        "cityName": "Jaipur",
        "mapImage": "ny_ic_jaipur_map",
        "cityCode": "std:0141","enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "",
        "callDriverInfoPost": false,
        "showSubscriptions": false,
        "cityLat": 28.618978,
        "cityLong": 77.207795,
        "supportNumber": "+918069724848",
        "languageKey": "HI_IN",
        "showDriverReferral": true,
        "showCustomerReferral": true,
        "uploadRCandDL": true,
        "enableYatriCoins": false,
        "vehicleNSImg": "ny_ic_auto_image",
        "registration": {
          "supportWAN": "919625724848",
          "callSupport": true,
          "whatsappSupport": true
        },
        "variantSubscriptionConfig": {
          "enableVariantBasedSubscription": true,
          "variantList": [],
          "enableCabsSubscriptionView": true,
          "staticViewPlans": staticSubscriptionConfig
        },
        "showEarningSection": true,
        "referral": {
          "domain": "https://www.getyatri.com"
          , "customerAppId": "net.openkochi.yatri"
          , "driverAppId": "net.openkochi.yatripartner"
        },
        "waitingCharges": 0.75,
        "waitingChargesConfig": {
          "cab": {
            "freeSeconds": 300,
            "perMinCharges": 1.0
          },
          "auto": {
            "freeSeconds": 180,
            "perMinCharges": 0.75
          },
           "bike" : {
             "freeSeconds" : 300,
             "perMinCharges" : 1.0
           }
        },
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "currency" : "₹",
        "rateCardConfig" : defRateCardConfig,
        "gstPercentage" :  "18",       
        "assets": {
          "auto_image": "ic_auto_rickshaw",
          "onboarding_auto_image": "ny_ic_auto_side",
          "empty_referral_auto" : "ny_ic_refer_now_auto_yatri_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_yatri_green.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_yatri,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_yatri.png"
        },
        "enableHvSdk" : false,
        "purpleRideConfig" : {
          "purpleRideConfigForAuto" : {
            "vehicleVariant" : "Auto",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          },
          "purpleRideConfigForCabs" : {
            "vehicleVariant" : "Cab",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          },
          "purpleRideConfigForBikes" : {
            "vehicleVariant" : "Bike",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          }
        }
      },
      {
        "cityName": "Chandigarh",
        "mapImage": "ny_ic_chandigarh_map",
        "cityCode": "std:0172",
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "",
        "callDriverInfoPost": false,
        "showSubscriptions": false,
        "cityLat": 28.618978,
        "cityLong": 77.207795,
        "supportNumber": "+918069724848",
        "languageKey": "HI_IN",
        "showDriverReferral": true,
        "showCustomerReferral": true,
        "uploadRCandDL": true,
        "enableYatriCoins": false,
        "vehicleNSImg": "ny_ic_auto_image",
        "registration": {
          "supportWAN": "919625724848",
          "callSupport": true,
          "whatsappSupport": true
        },
        "variantSubscriptionConfig": {
          "enableVariantBasedSubscription": true,
          "variantList": [],
          "enableCabsSubscriptionView": true,
          "staticViewPlans": staticSubscriptionConfig
        },
        "showEarningSection": true,
        "referral": {
          "domain": "https://www.getyatri.com"
          , "customerAppId": "net.openkochi.yatri"
          , "driverAppId": "net.openkochi.yatripartner"
        },
        "waitingCharges": 0.75,
        "waitingChargesConfig": {
          "cab": {
            "freeSeconds": 300,
            "perMinCharges": 1.0
          },
          "auto": {
            "freeSeconds": 180,
            "perMinCharges": 0.75
          },
           "bike" : {
             "freeSeconds" : 300,
             "perMinCharges" : 1.0
           }
        },
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "currency" : "₹",
        "rateCardConfig" : defRateCardConfig,
        "gstPercentage" :  "18",        
        "assets": {
          "auto_image": "ic_auto_rickshaw",
          "onboarding_auto_image": "ny_ic_auto_side",
          "empty_referral_auto" : "ny_ic_refer_now_auto_yatri_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_yatri_green.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_yatri,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_yatri.png"
        },
        "enableHvSdk" : false,
        "purpleRideConfig" : {
          "purpleRideConfigForAuto" : {
            "vehicleVariant" : "Auto",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          },
          "purpleRideConfigForCabs" : {
            "vehicleVariant" : "Cab",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          },
          "purpleRideConfigForBikes" : {
            "vehicleVariant" : "Bike",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          }
        }
      },
      {
        "cityName": "Thrissur",
        "mapImage": "ny_ic_thrissur_map",
        "cityCode": "std:0487",
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "",
        "callDriverInfoPost": false,
        "showSubscriptions": false,
        "cityLat": 9.931233,
        "cityLong": 76.267303,
        "supportNumber": "08069724930",
        "languageKey": "ML_IN",
        "showDriverReferral": true,
        "showCustomerReferral": true,
        "uploadRCandDL": true,
        "enableYatriCoins": false,
        "vehicleNSImg": "ny_ic_auto_image",
        "registration": {
          "supportWAN": "918618963188",
          "callSupport": true,
          "whatsappSupport": false
        },
        "variantSubscriptionConfig": {
          "enableVariantBasedSubscription": false,
          "variantList": [],
          "enableCabsSubscriptionView": false,
          "staticViewPlans": staticSubscriptionConfig
        },
        "showEarningSection": true,
        "referral": {
          "domain": "https://www.getyatri.com"
          , "customerAppId": "net.openkochi.yatri"
          , "driverAppId": "net.openkochi.yatripartner"
        },
        "waitingCharges": 1.50,
        "waitingChargesConfig": defWaitingChargesConfig,
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "currency" : "₹",
        "rateCardConfig" : defRateCardConfig,
        "gstPercentage" :  "18",       
        "assets": {
          "auto_image": "ny_ic_auto_side_view",
          "onboarding_auto_image": "ny_ic_auto_right_side_black",
          "empty_referral_auto" : "ny_ic_refer_now_auto_yatri_black,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_yatri_black.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_yatri,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_yatri.png"
        },
        "enableHvSdk" : false,
        "purpleRideConfig" : {
          "purpleRideConfigForAuto" : {
            "vehicleVariant" : "Auto",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          },
          "purpleRideConfigForCabs" : {
            "vehicleVariant" : "Cab",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          },
          "purpleRideConfigForBikes" : {
            "vehicleVariant" : "Bike",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          }
        }
      },
      {
        "cityName": "Trivandrum",
        "mapImage": "ny_ic_trivandrum_map",
        "cityCode": "std:0471",
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "",
        "callDriverInfoPost": false,
        "showSubscriptions": false,
        "cityLat": 9.931233,
        "cityLong": 76.267303,
        "supportNumber": "08069724930",
        "languageKey": "ML_IN",
        "showDriverReferral": true,
        "showCustomerReferral": true,
        "uploadRCandDL": true,
        "enableYatriCoins": false,
        "vehicleNSImg": "ny_ic_auto_image",
        "registration": {
          "supportWAN": "918618963188",
          "callSupport": true,
          "whatsappSupport": false
        },
        "variantSubscriptionConfig": {
          "enableVariantBasedSubscription": false,
          "variantList": [],
          "enableCabsSubscriptionView": false,
          "staticViewPlans": staticSubscriptionConfig
        },
        "showEarningSection": true,
        "referral": {
          "domain": "https://www.getyatri.com"
          , "customerAppId": "net.openkochi.yatri"
          , "driverAppId": "net.openkochi.yatripartner"
        },
        "waitingCharges": 1.50,
        "waitingChargesConfig": defWaitingChargesConfig,
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "currency" : "₹",
        "rateCardConfig" : defRateCardConfig,
        "gstPercentage" :  "18",        
        "assets": {
          "auto_image": "ny_ic_auto_side_view",
          "onboarding_auto_image": "ny_ic_auto_right_side_black",
          "empty_referral_auto" : "ny_ic_refer_now_auto_yatri_black,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_yatri_black.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_yatri,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_yatri.png"
        },
        "enableHvSdk" : false,
        "purpleRideConfig" : {
          "purpleRideConfigForAuto" : {
            "vehicleVariant" : "Auto",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          },
          "purpleRideConfigForCabs" : {
            "vehicleVariant" : "Cab",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          },
          "purpleRideConfigForBikes" : {
            "vehicleVariant" : "Bike",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          }
        }
      },
      {
        "cityName": "Kozhikode",
        "mapImage": "ny_ic_kochi_map",
        "cityCode": "std:0495",
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "",
        "callDriverInfoPost": false,
        "showSubscriptions": false,
        "cityLat": 9.931233,
        "cityLong": 76.267303,
        "supportNumber": "08069724930",
        "languageKey": "ML_IN",
        "showDriverReferral": true,
        "showCustomerReferral": true,
        "uploadRCandDL": true,
        "enableYatriCoins": false,
        "vehicleNSImg": "ny_ic_auto_image",
        "registration": {
          "supportWAN": "918618963188",
          "callSupport": true,
          "whatsappSupport": false
        },
        "variantSubscriptionConfig": {
          "enableVariantBasedSubscription": false,
          "variantList": [],
          "enableCabsSubscriptionView": false,
          "staticViewPlans": staticSubscriptionConfig
        },
        "showEarningSection": true,
        "referral": {
          "domain": "https://www.getyatri.com"
          , "customerAppId": "net.openkochi.yatri"
          , "driverAppId": "net.openkochi.yatripartner"
        },
        "waitingCharges": 1.50,
        "waitingChargesConfig": defWaitingChargesConfig,
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "currency" : "₹",
        "rateCardConfig" : defRateCardConfig,
        "gstPercentage" :  "18",       
        "assets": {
          "auto_image": "ny_ic_auto_side_view",
          "onboarding_auto_image": "ny_ic_auto_right_side_black",
          "empty_referral_auto" : "ny_ic_refer_now_auto_yatri_black,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_yatri_black.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_yatri,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_yatri.png"
        },
        "enableHvSdk" : false,
        "purpleRideConfig" : {
          "purpleRideConfigForAuto" : {
            "vehicleVariant" : "Auto",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          },
          "purpleRideConfigForCabs" : {
            "vehicleVariant" : "Cab",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          },
          "purpleRideConfigForBikes" : {
            "vehicleVariant" : "Bike",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          }
        }
      }
  ]
  , "coinsConfig" : {
    "minCoinSliderValue" : 250,
    "stepFunctionForCoinConversion" : 250,
    "eightPlusRidesCoins" : "+25",
    "purpleRideCoins" : "+5",
    "customerReferralCoins" : "+200",
    "whatAreYatriCoinFAQ" : "https://www.youtube.com/shorts/nc_oicjF9eI",
    "coinTermsAndConditions" : "https://docs.google.com/document/u/0/d/1hLnxlJLlC8bZIsm3WCoQFf7lsdffy-do",
    "howToEarnYatriCoinFAQ" : "https://www.youtube.com/shorts/hpmYpwfOG_E",
    "howToRedeemYatriCoinFAQ" : "https://www.youtube.com/watch?v=XW4WSWQpq80",
    "eightRideCoinEvent" : true,
    "prupleRideCoinEvent" : true,
    "driverToCustomerRefCoinEvent" : true
  }
  , "dashboard" : {
        "enable" : true,
        "url" : "https://www.getyatri.com/open?source=in-app"
    }
  , "profile" : {
      "showBookingOption" : true
    }
  , "waitTimeConfig" : {
    "thresholdDist" : 0.03,
    "routeDistance" : 30,
    "diffBtwTwoHeartBeats" : 10,
    "straightLineDist" : 0.015
  }
  , "rideCompletedCardConfig" : {
    "lottieQRAnim" : true
  }, 
  "vehicle" : {
    "validationPrefix" : "KA|AP|TS|DL|TN|PY|UP|HR|TG"
  }
  , "appUpdatePopupUrl" : "https://play.google.com/store/apps/details?id=net.openkochi.yatripartner&pcampaignid=web_share"
  })
}

let staticSubscriptionConfig = [
  {"price" : 90.0, "frequency" : "PER_DAY", "variantCategory" : "CarCategory", "name" : "DAILY_UNLIMITED", "introductoryOffer" :  "FREE_RIDE_OFFER", "showSelected" : false, "planDesc" : "CAB_DAILY_UNLIMITED_OFFER"},
  {"price" : 9.0, "frequency" : "PER_RIDE", "variantCategory" : "CarCategory", "name" : "DAILY_PER_RIDE", "introductoryOffer" : "" , "showSelected" : false, "planDesc" : "Up to a maximum of ₹99 per day-*$*-ದಿನಕ್ಕೆ ಗರಿಷ್ಠ ₹99-*$*-प्रति दिन अधिकतम ₹99 तक-*$*-প্রতিদিন সর্বোচ্চ ₹99 পর্যন্ত-*$*-പ്രതിദിനം പരമാവധി ₹99 വരെ-*$*-ஒரு நாளைக்கு அதிகபட்சம் ₹99 வரை-*$*-రోజుకు గరిష్టంగా ₹99 వరకు"},
  {"price" : 25.0, "frequency" : "PER_DAY", "variantCategory" : "AutoCategory", "name" : "DAILY_UNLIMITED", "introductoryOffer" : "NO_CHARGES_TILL", "showSelected" : true, "planDesc" : ""}
]


let defWaitingChargesConfig = {
    "cab" : {
      "freeSeconds" : 300,
      "perMinCharges" : 1.0
    },
    "auto" : {
      "freeSeconds" : 180,
      "perMinCharges" : 1.50
    },
    "bike" : {
      "freeSeconds" : 300,
      "perMinCharges" : 1.0
    }
  }


let defRentalWaitingChargesConfig = {
    "cab" : {
      "freeSeconds" : 180,
      "perMinCharges" : 2.0
    },
    "auto" : {
      "freeSeconds" : 180,
      "perMinCharges" : 1.0
    },
    "bike" : {
      "freeSeconds" : 180,
      "perMinCharges" : 2.0
    }
  }

let defRateCardConfig = {
    "showLearnMore" : false,
    "learnMoreVideoLink" : ""
  }