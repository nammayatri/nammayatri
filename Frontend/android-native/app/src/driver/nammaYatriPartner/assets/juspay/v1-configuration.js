window.version = window.version || {};
let version = "1.0.0";
if (typeof __VERSION__ !== "undefined") {
  version = __VERSION__
}
window.version["configuration"]= version;
const sessionInfo = JSON.parse(JBridge.getSessionInfo());
const appName = sessionInfo.app_name.toLowerCase();
window.getMerchantConfig = function () {
  return JSON.stringify({
    "RC_VALIDATION_TEXT": "KA|AP|TS|DL|TN|PY|UP|HR|TG",
    "DOCUMENT_LINK": "https://docs.google.com/document/d/1uL6CzKl_rt1ZSngcAOiafnTaI7Q4vO_6Ja9NHvYX3RU",
    "APP_LINK": "https://play.google.com/store/apps/details?id=in.juspay.nammayatripartner",
    "USER_APP_LINK": "https://nammayatri.in/link/rider/mvnw",
    "PRIVACY_POLICY_LINK": "https://docs.google.com/document/d/1N3ZJVwilWO9cIGFJXgl8h8au7lpdRyhXNMZJ8OXq4YY",
    "SPECIAL_ZONE_OTP_VIEW": "false",
    "StringKeys": ["NEED_IT_TO_ENABLE_LOCATION", "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER", "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT", "YOUR_LOCATION_HELPS_OUR_SYSTEM", "WHAT_IS_NAMMA_YATRI_BONUS", "EARNED_ON_APP", "TRAVELLED_ON_APP", "GUARANTEED_FIXED_PRICE", "GET_READY_FOR_YS_SUBSCRIPTION"],
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
      "MERCHANT_NAME" : appName.includes("namma") ? "Namma Yatri" : "Mana Yatri",
      "NEED_IT_TO_ENABLE_LOCATION": "Namma Yatri Partner collect location data to enable share your location to monitor driver current location, even when the app is closed or not in use.",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "Currently, We aren't operating with this vehicle registration number",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "You are about to place a call to the Namma Yatri Support Team. Do you want to proceed?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "Your location helps our system to map down all the near by autos and get you the quickest ride possible.",
      "WHAT_IS_NAMMA_YATRI_BONUS" : "What is Namma Yatri Bonus?",
      "EARNED_ON_APP" : "Earned on NY",
      "TRAVELLED_ON_APP" : "Travelled On Namma Yatri",
      "GUARANTEED_FIXED_PRICE" : "Guaranteed fixed price until \nApr 1, 2025",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "Get ready for\n Yatri Plans!"
    },
    "hindiStrings": {
      "MERCHANT_NAME" : appName.includes("namma") ? "नम्मा यात्री" : "मना यात्री",
      "NEED_IT_TO_ENABLE_LOCATION": "नम्मा यात्री पार्टनर ड्राइवर के लोकेशन की निगरानी के लिए अपना स्थान साझा करने के लिए लोकेशन डेटा एकत्र करता है, तब भी जब ऐप बंद हो या उपयोग में न हो।",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "वर्तमान में, हम इन वाहन पंजीकरण संख्या के साथ नहीं चल रहे हैं",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "आप नम्मा यात्री सपोर्ट टीम को कॉल करने वाले हैं। क्या आप आगे बढ़ना चाहते हैं?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "आपका स्थान हमारे सिस्टम को आस पास के सभी ऑटो को मैप करने में सहायता कर्ता है और आपको सबसे तेज सवारी प्रदान करता है",
      "WHAT_IS_NAMMA_YATRI_BONUS" : "नम्मा यात्री बोनस क्या है?",
      "EARNED_ON_APP" : "NY पर अर्जित मूल्य",
      "TRAVELLED_ON_APP" : "नम्मा यात्री पर तय की गई दूरी",
      "GUARANTEED_FIXED_PRICE" : "अप्रैल 1, 2025 तक निश्चित मूल्य गारंटी",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "नम्मा यात्री योजनाओं के लिए\nतैयार हो जाइए!"
    },
    "kannadaStrings": {
      "MERCHANT_NAME" : appName.includes("namma") ? "ನಮ್ಮ ಯಾತ್ರಿ" : "ಮನ ಯಾತ್ರಿ",
      "NEED_IT_TO_ENABLE_LOCATION": "ಅಪ್ಲಿಕೇಶನ್ ಮುಚ್ಚಿದಾಗ ಅಥವಾ ಬಳಕೆಯಲ್ಲಿಲ್ಲದಿದ್ದರೂ ಸಹ ಚಾಲಕ ಪ್ರಸ್ತುತ ಸ್ಥಳವನ್ನು ಮೇಲ್ವಿಚಾರಣೆ ಮಾಡಲು ನಿಮ್ಮ ಸ್ಥಳವನ್ನು ಹಂಚಿಕೊಳ್ಳಲು ಸಕ್ರಿಯಗೊಳಿಸಲು ನಮ್ಮ ಯಾತ್ರಿ ಪಾಲುದಾರರು ಸ್ಥಳ ಡೇಟಾವನ್ನು ಸಂಗ್ರಹಿಸುತ್ತಾರೆ.",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "ಪ್ರಸ್ತುತ, ನಾವು ಈ ವಾಹನ ನೋಂದಣಿ ಸಂಖ್ಯೆಯೊಂದಿಗೆ ಕಾರ್ಯನಿರ್ವಹಿಸುತ್ತಿಲ್ಲ",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "ನೀವು ನಮ್ಮ ಯಾತ್ರಿ ಬೆಂಬಲ ತಂಡಕ್ಕೆ ಕರೆ ಮಾಡಲಿರುವಿರಿ. ನೀವು ಮುಂದುವರೆಯಲು ಬಯಸುವಿರಾ?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "ನಿಮ್ಮ ಸ್ಥಳವು ನಮ್ಮ ಸಿಸ್ಟಂ ಅನ್ನು ಆಟೋಗಳ ಮೂಲಕ ನಕ್ಷೆ ಮಾಡಲು ಸಹಾಯ ಮಾಡುತ್ತದೆ ಮತ್ತು ನಿಮಗೆ ಸಾಧ್ಯವಿರುವ ವೇಗದ ಸವಾರಿಯನ್ನು ನೀಡುತ್ತದೆ.",
      "WHAT_IS_NAMMA_YATRI_BONUS" : "ನಮ್ಮ ಯಾತ್ರಿ ಬೋನಸ್ ಎಂದರೇನು?",
      "EARNED_ON_APP" : "NY ನಲ್ಲಿ ಗಳಿಸಿದ ಬೆಲೆ",
      "TRAVELLED_ON_APP" : "ನಮ್ಮ ಯಾತ್ರಿಯಲ್ಲಿ ಪ್ರಯಾಣಿಸಿದೆ",
      "GUARANTEED_FIXED_PRICE" : "ಅಪ್ರೈಲ್ 1, 2025 ವರೆಗೆ ಖಚಿತ ಬೆಲೆಯ ಗ್ಯಾರಂಟಿ",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "ನಮ್ಮ ಯಾತ್ರಿ ಯೋಜನೆಗಳಿಗೆ\nಸಿದ್ಧತೆ ಪಡೆಯಲು ಸಿದ್ಧವಾಗಿರಿ!"
    },
    "tamilStrings": {
      "MERCHANT_NAME" : appName.includes("namma") ? "நம்ம யாத்ரி" : "மன யாத்ரி",
      "NEED_IT_TO_ENABLE_LOCATION": "பயன்பாடு மூடப்பட்டிருந்தாலும் கூட, இயக்கி தற்போதைய இருப்பிடத்தை கண்காணிக்க உங்கள் இருப்பிடத்தைப் பகிர்வதற்கு நம்மா யாத்ரி கூட்டாளர் இருப்பிடத் தரவை சேகரிக்கவும்",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "தற்போது நாங்கள் இந்த வாகன பதிவு எண்களுடன் செயல்படவில்லை",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "நீங்கள் நம்ம யாத்ரி ஆதரவுக் குழுவிற்கு அழைப்பு விடுக்க உள்ளீர்கள். தொடர வேண்டுமா?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "உங்கள் இருப்பிடம் எங்கள் கணினியை ஆட்டோக்கள் மூலம் அருகிலுள்ள அனைத்து வரைபடத்தையும், விரைவாக சவாரி செய்ய உதவுகிறது.",
      "WHAT_IS_NAMMA_YATRI_BONUS" : "நம்ம யாத்ரி போனஸ் என்றால் என்ன?",
      "EARNED_ON_APP" : "NY இல் பெறப்பட்ட விலை",
      "TRAVELLED_ON_APP" : "நம்ம யாத்ரியில் நடந்த தூரம்",
      "GUARANTEED_FIXED_PRICE" : "ஏப்ரல் 1, 2025 வரை உத்தரவாதமான நிலையான விலை",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "நம்ம யாத்ரி திட்டங்களுக்கு தயாராகுங்கள்!"
    },
    "teluguStrings": {
      "MERCHANT_NAME" : appName.includes("namma") ? "నమ్మ యాత్రి" : "Mana yātri",
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
    , "acExplanation" : true
    , "rcLimit" : 3
    , "rateCardScreen" :{
        "showYoutubeVideo" : false,
        "showRateCard" : true
      }
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
            "offerBannerValidTill" : "2025-01-01T00:00:00",
            "offerBannerDeadline" : "January 1-*$*-ಜನವರಿ 1-*$*-1 जनवरी-*$*-ஜனவரி 1",
            "offerBannerPlans" : ["a35ffc7c-de0d-4dcc-83a8-e36a5a29cc1d"],
          },
          "highDueWarningLimit" : 75.0,
          "moveDriverToOfflineInHighDueDaily" : true,
          "enableSubscriptionPopups" : true,
          "faqLink" : "https://nammayatri.in/plans/",
          "supportNumber" : "08069724919",
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
    , "referral": {
      "type" : "LeaderBoard"
      , "link" : "https://nammayatri.in/link/rider/mvnw"
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
    }
    , "purpleRideConfig" : {
      "showPurpleVideos" : false,
      "visualImpairmentVideo" : "https://www.youtube.com/watch?v=2qYXl03N6Jg",
      "physicalImpairmentVideo" : "https://www.youtube.com/watch?v=B0C6SZTQO6k",
      "hearingImpairmentVideo" : "https://www.youtube.com/watch?v=udkWOt0serg",
      "genericAccessibilityVideo" : "https://youtu.be/5s21p2rI58c"
    }
    , "otpRegex" :  "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
    , "termsLink" : "https://docs.google.com/document/d/1WkiqYIIk7wxVlis2ArlarVM6Dszb2z4G2qAePlKBEn8"
    , "termsVersion" : 3.0
    , "privacyLink" : "https://docs.google.com/document/d/1N3ZJVwilWO9cIGFJXgl8h8au7lpdRyhXNMZJ8OXq4YY/edit?usp=sharing"
    , "feature" : {
      "enableBonus" : true
      , "enableImageUpload" : false
      , "enableGender" : true
      , "enableOtpRide" : false
      , "enableYatriCoins" : true
    }
    , "appData" : {
      "link" : "https://play.google.com/store/apps/details?id=in.juspay.nammayatripartner"
      , "name" : "Namma Yatri"
    }
    , "enableMockLocation" : true
    , "cityConfig" : [
      {
        "cityName" : "Bangalore",
        "mapImage" : "ny_ic_bangalore_map",
        "cityCode" : "std:080",
        "showSubscriptions" : true,
        "enableAdvancedBooking" : true,
        "enableGullak": true,
        "advancedRidePopUpYoutubeLink" : "" ,
        "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 12.971599,
        "cityLong" : 77.594566,
        "supportNumber" : "08069724919",
        "languageKey" : "KN_IN",
        "showScheduledRides" : true,
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
          "variantList": ["AutoCategory", "CarCategory"],
          "enableCabsSubscriptionView" : true,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
            "domain" : "https://nammayatri.in"
          , "customerAppId" : "in.juspay.nammayatri"
          , "driverAppId" : "in.juspay.nammayatripartner"
        },
        "waitingCharges" : 1.50,
        "waitingChargesConfig" : defWaitingChargesConfig,
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : { "showLearnMore" : true, "learnMoreVideoLink" : "https://www.youtube.com/shorts/NUTNKPzslpw" },
        "gstPercentage" : "18",
        "assets" :{
          "auto_image" : "ic_auto_rickshaw",
          "onboarding_auto_image" : "ny_ic_auto_side",
          "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
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
            "showVideo" : true,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=ZnA9uM8pLPc"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=SZ3NtePrLpM"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=ZJ-aRfkxYtI"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          },
          "purpleRideConfigForBikes" : {
            "vehicleVariant" : "Bike",
            "showVideo" : false,
            "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
            "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
          }
        },
        "rideStartAudio" : {
          "acCab" : {
            "tollAudio" : "https://assets.moving.tech/beckn/audios/toll_charges_background/kn.mp3",
            "acAudio" : "https://assets.moving.tech/beckn/audios/ac_background/kn.mp3",
          },
          "nonAcCab" : {
            "tollAudio" : "https://assets.moving.tech/beckn/audios/toll_charges_background/kn.mp3",
            "acAudio" : "https://assets.moving.tech/beckn/audios/non_ac_background/kn.mp3"
          },

          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName": "Thrissur",
        "mapImage": "ny_ic_thrissur_map",
        "cityCode": "std:0487",
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "",
        "callDriverInfoPost": false,
        "showSubscriptions": false,
        "cityLat": 9.931233,
        "cityLong": 76.267303,
        "supportNumber": "08069724930",
        "languageKey": "ML_IN",
        "showScheduledRides" : false,
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
        , "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Nalgonda",
        "mapImage" : "ny_ic_location_map",
        "cityCode" : "std:08682",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
        "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 17.055959610050486,
        "cityLong" : 79.2698763801053,
        "supportNumber" : "08069724900",
        "languageKey" : "TE_IN",
        "showScheduledRides" : false,
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
            "freeSeconds" : 180,
            "perMinCharges" : 2.0
          },
          "auto" : {
            "freeSeconds" : 300,
            "perMinCharges" : 2.0
          },
          "bike" : {
            "freeSeconds" : 180,
            "perMinCharges" : 2.0
          },
          "ambulance" : {
            "freeSeconds" : 480,
            "perMinCharges" : 2.0
          }
        },
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : defRateCardConfig,
        "gstPercentage" :  "18",       
        "assets" :{
          "auto_image" :  "ny_ic_black_yellow_auto_side_view",
          "onboarding_auto_image" : "ny_ic_auto_right_side_yellow",
          "empty_referral_auto" : "ny_ic_refer_now_auto_my,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_my.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_my,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_my.png"
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
        },
        "rideStartAudio" : {
          "acCab" : {
            "parkingAudio" : "https://assets.moving.tech/beckn/audios/parking_charges_background/te.mp3",
          },
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Hyderabad",
        "mapImage" : "ny_ic_hyderabad_map",
        "cityCode" : "std:040",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 17.402113,
        "cityLong" : 78.499827,
        "supportNumber" : "+918069724900",
        "languageKey" : "TE_IN",
        "showScheduledRides" : false,
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
            "freeSeconds" : 180,
            "perMinCharges" : 2.0
          },
          "auto" : {
            "freeSeconds" : 300,
            "perMinCharges" : 2.0
          },
          "bike" : {
            "freeSeconds" : 180,
            "perMinCharges" : 2.0
          },
          "ambulance" : {
            "freeSeconds" : 480,
            "perMinCharges" : 2.0
          }
        },
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : defRateCardConfig,
        "gstPercentage" :  "18",       
        "assets" :{
          "auto_image" :  "ny_ic_black_yellow_auto_side_view",
          "onboarding_auto_image" : "ny_ic_auto_right_side_yellow",
          "empty_referral_auto" : "ny_ic_refer_now_auto_my,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_my.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_my,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_my.png"
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
        },
        "rideStartAudio" : {
          "acCab" : {
            "parkingAudio" : "https://assets.moving.tech/beckn/audios/parking_charges_background/te.mp3",
          },
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Warangal",
        "mapImage" : "ny_ic_warangal_map",
        "cityCode" : "std:0870",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
        "callDriverInfoPost": false, 
        "cityLat" : 17.967432,
        "cityLong" : 79.588875,
        "supportNumber" : "+918069724900",
        "languageKey" : "TE_IN",
        "showScheduledRides" : false,
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
            "freeSeconds" : 180,
            "perMinCharges" : 2.0
          },
          "auto" : {
            "freeSeconds" : 300,
            "perMinCharges" : 2.0
          },
          "bike" : {
            "freeSeconds" : 180,
            "perMinCharges" : 2.0
          },
          "ambulance" : {
            "freeSeconds" : 480,
            "perMinCharges" : 2.0
          }
        },
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : defRateCardConfig,
        "gstPercentage" :  "18",       
        "assets" :{
          "auto_image" :  "ny_ic_black_yellow_auto_side_view",
          "onboarding_auto_image" : "ny_ic_auto_right_side_yellow",
          "empty_referral_auto" : "ny_ic_refer_now_auto_my,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_my.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_my,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_my.png"
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
        },
        "rideStartAudio" : {
          "acCab" : {
            "parkingAudio" : "https://assets.moving.tech/beckn/audios/parking_charges_background/te.mp3",
          },
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },{
        "cityName" : "Tirupati",
        "mapImage" : "ny_ic_location_map",
        "cityCode" : "std:0877",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
        "callDriverInfoPost": false, 
        "cityLat" : 13.620298,
        "cityLong" : 79.4323363,
        "supportNumber" : "+918069724900",
        "languageKey" : "TE_IN",
        "showScheduledRides" : false,
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
            "freeSeconds" : 180,
            "perMinCharges" : 2.0
          },
          "auto" : {
            "freeSeconds" : 300,
            "perMinCharges" : 2.0
          },
          "bike" : {
            "freeSeconds" : 180,
            "perMinCharges" : 2.0
          },
          "ambulance" : {
            "freeSeconds" : 480,
            "perMinCharges" : 2.0
          }
        },
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : defRateCardConfig,
        "gstPercentage" :  "18",       
        "assets" :{
          "auto_image" :  "ny_ic_black_yellow_auto_side_view",
          "onboarding_auto_image" : "ny_ic_auto_right_side_yellow",
          "empty_referral_auto" : "ny_ic_refer_now_auto_my,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_my.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_my,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_my.png"
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
        },
        "rideStartAudio" : {
          "acCab" : {
            "parkingAudio" : "https://assets.moving.tech/beckn/audios/parking_charges_background/te.mp3",
          },
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Mysore",
        "mapImage" : "ny_ic_mysuru_map",
        "cityCode" : "std:0821",
        "showSubscriptions" : true,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 12.295810,
        "cityLong" : 76.639381,
        "supportNumber" : "08069724919",
        "languageKey" : "KN_IN",
        "showScheduledRides" : false,
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
          "variantList" : ["AutoCategory", "CarCategory"],
          "enableCabsSubscriptionView" : true,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
          "domain" : "https://nammayatri.in"
        , "customerAppId" : "in.juspay.nammayatri"
        , "driverAppId" : "in.juspay.nammayatripartner"
      },
      "waitingCharges" : 1.50,
      "waitingChargesConfig" : defWaitingChargesConfig,
      "rentalWaitingChargesConfig" :{
        "cab" : {
          "freeSeconds" : 180
        , "perMinCharges" : 2.0
        },
        "auto" : {
          "freeSeconds" : 180
        , "perMinCharges" : 1.5
        },
        "bike" : {
          "freeSeconds" : 180
        , "perMinCharges" : 2.0
        },
        "ambulance" : {
          "freeSeconds" : 480,
          "perMinCharges" : 2.0
        }
      },
      "rateCardConfig" : { "showLearnMore" : true, "learnMoreVideoLink" : "https://www.youtube.com/shorts/NUTNKPzslpw" },
      "gstPercentage" : "18",
      "assets" :{
        "auto_image" : "ic_auto_rickshaw",
        "onboarding_auto_image" : "ny_ic_auto_side",
        "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
        "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
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
          "showVideo" : true,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=ZnA9uM8pLPc"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=SZ3NtePrLpM"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=ZJ-aRfkxYtI"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForBikes" : {
          "vehicleVariant" : "Bike",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        }
      },
      "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Delhi",
        "mapImage" : "ny_ic_delhi_map",
        "cityCode" : "std:011",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 28.618978,
        "cityLong" : 77.207795,
        "supportNumber" : "+918069724848",
        "languageKey" : "HI_IN",
        "showScheduledRides" : false,
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
          "domain" : "https://nammayatri.in"
        , "customerAppId" : "in.juspay.nammayatri"
        , "driverAppId" : "in.juspay.nammayatripartner"
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
        },
        "ambulance" : {
          "freeSeconds" : 480,
          "perMinCharges" : 2.0
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
      },
      "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Chennai",
        "mapImage" : "ny_ic_chennai_map",
        "cityCode" : "std:044",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 13.067439,
        "cityLong" : 80.237617,
        "supportNumber" : "08069724899",
        "languageKey" : "TA_IN",
        "showScheduledRides" : false,
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
          "variantList": ["AutoCategory", "CarCategory"],
          "enableCabsSubscriptionView" : true,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
          "domain" : "https://nammayatri.in"
        , "customerAppId" : "in.juspay.nammayatri"
        , "driverAppId" : "in.juspay.nammayatripartner"
      },
      "waitingCharges" : 1.00,
      "waitingChargesConfig" : {
        "cab" : {
          "freeSeconds" : 300,
          "perMinCharges" : 1.0
        },
        "auto" : {
          "freeSeconds" : 180,
          "perMinCharges" : 1.0
        },
        "bike" : {
          "freeSeconds" : 300,
          "perMinCharges" : 1.0
        },
        "ambulance" : {
          "freeSeconds" : 480,
          "perMinCharges" : 2.0
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
      },
      "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Coimbatore",
        "mapImage" : "ny_ic_coimbatore_map",
        "cityCode" : "std:0422",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 11.023481,
        "cityLong" : 76.966429,
        "supportNumber" : "",
        "languageKey" : "TA_IN",
        "showScheduledRides" : false,
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
          "domain" : "https://nammayatri.in"
        , "customerAppId" : "in.juspay.nammayatri"
        , "driverAppId" : "in.juspay.nammayatripartner"
      },
      "waitingCharges" : 1.50,
      "waitingChargesConfig" : defWaitingChargesConfig,
      "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
      "rateCardConfig" : defRateCardConfig,
      "gstPercentage" :  "18",      
      "assets" :{
        "auto_image" :  "ny_ic_black_yellow_auto_side_view",
        "onboarding_auto_image" : "ny_ic_auto_right_side_yellow",
        "empty_referral_auto" : "ny_ic_refer_now_auto_ny_yellow,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_yellow.png",
        "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
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
      },
      "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Pondicherry",
        "mapImage" : "ny_ic_puducherry_map",
        "cityCode" : "std:0413",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
        "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 11.943852,
        "cityLong" : 79.808292,
        "supportNumber" : "08069724899",
        "languageKey" : "TA_IN",
        "showScheduledRides" : false,
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
          "domain" : "https://nammayatri.in"
        , "customerAppId" : "in.juspay.nammayatri"
        , "driverAppId" : "in.juspay.nammayatripartner"
      },
      "waitingCharges" : 1.50,
      "waitingChargesConfig" : defWaitingChargesConfig,
      "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
      "rateCardConfig" : defRateCardConfig,
      "gstPercentage" :  "18",      
      "assets" :{
        "auto_image" :  "ny_ic_black_yellow_auto_side_view",
        "onboarding_auto_image" : "ny_ic_auto_right_side_yellow",
        "empty_referral_auto" : "ny_ic_refer_now_auto_ny_yellow,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_yellow.png",
        "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
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
      },
      "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Tumakuru",
        "mapImage" : "ny_ic_tumakuru_map",
        "cityCode" : "std:0816",
        "showSubscriptions" : true,
        "enableAdvancedBooking" : false,
        "enableGullak": true,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 15.32383804957557,
        "cityLong" : 75.88071672412116,
        "supportNumber" : "08069724919",
        "languageKey" : "KN_IN",
        "showScheduledRides" : false,
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
          "variantList" : ["AutoCategory", "CarCategory"],
          "enableCabsSubscriptionView" : true,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
          "domain" : "https://nammayatri.in"
        , "customerAppId" : "in.juspay.nammayatri"
        , "driverAppId" : "in.juspay.nammayatripartner"
      },
      "waitingCharges" : 1.50,
      "waitingChargesConfig" : defWaitingChargesConfig,
      "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
      "rateCardConfig" : { "showLearnMore" : true, "learnMoreVideoLink" : "https://www.youtube.com/shorts/NUTNKPzslpw" },
      "gstPercentage" : "18",
      "assets" :{
        "auto_image" : "ic_auto_rickshaw",
        "onboarding_auto_image" : "ny_ic_auto_side",
        "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
        "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
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
          "showVideo" : true,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=ZnA9uM8pLPc"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=SZ3NtePrLpM"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=ZJ-aRfkxYtI"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        },
        "purpleRideConfigForBikes" : {
          "vehicleVariant" : "Bike",
          "showVideo" : false,
          "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
          "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
        }
      },
      "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Gurugram",
        "mapImage" : "ny_ic_gurugram_map",
        "cityCode" : "std:0124",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 28.457523,
        "cityLong" : 77.026344,
        "supportNumber" : "+918069724848",
        "languageKey" : "HI_IN",
        "showScheduledRides" : false,
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
          "domain" : "https://nammayatri.in"
        , "customerAppId" : "in.juspay.nammayatri"
        , "driverAppId" : "in.juspay.nammayatripartner"
      },
      "waitingCharges" : 1.50,
      "waitingChargesConfig" : defWaitingChargesConfig,
      "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
      "rateCardConfig" : defRateCardConfig,
      "gstPercentage" :  "18",     
      "assets" :{
        "auto_image" : "ic_auto_rickshaw",
        "onboarding_auto_image" : "ny_ic_auto_side",
        "empty_referral_auto" : "ny_ic_refer_now_auto_yatri_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_yatri_green.png",
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
      },
      "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Noida",
        "mapImage" : "ny_ic_noida_map",
        "cityCode" : "std:01189",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 28.535517,
        "cityLong" : 77.391029,
        "supportNumber" : "+918069724848",
        "languageKey" : "HI_IN",
        "showScheduledRides" : false,
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
          "domain" : "https://nammayatri.in"
        , "customerAppId" : "in.juspay.nammayatri"
        , "driverAppId" : "in.juspay.nammayatripartner"
      },
      "waitingCharges" : 1.50,
      "waitingChargesConfig" : defWaitingChargesConfig,
      "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
      "rateCardConfig" : defRateCardConfig,
      "gstPercentage" :  "18",      
      "assets" :{
        "auto_image" : "ic_auto_rickshaw",
        "onboarding_auto_image" : "ny_ic_auto_side",
        "empty_referral_auto" : "ny_ic_refer_now_auto_yatri_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_yatri_green.png",
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
      },
      "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "TamilNaduCities",
        "mapImage" : "ny_ic_tamilnadu_map",
        "cityCode" :  "std:0422",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 11.1271,
        "cityLong" : 78.6569,
        "supportNumber" : "08069724899",
        "languageKey" : "TA_IN",
        "showScheduledRides" : false,
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
          "domain" : "https://nammayatri.in"
        , "customerAppId" : "in.juspay.nammayatri"
        , "driverAppId" : "in.juspay.nammayatripartner"
      },
      "waitingCharges" : 1.50,
      "waitingChargesConfig" : defWaitingChargesConfig,
      "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
      "rateCardConfig" : defRateCardConfig,
      "gstPercentage" :  "18",      
      "assets" :{
        "auto_image" :  "ny_ic_black_yellow_auto_side_view",
        "onboarding_auto_image" : "ny_ic_auto_right_side_yellow",
        "empty_referral_auto" : "ny_ic_refer_now_auto_ny_yellow,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_yellow.png",
        "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
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
      },
      "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Minneapolis",
        "mapImage" : "ny_ic_minneapolis_map",
        "cityCode" : "std:01189",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 44.977753,
        "cityLong" : -93.2650108,
        "supportNumber" : "+918069724848",
        "vehicleNSImg" : "ny_ic_auto_image_old",
        "languageKey" : "EN_US",
        "showScheduledRides" : false,
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
        "showEarningSection" : false,
        "referral" : {
          "domain" : "https://nammayatri.in"
        , "customerAppId" : "in.juspay.nammayatri"
        , "driverAppId" : "in.juspay.nammayatripartner"
      },
      "waitingCharges" : 1.50,
      "waitingChargesConfig" : defWaitingChargesConfig,
      "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
      "rateCardConfig" : defRateCardConfig,
      "gstPercentage" :  "18",     
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
      },
      "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Kochi",
        "mapImage" : "ny_ic_kochi_map",
        "cityCode" : "std:0484",
        "showSubscriptions" : true,
        "enableAdvancedBooking": false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink": "",
        "callDriverInfoPost": false, 
        "cityLat" : 9.931233,
        "cityLong" : 76.267303,
        "supportNumber" : "08069724930",
        "languageKey" : "ML_IN",
        "showScheduledRides" : false,
        "showDriverReferral" : true,
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
          "variantList" : [],
          "enableCabsSubscriptionView" : true,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
          "domain" : "https://nammayatri.in"
          , "customerAppId" : "in.juspay.nammayatri"
          , "driverAppId" : "in.juspay.nammayatripartner"
      },
      "waitingCharges" : 1.50,
      "waitingChargesConfig" : defWaitingChargesConfig,
      "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
      "rateCardConfig": defRateCardConfig,
      "gstPercentage" : "18",
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
      },
      "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Trichy",
        "mapImage" : "ny_ic_tamilnadu_map",
        "cityCode" : "std:0431",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 10.80942,
        "cityLong" : 78.693486,
        "supportNumber" : "08069724899",
        "languageKey" : "TA_IN",
        "showScheduledRides" : false,
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
          "domain" : "https://nammayatri.in"
        , "customerAppId" : "in.juspay.nammayatri"
        , "driverAppId" : "in.juspay.nammayatripartner"
      },
      "waitingCharges" : 1.00,
      "waitingChargesConfig" : {
        "cab" : {
          "freeSeconds" : 300,
          "perMinCharges" : 1.0
        },
        "auto" : {
          "freeSeconds" : 180,
          "perMinCharges" : 1.0
        },
        "bike" : {
          "freeSeconds" : 300,
          "perMinCharges" : 1.0
        },
        "ambulance" : {
          "freeSeconds" : 480,
          "perMinCharges" : 2.0
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
      },
      "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Thanjavur",
        "mapImage" : "ny_ic_tamilnadu_map",
        "cityCode" : "std:04362",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 10.778442,
        "cityLong" : 79.133848,
        "supportNumber" : "08069724899",
        "languageKey" : "TA_IN",
        "showScheduledRides" : false,
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
          "domain" : "https://nammayatri.in"
        , "customerAppId" : "in.juspay.nammayatri"
        , "driverAppId" : "in.juspay.nammayatripartner"
      },
      "waitingCharges" : 1.00,
      "waitingChargesConfig" : {
        "cab" : {
          "freeSeconds" : 300,
          "perMinCharges" : 1.0
        },
        "auto" : {
          "freeSeconds" : 180,
          "perMinCharges" : 1.0
        },
        "bike" : {
          "freeSeconds" : 300,
          "perMinCharges" : 1.0
        },
        "ambulance" : {
          "freeSeconds" : 480,
          "perMinCharges" : 2.0
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
      },
      "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Salem",
        "mapImage" : "ny_ic_tamilnadu_map",
        "cityCode" : "std:0427",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 11.656077,
        "cityLong" : 78.14412,
        "supportNumber" : "08069724899",
        "languageKey" : "TA_IN",
        "showScheduledRides" : false,
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
          "domain" : "https://nammayatri.in"
        , "customerAppId" : "in.juspay.nammayatri"
        , "driverAppId" : "in.juspay.nammayatripartner"
      },
      "waitingCharges" : 1.00,
      "waitingChargesConfig" : {
        "cab" : {
          "freeSeconds" : 300,
          "perMinCharges" : 1.0
        },
        "auto" : {
          "freeSeconds" : 180,
          "perMinCharges" : 1.0
        },
        "bike" : {
          "freeSeconds" : 300,
          "perMinCharges" : 1.0
        },
        "ambulance" : {
          "freeSeconds" : 480,
          "perMinCharges" : 2.0
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
      },
      "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Tirunelveli",
        "mapImage" : "ny_ic_tamilnadu_map",
        "cityCode" : "std:0462",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 8.723441,
        "cityLong" : 77.740959,
        "supportNumber" : "08069724899",
        "languageKey" : "TA_IN",
        "showScheduledRides" : false,
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
          "domain" : "https://nammayatri.in"
        , "customerAppId" : "in.juspay.nammayatri"
        , "driverAppId" : "in.juspay.nammayatripartner"
      },
      "waitingCharges" : 1.00,
      "waitingChargesConfig" : {
        "cab" : {
          "freeSeconds" : 300,
          "perMinCharges" : 1.0
        },
        "auto" : {
          "freeSeconds" : 180,
          "perMinCharges" : 1.0
        },
        "bike" : {
          "freeSeconds" : 300,
          "perMinCharges" : 1.0
        },
        "ambulance" : {
          "freeSeconds" : 480,
          "perMinCharges" : 2.0
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
      },
      "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Hosur",
        "mapImage" : "ny_ic_tamilnadu_map",
        "cityCode" : "std:04344",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 12.738274,
        "cityLong" : 77.82293,
        "supportNumber" : "08069724899",
        "languageKey" : "TA_IN",
        "showScheduledRides" : false,
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
          "domain" : "https://nammayatri.in"
        , "customerAppId" : "in.juspay.nammayatri"
        , "driverAppId" : "in.juspay.nammayatripartner"
      },
      "waitingCharges" : 1.00,
      "waitingChargesConfig" : {
        "cab" : {
          "freeSeconds" : 300,
          "perMinCharges" : 1.0
        },
        "auto" : {
          "freeSeconds" : 180,
          "perMinCharges" : 1.0
        },
        "bike" : {
          "freeSeconds" : 300,
          "perMinCharges" : 1.0
        },
        "ambulance" : {
          "freeSeconds" : 480,
          "perMinCharges" : 2.0
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
      },
      "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Madurai",
        "mapImage" : "ny_ic_tamilnadu_map",
        "cityCode" : "std:0452",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 9.920995,
        "cityLong" : 78.121278,
        "supportNumber" : "08069724899",
        "languageKey" : "TA_IN",
        "showScheduledRides" : false,
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
          "domain" : "https://nammayatri.in"
        , "customerAppId" : "in.juspay.nammayatri"
        , "driverAppId" : "in.juspay.nammayatripartner"
      },
      "waitingCharges" : 1.00,
      "waitingChargesConfig" : {
        "cab" : {
          "freeSeconds" : 300,
          "perMinCharges" : 1.0
        },
        "auto" : {
          "freeSeconds" : 180,
          "perMinCharges" : 1.0
        },
        "bike" : {
          "freeSeconds" : 300,
          "perMinCharges" : 1.0
        },
        "ambulance" : {
          "freeSeconds" : 480,
          "perMinCharges" : 2.0
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
      },
      "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Vellore",
        "mapImage" : "ny_ic_tamilnadu_map",
        "cityCode" : "std:0416",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
      "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 12.929078,
        "cityLong" : 79.126577,
        "supportNumber" : "08069724899",
        "languageKey" : "TA_IN",
        "showScheduledRides" : false,
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
          "domain" : "https://nammayatri.in"
        , "customerAppId" : "in.juspay.nammayatri"
        , "driverAppId" : "in.juspay.nammayatripartner"
      },
      "waitingCharges" : 1.00,
      "waitingChargesConfig" : {
        "cab" : {
          "freeSeconds" : 300,
          "perMinCharges" : 1.0
        },
        "auto" : {
          "freeSeconds" : 180,
          "perMinCharges" : 1.0
        },
        "bike" : {
          "freeSeconds" : 300,
          "perMinCharges" : 1.0
        },
        "ambulance" : {
          "freeSeconds" : 480,
          "perMinCharges" : 2.0
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
      },
      "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName": "Trivandrum",
        "mapImage": "ny_ic_trivandrum_map",
        "cityCode": "std:0471",
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "",
        "callDriverInfoPost": false,
        "showSubscriptions": false,
        "cityLat": 9.931233,
        "cityLong": 76.267303,
        "supportNumber": "08069724930",
        "languageKey": "ML_IN",
        "showScheduledRides" : false,
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
        , "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName": "Kozhikode",
        "mapImage": "ny_ic_kochi_map",
        "cityCode": "std:0495",
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "",
        "callDriverInfoPost": false,
        "showSubscriptions": false,
        "cityLat": 9.931233,
        "cityLong": 76.267303,
        "supportNumber": "08069724930",
        "languageKey": "ML_IN",
        "showScheduledRides" : false,
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
        , "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {"cityName" : "Pudukkottai",
        "mapImage" : "ny_ic_tamilnadu_map",
        "cityCode" : "std:04322",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "enableGullak": false,
        "advancedRidePopUpYoutubeLink" : "" ,
        "callDriverInfoPost": false, 
        "cityLat" : 10.38336055,
        "cityLong" : 78.80014621,
        "supportNumber" : "08069724899",
        "languageKey" : "TA_IN",
        "showScheduledRides" : false,
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
          "domain" : "https://nammayatri.in"
        , "customerAppId" : "in.juspay.nammayatri"
        , "driverAppId" : "in.juspay.nammayatripartner"
      },
      "waitingCharges" : 1.00,
      "waitingChargesConfig" : {
        "cab" : {
          "freeSeconds" : 300,
          "perMinCharges" : 1.0
        },
        "auto" : {
          "freeSeconds" : 180,
          "perMinCharges" : 1.0
        },
        "bike" : {
          "freeSeconds" : 300,
          "perMinCharges" : 1.0
        },
        "ambulance" : {
          "freeSeconds" : 480,
          "perMinCharges" : 2.0
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
      },
      "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        }
      },
      {
        "cityName" : "Davanagere",
        "mapImage" : "ny_ic_davanagere_map",
        "cityCode" : "std:08192",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : true,
        "advancedRidePopUpYoutubeLink" : "" ,
        "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 14.4519495,
        "cityLong" : 75.9236604,
        "supportNumber" : "08069724919",
        "languageKey" : "KN_IN",
        "showScheduledRides" : false,
        "showDriverReferral" : false,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "enableGullak": false,
        "vehicleNSImg" : "ny_ic_auto_image",
        "registration" : {
            "supportWAN" : "918618963188",
            "callSupport" : true,
            "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : false,
          "variantList" : [],
          "enableCabsSubscriptionView" : false,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
            "domain" : "https://nammayatri.in"
          , "customerAppId" : "in.juspay.nammayatri"
          , "driverAppId" : "in.juspay.nammayatripartner"
        },
        "waitingCharges" : 1.50,
        "waitingChargesConfig" : defWaitingChargesConfig,
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : { "showLearnMore" : true, "learnMoreVideoLink" : "https://www.youtube.com/shorts/NUTNKPzslpw" },
        "gstPercentage": "18",
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
        },
        "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        },
        "assets" :{
          "auto_image" : "ic_auto_rickshaw",
          "onboarding_auto_image" : "ny_ic_auto_side",
          "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
        }
      },
      {
        "cityName" : "Shivamogga",
        "mapImage" : "ny_ic_shivamogga_map",
        "cityCode" : "std:08182",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : true,
        "advancedRidePopUpYoutubeLink" : "" ,
        "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 13.9342598,
        "cityLong" : 75.5664889,
        "supportNumber" : "08069724919",
        "languageKey" : "KN_IN",
        "showScheduledRides" : false,
        "showDriverReferral" : false,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "enableGullak": false,
        "vehicleNSImg" : "ny_ic_auto_image",
        "registration" : {
            "supportWAN" : "918618963188",
            "callSupport" : true,
            "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : false,
          "variantList" : [],
          "enableCabsSubscriptionView" : false,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
            "domain" : "https://nammayatri.in"
          , "customerAppId" : "in.juspay.nammayatri"
          , "driverAppId" : "in.juspay.nammayatripartner"
        },
        "waitingCharges" : 1.50,
        "waitingChargesConfig" : defWaitingChargesConfig,
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : { "showLearnMore" : true, "learnMoreVideoLink" : "https://www.youtube.com/shorts/NUTNKPzslpw" },
        "gstPercentage": "18",
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
        },
        "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        },
        "assets" :{
          "auto_image" : "ic_auto_rickshaw",
          "onboarding_auto_image" : "ny_ic_auto_side",
          "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
        }
      },
      {
        "cityName" : "Hubli",
        "mapImage" : "ny_ic_hubli_map",
        "cityCode" : "std:0836",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : true,
        "advancedRidePopUpYoutubeLink" : "" ,
        "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 23.68946891656628,
        "cityLong" : 86.96920957618282,
        "supportNumber" : "08069724919",
        "languageKey" : "KN_IN",
        "showScheduledRides" : false,
        "showDriverReferral" : false,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "enableGullak": false,
        "vehicleNSImg" : "ny_ic_auto_image",
        "registration" : {
            "supportWAN" : "918618963188",
            "callSupport" : true,
            "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : false,
          "variantList" : [],
          "enableCabsSubscriptionView" : false,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
            "domain" : "https://nammayatri.in"
          , "customerAppId" : "in.juspay.nammayatri"
          , "driverAppId" : "in.juspay.nammayatripartner"
        },
        "waitingCharges" : 1.50,
        "waitingChargesConfig" : defWaitingChargesConfig,
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : { "showLearnMore" : true, "learnMoreVideoLink" : "https://www.youtube.com/shorts/NUTNKPzslpw" },
        "gstPercentage": "18",
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
        },
        "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        },
        "assets" :{
          "auto_image" : "ic_auto_rickshaw",
          "onboarding_auto_image" : "ny_ic_auto_side",
          "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
        }
      },
      {
        "cityName" : "Paris",
        "mapImage" : "ny_ic_location_map",
        "cityCode": "std:001",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "" ,
        "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 20.2376, 
        "cityLong" : 84.2700,
        "supportNumber" : "08069724919",
        "languageKey" : "KN_IN",
        "showScheduledRides" : false,
        "showDriverReferral" : false,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "enableGullak": false,
        "vehicleNSImg" : "ny_ic_auto_image",
        "registration" : {
            "supportWAN" : "918618963188",
            "callSupport" : true,
            "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : false,
          "variantList" : [],
          "enableCabsSubscriptionView" : false,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
            "domain" : "https://nammayatri.in"
          , "customerAppId" : "in.juspay.nammayatri"
          , "driverAppId" : "in.juspay.nammayatripartner"
        },
        "waitingCharges" : 1.50,
        "waitingChargesConfig" : defWaitingChargesConfig,
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : { "showLearnMore" : true, "learnMoreVideoLink" : "https://www.youtube.com/shorts/NUTNKPzslpw" },
        "gstPercentage": "18",
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
        },
        "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        },
        "assets" :{
          "auto_image" : "ic_auto_rickshaw",
          "onboarding_auto_image" : "ny_ic_auto_side",
          "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
        }
      },
      {
        "cityName" : "Odisha",
        "mapImage" : "ny_ic_location_map",
        "cityCode" : "std:001", // check this once from backend
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "" ,
        "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 20.2376, 
        "cityLong" : 84.2700,
        "supportNumber" : "08069724919",
        "languageKey" : "OD_IN",
        "showScheduledRides" : false,
        "showDriverReferral" : false,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "enableGullak": false,
        "vehicleNSImg" : "ny_ic_auto_image",
        "registration" : {
            "supportWAN" : "918618963188",
            "callSupport" : true,
            "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : false,
          "variantList" : [],
          "enableCabsSubscriptionView" : false,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
            "domain" : "https://odishayatri.in"
          , "customerAppId" : "in.mobility.odishayatri"
          , "driverAppId" : "in.mobility.odishayatripartner"
        },
        "waitingCharges" : 1.50,
        "waitingChargesConfig" : defWaitingChargesConfig,
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : { "showLearnMore" : true, "learnMoreVideoLink" : "https://www.youtube.com/shorts/NUTNKPzslpw" },
        "gstPercentage": "18",
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
        },
        "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        },
        "assets" :{
          "auto_image" : "ic_auto_rickshaw",
          "onboarding_auto_image" : "ny_ic_auto_side",
          "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
        }
      },
      {
        "cityName" : "Bhubaneshwar",
        "mapImage" : "ny_ic_location_map",
        "cityCode" : "std:0674",  // check this from backend
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "" ,
        "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 20.30378209, 
        "cityLong" : 85.82251309,
        "supportNumber" : "08069724914",
        "languageKey" : "OD_IN",
        "showScheduledRides" : false,
        "showDriverReferral" : false,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "enableGullak": false,
        "vehicleNSImg" : "ny_ic_auto_image",
        "registration" : {
            "supportWAN" : "918618963188",
            "callSupport" : true,
            "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : false,
          "variantList" : [],
          "enableCabsSubscriptionView" : false,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
            "domain" : "https://odishayatri.in"
          , "customerAppId" : "in.mobility.odishayatri"
          , "driverAppId" : "in.mobility.odishayatripartner"
        },
        "waitingCharges" : 1.50,
        "waitingChargesConfig" : defWaitingChargesConfig,
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : { "showLearnMore" : true, "learnMoreVideoLink" : "https://www.youtube.com/shorts/NUTNKPzslpw" },
        "gstPercentage": "18",
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
        },
        "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        },
        "assets" :{
          "auto_image" : "ic_auto_rickshaw",
          "onboarding_auto_image" : "ny_ic_auto_side",
          "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
        }
      },
      {
        "cityName" : "Cuttack",
        "mapImage" : "ny_ic_location_map",
        "cityCode" : "std:0671",  // check this from backend
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "" ,
        "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 20.46615822, 
        "cityLong" : 85.88302282,
        "supportNumber" : "08069724914",
        "languageKey" : "OD_IN",
        "showScheduledRides" : false,
        "showDriverReferral" : false,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "enableGullak": false,
        "vehicleNSImg" : "ny_ic_auto_image",
        "registration" : {
            "supportWAN" : "918618963188",
            "callSupport" : true,
            "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : false,
          "variantList" : [],
          "enableCabsSubscriptionView" : false,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
            "domain" : "https://nammayatri.in"
          , "customerAppId" : "in.juspay.nammayatri"
          , "driverAppId" : "in.juspay.nammayatripartner"
        },
        "waitingCharges" : 1.50,
        "waitingChargesConfig" : defWaitingChargesConfig,
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : { "showLearnMore" : true, "learnMoreVideoLink" : "https://www.youtube.com/shorts/NUTNKPzslpw" },
        "gstPercentage": "18",
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
        },
        "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        },
        "assets" :{
          "auto_image" : "ic_auto_rickshaw",
          "onboarding_auto_image" : "ny_ic_auto_side",
          "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
        }
      },
      {
        "cityName" : "Puri",
        "mapImage" : "ny_ic_location_map",
        "cityCode" : "std:06752",  // check this from backend
        "showSubscriptions" : false,
        "enableAdvancedBooking" : false,
        "advancedRidePopUpYoutubeLink" : "" ,
        "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 19.80495793, 
        "cityLong" : 85.82209410,
        "supportNumber" : "08069724914",
        "languageKey" : "KN_IN",
        "showScheduledRides" : false,
        "showDriverReferral" : false,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "enableGullak": false,
        "vehicleNSImg" : "ny_ic_auto_image",
        "registration" : {
            "supportWAN" : "918618963188",
            "callSupport" : true,
            "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : false,
          "variantList" : [],
          "enableCabsSubscriptionView" : false,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
            "domain" : "https://nammayatri.in"
          , "customerAppId" : "in.juspay.nammayatri"
          , "driverAppId" : "in.juspay.nammayatripartner"
        },
        "waitingCharges" : 1.50,
        "waitingChargesConfig" : defWaitingChargesConfig,
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : { "showLearnMore" : true, "learnMoreVideoLink" : "https://www.youtube.com/shorts/NUTNKPzslpw" },
        "gstPercentage": "18",
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
        },
        "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        },
        "assets" :{
          "auto_image" : "ic_auto_rickshaw",
          "onboarding_auto_image" : "ny_ic_auto_side",
          "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
        }
      },
      
      {
        "cityName" : "Bidar",
        "mapImage" : "ny_ic_location_map",
        "cityCode" : "std:8482",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : true,
        "advancedRidePopUpYoutubeLink" : "" ,
        "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 17.909688776221195,
        "cityLong" : 77.51956855369727,
        "supportNumber" : "08069724919",
        "languageKey" : "KN_IN",
        "showScheduledRides" : false,
        "showDriverReferral" : false,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "enableGullak": false,
        "vehicleNSImg" : "ny_ic_auto_image",
        "registration" : {
            "supportWAN" : "918618963188",
            "callSupport" : true,
            "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : false,
          "variantList" : [],
          "enableCabsSubscriptionView" : false,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
            "domain" : "https://nammayatri.in"
          , "customerAppId" : "in.juspay.nammayatri"
          , "driverAppId" : "in.juspay.nammayatripartner"
        },
        "waitingCharges" : 1.50,
        "waitingChargesConfig" : defWaitingChargesConfig,
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : { "showLearnMore" : true, "learnMoreVideoLink" : "https://www.youtube.com/shorts/NUTNKPzslpw" },
        "gstPercentage": "18",
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
        },
        "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        },
        "assets" :{
          "auto_image" : "ic_auto_rickshaw",
          "onboarding_auto_image" : "ny_ic_auto_side",
          "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
        }
      },
      {
        "cityName" : "Mangalore",
        "mapImage" : "ny_ic_mangalore_map",
        "cityCode" : "std:0824",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : true,
        "advancedRidePopUpYoutubeLink" : "" ,
        "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 12.9139747,
        "cityLong" : 74.8561126,
        "supportNumber" : "08069724919",
        "languageKey" : "KN_IN",
        "showScheduledRides" : false,
        "showDriverReferral" : false,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "enableGullak": false,
        "vehicleNSImg" : "ny_ic_auto_image",
        "registration" : {
            "supportWAN" : "918618963188",
            "callSupport" : true,
            "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : false,
          "variantList" : [],
          "enableCabsSubscriptionView" : false,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
            "domain" : "https://nammayatri.in"
          , "customerAppId" : "in.juspay.nammayatri"
          , "driverAppId" : "in.juspay.nammayatripartner"
        },
        "waitingCharges" : 1.50,
        "waitingChargesConfig" : defWaitingChargesConfig,
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : { "showLearnMore" : true, "learnMoreVideoLink" : "https://www.youtube.com/shorts/NUTNKPzslpw" },
        "gstPercentage": "18",
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
        },
        "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        },
        "assets" :{
          "auto_image" : "ic_auto_rickshaw",
          "onboarding_auto_image" : "ny_ic_auto_side",
          "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
        }
      },
      {
        "cityName" : "Gulbarga",
        "mapImage" : "ny_ic_gulbarga_map",
        "cityCode" : "std:08472",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : true,
        "advancedRidePopUpYoutubeLink" : "" ,
        "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 17.3237322,
        "cityLong" : 76.8354076,
        "supportNumber" : "08069724919",
        "languageKey" : "KN_IN",
        "showScheduledRides" : false,
        "showDriverReferral" : false,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "enableGullak": false,
        "vehicleNSImg" : "ny_ic_auto_image",
        "registration" : {
            "supportWAN" : "918618963188",
            "callSupport" : true,
            "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : true,
          "variantList": ["AutoCategory", "CarCategory"],
          "enableCabsSubscriptionView" : false,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
            "domain" : "https://nammayatri.in"
          , "customerAppId" : "in.juspay.nammayatri"
          , "driverAppId" : "in.juspay.nammayatripartner"
        },
        "waitingCharges" : 1.50,
        "waitingChargesConfig" : defWaitingChargesConfig,
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : { "showLearnMore" : true, "learnMoreVideoLink" : "https://www.youtube.com/shorts/NUTNKPzslpw" },
        "gstPercentage": "18",
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
        },
        "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        },
        "assets" :{
          "auto_image" : "ic_auto_rickshaw",
          "onboarding_auto_image" : "ny_ic_auto_side",
          "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
        }
      },
      {
        "cityName" : "Udupi",
        "mapImage" : "ny_ic_udupi_map",
        "cityCode" : "std:08200",
        "showSubscriptions" : false,
        "enableAdvancedBooking" : true,
        "advancedRidePopUpYoutubeLink" : "" ,
        "callDriverInfoPost": false, // Dummy link need to change
        "cityLat" : 13.3407096,
        "cityLong" : 74.7421016,
        "supportNumber" : "08069724919",
        "languageKey" : "KN_IN",
        "showScheduledRides" : false,
        "showDriverReferral" : false,
        "showCustomerReferral" : true,
        "uploadRCandDL" : true,
        "enableYatriCoins" : false,
        "enableGullak": false,
        "vehicleNSImg" : "ny_ic_auto_image",
        "registration" : {
            "supportWAN" : "918618963188",
            "callSupport" : true,
            "whatsappSupport" : true
        },
        "variantSubscriptionConfig" : {
          "enableVariantBasedSubscription" : false,
          "variantList" : [],
          "enableCabsSubscriptionView" : false,
          "staticViewPlans" : staticSubscriptionConfig
        },
        "showEarningSection" : true,
        "referral" : {
            "domain" : "https://nammayatri.in"
          , "customerAppId" : "in.juspay.nammayatri"
          , "driverAppId" : "in.juspay.nammayatripartner"
        },
        "waitingCharges" : 1.50,
        "waitingChargesConfig" : defWaitingChargesConfig,
        "rentalWaitingChargesConfig" : defRentalWaitingChargesConfig,
        "rateCardConfig" : { "showLearnMore" : true, "learnMoreVideoLink" : "https://www.youtube.com/shorts/NUTNKPzslpw" },
        "gstPercentage": "18",
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
        },
        "rideStartAudio" : {
          "acCab" : {},
          "nonAcCab" : {},
          "auto" : {},
          "bike" : {}
        },
        "assets" :{
          "auto_image" : "ic_auto_rickshaw",
          "onboarding_auto_image" : "ny_ic_auto_side",
          "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_green.png",
          "empty_referral_cab" : "ny_ic_refer_now_cab_ny,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ny.png"
        }
      }
  ]
  , "appUpdatePopupUrl" : "https://play.google.com/store/apps/details?id=in.juspay.nammayatripartner&pcampaignid=web_share"
  , "dashboard" : {
        "enable" : true,
        "url" : "https://nammayatri.in/open?source=in-app"
    }
  , "profile" : {
    "showBookingOption" : true
  }
  , "waitTimeConfig" : {
    "thresholdDist" : 0.05,
    "routeDistance" : 30,
    "diffBtwTwoHeartBeats" : 10,
    "straightLineDist" : 0.015
  }
  , "rideCompletedCardConfig" : {
      "lottieQRAnim" : true
  }
   , "vehicle" : {
      "validationPrefix" : "KA|AP|TS|DL|TN|PY|UP|HR|TG"
    }
  })
}

let staticSubscriptionConfig = [
  {"price" : 45.0, "frequency" : "PER_DAY", "variantCategory" : "CarCategory", "name" : "DAILY_UNLIMITED", "introductoryOffer" :  "", "showSelected" : false, "planDesc" : "CAB_DAILY_UNLIMITED_OFFER"},
  {"price" : 9.0, "frequency" : "PER_RIDE", "variantCategory" : "CarCategory", "name" : "DAILY_PER_RIDE", "introductoryOffer" : "" , "showSelected" : false, "planDesc" : "Up to a maximum of ₹90 per day-*$*-ದಿನಕ್ಕೆ ಗರಿಷ್ಠ ₹90-*$*-प्रति दिन अधिकतम ₹90 तक-*$*-প্রতিদিন সর্বোচ্চ ₹90 পর্যন্ত-*$*-പ്രതിദിനം പരമാവധി ₹90 വരെ-*$*-ஒரு நாளைக்கு அதிகபட்சம் ₹90 வரை-*$*-రోజుకు గరిష్టంగా ₹99 వరకు"},
  { "price": 25.0, "frequency": "PER_DAY", "variantCategory": "AutoCategory", "name": "DAILY_UNLIMITED", "introductoryOffer": "", "showSelected": false, "planDesc": "CAB_DAILY_UNLIMITED_OFFER"},
  { "price": 3.5, "frequency": "PER_RIDE", "variantCategory": "AutoCategory", "name": "DAILY_PER_RIDE", "introductoryOffer": "", "showSelected": false, "planDesc": "Up to a maximum of ₹35 per day-*$*-ದಿನಕ್ಕೆ ಗರಿಷ್ಠ ₹35-*$*-प्रति दिन अधिकतम ₹35 तक-*$*-প্রতিদিন সর্বোচ্চ ₹35 পর্যন্ত-*$*-പ്രതിദിനം പരമാവധി ₹35 വരെ-*$*-ஒரு நாளைக்கு அதிகபட்சம் ₹35 வரை-*$*-రోజుకు గరిష్టంగా ₹35 వరకు" }
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
    },
    "ambulance" : {
      "freeSeconds" : 480,
      "perMinCharges" : 2.0
    }
  }


let defRentalWaitingChargesConfig = {
  "cab" : {
    "freeSeconds" : 180,
    "perMinCharges" : 1.0
  },
  "auto" : {
    "freeSeconds" : 180,
    "perMinCharges" : 1.0
  },
  "bike" : {
    "freeSeconds" : 180,
    "perMinCharges" : 2.0
  },
  "ambulance" : {
    "freeSeconds" : 480,
    "perMinCharges" : 2.0
  }
}

let defRateCardConfig = {
    "showLearnMore" : false,
    "learnMoreVideoLink" : ""
  }