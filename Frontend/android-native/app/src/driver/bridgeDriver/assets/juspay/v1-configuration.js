window.version = window.version || {};
let version = "1.0.0";
if (typeof __VERSION__ !== "undefined") {
  version = __VERSION__
}
window.version["configuration"]= version;
window.getMerchantConfig = function () {
  return JSON.stringify({
    "RC_VALIDATION_TEXT": "KA|AP|TS|DL|TN|PY|UP|HR|TG",
    "DOCUMENT_LINK": "https://drive.google.com/file/d/1qYXbQUF4DVo2xNOawkHNTR_VVe46nggc",
    "APP_LINK": "https://play.google.com/store/apps/details?id=in.juspay.nammayatripartner",
    "USER_APP_LINK": "https://nammayatri.in/link/rider/mvnw",
    "PRIVACY_POLICY_LINK": "https://docs.google.com/document/d/1tF96MwtaEiq70y_P40E29Sy3X61moTc9",
    "SPECIAL_ZONE_OTP_VIEW": "false",
    "StringKeys": ["NEED_IT_TO_ENABLE_LOCATION", "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER", "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT", "YOUR_LOCATION_HELPS_OUR_SYSTEM", "WHAT_IS_NAMMA_YATRI_BONUS", "EARNED_ON_APP", "TRAVELLED_ON_APP", "GUARANTEED_FIXED_PRICE", "GET_READY_FOR_YS_SUBSCRIPTION", "DIRECT_PAYMENT_NO_COMMISSIONS" ,
                   "CUSTOMER_PAYS_DIRECTLY" ,
                   "HUNDRED_PERCENT_FARE_GOES_TO_YOU" ,
                   "FARE_SHOWN_IS_FARE_YOU_GET" ,
                   "BE_A_PART_OF_OPEN_MOBILITY_REVOLUTION" ,
                   "OUR_DATA_AND_PRODUCT_ARE_TRANSPARENT",
                   "VEHICLE_REGISTRATION_NUMBER",
                   "RE_ENTER_VEHICLE_REGISTRATION_NUMBER",
                   "DOWNLOAD_NAMMA_YATRI"],
    "fontType": "Assets",
    "currency": "$",
    "distanceUnit": "mi",
    "isGradient" : "false",
    "gradient": [],
    "addFavouriteScreenBackArrow" : "ny_ic_chevron_left_white,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_left_white.png",
    "popupBackground" : "#FFFFFF",
    "apiLoaderLottie": "primary_button_loader.json",
    "primaryButtonConfig" : {
      "loaderUrl" : "https://assets.moving.tech/beckn/mobilityredbus/user/lottie/primary_button_loader.json"
     },
    "primaryTextColor": "#FCC32C",
    "primaryBackground": "#2C2F3A",
    "colors" : {
          "green700" : "#378C66"
      },
    "showCorporateAddress" : true,
    "imageUploadOptional" : false,
    "BONUS_EARNED" : "true",
    "clientName" : "Bridge",
    "languageList" : [
      {"name":"English","value":"EN_US", "subtitle": "English"}
    ],
    "engilshInNative" : "English",
    "englishStrings": {
      "NEED_IT_TO_ENABLE_LOCATION": "Bridge Driver collect location data to enable share your location to monitor driver current location, even when the app is closed or not in use.",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "Currently, We aren't operating with this vehicle registration number",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "You are about to place a call to the Bridge Support Team. Do you want to proceed?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "Your location helps our system to map down all the near by autos and get you the quickest ride possible.",
      "WHAT_IS_NAMMA_YATRI_BONUS" : "What is Bridge Bonus?",
      "EARNED_ON_APP" : "Earned on Bridge",
      "TRAVELLED_ON_APP" : "Travelled On Bridge",
      "GUARANTEED_FIXED_PRICE" : "Guaranteed fixed price until \nApr 1, 2025",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "Get ready for\nBridge Plans!",
      "DIRECT_PAYMENT_NO_COMMISSIONS" : "Zero Commissions, Forever!",
      "CUSTOMER_PAYS_DIRECTLY" : "Your car, your customers, your \n earnings—all yours!",
      "HUNDRED_PERCENT_FARE_GOES_TO_YOU" : "Happy Drivers, Happy\nJourneys!",
      "FARE_SHOWN_IS_FARE_YOU_GET" : "Fair wages for you and fair prices for\n your customers. 100% transparent!",
      "BE_A_PART_OF_OPEN_MOBILITY_REVOLUTION" : "Connecting Minneapolis!",
      "OUR_DATA_AND_PRODUCT_ARE_TRANSPARENT" : "App by the Drivers, for the People.\n People before profits, always!",
      "VEHICLE_REGISTRATION_NUMBER": "Vehicle Number Plate",
      "RE_ENTER_VEHICLE_REGISTRATION_NUMBER": "Re-enter Vehicle Number Plate",
      "DOWNLOAD_NAMMA_YATRI" : "Download Bridge"
    },
    "hindiStrings": {
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
    , "rcLimit" : 3
    , "acExplanation" : true
    , "rateCardScreen" :  {
        "showYoutubeVideo" : true,
        "showRateCard" : true
      }
    , "showGenderBanner" : true
    , "defaultLanguage" : "EN_US"
    , "OTP_MESSAGE_REGEX" : "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
    , "leaderBoard": {
      "isMaskedName": false
     , "enable" : false
    }
    , "subscriptionConfig" : {
          "enableBlocking" : false,
          "onBoardingSubscription" : false,
          "showDUOfferBanner" : false,
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
          "enableSubscriptionPopups" : false,
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
          "showLottieSubscriptionScreen" : false,
          "backgroundGradient" : ["#E0D1FF" , "#F9F6FF"],
          "showUPIAutopay" : false,
          "promoTextColor" : "#7D4BFF",
          "showHowThisWorks" : true,
          "benefitsBgColor" : "#F6F1FF"
    }
    , "rideActionModelConfig" : {
      "showVehicleVariant" : false,
        "mapBackground" : "#7435FC"
    }
    , "referralType" : "LeaderBoard"
    , "referral": {
      "type" : "LeaderBoard"
      , "link" : "https://nammayatri.in/link/rider/mvnw"
    }
    , "gotoConfig" : {
      "maxGotoLocations" : 5,
      "enableGoto" : false
    }
    , "bottomNavConfig" : {
        "home": {
          "isVisible": true,
          "showNew": false
        },
        "rideHistory": {
          "isVisible": false,
          "showNew": false
        },
        "driverEarnings": {
          "isVisible": true,
          "showNew": false
        },
        "subscription": {
          "isVisible": true,
          "showNew": false
        },
        "referral": {
          "isVisible": true,
          "showNew": false
        },
        "notifications": {
          "isVisible": false,
          "showNew": false
        },
        "activeColor" : "#7D4BFF"
    }
    , "purpleRideConfig" : {
      "showPurpleVideos" : false,
      "visualImpairmentVideo" : "https://www.youtube.com/watch?v=2qYXl03N6Jg",
      "physicalImpairmentVideo" : "https://www.youtube.com/watch?v=B0C6SZTQO6k",
      "hearingImpairmentVideo" : "https://www.youtube.com/watch?v=udkWOt0serg",
      "genericAccessibilityVideo" : "https://youtu.be/5s21p2rI58c"
    }
    , "otpRegex" :  "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
    , "termsLink" : "https://docs.google.com/document/d/1JjV2CEh2y2pBJ5FPe8NUBdj5UpyhmPsEdArKWng0c4o"
    , "termsVersion" : 2.0
    , "privacyLink" : "https://docs.google.com/document/d/1NtSYItpiJnKbGkup2q88EYmkQ4pn0dul-pk6D1HYHLo"
    , "feature" : {
      "enableBonus" : false
      , "enableImageUpload" : false
      , "enableGender" : true
      , "enableOtpRide" : false
      , "enableYatriCoins" : false
    }
    , "appData" : {
      "link" : "https://play.google.com/store/apps/details?id=in.juspay.nammayatripartner"
      , "name" : "Bridge"
    }
    , "enableMockLocation" : false
  , "dashboard" : {
        "enable" : false,
        "url" : "https://nammayatri.in/open?source=in-app"
    }
  , "appUpdatePopupUrl" : "https://play.google.com/store/apps/details?id=com.mobility.movingtechdriver&pcampaignid=web_share"
  , "profile" : {
    "showBookingOption" : false
  , "enableMultipleRC" : true
  , "backgroundGradient" : ["#E0D1FF" , "#F9F6FF"]
  , "background" : "#F9F6FF"
  , "settingsBtnColor": "#7D4BFF"
  }
  , "waitTimeConfig" : {
    "thresholdDist" : 0.03,
    "routeDistance" : 30,
    "diffBtwTwoHeartBeats" : 10,
    "straightLineDist" : 0.015
  }
  , "primaryTextColor" : "#FFFFFF"
  , "primaryBackground" : "#7435FC"
  , "primaryGradientColor": ["#996DFF", "#1A9747FF"]
  , "rideCompletedCardConfig" : {
      "lottieQRAnim" : true,
      "topCardGradient" : ["#F2E2FF","#FAF5FF"],
      "bottomBackground" : "#FFFFFF"
  }
  , "showPaymentDetails": false
  , "secondaryBackground" : "#11032F"
   , "vehicle" : {
      "validationPrefix" : "KA|AP|TS|DL|TN|PY|UP|HR|TG"
    }
   , "welcomeScreen" : {
          "background" :"#F5EDFF"
        }
  , "enterMobileNumberScreen" : {
        "headerBackground": "#11032F"
      , "emailAuth" : true
      }
  , "defaultCountryCodeConfig" : {
      "countryName" : "UnitedStates"
    , "countryCode"  : "+1"
    , "countryShortCode" : "US"
    }
  , "flowConfig" : {
      "chooseCity" : {
        "runFlow" : false,
        "directAuth" : true,
        "defCity" : "Minneapolis",
        "useDefault" : true
      }
  }
  , "vehicleRegisterationScreen" : {
      "collectVehicleDetails" : true
  }
  , "vehicleRegisterationScreen" : {
    "collectVehicleDetails" : true
  }
  , "bookingPreferencesConfig" : {
      "primaryToggleBackground": "#7D4BFF"
    , "vehicleNumberBackground" : "#E0D1FF"
    , "vehicleNumberRadius" : 15.0
    , "rateCardGradient": ["#4D996dff", "#1A9747FF"]
  }
  , "benefitsScreen" : {
      "learnAndEarnItem" : {
          "statusBackground" : "#7D4BFF"
      }
  , "referralCardBackground":
          { "customer": "#F1F2F7"
          , "driver": "#E0D1FF"
          }
  }
  , "lmsVideoScreen": {
      "titleBackground" : "#F6F1FF"
    , "enableQuiz" : false
  }
  , "navigationAppConfig" : {
      "ios" : {
      "fallbackQuery" : "https://apps.apple.com/us/app/google-maps/id585027354"
      }
    }
  , "homeScreen": {
          "offlineBtnColor" : "#7D4BFF",
          "statsBackground" : "#F9F6FF",
          "statusPills": {
              "Silent": {
                  "background": "#7435FC"
              },
              "Online": {
                  "background": "#378C66"
              }
          }
      }
  })
}
