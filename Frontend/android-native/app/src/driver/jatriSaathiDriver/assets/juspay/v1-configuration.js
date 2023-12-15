window.version = window.version || {};
let version = "1.0.0";
if (typeof __VERSION__ !== "undefined") {
  version = __VERSION__
}
window.version["configuration"]= version;
window.getMerchantConfig = function () {
  return JSON.stringify({
    "APP_LINK": "https://play.google.com/store/apps/details?id=in.juspay.jatrisaathidriver",
    "USER_APP_LINK" : "https://nammayatri.in/link/rider/kTZ1",
    "PRIVACY_POLICY_LINK": "https://docs.google.com/document/d/1-bcjLOZ_gR0Rda2BNmkKnqVds8Pm23v1e7JbSDdM70E",
    "SPECIAL_ZONE_OTP_VIEW": "true",
    "StringKeys": ["WELCOME_TEXT",
      "ABOUT_TEXT",
      "NEED_IT_TO_ENABLE_LOCATION",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM",
      "EARNED_ON_APP",
      "TRAVELLED_ON_APP",
      "REPORT_ISSUE_CHAT_PLACEHOLDER",
      "MY_PLAN_TITLE",
      "CHOOSE_YOUR_PLAN",
      "OFFER_CARD_BANNER_TITLE",
      "TO_CONTINUE_USING_YATRI_SATHI",
      "YATRI_SATHI_FEE_PAYABLE_FOR_DATE",
      "PAYMENT_FAILED_DESC",
      "AADHAAR_LINKING_REQUIRED_DESCRIPTION",
      "COMPLETE_PAYMENT_TO_CONTINUE",
      "GET_READY_FOR_YS_SUBSCRIPTION",
      "SUBSCRIPTION_PLAN_STR",
      "FIND_HELP_CENTRE",
      "HOW_IT_WORKS",
      "GET_SPECIAL_OFFERS",
      "PAYMENT_PENDING_ALERT_DESC",
      "NO_OPEN_MARKET_RIDES"
    ],
    "fontType": "Assets",
    "currency": "₹",
    "isGradient" : "false",
    "BONUS_EARNED" : "false",
    "gradient": [],
    "addFavouriteScreenBackArrow" : "ny_ic_chevron_left_white,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_left_white.png",
    "popupBackground" : "#FFFFFF",
    "apiLoaderLottie": "primary_button_loader.json",
    "primaryTextColor": "#FCC32C",
    "primaryBackground": "#2C2F3A",
    "showCorporateAddress" : false,
    "imageUploadOptional" : true,
    "clientName" : "Yatri Sathi",
    "languageList": [{
      "name": "English",
      "value": "EN_US",
      "subtitle": "ইংরেজি"
    },
    {
      "name": "বাংলা",
      "value": "BN_IN",
      "subtitle": "Bengali"
    },
    {
      "name": "हिंदी",
      "value": "HI_IN",
      "subtitle": "Hindi"
    }
    ],
    "engilshInNative" : "ইংরেজি",
    "englishStrings": {
      "WELCOME_TEXT": "Welcome to Yatri Sathi Driver App",
      "ABOUT_TEXT": "Yatri Sathi partner is an open platform to connect drivers with riders. The app makes it convenient for drivers to find riders with proposed desired rates. No ride based commission, just pay small amount in the form of monthly subscription",
      "NEED_IT_TO_ENABLE_LOCATION": "Yatri Sathi Driver collect location data to enable share your location to monitor driver current location, even when the app is closed or not in use.",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "Currently,We allow only West Bengal registered number",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "You are about to place a call to the Yatri Sathi Support Team. Do you want to proceed?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "Your location helps our system to map down all the near by taxis and get you the quickest ride possible.",
      "EARNED_ON_APP" : "Earned on YS",
      "TRAVELLED_ON_APP" : "Travelled On Yatri Sathi",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "Describe your issue. Yatri Sathi will try to resolve it in under 24 hours.",
      "MY_PLAN_TITLE" : "Yatri Sathi Plans",
      "OFFER_CARD_BANNER_TITLE" : "Setup Autopay and pay only ₹1/ride from Jan 1-31",
      "TO_CONTINUE_USING_YATRI_SATHI" : "To continue using Yatri Sathi, please complete your payment for",
      "YATRI_SATHI_FEE_PAYABLE_FOR_DATE" : "Yatri Sathi fee payable for",
      "PAYMENT_FAILED_DESC" : "You may retry payment, or make the payment at your nearest Yatri Sathi booth",
      "AADHAAR_LINKING_REQUIRED_DESCRIPTION" : "To start driving for Yatri Sathi, please \n link your Aadhaar ID",
      "COMPLETE_PAYMENT_TO_CONTINUE" : "To continue using Yatri Sathi, please complete your payment",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "Get ready for\nYatri Sathi Plans!",
      "SUBSCRIPTION_PLAN_STR" : "Yatri Sathi Plan",
      "CHOOSE_YOUR_PLAN" : "Activate Plan Now!",
      "FIND_HELP_CENTRE" : "Find Help Desk",
      "HOW_IT_WORKS" : "How Autopay works?",
      "GET_SPECIAL_OFFERS" : "Guaranteed fixed price until\nJan 1, 2025",
      "PAYMENT_PENDING_ALERT_DESC" : "To continue taking rides on Yatri Sathi, clear your payment dues",
      "NO_OPEN_MARKET_RIDES" : "0 open market rides"
    },
    "hindiStrings": {
      "WELCOME_TEXT": "Welcome to Yatri Sathi Driver App",
      "ABOUT_TEXT": "Yatri Sathi partner चालकों को सवारियों से जोड़ने का एक खुला मंच है। ऐप ड्राइवरों के लिए प्रस्तावित वांछित दरों के साथ सवारियों को ढूंढना सुविधाजनक बनाता है। कोई सवारी आधारित कमीशन नहीं, बस मासिक सदस्यता के रूप में छोटी राशि का भुगतान करें",
      "NEED_IT_TO_ENABLE_LOCATION": "Yatri Sathi partner ड्राइवर के लोकेशन की निगरानी के लिए अपना स्थान साझा करने के लिए लोकेशन डेटा एकत्र करता है, तब भी जब ऐप बंद हो या उपयोग में न हो।",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "Currently,We allow only West Bengal registered number",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "आप जात्री साथी सपोर्ट टीम को कॉल करने वाले हैं। क्या आपकी आगे बढ़ने की इच्छा है?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "आपका स्थान हमारे सिस्टम को आस पास के सभी टैक्सियों को मैप करने में सहायता कर्ता है और आपको सबसे तेज सवारी प्रदान करता है",
      "EARNED_ON_APP" : "YS पर अर्जित मूल्य",
      "TRAVELLED_ON_APP" : "यात्री साथी पर तय की गई दूरी",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "अपनी समस्या बताएं। यात्री साथी 24 घंटे के अंदर इसका समाधान करने का प्रयास करेगा।",
      "MY_PLAN_TITLE" : "यात्री साथी प्लान्स",
      "OFFER_CARD_BANNER_TITLE" : "ऑटोपे सेटअप करें और 1-31 जनवरी तक केवल ₹1/सवारी का पेमेंट करें",
      "TO_CONTINUE_USING_YATRI_SATHI" : "यात्री साथी का उपयोग जारी रखने के लिए",
      "YATRI_SATHI_FEE_PAYABLE_FOR_DATE" : "यात्री साथी शुल्क लागू" ,
      "PAYMENT_FAILED_DESC" : "आप भुगतान को पुनः प्रयास कर सकते हैं, या अपने नजदीकी यात्री साथी बूथ पर भुगतान कर सकते हैं",
      "AADHAAR_LINKING_REQUIRED_DESCRIPTION" : "यात्री साथी के लिए ड्राइविंग शुरू करने के लिए, कृपया अपना आधार आईडी लिंक करें",
      "COMPLETE_PAYMENT_TO_CONTINUE" : "यात्री साथी का उपयोग जारी रखने के लिए, कृपया अपना भुगतान पूरा करें",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "यात्री साथी योजनाओं के लिए तैयार रहें!",
      "SUBSCRIPTION_PLAN_STR" : "यात्री साथी योजना",
      "CHOOSE_YOUR_PLAN" : "योजना अभी सक्रिय करें!",
      "FIND_HELP_CENTRE" : "सहायता डेस्क ढूंढें",
      "HOW_IT_WORKS" : "ऑटोपे कैसे काम करता है?",
      "GET_SPECIAL_OFFERS" : "1 जनवरी, 2025 तक निश्चित मूल्य की गारंटी",
      "PAYMENT_PENDING_ALERT_DESC" : "यात्री साथी पर यात्रा जारी रखने के लिए, अपनी शेष राशि चुकाएं",
      "NO_OPEN_MARKET_RIDES" : "0 ओपन मार्केट राइड"
    },
    "bengaliStrings": {
      "WELCOME_TEXT": "Welcome to Yatri Sathi Driver App",
      "ABOUT_TEXT": "যাত্রী সাথী অংশীদার চালকদের রাইডারদের সাথে সংযোগ করার জন্য একটি উন্মুক্ত প্ল্যাটফর্ম। অ্যাপটি চালকদের জন্য প্রস্তাবিত পছন্দসই রেট সহ রাইডার খুঁজে পেতে সুবিধাজনক করে তোলে। কোন রাইড ভিত্তিক কমিশন নেই, শুধুমাত্র মাসিক সাবস্ক্রিপশন আকারে অল্প পরিমাণ অর্থ প্রদান করুন",
      "NEED_IT_TO_ENABLE_LOCATION": "জাটি সাথি ড্রাইভার ড্রাইভারের বর্তমান অবস্থান নিরীক্ষণের জন্য আপনার অবস্থানটি ভাগ করে নিতে সক্ষম করতে অবস্থানের ডেটা সংগ্রহ করে, এমনকি অ্যাপটি বন্ধ থাকলেও বা ব্যবহার না করা হয়।",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "বর্তমানে, আমরা শুধুমাত্র পশ্চিমবঙ্গ নিবন্ধিত নম্বর অনুমোদন করি",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "আপনি জাত্রি সাথি সমর্থন দলকে কল করতে চলেছেন। আপনি কি এগিয়ে যেতে চান?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "আপনার অবস্থান আমাদের সিস্টেমকে ট্যাক্সি দ্বারা আশেপাশের সমস্ত স্থান ম্যাপ করতে এবং আপনাকে দ্রুততম রাইড করতে সাহায্য করে৷",
      "EARNED_ON_APP" : "YS এ অর্জিত মূল্য",
      "TRAVELLED_ON_APP" : "যাত্রী সাথীতে দূরত্ব ভ্রমণ",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "আপনার সমস্যাটি বর্ণনা করুন। যাত্রী সাথী 24 ঘন্টার মধ্যে এটি সমাধান করার চেষ্টা করবেন।",
      "MY_PLAN_TITLE" : "যাত্রী সাথী পরিকল্পনা",
      "OFFER_CARD_BANNER_TITLE" : "অটোপে সেটআপ করুন এবং জানুয়ারী 1-31 এর মধ্যে শুধুমাত্র ₹1/রাইড প্রদান করুন",
      "TO_CONTINUE_USING_YATRI_SATHI" : "Yatri Sathi ব্যবহার চালিয়ে যেতে, অনুগ্রহ করে আপনার অর্থপ্রদান সম্পূর্ণ করুন",
      "YATRI_SATHI_FEE_PAYABLE_FOR_DATE" : "Yatri Sathi ফি জন্য প্রদেয়" ,
      "PAYMENT_FAILED_DESC" : "আপনি আবার অর্থপ্রদানের চেষ্টা করতে পারেন, অথবা আপনার নিকটস্থ Yatri Sathi বুথে অর্থপ্রদান করতে পারেন",
      "AADHAAR_LINKING_REQUIRED_DESCRIPTION" : "যাত্রী সাথীর জন্য গাড়ি চালানো শুরু করতে, দয়া করে \n আপনার আধার আইডি লিঙ্ক করুন",
      "COMPLETE_PAYMENT_TO_CONTINUE" : "Yatri Sathi ব্যবহার চালিয়ে যেতে, অনুগ্রহ করে আপনার অর্থপ্রদান সম্পূর্ণ করুন",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "যাত্রী সাথী পরিকল্পনার জন্য প্রস্তুত হন!",
      "SUBSCRIPTION_PLAN_STR" : "যাত্রী সাথী পরিকল্পনা",
      "CHOOSE_YOUR_PLAN" : "এখনই প্ল্যান সক্রিয় করুন!",
      "FIND_HELP_CENTRE" : "সহায়তা ডেস্ক খুঁজুন",
      "HOW_IT_WORKS" : "স্বতঃপে কিভাবে কাজ করে?",
      "GET_SPECIAL_OFFERS" : "জানুয়ারি 1, 2025 পর্যন্ত গ্যারান্টিযুক্ত নির্দিষ্ট মূল্য",
      "PAYMENT_PENDING_ALERT_DESC" : "যাত্রী সাথীতে যাত্রা চালিয়ে যেতে, আপনার পেমেন্ট বকেয়া পরিশোধ করুন",
      "NO_OPEN_MARKET_RIDES" : "0 ওপেন মার্কেট রাইডস"
    },
    "logs": ["JUSPAY","FIREBASE","CLEVERTAP"]
    , "fontName" : "PlusJakartaSans"
    , "fontKannada" : "NotoSansKannada"
    , "allowAllMobileNumber" : false
    , "showGenderBanner" : false
    , "defaultLanguage" : "EN_US"
    , "navigationAppConfig" : {
      "query" : "google.navigation:q=%f,%f"
      , "packageName" : "com.google.android.apps.maps"
    }
    , "subscriptionConfig" : {
      "completePaymentPopup" : false,
      "supportNumber" : "08069724949",
      "enableSubscriptionPopups" : true,
      "maxDuesLimit" : 500.0,
      "faqLink" : "https://yatrisathi.in/plans/",
      "optionsMenuItems" : {
        "viewFaqs" : true,
        "viewAutopayDetails" : true,
        "paymentHistory" : true,
        "kioskLocation" : true
      },
      "offerBannerConfig" : {
        "showDUOfferBanner" : true,
        "offerBannerValidTill" : "2023-12-01T00:00:00",
        "offerBannerDeadline" : "Jan 1-31-*$*-ಜನವರಿ 1-31-*$*-1-31 जनवरी-*$*-ஜனவரி 1-31-*$*-জানুয়ারী 1-31",
        "offerBannerPlans" : ["25ade579-fd9c-4288-a015-337af085e66c"],
      },
      "lowDuesLimit" : 15.0,
      "highDueWarningLimit" : 75.0,
      "gradientConfig" : [{"id" : "c1a27b2c-8287-4d79-a5d9-99e1a0026203", colors : ["#29FF4D35", "#29FFE588"]},{"id" : "5eed42c1-2388-4a86-b68b-d9da2f674091", colors : ["#29FF4D35", "#29FFE588"]},{"id" : "b6d61915-65bb-4ca9-bbb7-a90be735a722", colors : ["#29FF4D35", "#29FFE588"]}],
      "enableSubscriptionSupportPopup" : true,
      "myPlanYoutubeLink" : "https://www.youtube.com/playlist?list=PLvMgI4c44A9Y2bykEuDAtHzgcubXOYqgU-*$*-https://www.youtube.com/playlist?list=PLvMgI4c44A9Y2bykEuDAtHzgcubXOYqgU-*$*-https://www.youtube.com/playlist?list=PLvMgI4c44A9Zl0IIQcZa7ZJrSjWPLfxpA-*$*-https://www.youtube.com/playlist?list=PLvMgI4c44A9Y8NLs_8TXc7biX-JkobrGB",
      "overlayYoutubeLink" : "https://youtube.com/shorts/nyJ1bIOsGfo-*$*-https://youtube.com/shorts/nyJ1bIOsGfo-*$*-https://youtu.be/RSKNT3NccPo-*$*-https://youtu.be/RSKNT3NccPo",
      "earnAmountInADay" : 5000,
      "showFeeBreakup" : true
    } 
    , "OTP_MESSAGE_REGEX" : "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
    , "autoPayBanner" : false
    , "referralType" : "QRScreen"
    , "profile" :
        { "bookingOptionMenuForTaxi" : true
        }
    , "profileVerification" : {
      "aadharVerificationRequired" : true
    } 
    , "bottomNavConfig" : {
      "subscription" : 
        { "isVisible" : true,
          "showNew" : true
        }
    }
    , "otpRegex" :  "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
    , "termsLink" : "https://docs.google.com/document/d/19pQUgTWXBqcM7bjy4SU1-z33r-iXsdPMfZggBTXbdR4"
    , "privacyLink" : "https://docs.google.com/document/d/1-bcjLOZ_gR0Rda2BNmkKnqVds8Pm23v1e7JbSDdM70E"
    , "feature" : {
      "enableBonus" : false
      , "enableImageUpload" : true
      , "enableGender" : false
      , "enableOtpRide" : true
    }
    , "appData" : {
      "link" : "https://play.google.com/store/apps/details?id=in.juspay.jatrisaathidriver"
      , "name" : "Yatri Sathi"
    }
    , "vehicle" : {
      "validationPrefix" :  "WB"
    }
    , "banners" :{
      "autoPay" : false
    }
    , "referral": {
      "link" : "https://nammayatri.in/link/rider/kTZ1"
    }
    , "enableMockLocation" : false
    , "flowConfig" : {
      "chooseCity" : {
        "runFlow" : false
      }
    }
    , "permissions" : {
      "locationPermission" : true,
      "notification" : false
    }
    , "homeScreen" : {
      "specialRideOtpView" : true,
      "showGenderBanner" : false
    }
  })
}