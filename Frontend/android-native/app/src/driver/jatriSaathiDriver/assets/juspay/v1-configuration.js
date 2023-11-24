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
    "StringKeys": ["WELCOME_TEXT", "ABOUT_TEXT", "NEED_IT_TO_ENABLE_LOCATION", "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER", "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT", "YOUR_LOCATION_HELPS_OUR_SYSTEM", "EARNED_ON_APP", "TRAVELLED_ON_APP", "REPORT_ISSUE_CHAT_PLACEHOLDER", "MY_PLAN_TITLE", "CHOOSE_YOUR_PLAN", "OFFER_CARD_BANNER_TITLE"],
    "showCorporateAddress" : false,
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
      "CHOOSE_YOUR_PLAN" : "Start your Plan Now!",
      "OFFER_CARD_BANNER_TITLE" : "Setup Autopay and pay only ₹3/ride from Jan 1-31"
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
      "CHOOSE_YOUR_PLAN" : "अपना प्लान शुरू करें!",
      "OFFER_CARD_BANNER_TITLE" : "ऑटोपे सेटअप करें और 1-31 जनवरी तक केवल ₹3/सवारी का पेमेंट करें"
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
      "CHOOSE_YOUR_PLAN" : "এখন প্ল্যান শুরু করুন!",
      "OFFER_CARD_BANNER_TITLE" : "অটোপে সেটআপ করুন এবং জানুয়ারী 1-31 এর মধ্যে শুধুমাত্র ₹3/রাইড প্রদান করুন"
    },
    "logs": ["JUSPAY","FIREBASE","CLEVERTAP"]
    , "defaultLanguage" : "EN_US"
    , "subscriptionConfig" : {
      "completePaymentPopup" : true,
      "supportNumber" : "08069724949",
      "maxDuesLimit" : 500.0,
      "faqLink" : "https://yatrisathi.in/plans/",
      "optionsMenuItems" : {
        "viewFaqs" : true,
        "viewAutopayDetails" : true
      },
      "offerBannerConfig" : {
        "showDUOfferBanner" : true,
        "offerBannerValidTill" : "2023-12-01T00:00:00",
        "offerBannerDeadline" : "Jan 1-31-*$*-ಜನವರಿ 1-31-*$*-1-31 जनवरी-*$*-ஜனவரி 1-31-*$*-জানুয়ারী 1-31",
        "offerBannerPlans" : ["25ade579-fd9c-4288-a015-337af085e66c"],
      },
      "gradientConfig" : [{"id" : "c1a27b2c-8287-4d79-a5d9-99e1a0026203", colors : ["#29FF4D35", "#29FFE588"]},{"id" : "5eed42c1-2388-4a86-b68b-d9da2f674091", colors : ["#29FF4D35", "#29FFE588"]}],
      "enableSubscriptionSupportPopup" : true,
      "myPlanYoutubeLink" : "https://www.youtube.com/playlist?list=PLvMgI4c44A9Y2bykEuDAtHzgcubXOYqgU-*$*-https://www.youtube.com/playlist?list=PLvMgI4c44A9Y2bykEuDAtHzgcubXOYqgU-*$*-https://www.youtube.com/playlist?list=PLvMgI4c44A9Zl0IIQcZa7ZJrSjWPLfxpA-*$*-https://www.youtube.com/playlist?list=PLvMgI4c44A9Y8NLs_8TXc7biX-JkobrGB"
    } 
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
    , "others" : {
      "otpRegex" :  "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
      , "termsLink" : "https://docs.google.com/document/d/19pQUgTWXBqcM7bjy4SU1-z33r-iXsdPMfZggBTXbdR4"
      , "privacyLink" : "https://docs.google.com/document/d/1-bcjLOZ_gR0Rda2BNmkKnqVds8Pm23v1e7JbSDdM70E"
    }
    , "features" : {
      "enableBonus" : false
      , "enableImageUpload" : true
      , "enableGender" : false
      , "enableOtpRide" : true
    }
    , "appDatas" : {
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
  })
}