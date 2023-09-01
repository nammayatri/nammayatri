window.getMerchantConfig = function () {
  return JSON.stringify({
    "StringKeys": ["ABOUT_APP_DESCRIPTION",
    "WELCOME_TEXT",
    "REQUEST_AUTO_RIDE",
    "CURRENTLY_WE_ARE_LIVE_IN_",
    "DRIVER_PICKUP_CHARGES",
    "YOU_ARE_ABOUT_TO_CALL_YATRI_SATHI_SUPPORT",
    "SUCCESSFUL_ONBOARD", "ABOUT_REFERRAL_PROGRAM_DISCRIPTION",
    "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER",
    "YOUR_LOCATION_HELPS_OUR_SYSTEM",
    "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT",
    "LOCATION_PERMISSION_SUBTITLE_NEW_USER",
    "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL",
    "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL",
    "CALL_NAMMA_YATRI_SUPPORT",
    "YOUR_FEEDBACK_HELPS_US"
    ],
    "isReferralEnabled": "false",
    "showBookingPreference": "false",
    "showRateCard": "false",
    "showDashboard": "false",
    "enableShareRide": "false",
    "shareAppTitle" : "Share Yatri Sathi!",
    "shareAppContent" : "Hey there!\n\n Download Yatri Sathi now! \n" + getAppLink(window.__OS),
    "DOCUMENT_LINK": "https://docs.google.com/document/d/19pQUgTWXBqcM7bjy4SU1-z33r-iXsdPMfZggBTXbdR4",
    "APP_LINK" : getAppLink(window.__OS),
    "PRIVACY_POLICY_LINK": "https://docs.google.com/document/d/1-bcjLOZ_gR0Rda2BNmkKnqVds8Pm23v1e7JbSDdM70E",
    "CUSTOMER_TIP": "true",
    "isShareAppEnabled": "true",
    "apiLoaderLottie": "primary_button_loader.json",
    "isEmergencyContacts": "true",
    "isChatEnabled": "true",
    "showPickUpandDrop": true,
    "currency": "₹",
    "loaderColor": "",
    "showHamMenu" : true,
    "showQuoteFindingText" : false,
    "alertDialogPrimaryColor": "#2194FF",
    "gradient": [],
    "addFavouriteScreenBackArrow" : "ny_ic_chevron_left_white,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_left_white.png",
    "isGradient" : "false",
    "primaryTextColor": "#FCC32C",
    "primaryBackground": "#2C2F3A",
    "estimateConfirmText": "Request a NammaYatri Ride",
    "autoConfirmingLoaderColor": "#80B2FF",
    "primaryButtonCornerRadius" : 8.0,
    "cancelSearchTextColor": "#E55454",
    "dashboardUrl" : "https://yatrisathi.in/open/?source=in-app",
    "quoteListItemConfig": {
      "primaryButtonCorner": 8.0,
      "expiresColor" : "#E55454",
      "driverImagebg": "#F1F1F1",
      "vehicleHeight" : 37,
      "vehicleWidth": 40
    },
    "searchLocationConfig": {
      "searchLocationTheme": "#2C2F3A",
      "setLocationOnMapColor" : "#6D7280",
      "strokeColor": "1,#E5E7EB",
      "enableLocationTagbar" : "true",
      "resultsCardCornerRadius" : 20.0,
      "showRateCardDetails" : true,
      "showAdditionalChargesText" : true,
      "lottieHeight": 96,
      "lottieWidth": 96,
      "primaryButtonHeight": 60,
      "backArrow" : "ny_ic_chevron_left_white,https://assets.juspay.in/beckn/jatrisaathi/user/images/ny_ic_chevron_left_white.png"
     },
     "ratingConfig": {
      "secondaryButtonTextColor": "#2C2F3A",
      "secondaryButtonStroke": "1,#2C2F3A",
      "buttonCornerRadius": 8.0
     },
     "cancelReasonConfig": {
      "secondaryButtonTextColor": "#2C2F3A",
      "secondaryButtonStroke": "1,#2C2F3A",
      "buttonCornerRadius": 8.0
     },
     "driverInfoConfig": {
      "ratingTextColor": "#454545",
      "ratingBackground": "#F1F1F1",
      "ratingStroke": "0,#717171",
      "ratingCornerRadius": 6.0,
      "callBackground": "#2053BB6F",
      "callButtonStroke": "0,#EB0055",
      "cardStroke": "1,#E5E7EB",
      "otpStroke": "0,#717171",
      "numberPlateBackground" : "#E9BE4D",
      "showCancelPrevention" : true,
      "showNumberPlatePrefix": true,
      "showNumberPlateSuffix": false,
      "callHeight": 24,
      "callWidth": 24,
      "showTrackingButton" : true
    },
    "quoteListModelBackground": "#2C2F3A",
    "confirmPickUpLocationBorder": "#E5E7EB",
    "logs": ["JUSPAY","FIREBASE","CLEVERTAP"],
    "quoteListModel": {
      "backgroundColor": "#2C2F3A",
      "textColor": "#FFFFFF",
      "loaderColor": "#80B2FF", 
      "otpTextBackground"  : "#2C2F3A",
      "otpBackground" : "#F1F1F1",
      "otpTextColor" : "#FFFFFF",
      "otpTitleColor"  : "#6D7280",
      "selectRideTextColor": "#2C2F3A",
      "lineImage" : "ic_line",
      "lottieHeight": 300,
      "lottieWidth": 300,
      "topMargin": 0,
      "noQuotesImageHeight": 115,
      "noQuotesImageWidth": 137,
      "closeIcon" : "ny_ic_close_white,https://assets.juspay.in/beckn/jatrisaathi/user/images/ny_ic_close_white.png"
    },
    "profileBackground": "#2C2F3A",
    "profileName": "#FFFFFF",
    "profileImage": "#012A72",
    "profileCompletion" : "#FCC32C",
    "cancelRideColor" : "#E55454",
    "infoIconUrl" : "ny_ic_info,https://assets.juspay.in/nammayatri/images/user/ny_ic_information_grey.png",
    "merchantLogo" : "ny_ic_yatri_sathi_logo,https://assets.juspay.in/beckn/jatrisaathi/user/images/ny_ic_yatri_sathi_logo.png",
    "feedbackBackground": "#2C2F3A",
    "sideBarList": ["MyRides", "Favorites", "EmergencyContacts", "HelpAndSupport", "Language", "Separator", "ShareApp", "About", "Logout"],
    "rateCardColor": "#2C2F3A",
    "nyBrandingVisibility": false,
    "fontType": "Assets",
    "black900": "#2C2F3A",
    "black800": "#454545",
    "red" : "#E55454",
    "showCorporateAddress" : false,
    "popupBackground" : "#FFFFFF",
    "profileEditGravity" : "center",
    "englishStrings": {
      "ABOUT_APP_DESCRIPTION": "Yatri Sathi is an open platform to connect riders with drivers. The app makes it convenient for riders to book a ride with meter rate hence minimal fare.",
      "WELCOME_TEXT": "Welcome to Yatri Sathi",
      "REQUEST_AUTO_RIDE": "Request Ride",
      "CURRENTLY_WE_ARE_LIVE_IN_": "Currently we're live in Kolkata, you can enjoy our services there",
      "DRIVER_PICKUP_CHARGES": "Service Charges",
      "YOU_ARE_ABOUT_TO_CALL_YATRI_SATHI_SUPPORT": "You are about to place a call to the Yatri Sathi Support Team. Do you want to proceed?",
      "SUCCESSFUL_ONBOARD": "You have successfully signed on to \n Yatri Sathi",
      "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "The referral program incentivises drivers to accept more rides, cancel less and serve you better by recognising and rewarding worthy drivers. \n\n You can help out by entering the driver’s referral code  and improve the quality of rides for the Yatri Sathi Community!",
      "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "\nYou can get a referral code by asking your Yatri Sathi Driver.",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM" : "Your location helps our system to map down all the near by taxis and get you the quickest ride possible.",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT" : "You are about to place a call to the Yatri Sathi Support Team. Do you want to proceed?",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "Welcome to Yatri Sathi! \nTo start booking rides, please allow us to find you!",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" : "",
      "CORPORATE_ADDRESS_DESCRIPTION" : "Department of Information Technology & Electronics Government of West Bengal, Monibhandar (5th and 6th floor), Premise of Webel Bhavan, Block - EP & GP, Sector-V, Salt Lake, Kolkata - 700091",
      "REGISTERED_ADDRESS_DESCRIPTION" : "Department of Information Technology & Electronics Government of West Bengal, Monibhandar (5th and 6th floor), Premise of Webel Bhavan, Block - EP & GP, Sector-V, Salt Lake, Kolkata - 700091",
      "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL" : "",
      "CALL_NAMMA_YATRI_SUPPORT" : "Call Yatri Sathi Support",
      "YOUR_FEEDBACK_HELPS_US" : "Your feedback helps us improve the Yatri Sathi experience"
    },
    "hindiStrings": {
      "ABOUT_APP_DESCRIPTION": "यात्री साथी सवारियों को चालकों से जोड़ने का एक खुला मंच है। ऐप राइडर्स के लिए मीटर रेट के साथ राइड बुक करना सुविधाजनक बनाता है इसलिए न्यूनतम किराया।",
      "WELCOME_TEXT": "स्वागत है साथी",
      "REQUEST_AUTO_RIDE": "राइड का अनुरोध करें",
      "CURRENTLY_WE_ARE_LIVE_IN_": "वर्तमान में हम कोलकाता में रहते हैं, आप वहां हमारी सेवाओं का आनंद ले सकते हैं",
      "DRIVER_PICKUP_CHARGES": "सेवा प्रभार",
      "YOU_ARE_ABOUT_TO_CALL_YATRI_SATHI_SUPPORT": "आप यात्री साथी सपोर्ट टीम को कॉल करने वाले हैं। क्या आपकी आगे बढ़ने की इच्छा है?",
      "SUCCESSFUL_ONBOARD": "आपने यात्री साथी पर सफलतापूर्वक हस्ताक्षर कर \n लिए हैं",
      "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "रेफ़रल कार्यक्रम ड्राइवरों को अधिक सवारी स्वीकार करने, कम रद्द करने और योग्य ड्राइवरों को पहचानने और पुरस्कृत करके आपको बेहतर सेवा देने के लिए प्रोत्साहित करता है। \n\n आप ड्राइवर का रेफ़रल कोड डालकर मदद कर सकते हैं और यात्री साथी समुदाय के लिए सवारी की गुणवत्ता में सुधार कर सकते हैं!",
      "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "\n आप अपने यात्री साथी ड्राइवर से पूछकर रेफ़रल कोड प्राप्त कर सकते हैं।",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM" : "आपका स्थान हमारे सिस्टम को आस पास के सभी टैक्सियों को मैप करने में सहायता कर्ता है और आपको सबसे तेज सवारी प्रदान करता है",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT" : "आप यात्री साथी सपोर्ट टीम को कॉल करने वाले हैं। क्या आपकी आगे बढ़ने की इच्छा है?",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "यात्री साथी में आपका स्वागत है! \nसवारी की बुकिंग शुरू करने के लिए, कृपया हमें आपको ढूंढने की अनुमति दें!",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" : "",
      "CORPORATE_ADDRESS_DESCRIPTION" : "पश्चिम बंगाल सरकार के सूचना प्रौद्योगिकी और इलेक्ट्रॉनिक्स विभाग, मोनीभंडार (5वीं और 6वीं मंजिल), वेबेल भवन का परिसर, ब्लॉक - ईपी और जीपी, सेक्टर-वी, साल्ट लेक, कोलकाता - 700091",
      "REGISTERED_ADDRESS_DESCRIPTION" : "पश्चिम बंगाल सरकार के सूचना प्रौद्योगिकी और इलेक्ट्रॉनिक्स विभाग, मोनीभंडार (5वीं और 6वीं मंजिल), वेबेल भवन का परिसर, ब्लॉक - ईपी और जीपी, सेक्टर-वी, साल्ट लेक, कोलकाता - 700091",
      "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL" : "",
      "CALL_NAMMA_YATRI_SUPPORT" : "यात्री साथी सहायता को कॉल करें",
      "YOUR_FEEDBACK_HELPS_US" : "आपका फीडबैक हमें यात्री साथी अनुभव को बेहतर बनाने में मदद करता है"
    },
    "bengaliStrings": {
      "ABOUT_APP_DESCRIPTION": "যাত্রী সাথী হল চালকদের সাথে রাইডারদের সংযোগ করার জন্য একটি উন্মুক্ত প্ল্যাটফর্ম। অ্যাপটি রাইডারদের জন্য মিটার রেট সহ একটি রাইড বুক করা সুবিধাজনক করে তোলে তাই ন্যূনতম ভাড়া।",
      "WELCOME_TEXT": "স্বাগতম সাথী",
      "REQUEST_AUTO_RIDE": "যাত্রার অনুরোধ করুন",
      "CURRENTLY_WE_ARE_LIVE_IN_": "বর্তমানে আমরা কলকাতায় থাকি, আপনি সেখানে আমাদের পরিষেবা উপভোগ করতে পারেন",
      "DRIVER_PICKUP_CHARGES": "সারভিস চারজ",
      "YOU_ARE_ABOUT_TO_CALL_YATRI_SATHI_SUPPORT": "আপনি যাত্রী সাথী সমর্থন দলকে একটি কল করতে চলেছেন৷ আপনি কি এগিয়ে যেতে চান?",
      "SUCCESSFUL_ONBOARD": "আপনি সফলভাবে যাত্রী সাথীতে \n স্বাক্ষর করেছেন",
      "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "রেফারেল প্রোগ্রাম চালকদের আরও রাইড গ্রহণ করতে, কম বাতিল করতে এবং যোগ্য ড্রাইভারদের চিনতে এবং পুরস্কৃত করার মাধ্যমে আপনাকে আরও ভাল পরিবেশন করতে উত্সাহিত করে। \n\n আপনি ড্রাইভারের রেফারেল কোড লিখে সাহায্য করতে পারেন এবং যাত্রী সাথী সম্প্রদায়ের জন্য রাইডের মান উন্নত করতে পারেন!",
      "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "\n আপনি আপনার যাত্রী সাথী ড্রাইভারকে জিজ্ঞাসা করে একটি রেফারেল কোড পেতে পারেন।",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM" : "আপনার অবস্থান আমাদের সিস্টেমকে ট্যাক্সি দ্বারা আশেপাশের সমস্ত স্থান ম্যাপ করতে এবং আপনাকে দ্রুততম রাইড করতে সাহায্য করে৷",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT" : "আপনি জাত্রি সাথি সমর্থন দলকে কল করতে চলেছেন। আপনি কি এগিয়ে যেতে চান?",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "যাত্রী সাথী-এ স্বাগতম \nরাইড বুকিং শুরু করতে, অনুগ্রহ করে আমাদের আপনাকে খুঁজে পেতে অনুমতি দিন!",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" : "",
      "CORPORATE_ADDRESS_DESCRIPTION" : "পশ্চিমবঙ্গ সরকারের তথ্য প্রযুক্তি ও ইলেকট্রনিক্স বিভাগ, মনিভান্ডার (৫ম এবং ৬ষ্ঠ তলা), ওয়েবেল ভবনের প্রাঙ্গণ, ব্লক - ইপি অ্যান্ড জিপি, সেক্টর-ভি, সল্টলেক, কলকাতা - 700091",
      "REGISTERED_ADDRESS_DESCRIPTION" : "পশ্চিমবঙ্গ সরকারের তথ্য প্রযুক্তি ও ইলেকট্রনিক্স বিভাগ, মনিভান্ডার (৫ম এবং ৬ষ্ঠ তলা), ওয়েবেল ভবনের প্রাঙ্গণ, ব্লক - ইপি অ্যান্ড জিপি, সেক্টর-ভি, সল্টলেক, কলকাতা - 700091",
      "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL" : "",
      "CALL_NAMMA_YATRI_SUPPORT" : "সহযাত্রী সমর্থন কল করুন",
      "YOUR_FEEDBACK_HELPS_US" : "আপনার প্রতিক্রিয়া আমাদের যাত্রী সাথী অভিজ্ঞতা উন্নত করতে সাহায্য করে"
    },
    "languageList": [{
      "name": "English",
      "value": "EN_US",
      "subTitle": "ইংরেজি"
    }, {
      "name": "বাংলা",
      "value": "BN_IN",
      "subTitle": "Bengali"
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
      "title" : "",
      "imageUrl" : ""
  }
  , "showDeleteAccount" : true
  , "fontName" : "PlusJakartaSans"
  , "fontKannada" : "NotoSansKannada"
  , "showGenderBanner" : true
  , "autoSelectBackground" : "#53BB6F"
  , "enableMockLocation" : false
  , "defaultLanguage" : "EN_US"
  , "specialLocationView" : true
  , "navigationAppConfig" : {
    "query" : "google.navigation:q=%f,%f"
  , "packageName" : "com.google.android.apps.maps"
}
  , "SUPPORT_EMAIL" : "yatrisathi.support@wb.gov.in"
  , "OTP_MESSAGE_REGEX" : "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
  , "internationalNumberEnabled" : true
  })
}

function getAppLink(os) {
  if (os == "ANDROID"){
      return "https://play.google.com/store/apps/details?id=in.juspay.jatrisaathi"
  }else {return ""}
}