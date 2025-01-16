window.version = window.version || {};
let version = "1.0.0";
if (typeof __VERSION__ !== "undefined") {
  version = __VERSION__
}
window.version["configuration"]= version;

function getAppLink(os) {
  if (os == "ANDROID"){
    return "https://play.google.com/store/apps/details?id=in.juspay.jatrisaathi"
  }else {return "https://apps.apple.com/us/app/yatri-sathi/id6451491581"}
}
window.getMerchantConfig = function () {
  return JSON.stringify({
    "StringKeys": [
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
      "CORPORATE_ADDRESS_DESCRIPTION",
      "REGISTERED_ADDRESS_DESCRIPTION",
      "CALL_NAMMA_YATRI_SUPPORT",
      "YOUR_FEEDBACK_HELPS_US",
      "LEARN_HOW_TEXT",
      "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE",
      "ACCESSIBILITY_TEXT",
      "TO_CATER_YOUR_SPECIFIC_NEEDS",
      "PLEASE_ENABLE_LOCATION_PERMISSION",
      "TAXI_FROM_ZONE",
      "MOST_LOVED_APP",
      "GO_TO_ZONE",
      "MOST_LOVED_APP",
      "REPORT_ISSUE_CHAT_PLACEHOLDER",
      "CALL_SUPPORT_DESCRIPTION",
      "WE_HOPE_THE_ISSUE_IS_RESOLVED",
      "WHO_CAN_TRACK_YOUR_RIDE",
      "ALERT_SAFETY_TEAM",
      "RECOMMEND_EMERGENCY_CONTACTS_TO_INSTALL",
      "SAFETY_TEAM_WILL_BE_ALERTED",
      "SHARE_RIDE_DESCRIPTION",
      "PLEASE_STAY_CALM_TEAM_ALERTED",
      "INDICATION_TO_EMERGENCY_CONTACTS",
      "EMERGENCY_CONTACTS_CAN_TAKE_ACTION"
    ],
    "isReferralEnabled": "true",
    "showBookingPreference": "false",
    "showRateCard": "true",
    "showDashboard": "false",
    "enableShareRide": "true",
    "autoVariantEnabled": true,
    "shareAppTitle" : "Share Yatri Sathi!",
    "shareAppContent" : "Hey there!\n\n Download Yatri Sathi now! \n" + getAppLink(window.__OS),
    "DOCUMENT_LINK": "https://docs.google.com/document/d/19pQUgTWXBqcM7bjy4SU1-z33r-iXsdPMfZggBTXbdR4",
    "appLink" : getAppLink(window.__OS),
    "PRIVACY_POLICY_LINK": "https://docs.google.com/document/d/1-bcjLOZ_gR0Rda2BNmkKnqVds8Pm23v1e7JbSDdM70E",
    "CUSTOMER_TIP": "true",
    "shareAppConfig" : {
      "title" : "Share Yatri Sathi!"
      , "description" : "Hey there!\n\n Download Yatri Sathi now! \n" + getAppLink(window.__OS)
    }
    , "otpRegex" :  "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
    , "termsLink" : "https://drive.google.com/file/d/1ALCfQeQiouaH3lzaVIozv8uCrxbbxvIR/view?usp=sharing"
    , "termsVersion" : 1.0
    , "privacyLink" : "https://docs.google.com/document/d/1-bcjLOZ_gR0Rda2BNmkKnqVds8Pm23v1e7JbSDdM70E"
    , "appData" : {
      "link" : getAppLink(window.__OS)
      , "supportMail" :"yatrisathi.support@wb.gov.in"
      , "name" : "Yatri Sathi"
      , "website" : "https://yatrisathi.in/"
    },
    "tipEnabledCities" : ["Kolkata", "Siliguri", "Asansol", "Durgapur"],
    "customerTip" : {
      "auto" : true,
      "cabs" : true
    },
    "acPopupConfig" : {
      "enableAcPopup" : true,
      "enableNonAcPopup" : true,
      "showAfterTime" : 5
  },
    "showChargeDesc" : false,
    "isShareAppEnabled": "true",
    "apiLoaderLottie": "primary_button_loader.json",
    "isEmergencyContacts": "true",
    "isChatEnabled": "true",
    "showPickUpandDrop": true,
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
    "rideCompletedCardConfig" : {
      "showCallSupport" : true
    },
    "quoteListItemConfig": {
      "primaryButtonCorner": 8.0,
      "expiresColor" : "#E55454",
      "driverImagebg": "#F1F1F1",
      "vehicleHeight" : 37,
      "vehicleWidth": 40
    },
    "searchLocationConfig": {
      "enableLocationTagbar" : "true",
      "showAdditionalChargesText" : false, // Temporary Fix until properly handled in backend's estimateFareBreakup
      "showDriverAddition" : false, // Temporary Fix until properly handled in backend's estimateFareBreakup
      "enableRateCard" : false
    },
    "logs": ["JUSPAY","FIREBASE","CLEVERTAP"],
    "merchantLogo" : "ny_ic_yatri_sathi_logo,https://assets.juspay.in/beckn/jatrisaathi/user/images/ny_ic_yatri_sathi_logo.png",
    "feedbackBackground": "#2C2F3A",
    "sideBarList": ["MyRides", "Tickets", "Favorites", "NammaSafety", "HelpAndSupport", "Language", "Separator", "ShareApp", "LiveStatsDashboard", "About", "Logout"],
    "rateCardColor": "#2C2F3A",
    "nyBrandingVisibility": false,
    "fontType": "Assets",
    "black900": "#2C2F3A",
    "black800": "#454545",
    "red" : "#E55454",
    "isAdvancedBookingEnabled" : true,
    "showCorporateAddress" : false,
    "popupBackground" : "#FFFFFF",
    "profileEditGravity" : "center",
    "englishStrings": {
      "MOST_LOVED_APP" : "Amar Shohor, Amar Sofor",
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
      "YOUR_FEEDBACK_HELPS_US" : "Your feedback helps us improve the Yatri Sathi experience",
      "LEARN_HOW_TEXT" : "Learn how Yatri Sathi caters to your needs",
      "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE": "Driver addition limits are in increments of ₹10",
      "ACCESSIBILITY_TEXT" : "Yatri Sathi, now customised for you!",
      "TO_CATER_YOUR_SPECIFIC_NEEDS" : "To cater to your specific needs, we have customised certain features of Yatri Sathi.",
      "PLEASE_ENABLE_LOCATION_PERMISSION" : "Please enable location permissions for Yatri Sathi from the settings app to start looking for rides.",
      "TAXI_FROM_ZONE" : "from Yatri Sathi Zone",
      "GO_TO_ZONE" : "Go to Yatri Sathi Zone",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "Describe your issue. Yatri Sathi will try to resolve it in under 24 hours.",
      "CALL_SUPPORT_DESCRIPTION" : "You are about to place a call to Yatri Sathi Support. Do you want to proceed?",
      "WE_HOPE_THE_ISSUE_IS_RESOLVED" : "We hope the issue is resolved, feel free to write to us at support@nammayatri.in for any queries.",
      "WHO_CAN_TRACK_YOUR_RIDE" : "Who can follow your ride on Yatri Sathi App",
      "ALERT_SAFETY_TEAM" : "Alert Yatri Sathi Safety Team",
      "RECOMMEND_EMERGENCY_CONTACTS_TO_INSTALL" : "We recommend emergency contacts to install Yatri Sathi for an enhanced tracking experience",
      "SAFETY_TEAM_WILL_BE_ALERTED" : "Upon SOS  Yatri Sathi safety team will be notified",
      "SHARE_RIDE_DESCRIPTION" : "On choosing this, Yatri Sathi will send app push notification to the emergency contacts",
      "PLEASE_STAY_CALM_TEAM_ALERTED" : "Please stay calm, Yatri Sathi safety team is alerted!",
      "INDICATION_TO_EMERGENCY_CONTACTS" : "Yatri Sathi will indicate to your emergency contacts that this is a test drill, ensuring a stress-free experience.",
      "EMERGENCY_CONTACTS_CAN_TAKE_ACTION" : "Emergency Contacts can follow/ take emergency response actions on Yatri Sathi App"
    },
    "hindiStrings": {
      "MOST_LOVED_APP" : "मेरा शहर, मेरी यात्रा",
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
      "YOUR_FEEDBACK_HELPS_US" : "आपका फीडबैक हमें यात्री साथी अनुभव को बेहतर बनाने में मदद करता है",
      "LEARN_HOW_TEXT" : "जानें कि यात्री साथी आपकी ज़रूरतों को कैसे पूरा करता है",
      "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE": "ड्राइवर जोड़ने की सीमा ₹10 की वृद्धि में है",
      "ACCESSIBILITY_TEXT" : "यात्री साथी, अब आपके लिए अनुकूलित!",
      "TO_CATER_YOUR_SPECIFIC_NEEDS" : "आपकी विशिष्ट आवश्यकताओं को पूरा करने के लिए, हमने यात्री साथी की कुछ विशेषताओं को अनुकूलित किया है",
      "PLEASE_ENABLE_LOCATION_PERMISSION" : "सवारी की तलाश शुरू करने के लिए कृपया सेटिंग ऐप से यात्री साथी के लिए स्थान अनुमतियां सक्षम करें।",
      "TAXI_FROM_ZONE" : "टैक्सी में सवार हों",
      "GO_TO_ZONE" : "यात्री साथी जोन पर जाएं" ,
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "अपने मामले का वर्णन करें। यात्री साथी 24 घंटे के अंदर इसका समाधान करने का प्रयास करेंगे।",
      "CALL_SUPPORT_DESCRIPTION" : "आप यात्री साथी सहायता को कॉल करने वाले हैं। क्या आपकी आगे बढ़ने की इच्छा है?",
      "WE_HOPE_THE_ISSUE_IS_RESOLVED" : "हमें उम्मीद है कि समस्या हल हो गई है, किसी भी प्रश्न के लिए बेझिझक हमें yatrisathi.support@wb.gov.in पर लिखें।",
      "WHO_CAN_TRACK_YOUR_RIDE": "कौन आपकी राइड को यात्री साथी ऐप पर ट्रैक कर सकता है",
      "ALERT_SAFETY_TEAM": "यात्री साथी सुरक्षा टीम को चेतावनी दें",
      "RECOMMEND_EMERGENCY_CONTACTS_TO_INSTALL": "हम जरूरी संपर्कों को बढ़िया ट्रैकिंग अनुभव के लिए यात्री साथी इंस्टॉल करने की सिफारिश करते हैं",
      "SAFETY_TEAM_WILL_BE_ALERTED": "SOS के अनुसार यात्री साथी सुरक्षा टीम को सूचित किया जाएगा",
      "SHARE_RIDE_DESCRIPTION": "इसे चुनने पर, यात्री साथी अप्लिकेशन पुश सूचना आपात संपर्कों को भेजेगा",
      "PLEASE_STAY_CALM_TEAM_ALERTED": "कृपया शांत रहें, यात्री साथी सुरक्षा टीम को चेतावनी दी गई है!",
      "INDICATION_TO_EMERGENCY_CONTACTS": "यात्री साथी आपके आपात संपर्कों को सूचित करेगा कि यह एक टेस्ट ड्रिल है, जो एक तनावमुक्त अनुभव सुनिश्चित करेगा",
      "EMERGENCY_CONTACTS_CAN_TAKE_ACTION": "आपात संपर्क व्यक्तियां यात्री साथी ऐप पर अनुकरण कर सकती हैं और आपात प्रतिक्रिया कार्रवाई ले सकती हैं।"
    },
    "bengaliStrings": {
      "MOST_LOVED_APP" : "আমার শহর, আমার সফর",
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
      "CALL_NAMMA_YATRI_SUPPORT" : "সহযাত্রী সাহায্য কল করুন",
      "YOUR_FEEDBACK_HELPS_US" : "আপনার প্রতিক্রিয়া আমাদের যাত্রী সাথী অভিজ্ঞতা উন্নত করতে সাহায্য করে",
      "LEARN_HOW_TEXT" : "জানুন কীভাবে যাত্রী সাথী আপনার প্রয়োজন মেটায়",
      "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE": "ড্রাইভার যোগ করার সীমা ₹10 বৃদ্ধিতে রয়েছে ",
      "ACCESSIBILITY_TEXT" : "যাত্রী সাথী, এখন আপনার জন্য কাস্টমাইজ করা হয়েছে!",
      "TO_CATER_YOUR_SPECIFIC_NEEDS" : "আপনার নির্দিষ্ট চাহিদা মেটাতে আমরা যাত্রী সাথীর কিছু বৈশিষ্ট্য কাস্টমাইজ করেছি",
      "PLEASE_ENABLE_LOCATION_PERMISSION" : "অনুগ্রহ করে সেটিংস অ্যাপ থেকে যাত্রী সাথীর জন্য অবস্থানের অনুমতি সক্ষম করুন যাতে রাইডের খোঁজ শুরু করা যায়।",
      "TAXI_FROM_ZONE" : "চড়ুন",
      "GO_TO_ZONE" : "যাত্রী সাথী জোনে যান",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "আপনার সমস্যাটি বর্ণনা. যাত্রী সাথী 24 ঘন্টার মধ্যে এটি সমাধান করার চেষ্টা করবে।",
      "CALL_SUPPORT_DESCRIPTION" : "আপনি যাত্রী সাথী সমর্থনে একটি কল করতে চলেছেন৷ আপনি কি এগিয়ে যেতে চান?",
      "WE_HOPE_THE_ISSUE_IS_RESOLVED" : "আমরা আশা করি সমস্যাটি সমাধান হয়ে গেছে, যেকোনো প্রশ্নের জন্য yatrisathi.support@wb.gov.in-এ আমাদের কাছে নির্দ্বিধায় লিখুন।",
      "WHO_CAN_TRACK_YOUR_RIDE": "কে যাত্রী সাথী অ্যাপ দ্বারা আপনার রাইড অনুসরণ করতে পারে",
      "ALERT_SAFETY_TEAM": "সতর্কতা যাত্রী সাথী সুরক্ষা দলে",
      "RECOMMEND_EMERGENCY_CONTACTS_TO_INSTALL": "আমরা অনুমোদন করি জরুরি যোগাযোগের মুখ্যত্ব বৃদ্ধি করতে যাত্রী সাথী ইনস্টল করা",
      "SAFETY_TEAM_WILL_BE_ALERTED": "SOS অনুসারে যাত্রী সাথী সুরক্ষা দলে জানানো হবে",
      "SHARE_RIDE_DESCRIPTION": "এইটা চয়ন করার সময়, যাত্রী সাথী অ্যাপ পুশ বিজ্ঞপ্তি পাঠাবে অতি জরুরি যোগাযোগের সাথে",
      "PLEASE_STAY_CALM_TEAM_ALERTED": "অনুগ্রহ করে শান্ত থাকুন, যাত্রী সাথী সুরক্ষা দলটি সতর্ক করা হয়েছে!",
      "INDICATION_TO_EMERGENCY_CONTACTS": "যাত্রী সাথী আপনার জরুরি যোগাযোগের সংকেত দেবে যে এটি একটি টেস্ট ড্রিল, যাত্রা অভিজ্ঞতার মন্তব্যে নিরাপদ অনুভূতি নিশ্চিত করা হয়",
      "EMERGENCY_CONTACTS_CAN_TAKE_ACTION": "জরুরি যোগাযোগ ব্যক্তিগণ যাত্রী সাথী অ্যাপ উপর অনুসরণ করতে পারে এবং জরুরি প্রতিক্রিয়া নিতে পারে"
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
    , "enableGeocoder" : false
    , "specialLocationView" : true
    , "navigationAppConfig" : {
      "query" : "google.navigation:q=%f,%f"
    , "packageName" : "com.google.android.apps.maps"
    , "walkQuery" : "google.navigation:q=%f,%f&mode=w"
  }
  , "SUPPORT_EMAIL" : "yatrisathi.support@wb.gov.in"
  , "OTP_MESSAGE_REGEX" : "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
  , "internationalNumberEnabled" : true
  , "callOptions" : ["ANONYMOUS", "DIRECT"]
  , "showNearByDrivers" : true
  , "features" : {
    "enableAutoReadOtp" : true ,
    "enableZooTicketBookingFlow" : true ,
    "enableAdditionalServices" : true,
      }
  , "showDisabilityBanner" : false
  , "showCheckoutRentalBanner" : true
  , "countryCodeConfig" : [
    {
      "countryName" : "US" 
    , "countryCode"  : "+1" 
    , "countryShortCode" : "US"
    },
    {
      "countryName" : "India" 
    , "countryCode"  : "+91" 
    , "countryShortCode" : "IN"
    }
  ]
  , "notifyRideConfirmationConfig" : 
      { "notify" : false, 
        "autoGeneratedText" : "Nomoshkar, I'll arrive in about "
      }
    , "estimateAndQuoteConfig" : 
      { "variantTypes" : [ ["AUTO_RICKSHAW"], ["SUV"], ["SEDAN", "TAXI_PLUS"], ["HATCHBACK"], ["TAXI"], ["BOOK_ANY"], ["BIKE"], ["SUV_PLUS"], ["DELIVERY_BIKE"],["AMBULANCE_VENTILATOR"],["AMBULANCE_AC_OXY"], ["AMBULANCE_AC"],["AMBULANCE_TAXI_OXY"] ,["AMBULANCE_TAXI"], ["HERITAGE_CAB"]]
      , "variantOrder" : ["AUTO_RICKSHAW", "BIKE", "BOOK_ANY", "HATCHBACK", "TAXI", "SEDAN", "TAXI_PLUS", "SUV", "SUV_PLUS", "DELIVERY_BIKE" , "AMBULANCE_VENTILATOR","AMBULANCE_AC_OXY", "AMBULANCE_AC","AMBULANCE_TAXI_OXY" ,"AMBULANCE_TAXI"]
      , "enableOnlyAuto" : false
      , "showNearByDrivers": true
      , "enableBookingPreference" : false
      , "showInfoIcon" : true
      , "genericLoaderLottie" : "ic_vehicle_processing,https://assets.moving.tech/beckn/jatrisaathi/user/lottie/ny_ic_generic_loader.json"
      , "variantInfo" : {
          "hatchback" : {
            "name" : "Hatchback",
            "image" : "ny_ic_hatchback_ac_side,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ic_hatchback_ac.png",
            "leftViewImage" : "ny_ic_hatchback_left_view,"
          },
          "taxiPlus" : {
            "name" : "AC Taxi",
            "image" : "ny_ic_sedan_ac_side,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_sedan_ac.png",
            "leftViewImage" : "ny_ic_sedan_left_view,"
          },
          "sedan" : {
            "name" : "Sedan",
            "image" : "ny_ic_sedan_ac_side,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_sedan_ac.png",
            "leftViewImage" : "ny_ic_sedan_left_view,"
          },
          "taxi" : {
            "name" : "Non AC Taxi",
            "image" : "ny_ic_taxi_side,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ic_taxi.png",
            "leftViewImage" : "ny_ic_hatchback_left_view,"
          },
          "suv" : {
            "name" : "SUV",
            "image" : "ny_ic_suv_ac_side,https://assets.juspay.in/beckn/jatrisaathi/jatrisaathicommon/images/ny_ic_suv_ac_side.png",
            "leftViewImage" : "ny_ic_suv_left_view,"
          },
          "autoRickshaw" : {
            "name" : "Auto Rickshaw",
            "image" : "ny_ic_auto_shadow,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_auto_shadow.png",
            "leftViewImage" : "ny_ic_auto_left_view,"
          },
          "evAutoRickshaw" : {
            "name" : "EV Auto Rickshaw",
            "image" : "ny_ic_auto_shadow,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_auto_shadow.png",
            "leftViewImage" : "ny_ic_auto_left_view,"
          },
          "bike" : {
            "name" : "Bike",
            "image": "ny_ic_bike_side,https://assets.moving.tech/beckn/jatrisaathi/user/images/ny_ic_bike_side.png",
            "leftViewImage" : "ny_ic_bike_left_side,https://assets.moving.tech/beckn/jatrisaathi/user/images/ny_ic_bike_left_side.png"
          },
          "bookAny" : {
            "name" : "Book Any",
            "image" : "ny_ic_cab_multiple,https://assets.juspay.in/beckn/jatrisaathicommon/images/ny_ic_cab_multiple.png",
            "leftViewImage" : ","
          },
          "suvPlus" : {
            "name" : "XL Plus",
            "image" : "ny_ic_suv_plus_side,https://assets.juspay.in/beckn/jatrisaathicommon/images/ny_ic_suv_plus_side.png",
            "leftViewImage" : "ny_ic_suv_plus_left_side,"
          },
          "deliveryBike" : {
            "name" : "2 Wheeler",
            "image" : "ny_ic_bike_side,https://assets.juspay.in/beckn/jatrisaathi/driver/images/ny_ic_bike_side.png",
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
            "image" : "ny_ic_heritage_cab_side,https://assets.moving.tech/beckn/jatrisaathi/jatrisaathicommon/images/ny_ic_heritage_cab_side.png",
            "leftViewImage" : "ny_ic_heritage_cab_left_side,https://assets.moving.tech/beckn/jatrisaathi/jatrisaathicommon/images/ny_ic_heritage_cab_left_side.png"
          }
        }
      }
    , "dashboard" : {
      "enable" : true
      , "url" : "https://yatrisathi.in/open/?source=in-app"
    }
    , "feature" : {
      "enableAutoReadOtp" : true ,
      "enableZooTicketBookingFlow" : true,
      "enableShareRide" : true,
      "enableAdditionalServices" : true,
      "enableSafetyFlow" : true,
      "enableEditPickupLocation" : true,
      "enableCustomerSupportForSafety" : true, 
      "enableSupport" : false,
      "enableHelpAndSupport" : true,
      "enableEditDestination" : true,
      "enableBusBooking" : true
    } 
    , "suggestedTripsAndLocationConfig" : {
      "minLocationsToBeShown" : 1,
      "minTripsToBeShown" : 1,
    }
    , "enableDeliveryService" : true
  })
}