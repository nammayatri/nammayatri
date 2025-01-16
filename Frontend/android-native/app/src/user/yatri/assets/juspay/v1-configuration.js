window.version = window.version || {};
let version = "1.0.0";
if (typeof __VERSION__ !== "undefined") {
  version = __VERSION__
}
window.version["configuration"]= version;

function getAppLink(os) {
  if (os == "ANDROID") {
    return "https://play.google.com/store/apps/details?id=net.openkochi.yatri"
  } else {
    return "https://apps.apple.com/in/app/yatri/id1615871038"
  }
}
window.getMerchantConfig = function () {
  return JSON.stringify({
    "StringKeys": [
      "REQUEST_AUTO_RIDE",
      "CURRENTLY_WE_ARE_LIVE_IN_",
      "DRIVER_PICKUP_CHARGES",
      "SUCCESSFUL_ONBOARD",
      "ABOUT_REFERRAL_PROGRAM_DISCRIPTION",
      "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL",
      "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL",
      "CALL_NAMMA_YATRI_SUPPORT",
      "YOUR_FEEDBACK_HELPS_US",
      "LEARN_HOW_TEXT",
      "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE",
      "ACCESSIBILITY_TEXT",
      "TO_CATER_YOUR_SPECIFIC_NEEDS",
      "PLEASE_ENABLE_LOCATION_PERMISSION",
      "REPORT_ISSUE_CHAT_PLACEHOLDER",
      "CALL_SUPPORT_DESCRIPTION" ,
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
    "showRateCard": "false",
    "showDashboard": "false",
    "enableShareRide": "true",
    "autoVariantEnabled": true,
    "shareAppTitle": "Share Yatri!",
    "shareAppContent": "Hey there!\n\nCheck out Yatri, India's first open mobility cab hailing app built for commuters in Kochi.\n100% Open Source\n\nDownload Yatri now! \n" + (getAppLink(window.__OS)) + "\n\n#beOpen #chooseOpen",
    "DOCUMENT_LINK": "https://docs.google.com/document/d/1zmQWO_L4EjyCXC3xSlp1f3DS2wI4HfbHxg42tXelWe0",
    "PRIVACY_POLICY_LINK": "https://docs.google.com/document/d/1gI_P4oZnVwE0O71rI4Mi8rpZbL9rsIRkyewbql85Np8",
    "appLink": getAppLink(window.__OS),
    "showChargeDesc" : true,
    "isShareAppEnabled": "true",
    "apiLoaderLottie": "primary_button_loader.json",
    "isChatEnabled": "true",
    "primaryTextColor": "#FCC32C",
    "primaryBackground": "#2C2F3A",
    "showCorporateAddress" : true,
    "gradient": [],
    "loaderColor": "",
    "showHamMenu" : true,
    "showQuoteFindingText" : false,
    "addFavouriteScreenBackArrow" : "ny_ic_chevron_left_white,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_left_white.png",
    "searchLocationTheme": "#2C2F3A",
    "estimateConfirmText": "Request a NammaYatri Ride",
    "autoConfirmingLoaderColor": "#80B2FF",
    "showPickUpandDrop": true,
    "currency": "₹",
    "alertDialogPrimaryColor": "#2194FF",
    "isGradient" : "false",
    "quoteListModelBackground": "#2C2F3A",
    "dashboard" : {
      "enable" : false
      , "url" : "https://www.getyatri.com/open/?source=in-app"
    },
    "shareAppConfig" : {
      "title" : "Share Yatri!"
      , "description" : "Hey there!\n\nCheck out Yatri, India's first open mobility cab hailing app built for commuters in Kochi.\n100% Open Source\n\nDownload Yatri now! \n" + (getAppLink(window.__OS)) + "\n\n#beOpen #chooseOpen"
    },
    "logs": ["JUSPAY","FIREBASE","CLEVERTAP"],
    "primaryButtonCornerRadius" : 8.0,
    "cancelSearchTextColor": "#E55454",
    "dashboardUrl" : "https://www.getyatri.com/open/?source=in-app",
    "searchLocationConfig": {
      "enableRateCard" : false,
      "showChargeDesc" : true
    },
    "quoteListModel": {
      "topMargin": 100
    },
    "profileBackground": "#2C2F3A",
    "profileEditGravity" : "center",
    "profileName": "#FFFFFF",
    "profileImage": "#012A72",
    "tipEnabledCities" : ["Bangalore", "Hyderabad"],
    "callOptions" : ["ANONYMOUS", "DIRECT"],
    "tipDisplayDuration" : 30,
    "profileCompletion" : "#FCC32C",
    "feedbackBackground": "#2C2F3A",
    "confirmPickUpLocationBorder": "#E5E7EB",
    "cancelRideColor" : "#E55454",
    "infoIconUrl" : "ny_ic_info,https://assets.juspay.in/nammayatri/images/user/ny_ic_information_grey.png",
    "merchantLogo" : "ic_launcher,https://assets.juspay.in/beckn/yatri/user/images/ic_launcher.png",
    "sideBarList": ["MyRides", "Favorites", "NammaSafety", "MetroTickets", "EmergencyContacts", "HelpAndSupport", "Language", "Separator", "ShareApp", "About", "Logout"],
    "rateCardColor": "#2C2F3A",
    "nyBrandingVisibility": false,
    "fontType": "Assets",
    "black900": "#2C2F3A",
    "black800": "#454545",
    "red" : "#E55454",
    "popupBackground" : "#FFFFFF",
    "englishStrings": {
      "MOST_LOVED_APP" : "",
      "REQUEST_AUTO_RIDE": "Request Ride",
      "CURRENTLY_WE_ARE_LIVE_IN_": "Currently we're live in Kochi, you can enjoy our services there",
      "DRIVER_PICKUP_CHARGES": "Service Charges",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT" : "You are about to place a call to the Yatri Support Team. Do you want to proceed?",
      "SUCCESSFUL_ONBOARD": "You have successfully signed on to \n Yatri",
      "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "The referral program incentivises drivers to accept more rides, cancel less and serve you better by recognising and rewarding worthy drivers. \n\n You can help out by entering the driver’s referral code  and improve the quality of rides for the Yatri Community!",
      "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "\nYou can get a referral code by asking your Yatri Driver.",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM" : "Your location helps our system to map down all the near by taxis and get you the quickest ride possible.",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "Welcome to Yatri \nTo start booking rides, please allow us to find you!",
      "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL" : "Moving Tech Innovations Private Limited <br> Indiqube Garden, Old no. 648, New No. 130, Grape Garden, 18th Main, 1st A Cross, 6th Block, Koramangala, Bangalore 560095.",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" : "Website: <u>https://www.getyatri.com/</u>",
      "CALL_NAMMA_YATRI_SUPPORT" : "Call Yatri Support",
      "YOUR_FEEDBACK_HELPS_US" : "Your feedback helps us improve the Yatri experience",
      "LEARN_HOW_TEXT" : "Learn how Yatri caters to your needs",
      "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE": "Driver addition limits are in increments of ₹10",
      "ACCESSIBILITY_TEXT" : "Yatri, now customised for you!",
      "TO_CATER_YOUR_SPECIFIC_NEEDS" : "To cater to your specific needs, we have customised certain features of Yatri.",
      "PLEASE_ENABLE_LOCATION_PERMISSION" : "Please enable location permissions for Yatri from the settings app to start looking for rides.",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "Describe your issue. Yatri will try to resolve it in under 24 hours.",
      "CALL_SUPPORT_DESCRIPTION" : "You are about to place a call to Yatri Support. Do you want to proceed?",
      "WE_HOPE_THE_ISSUE_IS_RESOLVED" : "We hope the issue is resolved, feel free to write to us at support@nammayatri.in for any queries.",
      "WHO_CAN_TRACK_YOUR_RIDE" : "Who can follow your ride on Yatri App",
      "ALERT_SAFETY_TEAM" : "Alert Yatri Safety Team",
      "RECOMMEND_EMERGENCY_CONTACTS_TO_INSTALL" : "We recommend emergency contacts to install Yatri for an enhanced tracking experience",
      "SAFETY_TEAM_WILL_BE_ALERTED" : "Upon SOS  Yatri safety team will be notified",
      "SHARE_RIDE_DESCRIPTION" : "On choosing this, Yatri will send app push notification to the emergency contacts",
      "PLEASE_STAY_CALM_TEAM_ALERTED" : "Please stay calm, Yatri safety team is alerted!",
      "INDICATION_TO_EMERGENCY_CONTACTS" : "Yatri will indicate to your emergency contacts that this is a test drill, ensuring a stress-free experience.",
      "EMERGENCY_CONTACTS_CAN_TAKE_ACTION" : "Emergency Contacts can follow/ take emergency response actions on Yatri App"
    },
    "malayalamStrings": {
      "MOST_LOVED_APP" : "",
      "REQUEST_AUTO_RIDE" : "റൈഡ് അഭ്യർത്ഥിക്കുക",
      "CURRENTLY_WE_ARE_LIVE_IN_": "നിലവിൽ ഞങ്ങളുടെ സേവനം ബെംഗളൂരുവിലും മൈസുരുവിലും ആണ് ഉള്ളത്, നിങ്ങൾക്ക് അവിടെ ഞങ്ങളുടെ സേവനങ്ങൾ ആസ്വദിക്കാൻ കഴിയും",
      "DRIVER_PICKUP_CHARGES": "ഡ്രൈവർ പിക്കപ്പ് നിരക്കുകൾ",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT" : "നിങ്ങൾ യാത്രി സപ്പോർട്ട് ടീമിലേക്ക് ഒരു കോൾ ചെയ്യാൻ പോകുകയാണ്. നിങ്ങൾക്ക് തുടരണോ?",
      "SUCCESSFUL_ONBOARD" : "നിങ്ങൾ വിജയകരമായി \n നമ്മ യാത്രിയിലേക്ക് പ്രവേശിച്ചിരിക്കുന്നു",
      "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "യോഗ്യരായ ഡ്രൈവർമാരെ കണ്ടെത്തി അവരെ അഭിനന്ദിക്കുകയും, തക്ക പ്രതിഫലം നൽകുകയും അതിലൂടെ കൂടുതൽ റൈഡറുകൾ സ്വീകരിക്കാനും, ക്യാൻസല്ലേഷൻസ് കുറക്കാനും, അതുവഴി നിങ്ങളെ മെച്ചപ്പെട്ട രീതിയിൽ സേവിക്കുവാനും ഡ്രൈവർമാരെ പ്രോത്സാഹിപ്പിക്കുകയും ചെയ്യാൻ ഉതകുന്ന ഒരു പദ്ധതി ആണ് റഫറൽ പ്രോഗ്രാം. ഡ്രൈവറുടെ റെഫെറൽ കോഡ് എന്റർ ചെയ്യുന്നതിലൂടെ നിങ്ങൾക്കും, നമ്മ യാത്രി കൂട്ടായ്മയ്ക്ക് വേണ്ടി റൈഡ്കളുടെ നിലവാരം പൊതുവെ മെച്ചപ്പെടുത്താൻ സഹായിക്കാവുന്നതാണ്!",
      "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "നിങ്ങളുടെ നമ്മ യാത്രി ഡ്രൈവറോട് ചോദിച്ചാൽ ഒരു റഫറൽ കോഡ് ലഭിക്കും.",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM" : "നിങ്ങളുടെ ലൊക്കേഷൻ ഞങ്ങളുടെ സിസ്റ്റത്തെ ടാക്സികൾ വഴി മാപ്പ് ചെയ്യാൻ സഹായിക്കുന്നു.",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "യാത്രിയിലേക്ക് സ്വാഗതം \nറൈഡുകൾ ബുക്കിംഗ് ആരംഭിക്കാൻ, നിങ്ങളെ കണ്ടെത്താൻ ഞങ്ങളെ അനുവദിക്കൂ!",
      "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL" : "മൂവിംഗ് ടെക് ഇന്നൊവേഷൻസ് പ്രൈവറ്റ് ലിമിറ്റഡ് <br> ഇൻഡിക്യൂബ് ഗാർഡൻ, പഴയ നമ്പർ. 648, പുതിയ നമ്പർ. 130, ഗ്രേപ്പ് ഗാർഡൻ, 18-ാം മെയിൻ, ഒന്നാം എ ക്രോസ്, ആറാം ബ്ലോക്ക്, കോറമംഗല, ബാംഗ്ലൂർ 560095.",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" : "വെബ്‌സൈറ്റ്: <u>https://www.getyatri.com/</u>",
      "CALL_NAMMA_YATRI_SUPPORT" : "യാത്രി സപ്പോർട്ടിലേക്ക് വിളിക്കുക",
      "YOUR_FEEDBACK_HELPS_US" : "യാത്രാ അനുഭവം മെച്ചപ്പെടുത്താൻ നിങ്ങളുടെ ഫീഡ്‌ബാക്ക് ഞങ്ങളെ സഹായിക്കുന്നു",
      "LEARN_HOW_TEXT" : "യാത്രി നിങ്ങളുടെ ആവശ്യങ്ങൾ എങ്ങനെ നിറവേറ്റുന്നുവെന്ന് അറിയുക",
      "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE": "ഡ്രൈവർ കൂട്ടിച്ചേർക്കൽ പരിധികൾ ₹10 ഇൻക്രിമെന്റിലാണ്",
      "ACCESSIBILITY_TEXT" : "യാത്രി, ഇപ്പോൾ നിങ്ങൾക്കായി ഇഷ്‌ടാനുസൃതമാക്കി!",
      "TO_CATER_YOUR_SPECIFIC_NEEDS" : "നിങ്ങളുടെ പ്രത്യേക ആവശ്യങ്ങൾ നിറവേറ്റുന്നതിനായി, ഞങ്ങൾ യാത്രിയുടെ ചില സവിശേഷതകൾ ഇഷ്‌ടാനുസൃതമാക്കിയിട്ടുണ്ട്.",
      "PLEASE_ENABLE_LOCATION_PERMISSION" : "റൈഡുകൾക്കായി തിരയാൻ ക്രമീകരണ ആപ്പിൽ നിന്ന് യാത്രയ്‌ക്ക് ലൊക്കേഷൻ അനുമതികൾ പ്രവർത്തനക്ഷമമാക്കുക.",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "നിങ്ങളുടെ പ്രശ്നം വിവരിക്കുക. 24 മണിക്കൂറിനുള്ളിൽ അത് പരിഹരിക്കാൻ യാത്രി ശ്രമിക്കും.",
      "CALL_SUPPORT_DESCRIPTION" : "നിങ്ങൾ യാത്രി സപ്പോർട്ടിലേക്ക് ഒരു കോൾ ചെയ്യാൻ പോകുകയാണ്. നിങ്ങൾക്ക് തുടരണോ?",
      "WE_HOPE_THE_ISSUE_IS_RESOLVED" : "പ്രശ്‌നം പരിഹരിച്ചുവെന്ന് ഞങ്ങൾ പ്രതീക്ഷിക്കുന്നു, എന്തെങ്കിലും ചോദ്യങ്ങൾക്ക് support@nammayatri.in എന്ന വിലാസത്തിൽ ഞങ്ങൾക്ക് എഴുതാൻ മടിക്കേണ്ടതില്ല.",
      "WHO_CAN_TRACK_YOUR_RIDE": "ആരുടെ യാത്ര ആപ്പിൽ നിന്നും നിങ്ങളുടെ യാത്ര പിന്തുണയ്ക്കാം",
      "ALERT_SAFETY_TEAM": "ഹാജരാക്കൽ യാത്ര സുരക്ഷാ ടീം",
      "RECOMMEND_EMERGENCY_CONTACTS_TO_INSTALL": "യാത്രികൾക്കായി അഭിനംദനങ്ങൾ ഇൻസ്റ്റാൾ ചെയ്യാനായി എമർജൻസി കോൺടാക്ടുകൾ ഇൻസ്റ്റാൾ ചെയ്യാൻ നാം ശുപാർശ ചെയ്യുന്നു",
      "SAFETY_TEAM_WILL_BE_ALERTED": "SOS ലെയ്പിലുള്ള സമയത്ത് യാത്ര സുരക്ഷാ ടീമിനെ അറിയിക്കുന്നതും",
      "SHARE_RIDE_DESCRIPTION": "ഇത് തിരഞ്ഞെടുക്കുമ്പോൾ, യാത്രി ആപ്പ് പുതിയ യാത്രക്കുള്ള അപ്ലിക്കേഷൻ പുശ് നോട്ടിഫിക്കേഷൻ അതിരുകൾക്ക് അയയ്ക്കും",
      "PLEASE_STAY_CALM_TEAM_ALERTED": "ദയവായി നിങ്ങൾക്ക് നിരീക്ഷണം നിരസിച്ചിരിക്കാനായി യാത്ര സുരക്ഷാ ടീമിനെ അറിയിക്കുക!",
      "INDICATION_TO_EMERGENCY_CONTACTS": "ഈ ഒരു പരീക്ഷണ പ്രവർത്തനം ആകുന്നത് നിങ്ങളുടെ അത്തം കാൾമുറിക്കുകളെ അറിയിപ്പ് ചെയ്യുന്നതാണെന്ന് യാത്ര പരിചയകൾ തെളിയിക്കും",
      "EMERGENCY_CONTACTS_CAN_TAKE_ACTION" : "എമർജൻസി കോൺടാക്ടുകൾ യാത്ര ആപ്പിൽ നിന്നും പിന്തുണയ്ക്കാം/ എമർജൻസി പ്രതിക്രിയകൾ എടുക്കാം"
    },
    "hindiStrings" : {
      "MOST_LOVED_APP" : "",
      "REQUEST_AUTO_RIDE" : "सवारी का अनुरोध करें",
      "CURRENTLY_WE_ARE_LIVE_IN_": "वर्तमान में हम कोच्चि में रहते हैं, आप वहां हमारी सेवाओं का आनंद ले सकते हैं",
      "DRIVER_PICKUP_CHARGES": "सेवा शुल्क",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT" : "आप यात्री सहायता टीम को कॉल करने वाले हैं। क्या आप आगे बढ़ना चाहते हैं?",
      "SUCCESSFUL_ONBOARD" : "आपने \nयात्री पर सफलतापूर्वक साइन इन कर लिया है",
      "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "रेफ़रल कार्यक्रम ड्राइवरों को अधिक सवारी स्वीकार करने, कम रद्द करने और योग्य ड्राइवरों को पहचानने और पुरस्कृत करके आपको बेहतर सेवा देने के लिए प्रोत्साहित करता है। \n\n आप ड्राइवर का रेफरल कोड दर्ज करके मदद कर सकते हैं और सवारी की गुणवत्ता में सुधार कर सकते हैं यात्री समुदाय!",
      "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "\nआप अपने यात्री ड्राइवर से पूछकर एक रेफरल कोड प्राप्त कर सकते हैं।",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM" : "आपका स्थान हमारे सिस्टम को आस-पास की सभी टैक्सियों को मैप करने और आपको यथासंभव त्वरित यात्रा दिलाने में मदद करता है।",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "यात्री में आपका स्वागत है \nसवारी की बुकिंग शुरू करने के लिए, कृपया हमें आपको ढूंढने की अनुमति दें!",
      "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL" : "मूविंग टेक इनोवेशन प्राइवेट लिमिटेड <br> इंडीक्यूब गार्डन, पुराना नंबर। 648, नया नंबर 130, ग्रेप गार्डन, 18वां मुख्य, पहला ए क्रॉस, 6वां ब्लॉक, कोरमंगला, बैंगलोर 560095।",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" : "वेबसाइट: <u>https://www.getyatri.com/</u>",
      "CALL_NAMMA_YATRI_SUPPORT" : "यात्री सहायता को कॉल करें",
      "YOUR_FEEDBACK_HELPS_US" : "आपका फीडबैक हमें यात्रा अनुभव को बेहतर बनाने में मदद करता है",
      "LEARN_HOW_TEXT" : "जानें कि यात्री आपकी आवश्यकताओं को कैसे पूरा करता है",
      "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE": "ड्राइवर जोड़ने की सीमा ₹10 की वृद्धि में है",
      "ACCESSIBILITY_TEXT" : "यात्री, अब आपके लिए अनुकूलित!",
      "TO_CATER_YOUR_SPECIFIC_NEEDS" : "आपकी विशिष्ट आवश्यकताओं को पूरा करने के लिए, हमने यात्री की कुछ विशेषताओं को अनुकूलित किया है।",
      "PLEASE_ENABLE_LOCATION_PERMISSION" : "सवारी की तलाश शुरू करने के लिए कृपया सेटिंग ऐप से यात्री के लिए स्थान अनुमतियाँ सक्षम करें।",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "अपनी समस्या बताएं। यात्री 24 घंटे के अंदर इसे हल करने का प्रयास करेगा।",
      "CALL_SUPPORT_DESCRIPTION" : "आप यात्री सहायता को कॉल करने वाले हैं। क्या आप आगे बढ़ना चाहते हैं?",
      "WE_HOPE_THE_ISSUE_IS_RESOLVED" : "हमें उम्मीद है कि समस्या सुलझ गई है, किसी भी प्रश्न के लिए बेझिझक हमें support@nammayatri.in पर लिखें।",
      "WHO_CAN_TRACK_YOUR_RIDE": "यात्री ऐप पर आपकी सवारी को कौन ट्रैक कर सकता है",
      "ALERT_SAFETY_TEAM": "अलर्ट यात्री सुरक्षा टीम",
      "RECOMMEND_EMERGENCY_CONTACTS_TO_INSTALL": "हम बेहतर ट्रैकिंग अनुभव के लिए आपातकालीन संपर्कों को यात्री इंस्टॉल करने की सलाह देते हैं",
      "SAFETY_TEAM_WILL_BE_ALERTED": "एसओएस पर यात्री सुरक्षा टीम को सूचित किया जाएगा",
      "SHARE_RIDE_DESCRIPTION": "इसे चुनने पर, यात्री आपातकालीन संपर्कों को ऐप पुश नोटिफिकेशन भेजेगा",
      "PLEASE_STAY_CALM_TEAM_ALERTED": "कृपया शांत रहें, यात्री सुरक्षा दल सतर्क है!",
      "INDICATION_TO_EMERGENCY_CONTACTS": "यात्री आपके आपातकालीन संपर्कों को संकेत देगा कि यह एक परीक्षण ड्रिल है, जो तनाव मुक्त अनुभव सुनिश्चित करेगा।"
    },
    "languageList": [{
      "name": "English",
      "value": "EN_US",
      "subTitle": "ഇംഗ്ലീഷ്"
    }, {
      "name": "മലയാളം",
      "value": "ML_IN",
      "subTitle": "Malayalam"
    },{
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
    , "autoSelectBackground" : "#53BB6F"
    , "enableMockLocation" : false
    , "defaultLanguage" : "EN_US"
    , "isEmergencyContacts": "true"
    , "enableGeocoder" : false
    , "metroTicketingConfig" : [
      { "cityName" : "kochi"
      , "cityCode" : "std:0484"
      , "customEndTime" : "23:59:59"
      , "customDates" : ["03/05/2024", "04/05/2024", "05/05/2024", "06/05/2024", "07/05/2024", "08/05/2024", "09/05/2024", "10/05/2024","11/05/2024"]
      , "metroStationTtl" : 10080
      , "metroHomeBannerImage" : "ny_ic_kochi_metro_banner"
      , "metroBookingBannerImage" : "ny_ic_kochi_metro_banner"
      , "bookingStartTime" : "05:45:00"
      , "bookingEndTime" : "22:30:00"
      , "ticketLimit" : {
        "roundTrip" : 1
      , "oneWay" : 6
    }
      },
      { "cityName" : "chennai"
      , "cityCode" : "std:040"
      , "customEndTime" : "01:00:00"
      , "customDates" : ["23/04/2024","28/04/2024","01/05/2024","12/05/2024"]
      , "metroStationTtl" : 10080
      , "metroHomeBannerImage" : "ny_ic_chennai_metro_discount_banner"
      , "metroBookingBannerImage" : "ny_ic_chennai_metro_banner"
      , "bookingStartTime" : "04:30:00"
      , "bookingEndTime" : "22:30:00"
      , "ticketLimit" : {
          "roundTrip" : 6
        , "oneWay" : 6
      }
      }
    ]
    , "specialLocationView" : true
    , "isAdvancedBookingEnabled" : true
    , "navigationAppConfig" : {
      "query" : "google.navigation:q=%f,%f"
      , "packageName" : "com.google.android.apps.maps"
    }
    , "SUPPORT_EMAIL" : "support@nammayatri.in"
    , "OTP_MESSAGE_REGEX" : "is your OTP for login to [A-Za-z]+ [A-Za-z]+"
    , "showDisabilityBanner" : false
    , "enableContactSupport" : false
    , "estimateAndQuoteConfig" : {
      "enableBookingPreference" : true,
      "variantTypes" : [ ["SUV"], ["HATCHBACK", "TAXI_PLUS", "SEDAN"], ["TAXI"], ["AUTO_RICKSHAW"] ],
      "variantOrder" : ["AUTO_RICKSHAW", "BOOK_ANY"],
      "variantInfo" : {
        "hatchback" : {
          "name" : "Hatchback",
          "image" : "ic_hatchback_ac,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ic_hatchback_ac.png",
          "leftViewImage": "ny_ic_hatchback_left_view,https://assets.moving.tech/beckn/common/user/images/ny_ic_hatchback_left_view.png"
          },
        "taxiPlus" : {
          "name" : "AC Taxi",
          "image" : "ny_ic_sedan_ac,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_sedan_ac.png",
          "leftViewImage": "ny_ic_sedan_left_view,https://assets.moving.tech/beckn/common/user/images/ny_ic_sedan_left_view.png"
        },
        "sedan" : {
          "name" : "Sedan",
          "image" : "ny_ic_sedan_ac_new,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_sedan_ac.png",
          "leftViewImage": "ny_ic_sedan_left_view,https://assets.moving.tech/beckn/common/user/images/ny_ic_sedan_left_view.png"
        },
        "taxi" : {
          "name" : "Non-AC Taxi",
          "image" : "ic_taxi,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ic_taxi.png",
          "leftViewImage": "ny_ic_sedan_left_view,https://assets.moving.tech/beckn/common/user/images/ny_ic_sedan_left_view.png"
        },
        "suv" : {
          "name" : "SUV",
          "image" : "ic_suv_ac,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ic_suv_ac.png",
          "leftViewImage": "ny_ic_suv_left_view,https://assets.moving.tech/beckn/common/user/images/ny_ic_suv_left_view.png"
        },
        "autoRickshaw" : {
          "name" : "Auto Rickshaw",
          "image" : "ny_ic_auto_shadow,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_auto_shadow.png",
          "leftViewImage": "ny_ic_auto_left_view,https://assets.moving.tech/beckn/common/user/images/ny_ic_auto_left_view.png"
        },
        "evAutoRickshaw" : {
          "name" : "EV Auto Rickshaw",
          "image" : "ny_ic_auto_shadow,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_auto_shadow.png",
          "leftViewImage": "ny_ic_auto_left_view,https://assets.moving.tech/beckn/common/user/images/ny_ic_auto_left_view.png"
        },
        "bookAny" : {
          "name" : "Book Any",
          "image" : "ny_ic_auto_cab_green,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_auto_cab_green.png"
        },
        "bike" : {
          "name" : "Bike",
          "image": "ny_ic_bike_side,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_bike_side.png",
          "leftViewImage" : "ny_ic_bike_left_side,https://assets.moving.tech/beckn/nammayatri/user/images/ny_ic_bike_left_side.png",
        },
        "suvPlus" : {
          "name" : "XL Plus",
          "image" : "ny_ic_suv_plus_side,https://assets.moving.tech/beckn/yatri/user/images/ny_ic_suv_plus_side.png",
          "leftViewImage" : "ny_ic_suv_plus_left_side,https://assets.moving.tech/beckn/yatri/user/images/ny_ic_suv_plus_left_side.png"
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
          "image" : "ny_ic_heritage_cab_side,https://assets.moving.tech/beckn/yatri/user/yatricommon/ny_ic_heritage_cab_side.png",
          "leftViewImage" : "ny_ic_heritage_cab_left_side,https://assets.moving.tech/beckn/yatri/yatricommon/images/ny_ic_heritage_cab_left_side.png"
        }
      }
    }
    , "feature" : {
      "enableShareRide" : true,
      "enableSupport": false,
      "enableEditDestination" : true,
      "enableEditPickupLocation" : true
    }
    , "otpRegex" :  "is your OTP for login to [A-Za-z]+ [A-Za-z]+"
    , "termsLink" :"https://docs.google.com/document/d/1zmQWO_L4EjyCXC3xSlp1f3DS2wI4HfbHxg42tXelWe0"
    , "termsVersion" : 1.0
    , "privacyLink" : "https://docs.google.com/document/d/1gI_P4oZnVwE0O71rI4Mi8rpZbL9rsIRkyewbql85Np8"
    , "appData" : {
      "link" : getAppLink(window.__OS)
      , "supportMail" :"support@nammayatri.in"
      , "name" : "Yatri"
      , "website" : "https://www.getyatri.com/"
      , "logoLight" : ""
    }
  })
}