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
    "StringKeys": ["ABOUT_APP_DESCRIPTION",
      "WELCOME_TEXT", "REQUEST_AUTO_RIDE",
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
      "MOST_LOVED_APP",
      "ABOUT_SOS_12",
      "SAFETY_MEASURE_5",
      "SHARE_LOCATION_AND_RIDE_DETAILS_WITH_NAMMA_YATRI_SUPPORT_TEAM",
      "TRIGGER_ALERT_TO_NAMMA_YATRI_SUPPORT",
      "TRIGGER_ALERT_TO_NAMMAYATRI_SUPPORT_TITLE",
      "PERSONAL_SAFETY_ACTION_2"
    ],
    "isReferralEnabled": "true",
    "showBookingPreference": "false",
    "showRateCard": "false",
    "showDashboard": "false",
    "enableShareRide": "false",
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
    "showCorporateAddress" : false,
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
    "profileCompletion" : "#FCC32C",
    "feedbackBackground": "#2C2F3A",
    "confirmPickUpLocationBorder": "#E5E7EB",
    "cancelRideColor" : "#E55454",
    "infoIconUrl" : "ny_ic_info,https://assets.juspay.in/nammayatri/images/user/ny_ic_information_grey.png",
    "merchantLogo" : "ic_launcher,https://assets.juspay.in/beckn/yatri/user/images/ic_launcher.png",
    "sideBarList": ["MyRides", "Favorites", "NammaSafety", "EmergencyContacts", "HelpAndSupport", "Language", "Separator", "ShareApp", "About", "Logout"],
    "rateCardColor": "#2C2F3A",
    "nyBrandingVisibility": false,
    "fontType": "Assets",
    "black900": "#2C2F3A",
    "black800": "#454545",
    "red" : "#E55454",
    "popupBackground" : "#FFFFFF",
    "englishStrings": {
      "ABOUT_APP_DESCRIPTION": "Yatri is an open platform to connect commuters with transport providers. The app makes it convenient for travellers to find available means of transport, and avail these options by connecting them with service providers ",
      "WELCOME_TEXT": "Welcome to Yatri",
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
      "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL" : "",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" : "",
      "CALL_NAMMA_YATRI_SUPPORT" : "Call Yatri Support",
      "YOUR_FEEDBACK_HELPS_US" : "Your feedback helps us improve the Yatri experience",
      "LEARN_HOW_TEXT" : "Learn how Yatri caters to your needs",
      "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE": "Driver addition limits are in increments of ₹10",
      "ACCESSIBILITY_TEXT" : "Yatri, now customised for you!",
      "TO_CATER_YOUR_SPECIFIC_NEEDS" : "To cater to your specific needs, we have customised certain features of Yatri.",
      "PLEASE_ENABLE_LOCATION_PERMISSION" : "Please enable location permissions for Yatri from the settings app to start looking for rides.",
      "ABOUT_SOS_12" : "Choose an appropriate SOS option from: Call Police (112)/ Call Yatri support/ Record video (to share with emergency contacts & Yatri",
      "SAFETY_MEASURE_5" : "Safety Training and Certification for all Yatri Drivers",
      "SHARE_LOCATION_AND_RIDE_DETAILS_WITH_NAMMA_YATRI_SUPPORT_TEAM" : "Share location and ride details with Yatri Support Team",
      "TRIGGER_ALERT_TO_NAMMA_YATRI_SUPPORT" : "Trigger alert to Yatri support",
      "TRIGGER_ALERT_TO_NAMMAYATRI_SUPPORT_TITLE" : "Trigger alert to Yatri support",
      "PERSONAL_SAFETY_ACTION_2" : "Call Yatri support",
    },
    "malayalamStrings": {
      "WELCOME_TEXT" : "യാത്രയിലേക്ക് സ്വാഗതം",
      "MOST_LOVED_APP" : "",
      "ABOUT_APP_DESCRIPTION" : "യാത്രക്കാരെ ഡ്രൈവർമാരുമായി ബന്ധിപ്പിക്കുന്നതിനുള്ള ഒരു ഓപ്പൺ പ്ലാറ്റ്‌ഫോമാണ് നമ്മ യാത്രി. ഈ ആപ്പ് റൈഡർമാർക്ക് മീറ്റർ നിരക്കിൽ റൈഡുകൾ ബുക്ക് ചെയ്യാൻ സൗകര്യപ്രദമാക്കുന്നു, അതിനാൽ നിരക്ക് കുറഞ്ഞിരിക്കും.",
      "REQUEST_AUTO_RIDE" : "റൈഡ് അഭ്യർത്ഥിക്കുക",
      "CURRENTLY_WE_ARE_LIVE_IN_": "നിലവിൽ ഞങ്ങളുടെ സേവനം ബെംഗളൂരുവിലും മൈസുരുവിലും ആണ് ഉള്ളത്, നിങ്ങൾക്ക് അവിടെ ഞങ്ങളുടെ സേവനങ്ങൾ ആസ്വദിക്കാൻ കഴിയും",
      "DRIVER_PICKUP_CHARGES": "ഡ്രൈവർ പിക്കപ്പ് നിരക്കുകൾ",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT" : "നിങ്ങൾ യാത്രി സപ്പോർട്ട് ടീമിലേക്ക് ഒരു കോൾ ചെയ്യാൻ പോകുകയാണ്. നിങ്ങൾക്ക് തുടരണോ?",
      "SUCCESSFUL_ONBOARD" : "നിങ്ങൾ വിജയകരമായി \n നമ്മ യാത്രിയിലേക്ക് പ്രവേശിച്ചിരിക്കുന്നു",
      "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "യോഗ്യരായ ഡ്രൈവർമാരെ കണ്ടെത്തി അവരെ അഭിനന്ദിക്കുകയും, തക്ക പ്രതിഫലം നൽകുകയും അതിലൂടെ കൂടുതൽ റൈഡറുകൾ സ്വീകരിക്കാനും, ക്യാൻസല്ലേഷൻസ് കുറക്കാനും, അതുവഴി നിങ്ങളെ മെച്ചപ്പെട്ട രീതിയിൽ സേവിക്കുവാനും ഡ്രൈവർമാരെ പ്രോത്സാഹിപ്പിക്കുകയും ചെയ്യാൻ ഉതകുന്ന ഒരു പദ്ധതി ആണ് റഫറൽ പ്രോഗ്രാം. ഡ്രൈവറുടെ റെഫെറൽ കോഡ് എന്റർ ചെയ്യുന്നതിലൂടെ നിങ്ങൾക്കും, നമ്മ യാത്രി കൂട്ടായ്മയ്ക്ക് വേണ്ടി റൈഡ്കളുടെ നിലവാരം പൊതുവെ മെച്ചപ്പെടുത്താൻ സഹായിക്കാവുന്നതാണ്!",
      "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "നിങ്ങളുടെ നമ്മ യാത്രി ഡ്രൈവറോട് ചോദിച്ചാൽ ഒരു റഫറൽ കോഡ് ലഭിക്കും.",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM" : "നിങ്ങളുടെ ലൊക്കേഷൻ ഞങ്ങളുടെ സിസ്റ്റത്തെ ടാക്സികൾ വഴി മാപ്പ് ചെയ്യാൻ സഹായിക്കുന്നു.",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "യാത്രിയിലേക്ക് സ്വാഗതം \nറൈഡുകൾ ബുക്കിംഗ് ആരംഭിക്കാൻ, നിങ്ങളെ കണ്ടെത്താൻ ഞങ്ങളെ അനുവദിക്കൂ!",
      "REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL" : "",
      "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL" : "",
      "CALL_NAMMA_YATRI_SUPPORT" : "യാത്രി സപ്പോർട്ടിലേക്ക് വിളിക്കുക",
      "YOUR_FEEDBACK_HELPS_US" : "യാത്രാ അനുഭവം മെച്ചപ്പെടുത്താൻ നിങ്ങളുടെ ഫീഡ്‌ബാക്ക് ഞങ്ങളെ സഹായിക്കുന്നു",
      "LEARN_HOW_TEXT" : "യാത്രി നിങ്ങളുടെ ആവശ്യങ്ങൾ എങ്ങനെ നിറവേറ്റുന്നുവെന്ന് അറിയുക",
      "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE": "ഡ്രൈവർ കൂട്ടിച്ചേർക്കൽ പരിധികൾ ₹10 ഇൻക്രിമെന്റിലാണ്",
      "ACCESSIBILITY_TEXT" : "യാത്രി, ഇപ്പോൾ നിങ്ങൾക്കായി ഇഷ്‌ടാനുസൃതമാക്കി!",
      "TO_CATER_YOUR_SPECIFIC_NEEDS" : "നിങ്ങളുടെ പ്രത്യേക ആവശ്യങ്ങൾ നിറവേറ്റുന്നതിനായി, ഞങ്ങൾ യാത്രിയുടെ ചില സവിശേഷതകൾ ഇഷ്‌ടാനുസൃതമാക്കിയിട്ടുണ്ട്.",
      "PLEASE_ENABLE_LOCATION_PERMISSION" : "റൈഡുകൾക്കായി തിരയാൻ ക്രമീകരണ ആപ്പിൽ നിന്ന് യാത്രയ്‌ക്ക് ലൊക്കേഷൻ അനുമതികൾ പ്രവർത്തനക്ഷമമാക്കുക.",
      "ABOUT_SOS_12" : "കുറഞ്ഞുപയോഗിക്കാൻ പോലീസ് (112) / യാത്രി പിന്തുണ / വീഡിയോ റെക്കോർഡ് (എയർ ഇമേജന്‍സി കോണ്ടാക്ട്സ് & യാത്രി) നിരവധി എസ്ഓഎസ് ഓപ്ഷനുകൾക്ക് ഉചിതമായി തിരഞ്ഞെടുക്കുക",
      "SAFETY_MEASURE_5" : "എല്ലാ യാത്രി ഡ്രൈവറുകളും സുരക്ഷാ പ്രശിക്ഷണം ലഭിക്കുന്നു",
      "SHARE_LOCATION_AND_RIDE_DETAILS_WITH_NAMMA_YATRI_SUPPORT_TEAM" : "യാത്രി പിന്തുണ ടീംയോട് ലൊക്കേഷൻ പങ്കിടുക വാഹന വിശദാംശങ്ങൾക്കോട്",
      "TRIGGER_ALERT_TO_NAMMA_YATRI_SUPPORT" : "യാത്രി പിന്തുണയ്ക്ക് ചെയ്ത മുന്നോട്ടു ചെയ്യുക",
      "TRIGGER_ALERT_TO_NAMMAYATRI_SUPPORT_TITLE" : "യാത്രി പിന്തുണയ്ക്ക് ചെയ്ത മുന്നോട്ടു ചെയ്യുക",
      "PERSONAL_SAFETY_ACTION_2" : "യാത്രി പിന്തുണയ്ക്കുക"
    },
    "languageList": [{
      "name": "English",
      "value": "EN_US",
      "subTitle": "ഇംഗ്ലീഷ്"
    }, {
      "name": "മലയാളം",
      "value": "ML_IN",
      "subTitle": "Malayalam"
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
    , "specialLocationView" : true
    , "navigationAppConfig" : {
      "query" : "google.navigation:q=%f,%f"
      , "packageName" : "com.google.android.apps.maps"
    }
    , "SUPPORT_EMAIL" : "nammayatri.support@juspay.in"
    , "OTP_MESSAGE_REGEX" : "is your OTP for login to [A-Za-z]+ [A-Za-z]+"
    , "showDisabilityBanner" : false
  
    , "estimateAndQuoteConfig" : {
      "enableBookingPreference" : false
    }
    , "feature" : {
      "enableShareRide" : false
    }
    , "otpRegex" :  "is your OTP for login to [A-Za-z]+ [A-Za-z]+"
    , "termsLink" :"https://docs.google.com/document/d/1zmQWO_L4EjyCXC3xSlp1f3DS2wI4HfbHxg42tXelWe0"
    , "privacyLink" : "https://docs.google.com/document/d/1gI_P4oZnVwE0O71rI4Mi8rpZbL9rsIRkyewbql85Np8"
    , "appData" : {
      "link" : getAppLink(window.__OS)
      , "supportMail" :"nammayatri.support@juspay.in"
      , "name" : "Yatri"
    },
  })
}