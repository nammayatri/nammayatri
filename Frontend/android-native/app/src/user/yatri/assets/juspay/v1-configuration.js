window.getMerchantConfig = function () {
  return JSON.stringify({
    "StringKeys": ["ABOUT_APP_DESCRIPTION", "WELCOME_TEXT", "REQUEST_AUTO_RIDE", "CURRENTLY_WE_ARE_LIVE_IN_", "DRIVER_PICKUP_CHARGES", "SUCCESSFUL_ONBOARD", "ABOUT_REFERRAL_PROGRAM_DISCRIPTION", "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER", "YOUR_LOCATION_HELPS_OUR_SYSTEM", "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT","LOCATION_PERMISSION_SUBTITLE_NEW_USER"],
    "isReferralEnabled": "false",
    "showBookingPreference": "false",
    "showRateCard": "false",
    "showDashboard": "false",
    "enableShareRide": "false",
    "shareAppTitle": "Share Yatri!",
    "shareAppContent": "Hey there!\n\nCheck out Yatri, India's first open mobility cab hailing app built for commuters in Kochi.\n100% Open Source\n\nDownload Yatri now! \n" + (getAppLink(window.__OS)) + "\n\n#beOpen #chooseOpen",
    "DOCUMENT_LINK": "https://docs.google.com/document/d/1zmQWO_L4EjyCXC3xSlp1f3DS2wI4HfbHxg42tXelWe0",
    "PRIVACY_POLICY_LINK": "https://docs.google.com/document/d/1gI_P4oZnVwE0O71rI4Mi8rpZbL9rsIRkyewbql85Np8",
    "APP_LINK": getAppLink(window.__OS),
    "CUSTOMER_TIP": "false",
    "isShareAppEnabled": "true",
    "apiLoaderLottie": "primary_button_loader.json",
    "isChatEnabled": "true",
    "primaryTextColor": "#FCC32C",
    "primaryBackground": "#2C2F3A",
    "searchLocationTheme": "#2C2F3A",
    "estimateConfirmText": "Request a NammaYatri Ride",
    "autoConfirmingLoaderColor": "#80B2FF",
    "quoteListModelBackground": "#2C2F3A",
    "logs": ["JUSPAY","FIREBASE"],
    "quoteListModel": {
      "backgroundColor": "#2C2F3A",
      "textColor": "#FFFFFF",
      "loaderColor": "#80B2FF", 
      "otpTextBackground"  : "#2C2F3A",
      "otpBackground" : "#F1F1F1",
      "otpTextColor" : "#FFFFFF",
      "otpTitleColor"  : "#6D7280"
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
    "merchantLogo" : "ic_launcher,https://assets.juspay.in/yatri/images/user/ny_ic_launcher.png",
    "sideBarList": ["MyRides", "Favorites", "EmergencyContacts", "HelpAndSupport", "Language", "Separator", "ShareApp", "About", "Logout"],
    "rateCardColor": "#2C2F3A",
    "nyBrandingVisibility": false,
    "fontType": "Assets",
    "black900": "#2C2F3A",
    "addFavouriteScreenBackArrow" : "ny_ic_chevron_left_white,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_left_white.png",
    "black800": "#454545",
    "red" : "#E55454",
    "popupBackground" : "#FFFFFF",
    "englishStrings": {
      "ABOUT_APP_DESCRIPTION": "Yatri is an open platform to connect commuters with transport providers. The app makes it convenient for travellers to find available means of transport, and avail these options by connecting them with service providers ",
      "WELCOME_TEXT": "Welcome to Yatri",
      "REQUEST_AUTO_RIDE": "Request Ride",
      "CURRENTLY_WE_ARE_LIVE_IN_": "Currently we're live in Kochi, you can enjoy our services there",
      "DRIVER_PICKUP_CHARGES": "Service Charges",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "You are about to place a call to the Yatri Support Team. Do you want to proceed?",
      "SUCCESSFUL_ONBOARD": "You have successfully signed on to \n Yatri",
      "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "The referral program incentivises drivers to accept more rides, cancel less and serve you better by recognising and rewarding worthy drivers. \n\n You can help out by entering the driver’s referral code  and improve the quality of rides for the Yatri Community!",
      "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "\nYou can get a referral code by asking your Yatri Driver.",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "Your location helps our system to map down all the near by taxis and get you the quickest ride possible.",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "Welcome to Yatri \nTo start booking rides, we require your device location."
    },
    "malayalamStrings": {
      "WELCOME_TEXT": "യാത്രയിലേക്ക് സ്വാഗതം",
      "ABOUT_APP_DESCRIPTION": "യാത്രക്കാരെ ഡ്രൈവർമാരുമായി ബന്ധിപ്പിക്കുന്നതിനുള്ള ഒരു ഓപ്പൺ പ്ലാറ്റ്‌ഫോമാണ് നമ്മ യാത്രി. ഈ ആപ്പ് റൈഡർമാർക്ക് മീറ്റർ നിരക്കിൽ റൈഡുകൾ ബുക്ക് ചെയ്യാൻ സൗകര്യപ്രദമാക്കുന്നു, അതിനാൽ നിരക്ക് കുറഞ്ഞിരിക്കും.",
      "REQUEST_AUTO_RIDE": "റൈഡ് അഭ്യർത്ഥിക്കുക",
      "CURRENTLY_WE_ARE_LIVE_IN_": "നിലവിൽ ഞങ്ങളുടെ സേവനം ബെംഗളൂരുവിലും മൈസുരുവിലും ആണ് ഉള്ളത്, നിങ്ങൾക്ക് അവിടെ ഞങ്ങളുടെ സേവനങ്ങൾ ആസ്വദിക്കാൻ കഴിയും",
      "DRIVER_PICKUP_CHARGES": "ഡ്രൈവർ പിക്കപ്പ് നിരക്കുകൾ",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "നിങ്ങൾ യാത്രി സപ്പോർട്ട് ടീമിലേക്ക് ഒരു കോൾ ചെയ്യാൻ പോകുകയാണ്. നിങ്ങൾക്ക് തുടരണോ?",
      "SUCCESSFUL_ONBOARD": "നിങ്ങൾ വിജയകരമായി \n നമ്മ യാത്രിയിലേക്ക് പ്രവേശിച്ചിരിക്കുന്നു",
      "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "യോഗ്യരായ ഡ്രൈവർമാരെ കണ്ടെത്തി അവരെ അഭിനന്ദിക്കുകയും, തക്ക പ്രതിഫലം നൽകുകയും അതിലൂടെ കൂടുതൽ റൈഡറുകൾ സ്വീകരിക്കാനും, ക്യാൻസല്ലേഷൻസ് കുറക്കാനും, അതുവഴി നിങ്ങളെ മെച്ചപ്പെട്ട രീതിയിൽ സേവിക്കുവാനും ഡ്രൈവർമാരെ പ്രോത്സാഹിപ്പിക്കുകയും ചെയ്യാൻ ഉതകുന്ന ഒരു പദ്ധതി ആണ് റഫറൽ പ്രോഗ്രാം. ഡ്രൈവറുടെ റെഫെറൽ കോഡ് എന്റർ ചെയ്യുന്നതിലൂടെ നിങ്ങൾക്കും, നമ്മ യാത്രി കൂട്ടായ്മയ്ക്ക് വേണ്ടി റൈഡ്കളുടെ നിലവാരം പൊതുവെ മെച്ചപ്പെടുത്താൻ സഹായിക്കാവുന്നതാണ്!",
      "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "നിങ്ങളുടെ നമ്മ യാത്രി ഡ്രൈവറോട് ചോദിച്ചാൽ ഒരു റഫറൽ കോഡ് ലഭിക്കും.",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "നിങ്ങളുടെ ലൊക്കേഷൻ ഞങ്ങളുടെ സിസ്റ്റത്തെ ടാക്സികൾ വഴി മാപ്പ് ചെയ്യാൻ സഹായിക്കുന്നു.",
      "LOCATION_PERMISSION_SUBTITLE_NEW_USER" : "യാത്രിയിലേക്ക് സ്വാഗതം \nബുക്കിംഗ് റൈഡുകൾ ആരംഭിക്കാൻ, ഞങ്ങൾക്ക് നിങ്ങളുടെ ഉപകരണ ലൊക്കേഷൻ ആവശ്യമാണ്."
    },
    "languageList": [{
      "name": "English",
      "value": "EN_US",
      "subTitle": ""
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
    "showCorporateAddress" : false
  })
}

function getAppLink(os) {
  if (os == "ANDROID") {
    return "https://play.google.com/store/apps/details?id=net.openkochi.yatri"
  } else {
    return "https://apps.apple.com/in/app/yatri/id1615871038"
  }
}