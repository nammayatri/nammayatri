window.version = window.version || {};
let version = "1.0.0";
if (typeof __VERSION__ !== "undefined") {
  version = __VERSION__
}
window.version["configuration"]= version;
window.getMerchantConfig = function () {
  return JSON.stringify({
      "homeScreen" : {
        "primaryBackground" : "#FFFFFF",
        "pickUpViewColor" : "#EFEFEF",
        "pickupLocationTextColor" : "#1D1D1D",
        "bannerViewVisibility" : false,
        "header" : {
          "menuButtonBackground" : "#F6F6F6",
          "showLogo" : false,
          "titleColor" : "#1D1D1D",
          "showSeparator" : true,
        },
        "whereToButton" : {
          "margin" : {
            "top" : 3
          },
          "shadow" : {
            "x": 0.3,
            "y": 0.3,
            "blur": 3.0,
            "spread": 10.0
          }
        }
      },
      "StringKeys": ["YOUR_LOCATION_HELPS_OUR_SYSTEM", "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL", "YOUR_FEEDBACK_HELPS_US"],
      "isReferralEnabled": "false",
      "showBookingPreference": "true",
      "showRateCard": "true",
      "showDashboard": "false",
      "enableShareRide": "false",
      "shareAppTitle": "Share App!",
      "shareAppContent": "Hey there!\n\nCheck India's first Zero Commission auto booking app.\n100% Open source | 100% Open Data\n\nDownload Namma Yatri now! \nhttps://nammayatri.in/link/rider/SJ8D \n\n #beOpen #chooseOpen",
      "DOCUMENT_LINK": "https://docs.google.com/document/d/1-oRR_oI8ncZRPZvFZEJZeCVQjTmXTmHA",
      "appLink": "",
      "PRIVACY_POLICY_LINK": "https://docs.google.com/document/d/128VU80K5E1iz-x6QnP1R127m_lwmDO3F",
      "CUSTOMER_TIP": "true",
      "isShareAppEnabled": "false",
      "addFavouriteScreenBackArrow": "ny_ic_chevron_left_white,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_left_white.png",
      "showPickUpandDrop": true,
      "apiLoaderLottie": "https://assets.juspay.in/beckn/mobilityredbus/user/lottie/primary_button_loader.json",
      "isEmergencyContacts": "false",
      "showCorporateAddress": false,
      "isChatEnabled": "false",
    "loaderColor": "#D63941",
    "loaderConfig" : {
      "color": "#D63941"
    },
      "isGradient": "false",
      "gradient": [],
      "primaryTextColor": "#FFFFFF",
      "primaryBackground": "#D63941",
        "estimateConfirmText": "",
        "autoConfirmingLoaderColor": "#80B2FF",
        "quoteListModelBackground": "#2C2F3A",
        "confirmPickUpLocationBorder": "#E5E7EB",
        "currency": "₹",
        "alertDialogPrimaryColor": "#BD5500",
        "logs": ["JUSPAY"],
        "primaryButtonCornerRadius": 8.0,
        "cancelSearchTextColor": "#D84E55",
      "quoteListItemConfig": {
        "primaryButtonCorner": 8.0,
        "expiresColor": "#DC3312",
        "driverImagebg": "#FED0E3",
        "vehicleHeight": 57,
        "vehicleWidth": 57
		 },
      "searchLocationConfig": {
        "searchLocationTheme": "#D84E55",
        "setLocationOnMapColor": "#2C2F3A",
        "strokeColor": "1,#818181",
        "enableLocationTagbar" : "true",
        "editTextBackground" : "#EFEFEF",
        "editTextDefaultColor" : "#818181",
        "backgroundColor" : "#FFFFFF",
        "separatorColor" : "#E4E4E4",
        "showSeparator" : true,
        "editTextColor" : "#818181",
        "hintColor" : "#E4E4E4",
        "resultsCardCornerRadius": 8.0,
        "showRateCardDetails": true,
        "lottieHeight": 96,
        "lottieWidth": 96,
        "primaryButtonHeight": 60,
        "backArrow": "ny_ic_chevron_left,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_left.png"
        },
        "ratingConfig": {
        "secondaryButtonTextColor": "#1D1D1D",
        "secondaryButtonStroke": "1,#1D1D1D",
        "buttonCornerRadius": 8.0
        },
        "cancelReasonConfig": {
        "secondaryButtonTextColor": "#1D1D1D",
        "secondaryButtonStroke": "1,#1D1D1D",
        "buttonCornerRadius": 8.0
        },
        "quoteListModel": {
        "backgroundColor": "#FFFFFF",
        "textColor": "#4F4F4F",
        "separatorColor" : "#E4E4E4",
        "showSeparator" : true,
        "loaderColor": "#80B2FF",
        "otpTextBackground": "#FFFFFF",
        "otpBackground": "#FFF0F6",
        "otpTextColor": "#3C2062",
        "otpTitleColor": "#6D7280",
        "selectRideTextColor": "#454545",
        "lineImage": "ic_line",
        "lottieHeight": 300,
        "lottieWidth": 300,
        "topMargin": 0,
        "noQuotesImageHeight": 115,
        "noQuotesImageWidth": 137,
        "closeIcon": "ny_ic_close,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_close.png",
        },
        "driverInfoConfig": {
        "ratingTextColor": "#D84E55",
        "ratingBackground": "#FFF0F6",
        "ratingStroke": "1,#717171",
        "ratingCornerRadius": 8.0,
        "callBackground": "#2053BB6F",
        "callButtonStroke": "0,#EB0055",
        "cardStroke": "1,#E5E7EB",
        "otpStroke": "1,#717171",
        "callHeight": 16,
        "callWidth": 16,
        "showNumberPlatePrefix": true,
        "showNumberPlateSuffix": false,
        "numberPlateBackground": "#E9BE4D",
        "showCancelPrevention": false,
        "showTrackingButton": false,
        "footerVisibility" : true
      , "footerImageUrl" : "ic_namma_yatri_logo,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_namma_yatri_logo.png"
      , "footerBackgroundColor" : "#FFFFFF"
        },
        "profileBackground": "#EFEFEF",
        "profileName": "#1D1D1D",
        "profileImage": "#012A72",
        "feedbackBackground": "#D63941",
        "profileCompletion": "#FFFFFF",
        "profileArrowImage": "ny_ic_chevron_right,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_chevron_right.png",
        "showProfileStatus": false,
        "cancelRideColor": "#BD5500",
        "merchantLogo": "ic_launcher,https://assets.juspay.in/beckn/mobilityredbus/user/images/ny_ic_launcher.png",
        "infoIconUrl": "ny_ic_info,https://assets.juspay.in/nammayatri/images/user/ny_ic_information_grey.png",
        "sideBarList": ["MyRides", "Favorites", "HelpAndSupport", "Language", "About"],
        "rateCardColor": "#D84E55",
        "profileEditGravity": "center",
        "nyBrandingVisibility": true,
        "fontType": "System",
        "black900": "#1D1D1D",
        "black800": "#454545",
        "showHamMenu": true,
        "showQuoteFindingText": false,
        "red": "#DC3312",
        "popupBackground": "#FFFFFF",
        "englishStrings": {
        "YOUR_LOCATION_HELPS_OUR_SYSTEM": "Your location helps our system to map down all the near by autos and get you the quickest ride possible.",
        "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL": "Website: <u>https://nammayatri.in/</u>",
        "YOUR_FEEDBACK_HELPS_US": "Your feedback helps us improve the Namma Yatri experience"
        },
        "hindiStrings": {
        "YOUR_LOCATION_HELPS_OUR_SYSTEM": "आपका स्थान हमारे सिस्टम को आस-पास के सभी ऑटो को मैप करने और आपको सबसे तेज़ संभव सवारी दिलाने में मदद करता है।",
        "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL": "वेबसाइट: <u>https://nammayatri.in/</u>",
        "YOUR_FEEDBACK_HELPS_US": "आपकी प्रतिक्रिया हमें नम्मा यात्री अनुभव को बेहतर बनाने में मदद करती है"
        },
        "kannadaStrings": {
        "YOUR_LOCATION_HELPS_OUR_SYSTEM": "ನಿಮ್ಮ ಸ್ಥಳವು ನಮ್ಮ ಸಿಸ್ಟಂ ಅನ್ನು ಆಟೋಗಳ ಮೂಲಕ ಮ್ಯಾಪ್ ಮಾಡಲು ಸಹಾಯ ಮಾಡುತ್ತದೆ ಮತ್ತು ನಿಮಗೆ ತ್ವರಿತವಾದ ಸವಾರಿ ಸಾಧ್ಯ.",
        "CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL": "ವೆಬ್‌ಸೈಟ್: <u>https://nammayatri.in/</u>",
        "YOUR_FEEDBACK_HELPS_US": "ನಿಮ್ಮ ಪ್ರತಿಕ್ರಿಯೆ ನಮ್ಮ ಯಾತ್ರಿ ಅನುಭವವನ್ನು ಸುಧಾರಿಸಲು ನಮಗೆ ಸಹಾಯ ಮಾಡುತ್ತದೆ"
        },
        "languageList": [{
        "name": "English",
        "value": "EN_US",
        "subTitle": ""
        }, {
        "name": "ಕನ್ನಡ",
        "value": "KN_IN",
        "subTitle": "Kannada"
        }, {
        "name": "हिंदी",
        "value": "HI_IN",
        "subTitle": "Hindi"
        }],
        "bannerConfig": {
        "backgroundColor": "#F0FAF0",
        "title": "Complete your profile for a personalised ride experience",
        "titleColor": "#21C179",
        "actionText": "Update now",
        "actionTextColor": "#27AE5F",
        "imageUrl": "ny_ic_banner_gender_feat,https://assets.juspay.in/beckn/merchantcommon/images/ny_ic_banner_gender_feat.png"
        },
        "terminateBtnConfig": {
        "visibility": true,
        "title": "Redbus",
        "backgroundColor" : "#F6F6F6",
        "imageUrl": "ny_ic_chevron_left,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_left.png"
        },
        "showDeleteAccount": false,
        "fontName": "PlusJakartaSans",
			"enableMockLocation": false,
			"fontKannada": "NotoSansKannada",
			"defaultLanguage": "EN_US",
			"dashboardUrl" : "",
			"showGenderBanner": false,
			"autoSelectBackground": "#53BB6F",
			"OTP_MESSAGE_REGEX": "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+",
			"SUPPORT_EMAIL": "nammayatri.support@juspay.in",
			"specialLocationView": false,
			"rideCompletedGradient": "#F2A2A2",
			"navigationAppConfig": {
			"packageName": "com.google.android.apps.maps",
        "query": "google.navigation:q=%f,%f"
        }
    , "features" : {
        "enableAutoReadOtp" : false
      , "enableLiveDashboard" : false
    }
    , "estimateAndQuoteConfig" : { "enableOnlyAuto" : true
    }
    , "rideCompletedCardConfig" : {
        "topCard" : {
          "gradient" : "#F2A2A2"
        , "enableGradient" : false
        , "background" : "#1D1D1D"
        }
      }
    , "enableContactSupport" : false
    , "feature" : {
      "enableAutoReadOtp" : false,
      "enableShareRide" : false,
      "enableChat": false,
      "enableEmergencyContacts": false,
      "enableReferral": false,
      "enableSupport": false,
    }
    , "dashboard" : {
      "enable" : false
    }
    , "fontConfig" : {
      "type" : "System"
    }
  })
}