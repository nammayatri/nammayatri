module MerchantConfig.DefaultConfig where

import MerchantConfig.Types

config :: AppConfig
config =
  { primaryTextColor: "#FCC32C"
  , primaryBackground: "#2C2F3A"
  , searchLocationTheme: "#2C2F3A"
  , estimateConfirmText: "Request a NammaYatri Ride"
  , autoConfirmingLoaderColor: "#80B2FF"
  , quoteListModelBackground: "#2C2F3A"
  , quoteListModel:
      { backgroundColor: "#2C2F3A"
      , textColor: "#FFFFFF"
      , loaderColor: "#80B2FF"
      , otpTextBackground : "#2C2F3A"
      , otpBackground: "#F1F1F1"
      , otpTextColor: "#FFFFFF"
      , otpTitleColor : "#6D7280"
      }
  , profileBackground: "#2C2F3A"
  , profileName: "#FFFFFF"
  , profileImage: "#012A72"
  , feedbackBackground: "#2C2F3A"
  , sideBarList: [ "MyRides", "Favorites", "EmergencyContacts", "HelpAndSupport", "Language", "Separator", "ShareApp", "LiveStatsDashboard", "Logout" ]
  , rateCardColor: "#2C2F3A"
  , nyBrandingVisibility: false
  , fontType: "Assets"
  , languageList : []
  , confirmPickUpLocationBorder: "#101010"
  , bannerConfig : {
        backgroundColor : "#F0FAF0"
      , title : "Complete your profile for a personalised ride experience"
      , titleColor :"#269574"
      , actionText : "Update now"
      , actionTextColor : "#269574"
      , imageUrl : "ny_ic_banner_gender_feat,https://assets.juspay.in/beckn/merchantcommon/images/ny_ic_banner_gender_feat.png" 
      }
  , popupBackground : "#FFFFFF"
  , profileCompletion : "#FCC32C"
  , cancelRideColor : "#E55454"
  , infoIconUrl : "ny_ic_info,https://assets.juspay.in/nammayatri/images/user/ny_ic_information_grey.png"
  , profileEditGravity : "center"
  , merchantLogo : "ic_launcher,https://assets.juspay.in/nammayatri/images/user/ny_ic_launcher.png"
  , logs : ["JUSPAY", "FIREBASE"]
  , showCorporateAddress : false
  , terminateBtnConfig : {
          visibility: true, 
          title : "Paytm",
          imageUrl : "ny_ic_chevron_left_double,https://assets.juspay.in/beckn/mobilitypaytm/mobilitypaytmcommon/ny_ic_chevron_left_double.png"
      }
  , showDeleteAccount : false
  }
