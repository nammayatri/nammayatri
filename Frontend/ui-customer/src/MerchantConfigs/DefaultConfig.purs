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
      }
  , profileBackground: "#2C2F3A"
  , profileName: "#FFFFFF"
  , profileImage: "#012A72"
  , feedbackBackground: "#2C2F3A"
  , sideBarList: [ "MyRides", "Favorites", "EmergencyContacts", "HelpAndSupport", "Language", "Separator", "ShareApp", "LiveStatsDashboard", "Logout" ]
  , otpBackground: "#F1F1F1"
  , otpTextColor: "#FFFFFF"
  , rateCardColor: "#2C2F3A"
  , nyBrandingVisibility: false
  , fontType: "Assets"
  , languageList : []
  , confirmPickUpLocationBorder: "#101010"
  }
