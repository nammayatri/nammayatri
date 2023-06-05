module Config.DefaultConfig where

import Config.Types

import Styles.Colors as Color

config :: AppConfig
config =
  {
    primaryTextColor :  "#FFFFFF",
    primaryBackground : "#00B8F5",
    merchantId : "UNKNOWN",
    searchLocationTheme : Color.black900,
    estimateConfirmText : "Request a NammaYatri Ride",
    autoConfirmingLoaderColor : "#00B8F5",
    quoteListModelBackground : Color.black900,
    quoteListModel : {
      backgroundColor : "#F5F9FE",
      textColor : Color.black900,
      loaderColor : Color.black900
    },
    profileBackground : "#81DFFA",
    profileName : "#101010",
    profileImage : "#012A72",
    feedbackBackground : "#D3D3D3",
    sideBarList : ["MyRides", "Favorites", "HelpAndSupport", "Language", "About"],
    otpBackground : "#F5F9FE",
    otpTextColor : "#101010",
    rateCardColor : "#00B8F5",
    nyBrandingVisibility : true,
    fontType : "System",
    confirmPickUpLocationBorder : "#101010"
  } 