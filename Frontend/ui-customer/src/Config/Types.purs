module Config.Types where

import Prelude
import Styles.Types (FontType)

type AppConfig =
  {
    primaryTextColor :: String,
    primaryBackground :: String,
    merchantId :: String,
    searchLocationTheme :: String,
    estimateConfirmText :: String,
    autoConfirmingLoaderColor :: String,
    quoteListModelBackground :: String,
    quoteListModel :: QuoteListConfig,
    profileBackground :: String,
    profileName :: String,
    profileImage :: String,
    feedbackBackground :: String,
    sideBarList :: Array String,
    otpBackground :: String,
    otpTextColor :: String,
    rateCardColor :: String,
    nyBrandingVisibility :: Boolean,
    fontType :: String,
    confirmPickUpLocationBorder ::String,
    logs :: Array String
  } 

type QuoteListConfig = {
  backgroundColor :: String,
  textColor :: String,
  loaderColor :: String
}