module Config.Types where

import Prelude


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
    sideBarList :: Array String,
    otpBackground :: String,
    otpTextColor :: String,
    rateCardColor :: String,
    nyBrandingVisibility :: Boolean
  } 

type QuoteListConfig = {
  backgroundColor :: String,
  textColor :: String,
  loaderColor :: String
}