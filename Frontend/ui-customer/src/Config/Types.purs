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
    isFirebaseEnabled :: Boolean
  } 

type QuoteListConfig = {
  backgroundColor :: String,
  textColor :: String,
  loaderColor :: String
}