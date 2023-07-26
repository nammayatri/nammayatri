module MerchantConfig.Types where

type AppConfig =
  {
    primaryTextColor :: String,
    primaryBackground :: String,
    fontType :: String,
    languageList :: Array Language,
    popupBackground :: String
  , defaultLanguage :: String
  } 

type Language =  {
  name :: String,
  value :: String,
  subtitle :: String
 }