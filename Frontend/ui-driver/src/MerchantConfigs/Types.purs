module MerchantConfig.Types where

type AppConfig =
  {
    primaryTextColor :: String,
    primaryBackground :: String,
    fontType :: String,
    languageList :: Array Language
  } 

type Language =  {
  name :: String,
  value :: String,
  subtitle :: String
 }