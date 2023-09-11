module MerchantConfig.Types where

type AppConfig =
  {
    primaryTextColor :: String,
    primaryBackground :: String,
    fontType :: String,
    languageList :: Array Language,
    popupBackground :: String,
    defaultLanguage :: String,
    imageUploadOptional :: Boolean,
    leaderBoard :: LeaderBoard
  } 

type Language =  {
  name :: String,
  value :: String,
  subtitle :: String
 }

type LeaderBoard = {
  isMaskedName :: Boolean
}