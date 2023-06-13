module MerchantConfig.Types where

type AppConfig =
  {
    primaryTextColor :: String,
    primaryBackground :: String,
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
    languageList :: Array Language,
    confirmPickUpLocationBorder ::String,
    bannerConfig :: BannerViewState,
    popupBackground :: String
  } 

type QuoteListConfig = {
  backgroundColor :: String,
  textColor :: String,
  loaderColor :: String
}

type Language =  {
  name :: String,
  value :: String,
  subTitle :: String
 }

type BannerViewState = {
  backgroundColor :: String,
  title :: String,
  titleColor :: String,
  actionText :: String,
  actionTextColor :: String,
  imageUrl :: String
}