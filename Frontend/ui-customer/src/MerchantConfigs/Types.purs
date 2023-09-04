module MerchantConfig.Types where
import Prelude
import Styles.Types (FontType)

type AppConfig =
  {
    primaryTextColor :: String,
    primaryBackground :: String,
    currency :: String,
    estimateConfirmText :: String,
    autoConfirmingLoaderColor :: String,
    quoteListModelBackground :: String,
    quoteListModel :: QuoteListConfig,
    profileBackground :: String,
    isGradient :: String,
    gradient :: Array String,
    showPickUpandDrop :: Boolean,
    profileName :: String,
    profileImage :: String,
    profileCompletion :: String,
    feedbackBackground :: String,
    sideBarList :: Array String,
    rateCardColor :: String,
    showHamMenu :: Boolean,
    showQuoteFindingText :: Boolean,
    nyBrandingVisibility :: Boolean,
    fontType :: String,
    languageList :: Array Language,
    confirmPickUpLocationBorder ::String,
    bannerConfig :: BannerViewState,
    popupBackground :: String,
    cancelRideColor :: String,
    infoIconUrl :: String,
    profileEditGravity :: String,
    merchantLogo :: String,
    logs :: Array String,
    showCorporateAddress :: Boolean,
    searchLocationConfig :: SearchLocationConfig,
    quoteListItemConfig :: QuoteListItemConfig,
    alertDialogPrimaryColor :: String,
    driverInfoConfig :: DriverInfoConfig,
    ratingConfig :: RatingConfig,
    primaryButtonCornerRadius :: Number,
    cancelSearchTextColor :: String,
    cancelReasonConfig :: CancelReasonConfig,
    terminateBtnConfig :: TerminateBtnConfig,
    showDeleteAccount :: Boolean
  , autoSelectBackground :: String
  , showGenderBanner :: Boolean
  , enableMockLocation :: Boolean
  , specialLocationView :: Boolean
  , internationalNumberEnabled :: Boolean
  , dashboardUrl :: String 
  , callOptions :: Array String
  , autoVariantEnabled :: Boolean
  , showDisabilityBanner :: Boolean
  } 

type QuoteListItemConfig = {
  primaryButtonCorner :: Number,
  expiresColor :: String,
  driverImagebg :: String,
  vehicleHeight :: Int,
  vehicleWidth :: Int
}

type RatingConfig = {
  secondaryButtonTextColor :: String,
  secondaryButtonStroke :: String,
  buttonCornerRadius :: Number
}

type CancelReasonConfig = {
  secondaryButtonTextColor :: String,
  secondaryButtonStroke :: String,
  buttonCornerRadius :: Number
}

type DriverInfoConfig = {
  ratingTextColor :: String,
  ratingBackground :: String,
  ratingStroke :: String,
  ratingCornerRadius :: Number,
  callBackground :: String,
  showTrackingButton :: Boolean,
  callButtonStroke :: String,
  cardStroke :: String,
  otpStroke :: String,
  showNumberPlatePrefix :: Boolean,
  showNumberPlateSuffix :: Boolean,
  callHeight :: Int,
  callWidth :: Int
, numberPlateBackground :: String
, showCancelPrevention :: Boolean
}

type SearchLocationConfig = {
  searchLocationTheme :: String, 
  setLocationOnMapColor :: String, 
  strokeColor :: String,
  enableLocationTagbar :: String,
  resultsCardCornerRadius :: Number,
  showRateCardDetails :: Boolean,
  showAdditionalChargesText :: Boolean,
  lottieHeight :: Int,
  lottieWidth :: Int,
  primaryButtonHeight :: Int
, backArrow :: String
}

type QuoteListConfig = {
  backgroundColor :: String,
  textColor :: String,
  loaderColor :: String,
  otpTextBackground :: String,
  otpBackground :: String,
  otpTextColor :: String,
  otpTitleColor :: String,
  selectRideTextColor :: String,
  lineImage :: String,
  lottieHeight :: Int,
  lottieWidth :: Int,
  topMargin :: Int,
  noQuotesImageHeight :: Int,
  noQuotesImageWidth :: Int,
  closeIcon :: String
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
type TerminateBtnConfig = {
    visibility :: Boolean, 
    title :: String,
    imageUrl :: String
}