module MerchantConfig.Types where

import Common.Types.Config

type AppConfig = AppConfigDriver CommonAppConfig

type AppConfigDriver a =
  {
    primaryTextColor :: String,
    primaryBackground :: String,
    languageList :: Array Language,
    popupBackground :: String,
    rideCompletedCardConfig :: RideCompletedCardConfig, 
    leaderBoard :: LeaderBoard,
    subscriptionConfig :: SubscriptionConfig,
    rideActionModelConfig :: RideActionModelConfig,
    profile :: ProfileConfig,
    showPaymentDetails :: Boolean,
    gotoConfig :: GotoConfig,
    profileVerification :: ProfileVerificationConfig,
    bottomNavConfig :: BottomNavConfig,
    purpleRideConfig :: PurpleRideConfig,
    mapConfig :: MapConfig,
    waitTimeConfig :: WaitTimeConfig,
    cityConfig :: Array CityConfig,
    unserviceableThreshold :: Number,
    enableMockLocation :: Boolean,
    flowConfig :: FlowConfig,
    permissions :: PermissionsConfig,
    homeScreen :: HomeScreenConfig,
    feature :: Features,
    vehicle :: VVConfig,
    allowAllMobileNumber ::Boolean,
    engilshInNative :: String,
    banners :: BannerConfig,
    referral :: ReferralConfig,
    enableDriverReferral :: Boolean,
    enableCustomerReferral :: Boolean,
    rideRequest :: RideRequestConfig,
    coinsConfig :: CoinsConfig,
    inAppKeyboardModalConfig :: InAppKeyboardModalConfig,
    chooseCity :: ChooseCityScreenConfig
    | a
  } 

type PurpleRideConfig = {
  showPurpleVideos :: Boolean,
  visualImpairmentVideo :: String,
  physicalImpairmentVideo :: String,
  hearingImpairmentVideo :: String,
  genericAccessibilityVideo :: String
  }

type Language =  {
  name :: String,
  value :: String,
  subtitle :: String
 }

type LeaderBoard = {
  isMaskedName :: Boolean
}

type ProfileVerificationConfig = {
  aadharVerificationRequired :: Boolean
}

type SubscriptionConfig =  {
  enableBlocking :: Boolean,
  completePaymentPopup :: Boolean,
  onBoardingSubscription :: Boolean,
  showLaterButtonforTimeRange :: Boolean,
  offerBannerConfig :: SubscriptionOfferBannerConfig,
  lowDuesLimit :: Number,
  maxDuesLimit :: Number,
  highDueWarningLimit :: Number,
  moveDriverToOfflineInHighDueDaily :: Boolean,
  enableSubscriptionPopups :: Boolean,
  faqLink :: String,
  supportNumber :: String,
  whatsappSupportLink :: String,
  myPlanYoutubeLink :: String,
  overlayYoutubeLink :: String,
  enableIntroductoryView :: Boolean,
  optionsMenuItems :: SubscriptionOptionsMenuItems,
  gradientConfig :: Array GradientConfig,
  enableSubscriptionSupportPopup :: Boolean,
  earnAmountInADay :: Int,
  showFeeBreakup :: Boolean
 }

type SubscriptionOfferBannerConfig = {
  showDUOfferBanner :: Boolean,
  offerBannerValidTill :: String,
  offerBannerDeadline :: String,
  offerBannerPlans :: Array String
}

type SubscriptionOptionsMenuItems = {
  managePlan :: Boolean,
  paymentHistory :: Boolean,
  viewFaqs :: Boolean,
  callSupport :: Boolean,
  chatSupport :: Boolean,
  kioskLocation :: Boolean,
  viewAutopayDetails :: Boolean
}

type GradientConfig = {
  id :: String,
  colors :: Array String
}

type RideActionModelConfig = {
  showVehicleVariant :: Boolean
}

type RideCompletedCardConfig = {
  showSavedCommission :: Boolean,
  lottieQRAnim :: Boolean
}

type ProfileConfig = {
  bookingOptionMenuForTaxi :: Boolean,
  showBookingOption :: Boolean
, checkRCStatusForBookingOption :: Boolean

}

type GotoConfig = {
  maxGotoLocations :: Int,
  enableGoto :: Boolean
}

type BottomNavConfig = {
  home :: BottomNavItemConfig,
  rideHistory :: BottomNavItemConfig,
  subscription :: BottomNavItemConfig,
  referral :: BottomNavItemConfig,
  notifications :: BottomNavItemConfig,
  driverEarnings :: BottomNavItemConfig
}

type BottomNavItemConfig = {
  showNew :: Boolean,
  isVisible :: Boolean
}

type MapConfig = {
  animationDuration :: Int
}

type WaitTimeConfig = {
  enableWaitTime :: Boolean,
  thresholdDist :: Number,
  thresholdTime :: Int
}

type FlowConfig = {
  chooseCity :: ChooseCityFlowConfig
}

type ChooseCityFlowConfig = {
  runFlow :: Boolean,
  defCity :: String
}

type PermissionsConfig = {
  locationPermission :: Boolean,
  notification :: Boolean
}

type HomeScreenConfig = {
  specialRideOtpView :: Boolean,
  showGenderBanner :: Boolean
}

type Features = {
  enableBonus :: Boolean
, enableImageUpload :: Boolean
, enableGender ::Boolean
, enableOtpRide :: Boolean
, enableSuggestions :: Boolean
, enableYatriCoins :: Boolean
}
 
 -- VV - VechileVerfication
type VVConfig = {
  validationPrefix :: String
}

type BannerConfig = {
  autoPay :: Boolean
}

type ReferralConfig = {
  type :: String
, link :: String
}

type RideRequestConfig = {
  negotiationUnit :: NegotiationUnit
}

type NegotiationUnit = {
  auto :: String,
  cab :: String
}

type CoinsConfig = {
  minCoinSliderValue :: Int,
  maxCoinSliderValue :: Int,
  numOfRideThresholdForCoins :: String,
  leaderBoardThresholdForCoins :: String,
  customerReferralCoins :: String,
  eightPlusRidesCoins :: String,
  purpleRideCoins :: String,
  rideCompletedCoins :: String,
  fiveStarRatingCoins :: String,
  oneOrTwoStarRatingCoins :: String,
  rideCancellationCoins :: String,
  whatAreYatriCoinFAQ :: String,
  howToEarnYatriCoinFAQ :: String,
  howToRedeemYatriCoinFAQ :: String
}

type InAppKeyboardModalConfig = {
  enableDeviceKeyboard :: Boolean
}
type ChooseCityScreenConfig = {
  straightLineDistLogic :: Boolean
}