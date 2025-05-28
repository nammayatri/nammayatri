module MerchantConfig.Types where

import Common.Types.Config
import Data.Maybe
import Foreign.Object (Object)

type AppConfig = AppConfigDriver CommonAppConfig

type AppConfigDriver a =
  {
    logWhitelistConfig :: LogWhitelistConfig,
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
    inAppKeyboardModalConfig :: InAppKeyboardModalConfig,
    chooseCity :: ChooseCityScreenConfig,
    safetyRide :: SafetyRideConfig,
    appUpdatePopupUrl :: String,
    showProfileAadhaarPan :: Boolean,
    clientName :: String,
    rateCardScreen :: RateCardScreenConfig,
    rcLimit :: Int,
    acExplanation :: Boolean,
    showMonthlyLeaderBoard :: Boolean,
    hotspotConfig :: HotspotConfig,
    rentalRideVideoConfig :: RentalRideVideoConfig,
    scheduledRideConfig :: ScheduledRideConfig,
    customerTokenForMeterRide :: String
    | a
  }

type LogWhitelistConfig = {
  aadhaarVerificationScreenLogWhitelist :: Array String,
  aboutUsScreenLogWhitelist :: Array String,
  acknowledgementScreenLogWhitelist :: Array String,
  addVehicleDetailsScreenLogWhitelist :: Array String,
  applicationStatusScreenLogWhitelist :: Array String,
  bankDetailScreenLogWhitelist :: Array String,
  benefitsLogWhitelist :: {benefitsScreenLogWhitelist :: Array String, lmsQuizScreenLogWhitelist :: Array String, lmsVideoScreenLogWhitelist :: Array String},
  bookingOptionsScreenLogWhitelist :: Array String,
  cancellationRateScreenLogWhitelist :: Array String,
  chooseCityScreenLogWhitelist :: Array String,
  chooseLanguageScreenLogWhitelist :: Array String,
  customerReferralTrackerScreenLogWhitelist :: Array String,
  documentCaptureScreenLogWhitelist :: Array String,
  documentDetailsScreenLogWhitelist :: Array String,
  driverCompleteProfileScreenLogWhitelist :: Array String,
  driverDetailsScreenLogWhitelist :: Array String,
  driverEarningsScreenLogWhitelist :: Array String,
  driverProfileScreenLogWhitelist :: Array String,
  driverRideRatingScreenLogWhitelist :: Array String,
  driverSavedLocationScreenLogWhitelist :: Array String,
  editAadhaarDetailsScreenLogWhitelist :: Array String,
  editBankDetailsScreenLogWhitelist :: Array String,
  enterMobileNumberScreenLogWhitelist :: Array String,
  enterOTPScreenLogWhitelist :: Array String,
  helpAndSupportScreenLogWhitelist :: Array String,
  homeScreenLogWhitelist :: Array String,
  hotspotScreenLogWhitelist :: Array String,
  metroWarriorsScreenLogWhitelist :: Array String,
  notificationsScreenLogWhitelist :: Array String,
  obBoardingSubscriptionScreenLogWhitelist :: Array String,
  paymentHistoryScreenLogWhitelist :: Array String,
  permissionsScreenLogWhitelist :: Array String,
  popUpScreenLogWhitelist :: Array String,
  rateCardScreenLogWhitelist :: Array String,
  referralScreenLogWhitelist :: Array String,
  registrationScreenLogWhitelist :: Array String,
  reportIssueChatScreenLogWhitelist :: Array String,
  rideHistoryScreenLogWhitelist :: Array String,
  rideRequestScreenLogWhitelist :: Array String,
  rideSelectionScreenLogWhitelist :: Array String,
  rideSummaryScreenLogWhitelist :: Array String,
  scheduledRideAcceptedScreenLogWhitelist :: Array String,
  selectLanguageScreenLogWhitelist :: Array String,
  splashScreenLogWhitelist :: Array String,
  subscriptionScreenLogWhitelist :: Array String,
  tripDetailsScreenLogWhitelist :: Array String,
  uploadAdhaarScreenLogWhitelist :: Array String,
  uploadDrivingLicenseScreenLogWhitelist :: Array String,
  uploadParcelImageScreenLogWhitelist :: Array String,
  vehicleDetailsScreenLogWhitelist :: Array String,
  welcomeScreenLogWhitelist :: Array String,
  writeToUsScreenLogWhitelist :: Array String
}

type RentalRideVideoConfig = {
  auto :: String,
  cab :: String
}

type ScheduledRideConfig = {
  scheduledBannerTimerValue :: Int
}

type PurpleRideConfig = {
  showPurpleVideos :: Boolean,
  visualImpairmentVideo :: String,
  physicalImpairmentVideo :: String,
  hearingImpairmentVideo :: String,
  genericAccessibilityVideo :: String
  }

type GeoCodeConfig = {
  radius :: Int,
  strictBounds :: Boolean
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
  showFeeBreakup :: Boolean,
  noChargesTillDate :: String,
  lowestFeesFromDate :: String
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
  thresholdTime :: Int,
  routeDistance :: Int,
  diffBtwTwoHeartBeats :: Int,
  straightLineDist :: Number
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
, enableAutoReferral :: Boolean
, enableSpecialPickup :: Boolean
, enableInterOperability :: Boolean
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
, customerAppId :: String
, driverAppId :: String
}

type RideRequestConfig = {
  negotiationUnit :: NegotiationUnit
}

type NegotiationUnit = {
  auto :: String,
  cab :: String
}

type CityConfig = {
  cityName :: String,
  mapImage :: String,
  cityCode :: String,
  showSubscriptions :: Boolean,
  enableAdvancedBooking :: Boolean,
  advancedRidePopUpYoutubeLink :: String,
  callDriverInfoPost :: Boolean,
  cityLat :: Number,
  cityLong :: Number,
  supportNumber :: String,
  languageKey :: String,
  showDriverReferral :: Boolean,
  showCustomerReferral :: Boolean,
  uploadRCandDL :: Boolean,
  enableYatriCoins :: Boolean,
  registration :: RegistrationConfig,
  vehicleNSImg :: String,
  variantSubscriptionConfig :: VariantSubscriptionConfig,
  showEarningSection :: Boolean,
  referral :: Referral,
  waitingCharges :: Number,
  waitingChargesConfig :: WaitingChargesConfig,
  rentalWaitingChargesConfig :: WaitingChargesConfig,
  rateCardConfig :: RateCardConfig,
  gstPercentage :: String,
  assets :: Assets,
  enableHvSdk :: Boolean,
  enableGullak :: Boolean,
  purpleRideConfig :: PurpleRideConfigForVehicle,
  rideStartAudio :: RideStartAudio,
  showScheduledRides :: Boolean,
  chatFooterBanner :: Maybe ChatFootBannerConfig,
  enableNammaMeter :: Maybe (Object Boolean), -- Deprecated. Use openMeter instead.
  openMeter :: Maybe OpenMeterConfig
}

type OpenMeterConfig = {
  enable :: Maybe (Object Boolean),
  addDestination :: Boolean,
  shareRideTracking :: Boolean
}


type ChatFootBannerConfig = {
  defaultText :: String,
  translations :: Object String,
  enable :: Boolean
}

type InAppKeyboardModalConfig = {
  enableDeviceKeyboard :: Boolean
}
type ChooseCityScreenConfig = {
  straightLineDistLogic :: Boolean
}

type SafetyRideConfig = {
  startTime :: String
, endTime :: String
}
type RateCardScreenConfig = {
  showYoutubeVideo :: Boolean,
  showRateCard :: Boolean,
  showTollCharges :: Boolean,
  showDriverAdditions :: Boolean
}

type RateCardConfig = {
  showLearnMore :: Boolean,
  learnMoreVideoLink :: String
}

type Assets ={
  auto_image :: String,
  onboarding_auto_image :: String,
  empty_referral_auto :: String,
  empty_referral_cab :: String
}

type PurpleRideConfigForVehicle = {
  purpleRideConfigForAuto :: VariantToDisabilityVideo,
  purpleRideConfigForCabs :: VariantToDisabilityVideo,
  purpleRideConfigForBikes :: VariantToDisabilityVideo
}

type RideStartAudio = {
  acCab :: StartAudioUrls,
  nonAcCab :: StartAudioUrls,
  auto :: StartAudioUrls,
  bike :: StartAudioUrls
}

type StartAudioUrls = {
  tollAudio :: Maybe String,
  acAudio :: Maybe String,
  parkingAudio :: Maybe String,
  defaultAudio :: Maybe String
}

type VariantToDisabilityVideo = {
  vehicleVariant :: String,
  showVideo :: Boolean,
  disabilityToVideo :: Array DisabilityToVideo,
  genericVideoForVariant :: String
}

type DisabilityToVideo = {
  disabilityType :: String,
  videoUrl :: String
}

type VariantSubscriptionConfig = {
  enableVariantBasedSubscription :: Boolean,
  variantList :: Array String,
  enableCabsSubscriptionView :: Boolean,
  staticViewPlans :: Array StaticViewPlans
}

type RegistrationConfig = {
  supportWAN :: String,
  callSupport :: Boolean,
  whatsappSupport :: Boolean
}

type StaticViewPlans = {
  price :: Number,
  frequency :: String,
  variantCategory :: String,
  name :: String,
  introductoryOffer :: String,
  showSelected :: Boolean,
  planDesc :: String
}

type WaitingChargesConfig = {
  cab :: ChargesEntity,
  auto :: ChargesEntity,
  bike :: ChargesEntity,
  ambulance :: ChargesEntity
}

type HotspotConfig = {
  veryHighHotspotColor :: String,
  highHotspotColor :: String,
  moderateHotspotColor :: String,
  veryHighRange :: Number,
  highRange :: Number,
  circleRadius ::Number,
  centerDeviation :: Number,
  showColorWithRelativeWeight :: Boolean,
  minCirclesNeededForSortedWeights :: Int
}
