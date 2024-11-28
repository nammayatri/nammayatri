{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Flow where

import Common.Types.Config
import ConfigProvider
import Constants.Configs
import Debug
import Helpers.Firebase
import Locale.Utils
import Log
import Mobility.Prelude
import Screens.SubscriptionScreen.Controller
import Engineering.Helpers.RippleCircles
import Common.Resources.Constants (zoomLevel)
import Common.Styles.Colors as Color
import Domain.Payments (APIPaymentStatus(..)) as PS
import Domain.Payments (PaymentStatus(..))
import Common.Types.App (Version(..), LazyCheck(..), Event, FCMBundleUpdate, CategoryListType)
import Components.ChatView.Controller (makeChatComponent')
import Constants as Constants
import Control.Monad.Except (runExceptT, runExcept)
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (any, concat, cons, elem, elemIndex, filter, find, foldl, head, last, length, mapWithIndex, null, snoc, sortBy, (!!))
import Data.Array as DA
import Resource.Constants as Const
import Data.Either (Either(..), either, isRight)
import Data.Function (on, flip)
import Data.Function.Uncurried (runFn1, runFn2)
import Data.Functor (map)
import Data.Int (ceil, fromString, round, toNumber)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, isNothing, maybe)
import Data.Number (fromString) as Number
import Data.Ord (compare)
import Data.Semigroup ((<>))
import Data.Set (toggle)
import Data.String (Pattern(..), split, toUpper, drop, indexOf, toLower, take)
import Data.String (length, null) as STR
import Data.String as DS
import Data.String.Common (joinWith, split, toUpper, trim)
import Data.String.CodeUnits (fromCharArray, toCharArray, splitAt)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse, for_)
import Data.Tuple (Tuple(..), fst, snd)
import DecodeUtil (stringifyJSON)
import Domain.Payments (APIPaymentStatus(..)) as PS
import Domain.Payments (PaymentStatus(..))
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn1, runEffectFn5, runEffectFn2, runEffectFn3, runEffectFn9, runEffectFn10)
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Engineering.Helpers.Commons (flowRunner, liftFlow, getNewIDWithTag, getVersionByKey, os, getExpiryTime, stringToVersion, setText, convertUTCtoISC, getCurrentUTC, markPerformance, setEventTimestamp, getTimeStampObject)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.LogEvent (logEvent, logEventWithParams, logEventWithMultipleParams)
import Engineering.Helpers.Suggestions (suggestionsDefinitions, getSuggestions)
import Engineering.Helpers.Suggestions as EHS
import Engineering.Helpers.Utils (loaderText, toggleLoader, reboot, showSplash, (?), fetchLanguage, capitalizeFirstChar, getCityFromCode, handleUpdatedTerms, getReferralCode)
import Foreign (unsafeToForeign, unsafeFromForeign)
import Foreign.Class (class Encode, encode, decode)
import Foreign.Generic (encodeJSON)
import Helpers.API (callApiBT, callApi)
import Helpers.Utils (isYesterday, LatLon(..), decodeErrorCode, decodeErrorMessage, getCurrentLocation, getDatebyCount, getDowngradeOptions, getGenderIndex, getNegotiationUnit, getPastDays, getPastWeeks, getTime, getcurrentdate, isDateGreaterThan, onBoardingSubscriptionScreenCheck, parseFloat, secondsLeft, toStringJSON, translateString, getDistanceBwCordinates, getCityConfig, getDriverStatus, getDriverStatusFromMode, updateDriverStatus, getLatestAndroidVersion, isDateNDaysAgo, getHvErrorMsg)
import Helpers.Utils as HU
import JBridge (cleverTapCustomEvent, cleverTapCustomEventWithParams, cleverTapEvent, cleverTapSetLocation, drawRoute, factoryResetApp, firebaseLogEvent, firebaseLogEventWithTwoParams, firebaseUserID, generateSessionId, getAndroidVersion, getCurrentLatLong, getCurrentPosition, getVersionCode, getVersionName, fetchPackageName, hideKeyboardOnNavigation, initiateLocationServiceClient, isBatteryPermissionEnabled, isInternetAvailable, isLocationEnabled, isLocationPermissionEnabled, isNotificationPermissionEnabled, isOverlayPermissionEnabled, metaLogEvent, metaLogEventWithTwoParams, openNavigation, removeAllPolylines, removeMarker, saveSuggestionDefs, saveSuggestions, setCleverTapUserData, setCleverTapUserProp, showMarker, startLocationPollingAPI, stopChatListenerService, stopLocationPollingAPI, toast, toggleBtnLoader, unregisterDateAndTime, withinTimeRange, mkRouteConfig, destroySignedCall)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.DefaultConfig as DC
import MerchantConfig.Types (AppConfig(..), Language, CityConfig)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import PaymentPage (checkPPInitiateStatus, consumeBP, initiatePP, paymentPageUI, PayPayload(..), PaymentPagePayload(..), getAvailableUpiApps, getPaymentPageLangKey, initiatePaymentPage, addLanguageToPayload)
import Prelude (Unit, bind, discard, pure, unit, unless, negate, void, when, map, otherwise, ($), (==), (/=), (&&), (||), (/), when, (+), show, (>), not, (<), (*), (-), (<=), (<$>), (>=), ($>), (<<<), const, (>>=))
import Presto.Core.Types.API (ErrorResponse(..))
import Presto.Core.Types.Language.Flow (delay, setLogField, getLogFields, doAff, fork, Flow)
import PrestoDOM (initUI, Visibility(..))
import RemoteConfig as RC
import Resource.Constants (decodeAddress)
import Resource.Constants as RC
import Screens as ScreenNames
import Screens.AddVehicleDetailsScreen.ScreenData (initData) as AddVehicleDetailsScreenData
import Screens.BookingOptionsScreen.Controller (downgradeOptionsConfig)
import Screens.BookingOptionsScreen.ScreenData as BookingOptionsScreenData
import Screens.RateCardScreen.ScreenData as RateCardScreenData
import Screens.DriverDetailsScreen.Controller (getGenderValue, genders, getGenderState)
import Screens.DriverProfileScreen.Controller (getDowngradeOptionsSelected)
import Screens.DriverProfileScreen.ScreenData (dummyDriverInfo)
import Screens.DriverProfileScreen.Transformer (transformSelectedVehicles)
import Screens.DriverSavedLocationScreen.Transformer (getLocationArray)
import Screens.DriverEarningsScreen.Transformer
import Screens.Handlers (chooseCityScreen, homeScreen)
import Screens.Handlers as UI
import Screens.HomeScreen.ComponentConfig (mapRouteConfig)
import Screens.HomeScreen.Controller (activeRideDetail, getPreviousVersion, getCoinPopupStatus)
import Screens.HomeScreen.ScreenData (dummyDriverRideStats)
import Screens.HomeScreen.ScreenData (initData) as HomeScreenData
import Screens.DocumentCaptureScreen.ScreenData (initData) as DocumentCaptureData
import Screens.AcknowledgementScreen.ScreenData (initData) as AckScreenInitData
import Screens.HomeScreen.Transformer (getDisabledLocById)
import Screens.HomeScreen.View (rideRequestPollingData)
import Screens.PaymentHistoryScreen.Controller (ScreenOutput(..))
import Screens.PaymentHistoryScreen.Transformer (buildTransactionDetails)
import Screens.PopUpScreen.Controller (transformAllocationData)
import Screens.RegistrationScreen.Controller (getStatusValue)
import Screens.ReportIssueChatScreen.Handler (reportIssueChatScreen) as UI
import Screens.CustomerReferralTrackerScreen.Handler (customerReferralTrackerScreen) as UI
import Screens.CustomerReferralTrackerScreen.Transformer (getDailyEarnings, getOrderStatus)
import Screens.CustomerReferralTrackerScreen.Types as CRST
import Screens.ReportIssueChatScreen.ScreenData (initData) as ReportIssueScreenData
import Screens.RideHistoryScreen.Transformer (getPaymentHistoryItemList)
import Screens.RideSelectionScreen.Handler (rideSelection) as UI
import Screens.RideSelectionScreen.View (getCategoryName)
import Screens.SubscriptionScreen.Transformer (alternatePlansTransformer)
import Screens.Types (AadhaarStage(..), ActiveRide, AllocationData, AutoPayStatus(..), DriverStatus(..), HomeScreenStage(..), HomeScreenState, UpdateRouteSrcDestConfig(..), KeyboardModalType(..), Location, PlanCardConfig, PromoConfig, ReferralType(..), StageStatus(..), SubscribePopupType(..), SubscriptionBannerType(..), SubscriptionPopupType(..), SubscriptionSubview(..), UpdatePopupType(..), ChooseCityScreenStage(..))
import Screens.Types as ST
import Screens.UploadDrivingLicenseScreen.ScreenData (initData) as UploadDrivingLicenseScreenData
import Services.API (AlternateNumberResendOTPResp(..), Category(Category), CreateOrderRes(..), CurrentDateAndTimeRes(..), DriverActiveInactiveResp(..),  DriverAlternateNumberResp(..), DriverArrivedReq(..), DriverProfileStatsReq(..), DriverProfileStatsResp(..), DriverRegistrationStatusReq(..), DriverRegistrationStatusResp(..), GenerateAadhaarOTPResp(..), GetCategoriesRes(GetCategoriesRes), DriverInfoReq(..), GetDriverInfoResp(..), GetOptionsRes(GetOptionsRes), GetPaymentHistoryResp(..), GetPaymentHistoryResp(..), GetPerformanceReq(..), GetPerformanceRes(..), GetRidesHistoryResp(..), GetRouteResp(..), IssueInfoRes(IssueInfoRes), LogOutReq(..), Option(Option), OrderStatusRes(..), OrganizationInfo(..), PaymentDetailsEntity(..), PostIssueReq(PostIssueReq), PostIssueRes(PostIssueRes),  RemoveAlternateNumberRequest(..), ResendOTPResp(..), RidesInfo(..), Route(..),  Status(..), SubscribePlanResp(..), TriggerOTPResp(..), UpdateDriverInfoReq(..), UpdateDriverInfoResp(..), ValidateImageReq(..), ValidateImageRes(..), Vehicle(..), VerifyAadhaarOTPResp(..), VerifyTokenResp(..), GenerateReferralCodeReq(..), GenerateReferralCodeRes(..), FeeType(..), ClearDuesResp(..), HistoryEntryDetailsEntityV2Resp(..), DriverProfileSummaryRes(..), DummyRideRequestReq(..), BookingTypes(..), UploadOdometerImageResp(UploadOdometerImageResp), GetRidesSummaryListResp(..), PayoutVpaStatus(..), ScheduledBookingListResponse (..), DriverReachedReq(..), ServiceTierType(..))
import Services.API as API
import Services.Accessor (_lat, _lon, _id, _orderId, _moduleId, _languagesAvailableForQuiz , _languagesAvailableForVideos, _deepLinkJSON, _payload)
import Services.Backend (driverRegistrationStatusBT, dummyVehicleObject, makeDriverDLReq, makeDriverRCReq, makeGetRouteReq, makeLinkReferralCodeReq, makeOfferRideReq, makeReferDriverReq, makeResendAlternateNumberOtpRequest, makeTriggerOTPReq, makeValidateAlternateNumberRequest, makeValidateImageReq, makeVerifyAlternateNumberOtpRequest, makeVerifyOTPReq, mkUpdateDriverInfoReq, walkCoordinate, walkCoordinates)
import Services.Backend as Remote
import Engineering.Helpers.Events as Events
import Services.Config (getBaseUrl)
import Storage (KeyStore(..), deleteValueFromLocalStore, getValueToLocalNativeStore, getValueToLocalStore, isLocalStageOn, isOnFreeTrial, setValueToLocalNativeStore, setValueToLocalStore)
import Timers (clearTimerWithId)
import Types.App (RIDE_SUMMARY_SCREEN_OUTPUT(..), LMS_QUIZ_SCREEN_OUTPUT(..), LMS_VIDEO_SCREEN_OUTPUT(..), REPORT_ISSUE_CHAT_SCREEN_OUTPUT(..), RIDES_SELECTION_SCREEN_OUTPUT(..), ABOUT_US_SCREEN_OUTPUT(..), BANK_DETAILS_SCREENOUTPUT(..), ADD_VEHICLE_DETAILS_SCREENOUTPUT(..), APPLICATION_STATUS_SCREENOUTPUT(..), DRIVER_DETAILS_SCREEN_OUTPUT(..), DRIVER_PROFILE_SCREEN_OUTPUT(..), CHOOSE_CITY_SCREEN_OUTPUT(..), DRIVER_RIDE_RATING_SCREEN_OUTPUT(..), ENTER_MOBILE_NUMBER_SCREEN_OUTPUT(..), ENTER_OTP_SCREEN_OUTPUT(..), FlowBT, GlobalState(..), HELP_AND_SUPPORT_SCREEN_OUTPUT(..), HOME_SCREENOUTPUT(..), MY_RIDES_SCREEN_OUTPUT(..), NOTIFICATIONS_SCREEN_OUTPUT(..), NO_INTERNET_SCREEN_OUTPUT(..), PERMISSIONS_SCREEN_OUTPUT(..), POPUP_SCREEN_OUTPUT(..), REGISTRATION_SCREEN_OUTPUT(..), RIDE_DETAIL_SCREENOUTPUT(..), PAYMENT_HISTORY_SCREEN_OUTPUT(..), SELECT_LANGUAGE_SCREEN_OUTPUT(..), ScreenStage(..), ScreenType(..), TRIP_DETAILS_SCREEN_OUTPUT(..), UPLOAD_ADHAAR_CARD_SCREENOUTPUT(..), UPLOAD_DRIVER_LICENSE_SCREENOUTPUT(..), VEHICLE_DETAILS_SCREEN_OUTPUT(..), WRITE_TO_US_SCREEN_OUTPUT(..), NOTIFICATIONS_SCREEN_OUTPUT(..), REFERRAL_SCREEN_OUTPUT(..), BOOKING_OPTIONS_SCREEN_OUTPUT(..), ACKNOWLEDGEMENT_SCREEN_OUTPUT(..), defaultGlobalState, SUBSCRIPTION_SCREEN_OUTPUT(..), NAVIGATION_ACTIONS(..), AADHAAR_VERIFICATION_SCREEN_OUTPUT(..), ONBOARDING_SUBSCRIPTION_SCREENOUTPUT(..), APP_UPDATE_POPUP(..), DRIVE_SAVED_LOCATION_OUTPUT(..), WELCOME_SCREEN_OUTPUT(..), DRIVER_EARNINGS_SCREEN_OUTPUT(..), BENEFITS_SCREEN_OUTPUT(..), CUSTOMER_REFERRAL_TRACKER_SCREEN_OUTPUT(..), HOTSPOT_SCREEN_OUTPUT(..), SCHEDULED_RIDE_ACCEPTED_SCREEN_OUTPUT(..), UPLOAD_PARCEL_IMAGE_SCREEN_OUTPUT(..))
import Types.App as TA
import Types.ModifyScreenState (modifyScreenState, updateStage)
import ConfigProvider
import Timers (clearTimerWithId, resetAllTimers)
import RemoteConfig as RC
import Locale.Utils
import Data.Array as DA
import Screens.Benefits.LmsQuizScreen.Transformer (transformQuizRespToQuestions)
import Screens.OnBoardingSubscriptionScreen.Transformer (transformReelsRespToReelsData)
import Helpers.API as HelpersAPI
import Engineering.Helpers.API as EHA
import LocalStorage.Cache (getValueFromCache)
import Effect.Unsafe (unsafePerformEffect)
import Common.Types.App as CTA
import AssetsProvider (renewFile)
import Control.Bind
import Presto.Core.Types.Language.Flow (await)
import Resource.Constants (hvSdkTokenExp)
import Services.Config as SC
import Presto.Core.Types.Language.Flow (await)
import Resource.Constants (hvSdkTokenExp)
import PrestoDOM.Core(terminateUI)
import Common.RemoteConfig.Utils as CommonRC
import Common.RemoteConfig.Types (FeaturesConfigData(..))
import RemoteConfig.Utils
import Screens.SubscriptionScreen.ScreenData as SubscriptionScreenInitData
import Engineering.Helpers.RippleCircles
import Screens.RideRequestScreen.ScreenData as RideRequestData
import Screens.RideSummaryScreen.Controller as RSC
import Screens.RideSummaryScreen.ScreenData (initData) as RideSummaryScreenData
import DecodeUtil as DU
import Helpers.SplashUtils (hideSplashAndCallFlow, toggleSetupSplash)
import RemoteConfig as RemoteConfig
import Control.Apply as CA
import Screens.MetroWarriorsScreen.Controller (getMetroWarriorFromLocationId, makeStationsData)
import Data.Newtype (unwrap)
import Data.Function.Uncurried as UC
import Screens.Benefits.BenefitsScreen.Controller as BSC
import PrestoDOM.Core (getPushFn)
import Screens.NotificationsScreen.Controller (notificationTransformer, notifisDetailStateTransformer)
import Engineering.Helpers.Utils as EHU

baseAppFlow :: Boolean -> Maybe Event -> Maybe (Either ErrorResponse GetDriverInfoResp) -> FlowBT String Unit
baseAppFlow baseFlow event driverInfoResponse = do
    when baseFlow $ do 
      lift $ lift $ initUI
    let bundleSplashConfig = RemoteConfig.getBundleSplashConfig "splash"
    when (baseFlow && bundleSplashConfig.enable) $ do 
      toggleSetupSplash true
      let _ = DU.setKeyInWindow "forceAppToNoInternetScreen" true
      pure unit
      
    liftFlowBT $ markPerformance "BASE_APP_FLOW_START"
    liftFlowBT $ Events.endMeasuringDuration "Flow.mainFlow"
    liftFlowBT $ Events.initMeasuringDuration "Flow.baseAppFlow"
    liftFlowBT $ setEventTimestamp "baseAppFlow"    
    versionCode <- lift $ lift $ liftFlow $ getVersionCode
    liftFlowBT $ runEffectFn1 EHC.resetIdMap ""
    liftFlowBT $ resetAllTimers
    -- checkVersion versionCode -- TODO:: Need to handle it properly considering multiple cities and apps
    checkTimeSettings
    cacheAppParameters versionCode baseFlow
    updateNightSafetyPopup
    void $ liftFlowBT initiateLocationServiceClient
    updateOperatingCity
    void $ pure $ saveSuggestions "SUGGESTIONS" (getSuggestions "")
    void $ pure $ saveSuggestionDefs "SUGGESTIONS_DEFINITIONS" (suggestionsDefinitions "")
    setValueToLocalStore CURRENCY (getCurrency Constants.appConfig)
    if getValueToLocalStore SHOW_SUBSCRIPTIONS == "__failed" then setValueToLocalStore SHOW_SUBSCRIPTIONS "false" else pure unit  
    liftFlowBT $ markPerformance "BASE_APP_FLOW_END"
    when baseFlow $ showParcelIntroductionPopup
    initialFlow    
    where
    updateOperatingCity :: FlowBT String Unit
    updateOperatingCity = do
      let city = getValueToLocalStore DRIVER_LOCATION
      if city /= "__failed" then do
        void $ pure $ setValueToLocalStore DRIVER_LOCATION (capitalize (toLower city))
        else pure unit
        
    cacheAppParameters :: Int -> Boolean -> FlowBT String Unit
    cacheAppParameters versionCode baseFlow = do
      let bundle = getVersionByKey "app"
          config = getVersionByKey "configuration"
          driverId = (getValueToLocalStore DRIVER_ID)
          appSessionCount = getValueToLocalStore APP_SESSION_TRACK_COUNT
          movedToOfflineDate = getValueToLocalStore MOVED_TO_OFFLINE_DUE_TO_HIGH_DUE
      versionName <- lift $ lift $ liftFlow $ getVersionName
      packageName <- lift $ lift $ liftFlow $ fetchPackageName unit
      void $ pure $ setCleverTapUserProp [{key : "App Version", value : unsafeToForeign versionName},
                                          {key : "Bundle version", value : unsafeToForeign bundle},
                                          {key : "Platform", value : unsafeToForeign os}]
      setValueToLocalStore VERSION_NAME versionName
      setValueToLocalStore BUNDLE_VERSION bundle
      setValueToLocalStore PACKAGE_NAME packageName
      setValueToLocalStore CONFIG_VERSION config
      setValueToLocalStore BASE_URL (getBaseUrl "dummy")
      setValueToLocalStore RIDE_REQUEST_BUFFER "0"
      setValueToLocalStore IS_BANNER_ACTIVE "True"
      setValueToLocalStore MESSAGES_DELAY "0"
      setValueToLocalStore SHOW_PAYMENT_MODAL "true"
      setValueToLocalStore IS_DRIVER_STATS_CALLED "false"
      setValueToLocalNativeStore BUNDLE_VERSION bundle
      setValueToLocalNativeStore GPS_METHOD "CURRENT"
      setValueToLocalNativeStore MAKE_NULL_API_CALL "NO"
      setValueToLocalStore IS_DRIVER_AT_PICKUP "false"
      setValueToLocalStore DISABLE_WIDGET "false"
      setValueToLocalStore BUNDLE_TIME_OUT "500"
      setValueToLocalStore ENABLE_SPECIAL_PICKUP_WIDGET "true"
      setValueToLocalStore POINTS_FACTOR "3"
      setValueToLocalStore ACCURACY_THRESHOLD "23.0"
      setValueToLocalStore LOGS_TRACKING "false"
      setValueToLocalStore FUNCTION_EXECUTED_IN_SESSION "false"
      when baseFlow $ setValueToLocalStore APP_SESSION_TRACK_COUNT if (appSessionCount /= "false") then "false" else "true"
      if ((movedToOfflineDate /= "" && isYesterday movedToOfflineDate) || movedToOfflineDate == "__failed") 
        then setValueToLocalStore MOVED_TO_OFFLINE_DUE_TO_HIGH_DUE "" 
      else pure unit
      when ((getValueToLocalStore SESSION_ID == "__failed") || (getValueToLocalStore SESSION_ID == "(null)")) $ do
        setValueToLocalStore SESSION_ID (generateSessionId unit)
      if(driverId == "__failed") then void $ lift $ lift $ setLogField "driver_id" $ encode ("null")
        else do
          void $ pure $ firebaseUserID driverId
          void $ lift $ lift $ setLogField "driver_id" $ encode (driverId)
      void $ lift $ lift $ setLogField "app_version" $ encode (show versionCode)
      void $ lift $ lift $ setLogField "bundle_version" $ encode (bundle)
      void $ lift $ lift $ setLogField "config_version" $ encode config
      void $ lift $ lift $ setLogField "platform" $ encode (os)

    initialFlow :: FlowBT String Unit
    initialFlow = do
      liftFlowBT $ markPerformance "INITIAL_FLOW_START"
      liftFlowBT $ Events.endMeasuringDuration "Flow.baseAppFlow"
      liftFlowBT $ Events.initMeasuringDuration "Flow.initialFlow"
      config <- getAppConfigFlowBT Constants.appConfig
      let regToken = getValueToLocalStore REGISTERATION_TOKEN
      isLocationPermission <- lift $ lift $ liftFlow $ isLocationPermissionEnabled unit
      liftFlowBT $ Events.endMeasuringDuration "Flow.initialFlow"
      liftFlowBT $ markPerformance "INITIAL_FLOW_END"
      if isTokenValid regToken then do
        checkRideAndInitiate event driverInfoResponse
      else if not config.flowConfig.chooseCity.runFlow then
        chooseLanguageFlow event
      else if (getValueToLocalStore DRIVER_LOCATION == "__failed" || getValueToLocalStore DRIVER_LOCATION == "--" || not isLocationPermission) then do
        chooseCityFlow event
      else do
        authenticationFlow "" event

    updateNightSafetyPopup :: FlowBT String Unit
    updateNightSafetyPopup = do
      let curr_time = convertUTCtoISC (getCurrentUTC "") "HH:mm:ss"
          config = getAppConfig appConfig
          withInTime = JB.withinTimeRange curr_time config.safetyRide.startTime config.safetyRide.endTime
      if (not withInTime) then 
        setValueToLocalStore NIGHT_SAFETY_POP_UP "false"
      else 
        pure unit 

    showParcelIntroductionPopup :: FlowBT String Unit
    showParcelIntroductionPopup = do
      let showParcelInfo = getValueToLocalStore SHOW_PARCEL_INTRODUCTION_POPUP
      if showParcelInfo /= "false" then do
        case driverInfoResponse of
          Just (Right (GetDriverInfoResp driverInfoResp)) -> do      
            case driverInfoResp.linkedVehicle of
              Just (Vehicle vehicle) -> modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { showParcelIntroductionPopup = vehicle.serviceTierType == Just "BIKE" }})
              Nothing -> pure unit
          _ -> pure unit
      else pure unit

authenticationFlow :: String -> Maybe Event -> FlowBT String Unit
authenticationFlow _ mbEvent = do
  liftFlowBT $ markPerformance "AUTHENTICATION_FLOW"
  if EHC.isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion (getMerchant FunctionCall)) then  hideSplashAndCallFlow (loginFlow mbEvent)  else welcomeScreenFlow mbEvent

chooseLanguageFlow :: Maybe Event -> FlowBT String Unit
chooseLanguageFlow mbEvent = do
  liftFlowBT $ markPerformance "CHOOSE_LANGUAGE_FLOW"
  hideSplashAndCallFlow $ pure unit
  action <- UI.chooseLanguage
  case action of
    TA.LOGIN_FLOW -> loginFlow mbEvent

checkRideAndInitiate :: Maybe Event -> Maybe (Either ErrorResponse GetDriverInfoResp) -> FlowBT String Unit
checkRideAndInitiate event driverInfoResponse = do
  liftFlowBT $ markPerformance "CHECK_RIDE_AND_INITIATE_START"
  Tuple mbRideListResponse activeRide <- 
    case driverInfoResponse of
      Just (Right (GetDriverInfoResp driverInfoResp)) -> do        
        modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps { driverInformation = Just (GetDriverInfoResp driverInfoResp) }
        pure (Tuple Nothing driverInfoResp.onRide)
      _ -> do
        GetRidesHistoryResp rideListResponse <- Remote.getRideHistoryReqBT "2" "0" "true" "null" "null"
        let activeRide = not (null rideListResponse.list)
        pure (Tuple (Just $ GetRidesHistoryResp rideListResponse) activeRide)
  void $ lift $ lift $ fork $ checkAndDownloadMLModel
  liftFlowBT $ markPerformance "CHECK_RIDE_AND_INITIATE_END"
  modifyScreenState $ HomeScreenStateType (\homeScreenState → homeScreenState { props { retryRideList = activeRide }})
  activeRide ?
    currentRideFlow mbRideListResponse (Just activeRide)
    $ do
      void $ updateStage $ HomeScreenStage HomeScreen
      getDriverInfoFlow event mbRideListResponse driverInfoResponse true Nothing false
    where 
      checkAndDownloadMLModel :: Flow GlobalState Unit
      checkAndDownloadMLModel = do
        let language = getLanguageLocale languageKey
        downloadedLanguages <- doAff $ makeAff \cb -> JB.listDownloadedTranslationModels (cb <<< Right) 1000 $> nonCanceler
        if (language /= "__failed" && not (languageExists downloadedLanguages language)) then do
          void $ liftFlow $ runEffectFn1 JB.downloadMLTranslationModel language
        else pure unit
      languageExists :: Array String -> String -> Boolean
      languageExists languages lang =
        let firstTwoChars = toLower $ take 2 lang
        in firstTwoChars `elem` languages

checkVersion :: Int -> FlowBT String Unit
checkVersion versioncode = do
  when (getValueToLocalNativeStore IS_RIDE_ACTIVE /= "true" && versioncode < (getLatestAndroidVersion (getMerchant FunctionCall))) $ do
    hideSplashAndCallFlow $ pure unit
    modifyScreenState $ AppUpdatePopUpScreenType (\appUpdatePopUpScreenState → appUpdatePopUpScreenState {updatePopup = AppVersion})
    fl <- UI.handleAppUpdatePopUp
    case fl of
      UpdateNow -> checkVersion versioncode
      Later -> pure unit

appUpdatedFlow :: FCMBundleUpdate -> ST.AppUpdatePoppupFlowType -> FlowBT String Unit
appUpdatedFlow payload flowType = do
  hideSplashAndCallFlow $ pure unit
  modifyScreenState $ AppUpdatePopUpScreenType (\appUpdatePopUpScreenState → appUpdatePopUpScreenState {updatePopup = AppUpdated ,appUpdatedView{secondaryText=payload.description,primaryText=payload.title,coverImageUrl=payload.image, popupFlowType = flowType}})
  fl <- UI.handleAppUpdatePopUp
  case fl of
    UpdateNow -> do 
      liftFlowBT showSplash
      liftFlowBT reboot
    Later -> if flowType == ST.REG_PROF_PAN_AADHAAR then onBoardingFlow else pure unit

checkTimeSettings :: FlowBT String Unit
checkTimeSettings = do
  isEnabled <- liftFlowBT $ runEffectFn1 JB.isNetworkTimeEnabled unit
  logField_ <- lift $ lift $ getLogFields
  if isEnabled then do
    liftFlowBT $ logEvent logField_ "ny_network_time_enabled"
    liftFlowBT $ unregisterDateAndTime
  else do
    liftFlowBT $ logEvent logField_ "ny_network_time_disabled"
    modifyScreenState $ AppUpdatePopUpScreenType (\appUpdatePopUpScreenState -> appUpdatePopUpScreenState { updatePopup =DateAndTime })
    hideSplashAndCallFlow $ pure unit
    void $ UI.handleAppUpdatePopUp
    checkTimeSettings


getMerchantName :: Merchant -> String
getMerchantName merchant =
  case merchant of
    NAMMAYATRI -> "Nammayatri"
    YATRI -> "Yatri"
    YATRISATHI -> "YatriSathi"
    MOBILITY_PM -> "MobilityPM"
    MOBILITY_RS -> "MobilityRS"
    PASSCULTURE -> "PassCulture"

ifNotRegistered :: Unit -> Boolean
ifNotRegistered _ = getValueToLocalStore REGISTERATION_TOKEN == "__failed"

isTokenValid :: String -> Boolean
isTokenValid = (/=) "__failed"

loginFlow :: Maybe Event -> FlowBT String Unit
loginFlow mbEvent = do
  liftFlowBT $ markPerformance "LOGIN_FLOW"
  hideSplashAndCallFlow $ pure unit
  logField_ <- lift $ lift $ getLogFields
  appConfig <- getAppConfigFlowBT Constants.appConfig
  (GlobalState allState) <- getState
  when (allState.globalProps.addTimestamp) $ do
    liftFlowBT $ setEventTimestamp "loginFlow"
    logData <- liftFlowBT $ getTimeStampObject unit
    liftFlowBT $ logEventWithMultipleParams logField_ "sending_logs" logData
    modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{addTimestamp = false}
  setValueToLocalStore T_AND_C_VERSION (show appConfig.termsVersion)
  mobileNo <- UI.enterMobileNumber
  case mobileNo of
    GO_TO_ENTER_OTP updateState -> do
      liftFlowBT $ logEvent logField_ "ny_driver_otp_trigger"
      latLong <- getCurrentLocation 0.0 0.0 0.0 0.0 400 false true
      TriggerOTPResp triggerOtpResp <- Remote.triggerOTPBT (makeTriggerOTPReq updateState.data.mobileNumber latLong)
      modifyScreenState $ EnterOTPScreenType (\enterOTPScreen → enterOTPScreen { data { tokenId = triggerOtpResp.authId}})
      enterOTPFlow mbEvent

enterOTPFlow :: Maybe Event -> FlowBT String Unit
enterOTPFlow mbEvent = do
  action <- UI.enterOTP
  logField_ <- lift $ lift $ getLogFields
  case action of
    DRIVER_INFO_API_CALL updatedState -> do
      void $ lift $ lift $ loaderText (getString SENDING_OTP) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      (VerifyTokenResp resp) <- Remote.verifyTokenBT (makeVerifyOTPReq updatedState.data.otp) updatedState.data.tokenId
      void $ lift $ lift $ liftFlow $ logEvent logField_ "ny_driver_verify_otp"
      void $ pure $ metaLogEvent "ny_driver_verify_otp"
      let driverId = ((resp.person)^. _id)
      if(driverId == "__failed") then do
        void $ lift $ lift $ setLogField "driver_id" $ encode ("null")
        pure unit
        else do
          void $ lift $ lift $ setLogField "driver_id" $ encode (driverId)
          pure unit
      setValueToLocalStore DRIVER_ID driverId
      void $ liftFlowBT $ setCleverTapUserData "Identity" (getValueToLocalStore DRIVER_ID)
      setValueToLocalStore REGISTERATION_TOKEN resp.token -- add from response
      if (getValueToLocalStore REFERRER_URL) == "__failed" then
        void $ pure $ JB.extractReferrerUrl unit
      else pure unit
      (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT $ UpdateDriverInfoReq $ mkUpdateDriverInfoReq ""
      void $ lift $ lift $ toggleLoader false
      getDriverInfoFlow mbEvent Nothing Nothing true Nothing true
    RETRY updatedState -> do
      modifyScreenState $ EnterOTPScreenType (\enterOTPScreen -> updatedState)
      (ResendOTPResp resp_resend) <- Remote.resendOTPBT updatedState.data.tokenId
      pure $ toast $ getString OTP_HAS_BEEN_RESENT
      modifyScreenState $ EnterOTPScreenType (\enterOTPScreen → enterOTPScreen { data { tokenId = resp_resend.authId, attemptCount = resp_resend.attempts}})
      enterOTPFlow mbEvent

getDriverInfoFlow :: Maybe Event -> Maybe GetRidesHistoryResp -> Maybe (Either ErrorResponse GetDriverInfoResp) -> Boolean -> Maybe Boolean -> Boolean -> FlowBT String Unit
getDriverInfoFlow event activeRideResp driverInfoResp updateShowSubscription isAdvancedBookingEnabled updateCTBasicData = do
  liftFlowBT $ markPerformance "GET_DRIVER_INFO_FLOW_START"
  case driverInfoResp of
    Just driverInfoResp -> runDriverInfoFlow driverInfoResp true
    Nothing -> do
      getDriverInfoApiResp <- lift $ lift $ Remote.getDriverInfoApi ""
      runDriverInfoFlow getDriverInfoApiResp false
  where    
    runDriverInfoFlow driverInfoRes updateFeatureFlags = do
      config <- getAppConfigFlowBT Constants.appConfig
      logField_ <- lift $ lift $ getLogFields
      case driverInfoRes of
        Right (GetDriverInfoResp getDriverInfoResp) -> do
          let cityConfigFromCityCode = HU.getCityConfigFromCityCode config.cityConfig (fromMaybe "" getDriverInfoResp.operatingCity)
          void $ pure $ setValueToLocalStore DRIVER_LOCATION  (capitalize $ cityConfigFromCityCode.cityName)
          setValueToLocalStore FREE_TRIAL_DAYS (show (fromMaybe 0 getDriverInfoResp.freeTrialDaysLeft))
          updateFirebaseToken getDriverInfoResp.maskedDeviceToken getUpdateToken
          when updateCTBasicData $ liftFlowBT $ updateInitialCleverTapUserProps (GetDriverInfoResp getDriverInfoResp)
          liftFlowBT $ updateCleverTapUserProps (GetDriverInfoResp getDriverInfoResp)
          maybe (pure unit) (setValueToLocalStore MOBILE_NUMBER_KEY) getDriverInfoResp.mobileNumber
          let cityConfig = getCityConfig config.cityConfig (getValueToLocalStore DRIVER_LOCATION)
          when (updateFeatureFlags && cityConfig.callDriverInfoPost) $ do
            void $ lift $ lift $ fork $ Remote.getDriverInfoApi ""
          if getDriverInfoResp.enabled 
            then do
              deleteValueFromLocalStore ENTERED_RC
              if getValueToLocalStore IS_DRIVER_ENABLED == "false" 
                then do
                  void $ pure $ firebaseLogEvent "ny_driver_enabled"
                  void $ pure $ metaLogEvent "ny_driver_enabled"
                  let (Vehicle linkedVehicle) = fromMaybe dummyVehicleObject getDriverInfoResp.linkedVehicle
                  let category = fromMaybe "" linkedVehicle.category
                  when (category /= "") $ do
                    void $ lift $ lift $ liftFlow $ logEvent logField_ $ "ny_driver_" <> toLower category <> "_enabled"  
                    void $ pure $ metaLogEvent $ "ny_driver_" <> toLower category <> "_enabled"              
                else pure unit
              setValueToLocalStore IS_DRIVER_ENABLED "true" 
              if updateShowSubscription 
                then updateSubscriptionForVehicleVariant (GetDriverInfoResp getDriverInfoResp) config
                else pure unit
              (GlobalState allState) <- getState -- TODO:: Temp fix - need to work on improving caching more using SQLite
              modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{driverInformation = Just (GetDriverInfoResp getDriverInfoResp)}
              updateDriverDataToStates
              void $ liftFlowBT $ runEffectFn1 consumeBP unit
              if (isJust getDriverInfoResp.autoPayStatus) 
                then setValueToLocalStore TIMES_OPENED_NEW_SUBSCRIPTION "5"
                else pure unit
              permissionsGiven <- checkAllPermissions true config.permissions.locationPermission            
              if permissionsGiven
                then do
                  liftFlowBT $ markPerformance "GET_DRIVER_INFO_FLOW_END"
                  handleDeepLinksFlow event activeRideResp (Just getDriverInfoResp.onRide)
                else do
                  modifyScreenState $ PermissionsScreenStateType (\permissionScreen -> permissionScreen{props{isDriverEnabled = true}})
                  permissionsScreenFlow event activeRideResp (Just getDriverInfoResp.onRide)
            else do
              -- modifyScreenState $ ApplicationStatusScreenType (\applicationStatusScreen -> applicationStatusScreen {props{alternateNumberAdded = isJust getDriverInfoResp.alternateNumber}})
              setValueToLocalStore IS_DRIVER_ENABLED "false"
              if getDriverInfoResp.verified 
                then setValueToLocalStore IS_DRIVER_VERIFIED "true"
                else do
                  setValueToLocalStore IS_DRIVER_VERIFIED "false"
                  modifyScreenState $ RegisterScreenStateType (\registerationScreen -> registerationScreen{data{phoneNumber = fromMaybe "" getDriverInfoResp.mobileNumber}} )
          onBoardingFlow
        Left errorPayload -> do
          if ((decodeErrorCode errorPayload.response.errorMessage) == "VEHICLE_NOT_FOUND" || (decodeErrorCode errorPayload.response.errorMessage) == "DRIVER_INFORMATON_NOT_FOUND")
            then onBoardingFlow
            else do
              void $ pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
              if getValueToLocalStore IS_DRIVER_ENABLED == "true" then do                
                permissionsGiven <- checkAllPermissions true config.permissions.locationPermission                
                if permissionsGiven then
                  handleDeepLinksFlow event activeRideResp Nothing
                  else permissionsScreenFlow event activeRideResp Nothing
                else do
                  onBoardingFlow
    getUpdateToken :: String -> FlowBT String Unit
    getUpdateToken token = 
      let initialData = Remote.mkUpdateDriverInfoReq ""
          requiredData = initialData{deviceToken = Just token}
      in void $ Remote.updateDriverInfoBT (UpdateDriverInfoReq requiredData)

updateSubscriptionForVehicleVariant :: GetDriverInfoResp -> AppConfig -> FlowBT String Unit
updateSubscriptionForVehicleVariant (GetDriverInfoResp getDriverInfoResp) appConfig = do
  let cityConfig = getCityConfig appConfig.cityConfig (getValueToLocalStore DRIVER_LOCATION)
      vehicleVariant = getDriverInfoResp.linkedVehicle
  if cityConfig.variantSubscriptionConfig.enableVariantBasedSubscription then do
    case vehicleVariant of
      Just (API.Vehicle vehicle) -> do
        let category = RC.getCategoryFromVariant vehicle.variant
        case category of
          Just categoryValue -> do
            setValueToLocalStore VEHICLE_CATEGORY $ show categoryValue
            void $ pure $ setValueToLocalStore SHOW_SUBSCRIPTIONS 
              $ if DA.elem (show categoryValue) cityConfig.variantSubscriptionConfig.variantList 
                  then "true"
                else "false"
          Nothing -> do
            void $ lift $ lift $ fork callGetAllRcData
            pure unit
      Nothing -> do
        void $ lift $ lift $ fork callGetAllRcData
        pure unit
  else do
    setValueToLocalStore SHOW_SUBSCRIPTIONS (show cityConfig.showSubscriptions)
  where 
    callGetAllRcData :: Flow GlobalState Unit
    callGetAllRcData = do
      response <- Remote.getAllRcData (API.GetAllRcDataReq)
      case response of
        Right (API.GetAllRcDataResp getAllRcsDataResp) -> do
            let isCabVariant = foldl (\acc (API.GetAllRcDataRecords item) -> do
                    let API.VehicleDetails details = item.rcDetails
                    if details.vehicleVariant == Just "AUTO_RICKSHAW" 
                      then false
                    else if item.rcActive 
                      then acc && true
                    else acc ) true getAllRcsDataResp
            if isCabVariant then do
              JB.setKeyInSharedPrefKeys "SHOW_SUBSCRIPTIONS" "false"
            else do
              JB.setKeyInSharedPrefKeys "SHOW_SUBSCRIPTIONS" "true"
        Left _ -> pure unit
        
handleDeepLinksFlow :: Maybe Event -> Maybe GetRidesHistoryResp -> Maybe Boolean -> FlowBT String Unit
handleDeepLinksFlow event activeRideResp isActiveRide = do
  liftFlowBT $ markPerformance "HANDLE_DEEP_LINKS_FLOW"
  case event of -- TODO:: Need to handle in generic way for all screens. Could be part of flow refactoring
        Just e -> 
          case e.data of
            "plans" | getValueToLocalNativeStore IS_RIDE_ACTIVE /= "true" && getValueToLocalNativeStore DISABLE_WIDGET /= "true" -> hideSplashAndCallFlow updateAvailableAppsAndGoToSubs
            "lang" -> do
              modifyScreenState $ SelectLanguageScreenStateType (\selectLangState -> selectLangState{ props{ onlyGetTheSelectedLanguage = false, selectedLanguage = "", selectLanguageForScreen = ""}})
              hideSplashAndCallFlow selectLanguageFlow
            "contest" -> hideSplashAndCallFlow referralFlow
            "coins" -> do 
              modifyScreenState $ DriverEarningsScreenStateType (\driverEarningsScreen -> driverEarningsScreen {props{subView = ST.YATRI_COINS_VIEW}})
              hideSplashAndCallFlow driverEarningsFlow
            "online" -> do
              changeDriverStatus Online
              pure unit
            "pref" -> do
                globalState <- getState
                void $ getDriverInfoDataFromCache globalState false
                modifyScreenState $ BookingOptionsScreenType (\bookingOptions ->  bookingOptions{ props{ fromDeepLink = true } })
                bookingOptionsFlow
            "addupi" -> do 
              modifyScreenState $ CustomerReferralTrackerScreenStateType (\customerReferralTracker -> customerReferralTracker{props{fromDeepLink = true, openPP = true}})
              hideSplashAndCallFlow customerReferralTrackerFlow
            "alerts" -> do
              let gPayload = EHC.getGlobalPayload "__payload"
              hideSplashAndCallFlow $ notificationFlow gPayload
            _ | startsWith "ginit" e.data -> hideSplashAndCallFlow $ gullakDeeplinkFlow e.data
            _ -> pure unit
        Nothing -> pure unit
  (GlobalState allState) <- getState
  case allState.notificationScreen.selectedNotification of
    Just _ -> hideSplashAndCallFlow $ notificationFlow Nothing
    Nothing -> pure unit
  case allState.globalProps.callScreen of
    ScreenNames.SUBSCRIPTION_SCREEN -> hideSplashAndCallFlow updateAvailableAppsAndGoToSubs
    _ -> pure unit
  checkPreRequisites activeRideResp isActiveRide

gullakDeeplinkFlow :: String -> FlowBT String Unit
gullakDeeplinkFlow event = do
  let token = getValueToLocalStore GULLAK_TOKEN
      gullakRemoteConfig = CommonRC.gullakConfig $ getValueToLocalStore DRIVER_LOCATION
      splitLinkArr = split (Pattern "+") event
      loadDynamicModule = fromMaybe false $ UC.runFn3 DU.getAnyFromWindow "loadDynamicModule" Nothing Just
      showVideo = (fromMaybe "" (splitLinkArr !! 1)) == "true"
      installOnly = (fromMaybe "" (splitLinkArr !! 2)) == "true"
      videoLink = fromMaybe "" gullakRemoteConfig.videoUrl
  if not loadDynamicModule then homeScreenFlow else do
    push <- lift $ lift $ liftFlow $ getPushFn Nothing "BenefitsScreen"
    if HU.isTokenWithExpValid token then do
      void $ pure $ UC.runFn4 JB.emitJOSEventWithCb "gl_sdk" (JB.josEventInnerPayload {param1 = token, param2 = show installOnly}) push BSC.GullakSDKResponse
      pure unit
    else do
      response <- lift $ lift $ HelpersAPI.callApi $ API.GetSdkTokenReq "0" API.Gullak
      case response of
        Left _ -> do
          void $ pure $ JB.toast $ getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
          pure unit
        Right (API.GetSdkTokenResp resp) -> do
          void $ pure $ setValueToLocalStore GULLAK_TOKEN $ resp.token <> "<$>" <> fromMaybe "" resp.expiry
          void $ pure $ UC.runFn4 JB.emitJOSEventWithCb "gl_sdk" (JB.josEventInnerPayload {param1 = resp.token, param2 = show installOnly}) push BSC.GullakSDKResponse
          pure unit
    if showVideo && (not (DS.null videoLink)) then void $ liftFlowBT $ JB.openUrlInApp videoLink else pure unit
    homeScreenFlow


checkPreRequisites :: Maybe GetRidesHistoryResp -> Maybe Boolean -> FlowBT String Unit
checkPreRequisites activeRideResp isActiveRide = do
  liftFlowBT $ markPerformance "CHECK_PRE_REQUISITES_FLOW"
  status <- checkAndUpdateRCStatus
  status ? do
    checkStatusAndStartLocationUpdates
    currentRideFlow activeRideResp isActiveRide
    $ homeScreenFlow

checkAndUpdateRCStatus :: FlowBT String Boolean
checkAndUpdateRCStatus = do
  (GlobalState globalstate) <- getState
  (GetDriverInfoResp getDriverInfoResp) <- getDriverInfoDataFromCache (GlobalState globalstate) false
  case getDriverInfoResp.linkedVehicle of
    Nothing -> do
      when (globalstate.homeScreen.props.driverStatusSet /= Offline) $ changeDriverStatus Offline
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {rcActive = false, rcDeactivePopup = true}})
      pure $ false
    Just _ -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {rcActive = true, rcDeactivePopup = false}})
      pure $ true

checkStatusAndStartLocationUpdates :: FlowBT String Unit
checkStatusAndStartLocationUpdates = do
  globalstate <- getState 
  (GetDriverInfoResp getDriverInfoResp) <- getDriverInfoDataFromCache globalstate false
  let isNoDriverMode = isNothing getDriverInfoResp.mode
      driverMode = fromMaybe "" getDriverInfoResp.mode
      isOffline = if isNoDriverMode  then getDriverInfoResp.active else driverMode == "OFFLINE"
      currentMode = if isNoDriverMode then show $ updateDriverStatus (getDriverInfoResp.active) else driverMode
  if isNoDriverMode then pure unit else void $ pure $ setCleverTapUserProp [{key : "Mode", value : unsafeToForeign driverMode}]
  setDriverStatusInLocal (show (not isOffline)) $ show $ getDriverStatusFromMode currentMode
  liftFlowBT $ if isOffline then do 
                    stopLocationPollingAPI 
                    void $ pure $ stopGrpcService
               else do
                    startLocationPollingAPI
                    void $ pure $ startGrpcService


startGrpcService :: FlowBT String Unit
startGrpcService =
  let grpcServiceRunning = runFn1 JB.isServiceRunning "in.juspay.mobility.app.GRPCNotificationService"
      locationUpdateServiceRunning = runFn1 JB.isServiceRunning "in.juspay.mobility.app.LocationUpdateService"
  in
  if locationUpdateServiceRunning && (not grpcServiceRunning) then void $ pure $ JB.startService "in.juspay.mobility.app.GRPCNotificationService"
  else pure unit

stopGrpcService :: FlowBT String Unit
stopGrpcService =
  let grpcServiceRunning = runFn1 JB.isServiceRunning "in.juspay.mobility.app.GRPCNotificationService"
  in
  if grpcServiceRunning then void $ pure $ JB.stopService "in.juspay.mobility.app.GRPCNotificationService"
  else pure unit

convertToRequestStatus :: String -> API.ValidationStatus
convertToRequestStatus status =
  case status of 
    "auto_approved" -> API.AUTO_APPROVED
    "auto_declined" -> API.AUTO_DECLINED
    "needs_review" -> API.NEEDS_REVIEW
    _ -> API.NEEDS_REVIEW

getSdkTokenFromCache :: FlowBT String String
getSdkTokenFromCache = do
  let cachedData = getValueToLocalStore CACHED_SDK_TOKEN_DATA
  if cachedData /= "__failed"
    then do
      let splitData = split (Pattern ":") cachedData
      case splitData of
        [token, expiry] -> do 
          let isExpired = runFn1 JB.isSdkTokenExpired expiry
          if isExpired
            then cacheNewToken
          else pure token
        _ -> cacheNewToken
  else cacheNewToken
  where
    cacheNewToken = do
      (API.GetSdkTokenResp tokenResp) <- Remote.getSdkTokenBT (show hvSdkTokenExp) API.HyperVerge 
      let expiry = runFn1 JB.makeSdkTokenExpiry hvSdkTokenExp
      setValueToLocalStore CACHED_SDK_TOKEN_DATA (tokenResp.token <> ":" <> expiry)
      pure tokenResp.token

onBoardingFlow :: FlowBT String Unit
onBoardingFlow = do
  token <- getSdkTokenFromCache
  logField_ <- lift $ lift $ getLogFields
  void $ pure $ hideKeyboardOnNavigation true
  config <- getAppConfigFlowBT Constants.appConfig
  setValueToLocalStore LOGS_TRACKING "true"
  GlobalState allState <- getState
  DriverRegistrationStatusResp driverRegistrationResp <- driverRegistrationStatusBT $ DriverRegistrationStatusReq true
  let cityConfig = getCityConfig config.cityConfig (getValueToLocalStore DRIVER_LOCATION)
      registrationState = allState.registrationScreen
      driverEnabled = fromMaybe false driverRegistrationResp.enabled
      merchantName = getMerchantName $ getMerchant FunctionCall
  permissions <- checkAllPermissions false config.permissions.locationPermission
  if isNothing allState.globalProps.onBoardingDocs then updateOnboardingDocs registrationState.props.manageVehicle else pure unit
  GlobalState updatedGs <- getState
  if driverEnabled && config.feature.enableAutoReferral && (cityConfig.showDriverReferral || config.enableDriverReferral) 
    then do
      let referralCode = getReferralCode (getValueToLocalStore REFERRER_URL)
      if getValueToLocalStore REFERRAL_CODE_ADDED /= "true" && isJust referralCode 
        then do
          case referralCode of
            Just code -> activateReferralCode registrationState code
            Nothing   -> pure unit
        else pure unit
    else pure unit
  let limitReachedFor = if driverRegistrationResp.rcVerificationStatus == "LIMIT_EXCEED" then Just "RC"
                        else if driverRegistrationResp.dlVerificationStatus == "LIMIT_EXCEED" then Just "DL" 
                        else Nothing
      referralCodeAdded = getValueToLocalStore REFERRAL_CODE_ADDED == "true"
      uiCurrentCategory = if manageVehicle then registrationState.props.manageVehicleCategory else  RC.decodeVehicleType $ getValueToLocalStore VEHICLE_CATEGORY
      registerationStepsCabs = maybe [] (\(API.OnboardingDocsRes mbDoc) -> mkRegSteps $ fromMaybe [] mbDoc.cabs) updatedGs.globalProps.onBoardingDocs
      registerationStepsAutos = maybe [] (\(API.OnboardingDocsRes mbDoc) -> mkRegSteps $ fromMaybe [] mbDoc.autos) updatedGs.globalProps.onBoardingDocs
      registerationStepsBike = maybe [] (\(API.OnboardingDocsRes mbDoc) -> mkRegSteps $ fromMaybe [] mbDoc.bikes) updatedGs.globalProps.onBoardingDocs
      registerationStepsAmbulance = maybe [] (\(API.OnboardingDocsRes mbDoc) -> mkRegSteps $ fromMaybe [] mbDoc.ambulances) updatedGs.globalProps.onBoardingDocs
      registerationStepsTruck = maybe [] (\(API.OnboardingDocsRes mbDoc) -> mkRegSteps $ fromMaybe [] mbDoc.trucks) updatedGs.globalProps.onBoardingDocs
      registerationStepsBus = maybe [] (\(API.OnboardingDocsRes mbDoc) -> mkRegSteps $ fromMaybe [] mbDoc.bus) updatedGs.globalProps.onBoardingDocs
      checkAvailability field = maybe false (\(API.OnboardingDocsRes mbDoc) -> isJust (field mbDoc)) updatedGs.globalProps.onBoardingDocs
      variantList = (if checkAvailability _.bikes then [ST.BikeCategory] else []) <> (if checkAvailability _.autos then [ST.AutoCategory] else []) <> (if checkAvailability _.cabs then [ST.CarCategory] else []) <> (if checkAvailability _.ambulances then [ST.AmbulanceCategory] else []) <> (if checkAvailability _.trucks then [ST.TruckCategory] else []) <> (if checkAvailability _.bus then [ST.BusCategory] else [])
      mismatchLogic vehicleDocument = (uiCurrentCategory == (RC.transformVehicleType $ Just vehicleDocument.userSelectedVehicleCategory)) && isJust vehicleDocument.verifiedVehicleCategory && (Just vehicleDocument.userSelectedVehicleCategory /= vehicleDocument.verifiedVehicleCategory)
      vehicleTypeMismatch = not registrationState.props.manageVehicle && any (\(API.VehicleDocumentItem item) -> mismatchLogic item) driverRegistrationResp.vehicleDocuments
      documentStatusList = mkStatusList (DriverRegistrationStatusResp driverRegistrationResp)
      verifiedRC = find (\docStatus -> docStatus.status == ST.COMPLETED && docStatus.docType == ST.VEHICLE_DETAILS_OPTION && docStatus.verifiedVehicleCategory == uiCurrentCategory) documentStatusList
      onboardingRC = case verifiedRC of
                  Just rcItem -> rcItem.regNo
                  Nothing -> Nothing
      localStoreRC = getValueToLocalStore ENTERED_RC
      enteredRC = if localStoreRC == "__failed" then Nothing else Just localStoreRC
      rcNo = if manageVehicle then enteredRC else onboardingRC
      filteredVehicleDocs = if manageVehicle then filter (\docStatus -> docStatus.regNo == rcNo) documentStatusList else documentStatusList
      manageVehicle = registrationState.props.manageVehicle
          
  modifyScreenState $ RegisterScreenStateType (\registerationScreen -> 
                  registerationScreen { data { 
                      vehicleDetailsStatus = getStatusValue driverRegistrationResp.rcVerificationStatus,
                      drivingLicenseStatus = getStatusValue driverRegistrationResp.dlVerificationStatus, 
                      lastUpdateTime = convertUTCtoISC (getCurrentUTC "") "hh:mm A",
                      enteredDL = getValueToLocalStore ENTERED_DL,
                      enteredRC = localStoreRC,
                      registerationStepsCabs = registerationStepsCabs,
                      registerationStepsAuto = registerationStepsAutos,
                      registerationStepsAmbulance = registerationStepsAmbulance,
                      registerationStepsTruck = registerationStepsTruck,
                      registerationStepsBus = registerationStepsBus,
                      documentStatusList = filteredVehicleDocs,
                      registerationStepsBike = registerationStepsBike,
                      variantList = variantList,
                      linkedRc = rcNo,
                      vehicleTypeMismatch = vehicleTypeMismatch,
                      permissionsStatus = if permissions then ST.COMPLETED else ST.NOT_STARTED,
                      cityConfig = cityConfig,
                      vehicleCategory = uiCurrentCategory,
                      accessToken = token
                  }, props {limitReachedFor = limitReachedFor, referralCodeSubmitted = referralCodeAdded, driverEnabled = driverEnabled}})
  hideSplashAndCallFlow (pure unit)
  flow <- UI.registration
  case flow of
    UPLOAD_DRIVER_LICENSE state -> do
      modifyScreenState $ UploadDrivingLicenseScreenStateType $ \_ -> UploadDrivingLicenseScreenData.initData { data {
        mobileNumber = state.data.phoneNumber,
        cityConfig = state.data.cityConfig,
        vehicleCategory = state.data.vehicleCategory
        }}
      uploadDrivingLicenseFlow
    UPLOAD_VEHICLE_DETAILS state rcNumberPrefixList -> do
      modifyScreenState $ AddVehicleDetailsScreenStateType $ \_ -> AddVehicleDetailsScreenData.initData { data {
        driverMobileNumber = state.data.phoneNumber, 
        cityConfig = state.data.cityConfig,
        vehicleCategory = state.data.vehicleCategory,
        rcNumberPrefixList = rcNumberPrefixList
        }}
      addVehicleDetailsflow false
    PERMISSION_SCREEN state -> do
      modifyScreenState $ PermissionsScreenStateType $ \permissionsScreen -> permissionsScreen { data {driverMobileNumber = state.data.phoneNumber}}
      permissionsScreenFlow Nothing Nothing Nothing
    AADHAAR_PAN_SELFIE_UPLOAD state (ST.HyperVergeKycResult result) -> do 
      let currentTime = getCurrentUTC ""
      let status = fromMaybe "needs_review" result.status
      let convertedStatus = convertToRequestStatus status
      void $ lift $ lift $ loaderText (getString VALIDATING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      case result.details of 
        Just (ST.LIVE_SELFIE (ST.LiveSelfie detail)) | (isJust detail.selfieURL) -> do
          if result.status == (Just "auto_declined")
            then if isJust detail.errorCode then void $ pure $ JB.toast $ getHvErrorMsg detail.errorCode else pure unit
          else pure unit
          image' <- lift $ lift $ doAff $ JB.encodeToBase64Type (fromMaybe "" detail.selfieURL) 10000
          case image' of 
            Nothing -> void $ pure $ toast (getString TIMEOUT)
            Just image -> do
              resp <- lift $ lift $ Remote.validateImage (Remote.makeValidateImageReq image "ProfilePhoto" Nothing (Just convertedStatus) result.transactionId state.data.vehicleCategory)
              case resp of
                Left errPayload -> if result.status /= (Just "auto_declined") then void $ pure $ toast $ Remote.getCorrespondingErrorMessage errPayload else pure unit
                Right response -> pure unit
        Just (ST.PAN_DETAILS (ST.PanDetails detail)) | (isJust detail.panURL) -> do
          if result.status == (Just "auto_declined")
            then if isJust detail.errorCode then void $ pure $ JB.toast $ getHvErrorMsg detail.errorCode else pure unit
          else pure unit
          let panNum = fromMaybe "" detail.pan
          if DS.null panNum
            then do
              if result.status /= (Just "auto_declined") then void $ pure $ toast (getString CANNOT_DETECT_PAN_CARD) else pure unit
              void $ lift $ lift $ delay $ Milliseconds 100.0 -- This delay is added for toggleloader to work.
          else do
            image' <- lift $ lift $ doAff $ JB.encodeToBase64Type (fromMaybe "" detail.panURL) 10000
            case image' of
              Nothing -> void $ pure $ toast (getString TIMEOUT)
              Just image -> do
                resp <- lift $ lift $ Remote.validateImage (Remote.makeValidateImageReq image "PanCard" Nothing (Just convertedStatus) result.transactionId state.data.vehicleCategory)
                case resp of
                  Right (ValidateImageRes response)-> do
                    resp <- lift $ lift $ Remote.registerDriverPAN (Remote.makePANCardReq true currentTime detail.dob detail.name (Just response.imageId) Nothing panNum (convertedStatus) result.transactionId detail.panDB_name)
                    case resp of
                      Left errPayload -> if result.status /= (Just "auto_declined") then void $ pure $ toast $ Remote.getCorrespondingErrorMessage errPayload else pure unit
                      Right response -> pure unit
                  Left errPayload -> if result.status /= (Just "auto_declined") then void $ pure $ toast $ Remote.getCorrespondingErrorMessage errPayload else pure unit
        Just (ST.AADHAAR_DETAILS (ST.AadhaarCardDetails detail)) | (isJust detail.aadhaarFrontURL && isJust detail.aadhaarBackURL)-> do
          if result.status == (Just "auto_declined")
            then if isJust detail.errorCode then void $ pure $ JB.toast $ getHvErrorMsg detail.errorCode else pure unit
          else pure unit
          let aadhaarNum = fromMaybe "" detail.idNumber
          if DS.null aadhaarNum || detail.errorCode == Just "113" -- or condition added to make sure both sides are detected
            then do
              if result.status /= (Just "auto_declined") then void $ pure $ toast (getString CANNOT_DETECT_AADHAAR) else pure unit
              void $ lift $ lift $ delay $ Milliseconds 100.0 -- This delay is added for toggleloader to work.
          else do
            imageFrontControl <- lift $ lift $ fork $ doAff $ JB.encodeToBase64Type (fromMaybe "" detail.aadhaarFrontURL) 10000
            imageBack <- lift $ lift $ doAff $ JB.encodeToBase64Type (fromMaybe "" detail.aadhaarBackURL) 10000
            imageFront <- lift $ lift  $ await imageFrontControl
            case imageFront, imageBack of
              Just imageFront, Just imageBack -> do
                respFrontImage <- lift $ lift $ Remote.validateImage (Remote.makeValidateImageReq imageFront "AadhaarCard" Nothing (Just convertedStatus) result.transactionId state.data.vehicleCategory)
                respBackImage <- lift $ lift $ Remote.validateImage (Remote.makeValidateImageReq imageBack "AadhaarCard" Nothing (Just convertedStatus) result.transactionId state.data.vehicleCategory)
                case respFrontImage, respBackImage of
                  Right (ValidateImageRes frontResp), Right (ValidateImageRes backResp) | isJust result.transactionId -> do
                    resp <- lift $ lift $ Remote.registerDriverAadhaar (Remote.makeAadhaarCardReq (Just backResp.imageId) (Just frontResp.imageId) detail.address true currentTime detail.dob (Just aadhaarNum) detail.fullName (convertedStatus) (fromMaybe "" result.transactionId))
                    case resp of
                      Left errPayload -> if result.status /= (Just "auto_declined") then void $ pure $ toast $ Remote.getCorrespondingErrorMessage errPayload else pure unit
                      Right response -> pure unit
                  Left errPayload , _ -> if result.status /= (Just "auto_declined") then void $ pure $ toast $ Remote.getCorrespondingErrorMessage errPayload else pure unit
                  _, Left errPayload -> if result.status /= (Just "auto_declined") then void $ pure $ toast $ Remote.getCorrespondingErrorMessage errPayload else pure unit 
                  _, _ -> if result.status /= (Just "auto_declined") then void $ pure $ toast $ getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER else pure unit 
              _, _ -> void $ pure $ toast (getString TIMEOUT)
            pure unit
        _ -> do
          void $ lift $ lift $ delay $ Milliseconds 100.0 -- This delay is added for toggleloader to work.
          void $ pure $ toast $ getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
      void $ lift $ lift $ toggleLoader false
      onBoardingFlow
    GO_TO_APP_UPDATE_POPUP_SCREEN _ -> appUpdatedFlow {title : (getString APP_UPDATE), description : (getString APP_UPDATE_MESSAGE), image : ""} ST.REG_PROF_PAN_AADHAAR
    AADHAAR_PAN_SELFIE_UPLOAD state _ -> onBoardingFlow
    LOGOUT_FROM_REGISTERATION_SCREEN -> logoutFlow
    GO_TO_HOME_SCREEN_FROM_REGISTERATION_SCREEN state -> 
      if state.props.manageVehicle
      then 
        driverProfileFlow
      else do
        modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{onBoardingDocs = Nothing, firstTimeOnboardingStatus = true } 
        modifyScreenState $ AcknowledgementScreenType $ \_ -> AckScreenInitData.initData { data {
          title = Just $ getString CONGRATULATIONS,
          description = Just $ getString (YOU_ARE_ALL_SET_TO_TAKE_RIDES merchantName),
          primaryButtonText = Just $ getString CONTINUE,
          illustrationAsset = "success_lottie.json"}}
        ackScreenFlow $ getDriverInfoFlow Nothing Nothing Nothing false (Just state.data.cityConfig.enableAdvancedBooking) true
    REFRESH_REGISTERATION_SCREEN -> do
      modifyScreenState $ RegisterScreenStateType (\registerScreen -> registerScreen { props { refreshAnimation = false}})
      onBoardingFlow
    GO_TO_ONBOARD_SUBSCRIPTION state -> do
      let onBoardingSubscriptionViewCount =  fromMaybe 0 (fromString (getValueToLocalNativeStore ONBOARDING_SUBSCRIPTION_SCREEN_COUNT))
      modifyScreenState $ OnBoardingSubscriptionScreenStateType (\onBoardingSubscriptionScreen -> onBoardingSubscriptionScreen{data{vehicleCategory = state.data.vehicleCategory}})
      onBoardingSubscriptionScreenFlow onBoardingSubscriptionViewCount
    REFERRAL_CODE_SUBMIT state -> do
      activateReferralCode state state.data.referralCode
      onBoardingFlow
    DOCUMENT_CAPTURE_FLOW state doctype -> do
      let defState = DocumentCaptureData.initData
      modifyScreenState $ DocumentCaptureScreenStateType (\_ -> defState { data { cityConfig = state.data.cityConfig, docType = doctype, vehicleCategory = state.data.vehicleCategory, linkedRc = state.data.linkedRc}})
      documentcaptureScreenFlow
    SELECT_LANG_FROM_REGISTRATION -> do
      modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{onBoardingDocs = Nothing}
      modifyScreenState $ SelectLanguageScreenStateType (\selectLangState -> selectLangState{ props{ onlyGetTheSelectedLanguage = false, selectedLanguage = "", selectLanguageForScreen = "", fromOnboarding = true}})
      selectLanguageFlow
  where 
    mkStatusList :: DriverRegistrationStatusResp -> Array ST.DocumentStatus
    mkStatusList (DriverRegistrationStatusResp driverRegistrationStatusResp) = 
      let driversDocument = driverRegistrationStatusResp.driverDocuments
          vehicleDoc = driverRegistrationStatusResp.vehicleDocuments
          vehicleDoc' = DA.foldl (\acc (API.VehicleDocumentItem vDoc) -> acc <> transfromDocumentStatusItem vDoc.documents vDoc.userSelectedVehicleCategory vDoc.verifiedVehicleCategory (Just vDoc.registrationNo)) [] vehicleDoc
          driversDocument' = transfromDocumentStatusItem driversDocument "" Nothing Nothing
      in driversDocument' <> vehicleDoc'
      
    mkRegSteps :: Array API.OnboardingDoc -> Array ST.StepProgress
    mkRegSteps onBoardingDocsArr = 
      map (\(API.OnboardingDoc step) ->
              { stageName : step.title,
                stage : RC.transformToRegisterationStep step.documentType,
                subtext : fromMaybe "" step.description,
                isMandatory : step.isMandatory,
                isDisabled : step.isDisabled,
                disableWarning : fromMaybe "" step.disableWarning,
                isHidden : step.isHidden,
                dependencyDocumentType : map (\item -> RC.transformToRegisterationStep item) step.dependencyDocumentType,
                rcNumberPrefixList : step.rcNumberPrefixList
              }) onBoardingDocsArr
    
    transfromDocumentStatusItem :: Array API.DocumentStatusItem ->  String -> Maybe String -> Maybe String -> Array ST.DocumentStatus
    transfromDocumentStatusItem statusItem userSelectedVehicle verifiedVehicleCategory regNo =
      map (\(API.DocumentStatusItem documentStatusItem) -> {
              vehicleType : RC.transformVehicleType $ Just userSelectedVehicle,
              status : getStatusValue documentStatusItem.verificationStatus,
              docType : RC.transformToRegisterationStep documentStatusItem.documentType,
              verificationMessage : documentStatusItem.verificationMessage,
              verifiedVehicleCategory : RC.transformVehicleType verifiedVehicleCategory,
              regNo : regNo
            }
          ) statusItem

    updateOnboardingDocs :: Boolean ->  FlowBT String Unit
    updateOnboardingDocs manageVehicle = do
      (resp :: (Either ErrorResponse API.OnboardingDocsRes)) <- lift $ lift $ HelpersAPI.callApi $ API.OnboardingDocsReq true manageVehicle
      case resp of
        Right docs -> modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{onBoardingDocs = Just docs }
        Left err -> pure unit -- handle error

updateDriverVersion :: Maybe Version -> Maybe Version -> FlowBT String Unit
updateDriverVersion dbClientVersion dbBundleVersion = do
  if (isJust dbClientVersion && isJust dbBundleVersion) then do
    let versionName = getValueToLocalStore VERSION_NAME
        bundle = getValueToLocalStore BUNDLE_VERSION
        Version clientVersion = stringToVersion versionName
        Version bundleVersion = stringToVersion bundle
        Version bundleVersion' = fromMaybe (Version bundleVersion) dbBundleVersion
        Version clientVersion' = fromMaybe (Version clientVersion) dbClientVersion
    if any (_ == -1) [clientVersion.minor, clientVersion.major, clientVersion.maintenance,bundleVersion.minor,bundleVersion.major,bundleVersion.maintenance] then pure unit
      else if ( bundleVersion' /= bundleVersion || clientVersion' /= clientVersion)  then do
      let initialData = mkUpdateDriverInfoReq ""
          requiredData = initialData{clientVersion = Just (Version clientVersion),bundleVersion = Just (Version bundleVersion)}
      (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT (UpdateDriverInfoReq requiredData)
      pure unit
    else pure unit
  else pure unit

aadhaarVerificationFlow :: FlowBT String Unit
aadhaarVerificationFlow = do
  hideSplashAndCallFlow (pure unit)
  out <- UI.aadhaarVerificationScreen
  case out of
    ENTER_AADHAAR_OTP state -> do
      void $ lift $ lift $ loaderText (getString VALIDATING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      res <- lift $ lift $ Remote.triggerAadhaarOtp state.data.aadhaarNumber
      void $ lift $ lift $ toggleLoader false
      case res of
        Right (GenerateAadhaarOTPResp resp) -> do
          -- let _ = toast resp.message
          case resp.statusCode of
            "1001" -> do
              modifyScreenState $ AadhaarVerificationScreenType (\_ -> state{props{currentStage = VerifyAadhaar, btnActive = false}})
              aadhaarVerificationFlow
            _ -> aadhaarVerificationFlow
        Left errorPayload -> do
          let errorCode = decodeErrorCode errorPayload.response.errorMessage
          case errorCode of
            "AADHAAR_NUMBER_NOT_EXIST" -> pure $ toast $ getString AADHAAR_NUMBER_NOT_EXIST
            "AADHAAR_ALREADY_LINKED" -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
            "INVALID_AADHAAR" -> do
              void $ pure $ toast $ decodeErrorMessage errorPayload.response.errorMessage
              modifyScreenState $ AadhaarVerificationScreenType (\_ -> state{props{showErrorAadhaar = true, btnActive = false}})
            _ -> do
              pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
              modifyScreenState $ AadhaarVerificationScreenType (\_ -> state{props{currentStage = AadhaarDetails, btnActive = false}})
          aadhaarVerificationFlow
    VERIFY_AADHAAR_OTP state -> do
      void $ lift $ lift $ toggleLoader true
      res <- lift $ lift $ Remote.verifyAadhaarOtp state.data.otp
      void $ lift $ lift $ toggleLoader false
      case res of
        Right (VerifyAadhaarOTPResp resp) -> do
          if resp.code == 200 then if state.props.fromHomeScreen then getDriverInfoFlow Nothing Nothing Nothing false Nothing false else onBoardingFlow
            else do
              void $ pure $ toast $ getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
              modifyScreenState $ AadhaarVerificationScreenType (\_ -> state{props{currentStage = EnterAadhaar, btnActive = false}})
              aadhaarVerificationFlow
        Left errorPayload -> do
          let stage = if (decodeErrorCode errorPayload.response.errorMessage) == "INVALID_OTP" then VerifyAadhaar else AadhaarDetails
          void $ pure if (decodeErrorCode errorPayload.response.errorMessage) == "INVALID_OTP" then toast $ getString INVALID_OTP else unit
          modifyScreenState $ AadhaarVerificationScreenType (\_ -> state{props{currentStage = stage, btnActive = false}})
          aadhaarVerificationFlow
    RESEND_AADHAAR_OTP state -> do
      res <- lift $ lift $ Remote.triggerAadhaarOtp state.data.aadhaarNumber
      case res of
        Right (GenerateAadhaarOTPResp resp) -> do
          case resp.statusCode of
            "1001" -> do
              modifyScreenState $ AadhaarVerificationScreenType (\_ -> state{props{currentStage = VerifyAadhaar}})
              aadhaarVerificationFlow
            _ -> do
              void $ pure $ toast $ getString VERIFICATION_FAILED
              modifyScreenState $ AadhaarVerificationScreenType (\_ -> state{props{currentStage = EnterAadhaar}})
              aadhaarVerificationFlow
        Left errorPayload -> do
          let errorCode = decodeErrorCode errorPayload.response.errorMessage
          case errorCode of
            "INVALID_AADHAAR" -> do
              void $ pure $ toast $ getString VERIFICATION_FAILED
              modifyScreenState $ AadhaarVerificationScreenType (\_ -> state{props{currentStage = EnterAadhaar,showErrorAadhaar = true, btnActive = false}})
            "GENERATE_AADHAAR_OTP_EXCEED_LIMIT" -> pure $ toast $ getString OTP_RESEND_LIMIT_EXCEEDED
            _ -> pure $ toast $ decodeErrorMessage errorPayload.response.errorMessage
          modifyScreenState $ AadhaarVerificationScreenType (\aadhaarVerification -> aadhaarVerification{props{currentStage = EnterAadhaar, btnActive = false}})
          aadhaarVerificationFlow
    GO_TO_HOME_FROM_AADHAAR -> do
      (GlobalState state) <- getState
      modifyScreenState $ AadhaarVerificationScreenType (\_ -> state.aadhaarVerificationScreen)
      getDriverInfoFlow Nothing Nothing Nothing false Nothing false
    LOGOUT_FROM_AADHAAR -> logoutFlow
    SEND_UNVERIFIED_AADHAAR_DATA state -> do
      void $ lift $ lift $ toggleLoader true
      unVerifiedAadhaarDataResp <- lift $ lift $ Remote.unVerifiedAadhaarData state.data.driverName state.data.driverGender state.data.driverDob
      case unVerifiedAadhaarDataResp of
        Right resp -> do
          void $ lift $ lift $ toggleLoader false
          if state.props.fromHomeScreen then getDriverInfoFlow Nothing Nothing Nothing false Nothing false else onBoardingFlow
        Left errorPayload -> do
          void $ lift $ lift $ toggleLoader false
          void $ pure $ toast $ decodeErrorMessage errorPayload.response.errorMessage
          aadhaarVerificationFlow


uploadDrivingLicenseFlow :: FlowBT String Unit
uploadDrivingLicenseFlow = do
  (GlobalState state) <- getState
  logField_ <- lift $ lift $ getLogFields
  flow <- UI.uploadDrivingLicense
  case flow of
    VALIDATE_DL_DETAILS state -> do
      validateImageResp <- lift $ lift $ Remote.validateImage (makeValidateImageReq state.data.imageFront "DriverLicense" Nothing Nothing Nothing state.data.vehicleCategory)
      case validateImageResp of
       Right (ValidateImageRes resp) -> do
        liftFlowBT $ logEvent logField_ "ny_driver_dl_photo_confirmed"
        modifyScreenState $ UploadDrivingLicenseScreenStateType (\uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { data {imageIDFront = resp.imageId}, props{errorVisibility = false}})
        registerDriverDLResp <- lift $ lift $ Remote.registerDriverDL (makeDriverDLReq state.data.driver_license_number state.data.dob state.data.dateOfIssue resp.imageId resp.imageId state.data.vehicleCategory)
        void $ pure $ setValueToLocalStore ENTERED_DL state.data.driver_license_number
        case registerDriverDLResp of
          Right (API.ApiSuccessResult resp) -> do
            liftFlowBT $ logEvent logField_ "ny_driver_submit_dl_details"
            setValueToLocalStore DOCUMENT_UPLOAD_TIME (getCurrentUTC "")
            modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { props {validating = false, successfulValidation = true}}
            modifyScreenState $ RegisterScreenStateType (\registerationScreen -> registerationScreen { data { vehicleDetailsStatus = ST.COMPLETED}})
            uploadDrivingLicenseFlow
          Left errorPayload -> do
            modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { data {dateOfIssue = Just ""}}
            if errorPayload.code == 400 || (errorPayload.code == 500 && (decodeErrorCode errorPayload.response.errorMessage) == "UNPROCESSABLE_ENTITY") then do
              let correspondingErrorMessage =  Remote.getCorrespondingErrorMessage errorPayload
              modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { props {errorVisibility = true, validating = false, validateProfilePicturePopUp = false, openHowToUploadManual = false}, data {errorMessage = correspondingErrorMessage}}
              uploadDrivingLicenseFlow
              else do
                void $ pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
                uploadDrivingLicenseFlow
        onBoardingFlow
       Left errorPayload -> do
        if errorPayload.code == 429 && (decodeErrorCode errorPayload.response.errorMessage) == "IMAGE_VALIDATION_EXCEED_LIMIT" then do
          modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen {props { validating = false}}
          modifyScreenState $ RegisterScreenStateType (\registerationScreen -> registerationScreen { props {limitReachedFor = Just "DL"}})
          onBoardingFlow
          else if errorPayload.code == 400 || (errorPayload.code == 500 && (decodeErrorCode errorPayload.response.errorMessage) == "UNPROCESSABLE_ENTITY") then do
            let correspondingErrorMessage =  Remote.getCorrespondingErrorMessage errorPayload
            modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { props {errorVisibility = true, validating = false}, data {errorMessage = correspondingErrorMessage, imageFrontUrl = state.data.imageFront, imageFront = "IMAGE_NOT_VALIDATED"}}
            uploadDrivingLicenseFlow
            else do
              void $ pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
              modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { data {imageFrontUrl = state.data.imageFront, imageFront = "IMAGE_NOT_VALIDATED"}, props{validating = false}}
              uploadDrivingLicenseFlow
      

    LOGOUT_ACCOUNT -> logoutFlow

    VALIDATE_DATA_API state -> do
      void $ lift $ lift $ loaderText (getString VALIDATING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      registerDriverDLResp <- lift $ lift $ Remote.registerDriverDL (makeDriverDLReq state.data.driver_license_number state.data.dob state.data.dateOfIssue state.data.imageIDFront state.data.imageIDFront state.data.vehicleCategory)
      void $ pure $ setValueToLocalStore ENTERED_DL state.data.driver_license_number
      case registerDriverDLResp of
        Right (API.ApiSuccessResult resp) -> do
          void $ lift $ lift $ toggleLoader false
          liftFlowBT $ logEvent logField_ "ny_driver_submit_dl_details"
          setValueToLocalStore DOCUMENT_UPLOAD_TIME (getCurrentUTC "")
          modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { props {validating = false}}
          onBoardingFlow
        Left errorPayload -> do
          void $ lift $ lift $ toggleLoader false
          modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { data {dateOfIssue = Just ""}}
          if errorPayload.code == 400 || (errorPayload.code == 500 && (decodeErrorCode errorPayload.response.errorMessage) == "UNPROCESSABLE_ENTITY") then do
            let cityConfig = getCityConfig state.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION)
                hideError = not cityConfig.uploadRCandDL && ((decodeErrorCode errorPayload.response.errorMessage) == "IMAGE_NOT_FOUND")
                correspondingErrorMessage =  Remote.getCorrespondingErrorMessage errorPayload
            modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { props {errorVisibility = not hideError, validating = false, validateProfilePicturePopUp = false, openHowToUploadManual = false}, data {errorMessage = if hideError then "" else correspondingErrorMessage }}
            uploadDrivingLicenseFlow
            else do
              void $ pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
              uploadDrivingLicenseFlow

    GOTO_VEHICLE_DETAILS_SCREEN -> addVehicleDetailsflow false
    GOTO_ONBOARDING_FLOW -> onBoardingFlow
    CHANGE_VEHICLE_FROM_DL_SCREEN -> do
      deleteValueFromLocalStore VEHICLE_CATEGORY
      onBoardingFlow
    CHANGE_LANG_FROM_DL_SCREEN -> do
      modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{onBoardingDocs = Nothing}
      modifyScreenState $ SelectLanguageScreenStateType (\selectLangState -> selectLangState{ props{ onlyGetTheSelectedLanguage = false, selectedLanguage = "", selectLanguageForScreen = "", fromOnboarding = true}})
      selectLanguageFlow


addVehicleDetailsflow :: Boolean -> FlowBT String Unit
addVehicleDetailsflow addRcFromProf = do
  logField_ <- lift $ lift $ getLogFields
  modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen  -> addVehicleDetailsScreen{props{addRcFromProfile = addRcFromProf }})
  (GlobalState globalState) <- getState
  flow <- UI.addVehicleDetails
  case flow of
    VALIDATE_DETAILS state -> do
      validateImageResp <- lift $ lift $ Remote.validateImage (makeValidateImageReq state.data.rc_base64 "VehicleRegistrationCertificate" Nothing Nothing Nothing state.data.vehicleCategory)
      void $ pure $ setValueToLocalStore ENTERED_RC state.data.vehicle_registration_number
      case validateImageResp of
       Right (ValidateImageRes resp) -> do
        liftFlowBT $ logEvent logField_ "ny_driver_rc_photo_confirmed"
        modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen -> addVehicleDetailsScreen {data { rcImageID = resp.imageId}})
        if (state.data.rcImageID == "IMAGE_NOT_VALIDATED") then do
          modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { data { dateOfRegistration = Just ""},props{ addRcFromProfile = addRcFromProf}}
          addVehicleDetailsflow state.props.addRcFromProfile
        else do
          registerDriverRCResp <- lift $ lift $ Remote.registerDriverRC (makeDriverRCReq state.data.vehicle_registration_number resp.imageId state.data.dateOfRegistration true state.data.vehicleCategory state.props.buttonIndex state.data.oxygen state.data.ventilator )
          case registerDriverRCResp of
            Right (API.ApiSuccessResult resp) -> do
              void $ pure $ Events.addEvent (Events.defaultEventObject "rc_upload_result") { module = "vehicle_registration_page", source = "RC", payload = "success" }
              void $ pure $ toast $ getString RC_ADDED_SUCCESSFULLY
              modifyScreenState $ HomeScreenStateType $ \hss -> hss{ props {acExplanationPopup = true}}
              liftFlowBT $ logEvent logField_ "ny_driver_submit_rc_details"
              setValueToLocalStore DOCUMENT_UPLOAD_TIME (getCurrentUTC "")
              (GlobalState state') <- getState
              let profileState = state'.driverProfileScreen
              if (not addRcFromProf) then do
                modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { props {validating = false, successfulValidation = true}}
                modifyScreenState $ RegisterScreenStateType (\registerationScreen -> registerationScreen { data { vehicleDetailsStatus = ST.COMPLETED}})
                addVehicleDetailsflow state.props.addRcFromProfile
              else do
                (DriverRegistrationStatusResp resp ) <- driverRegistrationStatusBT $ DriverRegistrationStatusReq true
                let multiRcStatus  = getStatusValue resp.rcVerificationStatus
                modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { props {validating = false, multipleRCstatus = multiRcStatus, validateProfilePicturePopUp = false}}
                addVehicleDetailsflow state.props.addRcFromProfile
            Left errorPayload -> do
              void $ pure $ Events.addEvent (Events.defaultEventObject "rc_upload_result") { module = "vehicle_registration_page", source = "RC", payload = "failure" }
              modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { data { dateOfRegistration = Just ""}, props{validating = false}}
              if errorPayload.code == 400 || (errorPayload.code == 500 && (decodeErrorCode errorPayload.response.errorMessage) == "UNPROCESSABLE_ENTITY") then do
                let correspondingErrorMessage =  Remote.getCorrespondingErrorMessage errorPayload
                modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { props {errorVisibility = true, validating = false, validateProfilePicturePopUp = false, openHowToUploadManual = false}, data {errorMessage = correspondingErrorMessage}}
                addVehicleDetailsflow state.props.addRcFromProfile
                else do
                  void $ pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
                  addVehicleDetailsflow state.props.addRcFromProfile
       Left errorPayload -> do
        void $ lift $ lift $ toggleLoader false
        if errorPayload.code == 429 && (decodeErrorCode errorPayload.response.errorMessage) == "IMAGE_VALIDATION_EXCEED_LIMIT" then do
          modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen -> addVehicleDetailsScreen {props { validateProfilePicturePopUp = false, validating = false, multipleRCstatus = FAILED}})
          modifyScreenState $ RegisterScreenStateType (\registerationScreen -> registerationScreen { props {limitReachedFor = Just "RC"}})
          if state.props.addRcFromProfile then addVehicleDetailsflow state.props.addRcFromProfile
          else onBoardingFlow
          else if errorPayload.code == 400 || (errorPayload.code == 500 && (decodeErrorCode errorPayload.response.errorMessage) == "UNPROCESSABLE_ENTITY") then do
            let correspondingErrorMessage =  Remote.getCorrespondingErrorMessage errorPayload
            modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { props {errorVisibility = true, validating = false}, data {errorMessage = correspondingErrorMessage , rcImageID = "IMAGE_NOT_VALIDATED" }}
            addVehicleDetailsflow state.props.addRcFromProfile 
            else do
              void $ pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
              modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { data {rcImageID = "IMAGE_NOT_VALIDATED" }, props{validating = false}}
              addVehicleDetailsflow state.props.addRcFromProfile
    VALIDATE_RC_DATA_API_CALL state -> do
        liftFlowBT $ logEvent logField_ "ny_driver_rc_photo_confirmed"
        modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen -> addVehicleDetailsScreen {data { rcImageID = state.data.rcImageID}})
        if (state.data.rcImageID == "IMAGE_NOT_VALIDATED") then do
          modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { data { dateOfRegistration = Just ""},props{ addRcFromProfile = addRcFromProf}}
          addVehicleDetailsflow state.props.addRcFromProfile
        else do
          registerDriverRCResp <- lift $ lift $ Remote.registerDriverRC (makeDriverRCReq state.data.vehicle_registration_number state.data.rcImageID state.data.dateOfRegistration true state.data.vehicleCategory state.props.buttonIndex state.data.oxygen state.data.ventilator)
          void $ pure $ setValueToLocalStore ENTERED_RC state.data.vehicle_registration_number
          case registerDriverRCResp of
            Right (API.ApiSuccessResult resp) -> do
              void $ pure $ toast $ getString RC_ADDED_SUCCESSFULLY
              modifyScreenState $ HomeScreenStateType $ \hss -> hss{ props {acExplanationPopup = true}}
              liftFlowBT $ logEvent logField_ "ny_driver_submit_rc_details"
              setValueToLocalStore DOCUMENT_UPLOAD_TIME (getCurrentUTC "")
              (GlobalState state') <- getState
              let profileState = state'.driverProfileScreen
              if (null profileState.data.rcDataArray) then do
                modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { props {validating = false}}
                onBoardingFlow
              else do
                modifyScreenState $ DriverProfileScreenStateType $ \driverProfileScreen -> driverProfileScreen { props { screenType = ST.VEHICLE_DETAILS}}
                driverProfileFlow
            Left errorPayload -> do
              modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { data { dateOfRegistration = Just ""}, props{validating = false}}
              if errorPayload.code == 400 || (errorPayload.code == 500 && (decodeErrorCode errorPayload.response.errorMessage) == "UNPROCESSABLE_ENTITY") then do
                let correspondingErrorMessage =  Remote.getCorrespondingErrorMessage errorPayload
                modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { props {errorVisibility = true, validating = false, validateProfilePicturePopUp = false, openHowToUploadManual = false}, data {errorMessage = correspondingErrorMessage}}
                addVehicleDetailsflow state.props.addRcFromProfile
                else do
                  void $ pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
                  addVehicleDetailsflow state.props.addRcFromProfile

    LOGOUT_USER -> logoutFlow
    REFER_API_CALL state -> do
      void $ lift $ lift $ loaderText (getString VALIDATING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      referDriverResponse <- lift $ lift $ Remote.referDriver (makeReferDriverReq state.data.referral_mobile_number)
      case referDriverResponse of
        Right (API.ApiSuccessResult resp) -> do
          void $ lift $ lift $ toggleLoader false
          modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen -> state{ props{isValid = false, openReferralMobileNumber = false, referralViewstatus = true}})
          addVehicleDetailsflow state.props.addRcFromProfile
        Left errorPayload -> do
          void $ lift $ lift $ toggleLoader false
          modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen -> state{ props{ isValid = true, openReferralMobileNumber = true, referralViewstatus = false}})
          addVehicleDetailsflow state.props.addRcFromProfile
    APPLICATION_STATUS_SCREEN -> applicationSubmittedFlow "StatusScreen"
    ONBOARDING_FLOW -> onBoardingFlow
    DRIVER_PROFILE_SCREEN -> do 
      modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen {props = driverProfileScreen.props { screenType = ST.VEHICLE_DETAILS, openSettings = false}})
      driverProfileFlow
    RC_ACTIVATION state -> do
      void $ lift $ lift $ loaderText (getString VALIDATING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      activateRCResp <- lift $ lift $ Remote.makeRcActiveOrInactive (Remote.makeRcActiveOrInactiveReq true (state.data.vehicle_registration_number))
      case activateRCResp of
        Right (API.ApiSuccessResult resp) -> do
          void $ lift $ lift $ toggleLoader false
          pure $ toast $ "RC-"<>state.data.vehicle_registration_number<> (getString IS_ACTIVE_NOW)
          refreshDriverProfile
          driverProfileFlow
        Left errorPayload -> do
          void $ lift $ lift $ toggleLoader false
          let codeMessage = decodeErrorCode errorPayload.response.errorMessage
          -- if codeMessage == "RC_ACTIVE_ON_OTHER_ACCOUNT" || codeMessage == "RC_Vehicle_ON_RIDE" then do
          --   modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> state {props = driverProfileScreen.props { openSettings = false, alreadyActive = true, screenType = ST.VEHICLE_DETAILS}})
          -- else do
          --    modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> state {props = driverProfileScreen.props { openSettings = false, screenType = ST.VEHICLE_DETAILS}})
          --    pure $ toast $ (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
          refreshDriverProfile
          driverProfileFlow
        where 
          refreshDriverProfile = do 
            getDriverInfoApiResp <- lift $ lift $ Remote.getDriverInfoApi ""
            case getDriverInfoApiResp of
              Right getDriverInfoResp -> do
                modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{driverInformation = Just getDriverInfoResp}
                updateDriverDataToStates
              Left _ -> pure unit
    CHANGE_VEHICLE_FROM_RC_SCREEN -> do
      deleteValueFromLocalStore VEHICLE_CATEGORY
      onBoardingFlow
    CHANGE_LANG_FROM_RC_SCREEN -> do
      modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{onBoardingDocs = Nothing}
      modifyScreenState $ SelectLanguageScreenStateType (\selectLangState -> selectLangState{ props{ onlyGetTheSelectedLanguage = false, selectedLanguage = "", selectLanguageForScreen = "", fromOnboarding = true}})
      selectLanguageFlow

applicationSubmittedFlow :: String -> FlowBT String Unit
applicationSubmittedFlow screenType = do
  hideSplashAndCallFlow $ pure unit
  logField_ <- lift $ lift $ getLogFields
  action <- UI.applicationStatus screenType
  setValueToLocalStore TEST_FLOW_FOR_REGISTRATOION "COMPLETED"
  case action of
    GO_TO_HOME_FROM_APPLICATION_STATUS -> getDriverInfoFlow Nothing Nothing Nothing false Nothing true
    GO_TO_UPLOAD_DL_SCREEN -> do
      let (GlobalState defaultEpassState') = defaultGlobalState
      modifyScreenState $ UploadDrivingLicenseScreenStateType (\_ -> defaultEpassState'.uploadDrivingLicenseScreen)
      modifyScreenState $ AddVehicleDetailsScreenStateType (\_ -> defaultEpassState'.addVehicleDetailsScreen)
      pure $ setText (getNewIDWithTag "EnterDrivingLicenseEditText") ""
      pure $ setText (getNewIDWithTag "ReEnterDrivingLicenseEditText") ""
      uploadDrivingLicenseFlow
    GO_TO_VEHICLE_DETAIL_SCREEN -> do
      let (GlobalState defaultEpassState') = defaultGlobalState
      modifyScreenState $ UploadDrivingLicenseScreenStateType (\_ -> defaultEpassState'.uploadDrivingLicenseScreen)
      modifyScreenState $ AddVehicleDetailsScreenStateType (\_ -> defaultEpassState'.addVehicleDetailsScreen)
      pure $ setText (getNewIDWithTag "VehicleRegistrationNumber") ""
      pure $ setText (getNewIDWithTag "ReenterVehicleRegistrationNumber") ""
      addVehicleDetailsflow false
    VALIDATE_NUMBER state -> do
      getAlternateMobileResp <- lift $ lift $ Remote.validateAlternateNumber (makeValidateAlternateNumberRequest (state.data.mobileNumber))
      case  getAlternateMobileResp of
            Right (DriverAlternateNumberResp resp) -> do
              modifyScreenState $ ApplicationStatusScreenType (\applicationStatusScreen -> state{props{enterMobileNumberView = true,enterOtp=true,buttonVisibilty=false,isValidOtp=false}})
              applicationSubmittedFlow screenType
            Left errorPayload -> do
              if (errorPayload.code == 400 && (decodeErrorCode errorPayload.response.errorMessage) == "INVALID_REQUEST") then do
                modifyScreenState $ ApplicationStatusScreenType (\applicationStatusScreen -> state {props{enterMobileNumberView = true, isAlternateMobileNumberExists = true,enterOtp = false}})
              else do
                pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
                modifyScreenState $ ApplicationStatusScreenType (\applicationStatusScreen -> state {props{enterMobileNumberView = false,enterOtp=false,buttonVisibilty=false}})
              applicationSubmittedFlow screenType
    VALIDATE_OTP state -> do
      getVerifyAlternateMobileOtpResp <- lift $ lift $ Remote.verifyAlternateNumberOTP (makeVerifyAlternateNumberOtpRequest (state.data.otpValue))
      case getVerifyAlternateMobileOtpResp of
        Right (API.ApiSuccessResult resp) -> do
          pure $ toast $ getString NUMBER_ADDED_SUCCESSFULLY
          modifyScreenState $ ApplicationStatusScreenType (\applicationStatusScreen ->  state{props{enterOtp = false , enterMobileNumberView = false , alternateNumberAdded = true}})
          applicationSubmittedFlow screenType
        Left errorPayload -> do
            if (errorPayload.code == 400 && (decodeErrorCode errorPayload.response.errorMessage) == "INVALID_AUTH_DATA") then do
                modifyScreenState $ ApplicationStatusScreenType (\applicationStatusScreen -> state{props{isValidOtp = true}})
                applicationSubmittedFlow screenType
            else if (errorPayload.code == 429 && (decodeErrorCode errorPayload.response.errorMessage == "HITS_LIMIT_EXCEED")) then do
              pure $ toast $ getString OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER
              (setValueToLocalStore INVALID_OTP_TIME (getCurrentUTC ""))
              modifyScreenState $ ApplicationStatusScreenType (\applicationStatusScreen -> state{props{enterOtp = false , enterMobileNumberView = false}})
              applicationSubmittedFlow screenType
            else do
              pure $ toast $ getString SOMETHING_WENT_WRONG
              applicationSubmittedFlow screenType
    RESEND_OTP_TO_ALTERNATE_NUMBER state-> do
      let number =  state.data.mobileNumber
      getAlternateMobileResendOtpResp <- lift $ lift $ Remote.resendAlternateNumberOTP (makeResendAlternateNumberOtpRequest (number))
      case getAlternateMobileResendOtpResp of
            Right (AlternateNumberResendOTPResp resp) -> do
                pure $ toast $ getString OTP_HAS_BEEN_RESENT
                applicationSubmittedFlow screenType
            Left errorPayload -> do
              if (errorPayload.code == 400 &&(decodeErrorCode errorPayload.response.errorMessage) == "AUTH_BLOCKED") then do
                  pure $ toast $ getString OTP_RESENT_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER
                  applicationSubmittedFlow screenType
              else do
                  pure $ toast $ getString SOMETHING_WENT_WRONG
                  applicationSubmittedFlow screenType
    LOGOUT_ACCOUT -> logoutFlow

driverCompleteProfileFlow :: FlowBT String Unit
driverCompleteProfileFlow = do
  action <- UI.driverCompleteProfileScreen
  case action of
    _ ->  homeScreenFlow

driverProfileFlow :: FlowBT String Unit
driverProfileFlow = do
  logField_ <- lift $ lift $ getLogFields
  modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen{props{isRideActive = getValueToLocalStore IS_RIDE_ACTIVE == "true"} })
  action <- UI.driverProfileScreen
  case action of
    GO_TO_HOME_FROM_PROFILE state -> do 
      modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{driverInformation = state.data.driverInfoResponse}
      homeScreenFlow
    GO_TO_REFERRAL_SCREEN_FROM_DRIVER_PROFILE_SCREEN -> referralFlow
    DRIVER_DETAILS_SCREEN -> driverDetailsFlow
    VEHICLE_DETAILS_SCREEN -> vehicleDetailsFlow
    ABOUT_US_SCREEN -> aboutUsFlow
    SELECT_LANGUAGE_SCREEN -> do
      liftFlowBT $ logEvent logField_ "ny_driver_language_select" 
      modifyScreenState $ SelectLanguageScreenStateType (\selectLangState -> selectLangState{ props{ onlyGetTheSelectedLanguage = false, selectedLanguage = "", selectLanguageForScreen = "", fromOnboarding = false}})
      selectLanguageFlow
    ON_BOARDING_FLOW -> onBoardingFlow
    DOCUMENTS_FLOW -> documentDetailsScreen
    GO_TO_LOGOUT -> logoutFlow
      
    HELP_AND_SUPPORT_SCREEN -> do
      liftFlowBT $ logEvent logField_ "ny_driver_help"
      let language = ( case getLanguageLocale languageKey of
                         "HI_IN" -> "hi"
                         "KN_IN" -> "kn"
                         "TA_IN" -> "ta"
                         "TE_IN" -> "te"
                         _       -> "en"
                     )
      categories' <- HU.sortIssueCategories language <$> Remote.getCategoriesBT language
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen { data { categories = categories', goBackTo = ScreenNames.DRIVER_PROFILE_SCREEN } } )
      helpAndSupportFlow
    GO_TO_DRIVER_HISTORY_SCREEN -> do
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> rideHistoryScreen{offsetValue = 0, currentTab = "COMPLETED"})
      myRidesScreenFlow
    GO_TO_EDIT_BANK_DETAIL_SCREEN -> editBankDetailsFlow
    NOTIFICATIONS_SCREEN -> notificationFlow Nothing
    GO_TO_BOOKING_OPTIONS_SCREEN state-> do
      let downgradeOptions = (downgradeOptionsConfig state.data.vehicleSelected) <$> state.data.downgradeOptions
      modifyScreenState $ BookingOptionsScreenType (\bookingOptions -> bookingOptions{  data{ vehicleType = state.data.driverVehicleType
                                                                                            , vehicleNumber = state.data.vehicleRegNumber
                                                                                            , vehicleName = state.data.vehicleModelName
                                                                                            , vehicleCapacity = state.data.capacity
                                                                                            , downgradeOptions = downgradeOptions}
                                                                                     , props{ downgraded = not (length (filter (not _.isSelected) downgradeOptions) > 0) && not (null downgradeOptions) , canSwitchToRental = state.props.canSwitchToRental, canSwitchToInterCity = state.props.canSwitchToInterCity, canSwitchToIntraCity = state.props.canSwitchToIntraCity}
                                                                                     })
      bookingOptionsFlow
    GO_TO_ACTIVATE_OR_DEACTIVATE_RC state -> do
      res <- lift $ lift $ Remote.makeRcActiveOrInactive (Remote.makeRcActiveOrInactiveReq (not state.data.isRCActive) (state.data.rcNumber))
      case res of
        Right (API.ApiSuccessResult response) -> do
          pure $ toast $ if state.data.isRCActive then "RC-"<>state.data.rcNumber<>" "<> (getString DEACTIVATED) else "RC-"<>state.data.rcNumber<> (getString IS_ACTIVE_NOW)
          globalstate <- getState
          if state.data.isRCActive then do
            (GetDriverInfoResp getDriverInfoResp) <- getDriverInfoDataFromCache globalstate false
            let status = getDriverStatus $ fromMaybe "" getDriverInfoResp.mode  
            when (status /= Offline) $ changeDriverStatus Offline
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {rcActive = false, rcDeactivePopup = true}})
          else do
            (GetDriverInfoResp getDriverInfoResp) <- getDriverInfoDataFromCache globalstate true
            let status = getDriverStatus $ fromMaybe "" getDriverInfoResp.mode
            config <- getAppConfigFlowBT Constants.appConfig
            updateSubscriptionForVehicleVariant (GetDriverInfoResp getDriverInfoResp) config
            when (status /= Offline && not (fromMaybe false getDriverInfoResp.isVehicleSupported)) $ changeDriverStatus Offline
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {rcActive = true, rcDeactivePopup = false}}) 
          modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> state {props = driverProfileScreen.props { openSettings = false, alreadyActive = false,screenType = ST.VEHICLE_DETAILS}})
          refreshDriverProfile
          driverProfileFlow
        Left errorPayload -> do
          let codeMessage = decodeErrorCode errorPayload.response.errorMessage
          if codeMessage == "RC_ACTIVE_ON_OTHER_ACCOUNT" || codeMessage == "RC_Vehicle_ON_RIDE" then do
            modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> state {props = driverProfileScreen.props { openSettings = false, alreadyActive = true, screenType = ST.VEHICLE_DETAILS}})
          else do
             modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> state {props = driverProfileScreen.props { openSettings = false, screenType = ST.VEHICLE_DETAILS}})
             pure $ toast $ (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
          refreshDriverProfile
          driverProfileFlow
      where 
          refreshDriverProfile = do 
            getDriverInfoApiResp <- lift $ lift $ Remote.getDriverInfoApi ""
            case getDriverInfoApiResp of
              Right getDriverInfoResp -> do
                modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{driverInformation = Just getDriverInfoResp}
                updateDriverDataToStates
              Left _ -> pure unit
    GO_TO_DELETE_RC state -> do 
      resp <- lift $ lift $ Remote.deleteRc (Remote.deleteRcReq state.data.rcNumber)
      case resp of
        Right res-> do
          pure $ toast $ "RC-"<>state.data.rcNumber<>" "<> (getString REMOVED)
          modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> state {props = driverProfileScreen.props { alreadyActive = false, screenType = ST.VEHICLE_DETAILS}})
        Left error-> do
          pure $ toast $ decodeErrorMessage error.response.errorMessage
          modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> state {props = driverProfileScreen.props { screenType = ST.VEHICLE_DETAILS}})
      driverProfileFlow
    ADD_RC state -> do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> state {props = driverProfileScreen.props { alreadyActive = false}})
      let (GlobalState defaultEpassState) = defaultGlobalState
      pure $ setText (getNewIDWithTag "VehicleRegistrationNumber") ""
      pure $ setText (getNewIDWithTag "ReenterVehicleRegistrationNumber") ""
      deleteValueFromLocalStore ENTERED_RC
      modifyScreenState $ AddVehicleDetailsScreenStateType (\_ -> defaultEpassState.addVehicleDetailsScreen)
      modifyScreenState $ RegistrationScreenStateType (\regScreenState -> regScreenState{ props{manageVehicle = true, manageVehicleCategory = Nothing}, data { linkedRc = Nothing}})
      onBoardingFlow
    SUBCRIPTION -> updateAvailableAppsAndGoToSubs
    GO_TO_CALL_DRIVER state -> do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> state {props = driverProfileScreen.props { alreadyActive = false}})
      void $ Remote.callDriverToDriverBT  state.data.rcNumber
      pure $ toast $ (getString CALL_REQUEST_HAS_BEEN_PLACED)
      driverProfileFlow
    TA.GO_HOME state -> do
      modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{driverInformation = state.data.driverInfoResponse}
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data {gender = fromMaybe "UNKNOWN" state.data.driverGender}})
      homeScreenFlow
    DRIVER_ALTERNATE_CALL_API1 updatedState -> do
      let number =  updatedState.data.driverEditAlternateMobile
      getAlternateMobileResp <- lift $ lift $ Remote.validateAlternateNumber (makeValidateAlternateNumberRequest (fromMaybe "" (number)))
      case  getAlternateMobileResp of
            Right (DriverAlternateNumberResp resp) -> do
                  modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> updatedState {props {enterOtpModal = true}})
                  driverProfileFlow
            Left errorPayload -> do
               if (errorPayload.code == 400 && ((decodeErrorCode errorPayload.response.errorMessage) == "INVALID_REQUEST")) then do
                modifyScreenState $ DriverProfileScreenStateType $ \driverProfileScreen -> updatedState { props {numberExistError = true, alternateNumberView = true}}
                driverProfileFlow
               else do
                  pure $ toast $ (getString ALTERNATE_NUMBER_CANNOT_BE_ADDED)
                  modifyScreenState $ DriverProfileScreenStateType $ \driverProfileScreen -> updatedState {props {isEditAlternateMobile = false, alternateNumberView = false}}
                  driverProfileFlow

    RESEND_ALTERNATE_OTP1 updatedState -> do
      let number = updatedState.data.driverEditAlternateMobile
      getAlternateMobileResendOtpResp <- lift $ lift $ Remote.resendAlternateNumberOTP (makeResendAlternateNumberOtpRequest (fromMaybe "" (number)))
      case getAlternateMobileResendOtpResp of
            Right (AlternateNumberResendOTPResp resp) -> do
                pure $ toast (getString OTP_RESENT)
                modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> updatedState)
                driverProfileFlow
            Left errorPayload -> do
              if (errorPayload.code == 400 &&(decodeErrorCode errorPayload.response.errorMessage) == "AUTH_BLOCKED") then do
                  modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> updatedState)
                  pure $ toast (getString OTP_RESEND_LIMIT_EXCEEDED)
                  driverProfileFlow
              else do
                  pure $ toast (decodeErrorMessage errorPayload.response.errorMessage)
                  modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> updatedState {data{ driverEditAlternateMobile = Nothing} , props{  otpIncorrect = false ,otpAttemptsExceeded = false , alternateNumberView = false, enterOtpModal=false, alternateMobileOtp = ""}})
                  driverProfileFlow

    VERIFY_OTP1 state -> do
       let toast_value = if not state.props.isEditAlternateMobile then (getString NUMBER_ADDED_SUCCESSFULLY) else (getString NUMBER_EDITED_SUCCESSFULLY)
           finalAlternateMobileNumber = state.data.driverEditAlternateMobile
       getVerifyAlternateMobileOtpResp <- lift $ lift $ Remote.verifyAlternateNumberOTP (makeVerifyAlternateNumberOtpRequest (state.props.alternateMobileOtp))
       case getVerifyAlternateMobileOtpResp of
         Right (API.ApiSuccessResult resp) -> do
              pure $ toast (toast_value)
              liftFlowBT $ logEvent logField_ "ny_driver_added_alternate_number"
              void $ pure $ finalAlternateMobileNumber >>= \alternateNumber -> void $ pure $ setCleverTapUserProp [{key : "Alternate Number", value : unsafeToForeign alternateNumber }]
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data = homeScreen.data {  driverAlternateMobile = finalAlternateMobileNumber  }})
              modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen {data { driverAlternateNumber = finalAlternateMobileNumber, driverEditAlternateMobile = Nothing}, props { otpIncorrect = false ,otpAttemptsExceeded = false , alternateMobileOtp = "",isEditAlternateMobile = false, alternateNumberView = false, enterOtpModal=false, checkAlternateNumber=false}})
              driverProfileFlow
         Left errorPayload -> do
            if (errorPayload.code == 400 && (decodeErrorCode errorPayload.response.errorMessage) == "INVALID_AUTH_DATA") then do
               if (state.data.otpLimit == 1)
               then do
                modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> state { props {otpAttemptsExceeded = true,  alternateNumberView = false, enterOtpModal=false}})
                setValueToLocalStore SET_ALTERNATE_TIME ((getCurrentUTC ""))
                driverProfileFlow
               else do
                let otpExceeded = ((state.data.otpLimit - 1) <= 0)
                modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> state {data  { otpLimit = state.data.otpLimit - 1 } , props {otpIncorrect = (if (otpExceeded) then false else true) ,otpAttemptsExceeded = otpExceeded,alternateMobileOtp = ""}})
                driverProfileFlow
            else if (errorPayload.code == 429 && (decodeErrorCode errorPayload.response.errorMessage == "HITS_LIMIT_EXCEED"))
                then do
                  modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> state { props {otpAttemptsExceeded = true, alternateNumberView = false, enterOtpModal=false}})
                  setValueToLocalStore SET_ALTERNATE_TIME ((getCurrentUTC ""))
                  driverProfileFlow
            else do
                pure $ toast (decodeErrorMessage errorPayload.response.errorMessage)
                modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> state {data{ driverEditAlternateMobile = Nothing} , props{  otpIncorrect = false ,otpAttemptsExceeded = false , alternateNumberView = false, enterOtpModal=false , alternateMobileOtp = ""}})
                driverProfileFlow

    ALTERNATE_NUMBER_REMOVE1 state -> do
       getAlternateMobileRemoveResp <- lift $ lift $ Remote.removeAlternateNumber (RemoveAlternateNumberRequest {} )
       case  getAlternateMobileRemoveResp of
          Right (API.ApiSuccessResult resp) -> do
                pure $ toast (getString NUMBER_REMOVED_SUCCESSFULLY)
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data = homeScreen.data {  driverAlternateMobile = Nothing  }})
                modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen {data {driverAlternateNumber = Nothing}, props{checkAlternateNumber = true, numberExistError = false, alternateNumberView = false }})
                driverProfileFlow
          Left errorPayload -> do
               void $ pure $ toast $ (decodeErrorCode errorPayload.response.errorMessage)
               modifyScreenState $ DriverProfileScreenStateType $ \driverProfileScreen -> state
               driverProfileFlow
    DRIVER_GENDER1 state -> do
      let genderSelected = state.data.driverGender
      let initialData = mkUpdateDriverInfoReq ""
          requiredData = initialData{gender = genderSelected}
      (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT (UpdateDriverInfoReq requiredData)
      pure $ toast (getString GENDER_UPDATED)
      void $ pure $ genderSelected >>= \gender -> void $ pure $ setCleverTapUserProp [{key : "gender", value :  unsafeToForeign gender }]
      modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> state { data {driverGender = genderSelected}})
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props = homeScreen.props {  showGenderBanner = false}})
      setValueToLocalStore IS_BANNER_ACTIVE "False"
      driverProfileFlow
    UPDATE_LANGUAGES language -> do
      let initialData = mkUpdateDriverInfoReq ""
          requiredData = initialData{languagesSpoken = Just language}
      (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT (UpdateDriverInfoReq requiredData)
      modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen { props {updateLanguages = false}})
      driverProfileFlow

    SAVED_LOCATIONS_SCREEN -> do
      let (GlobalState defaultEpassState') = defaultGlobalState
      modifyScreenState $ DriverSavedLocationScreenStateType (\_ ->  defaultEpassState'.driverSavedLocationScreen)
      goToLocationFlow 

    DRIVER_COMPLETING_PROFILE_SCREEN vehicleType -> do
      modifyScreenState $ DriverCompleteProfileScreenStateType (\state -> state { data {vehicleType = vehicleType}})
      driverCompleteProfileFlow
    
    VIEW_PENDING_VEHICLE rcNumber vehicleCategory -> do
      setValueToLocalStore ENTERED_RC rcNumber
      modifyScreenState $ RegistrationScreenStateType (\regScreenState -> regScreenState{ props{manageVehicle = true, manageVehicleCategory = Just vehicleCategory }, data { linkedRc = Nothing}})
      onBoardingFlow
    
    CANCELLATION_RATE_SCREEN updatedState -> do
      let (GlobalState defaultEpassState') = defaultGlobalState
      modifyScreenState $ CancellationRateScreenStateType (\_ -> updatedState)
      cancellationRateFlow

documentDetailsScreen :: FlowBT String Unit
documentDetailsScreen = do
  action <- UI.documentDetailsScreen
  documentDetailsScreen 

driverDetailsFlow :: FlowBT String Unit
driverDetailsFlow = do
  action <- UI.driverDetailsScreen
  case action of
    DRIVER_ALTERNATE_CALL_API updatedState -> do
      let number =  if (updatedState.props.isEditAlternateMobile) then updatedState.data.driverEditAlternateMobile else updatedState.data.driverAlternateMobile
      getAlternateMobileResp <- lift $ lift $ Remote.validateAlternateNumber (makeValidateAlternateNumberRequest (fromMaybe "" (number)))
      case  getAlternateMobileResp of
            Right (DriverAlternateNumberResp resp) -> do
                  modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen ->updatedState)
                  driverDetailsFlow
            Left errorPayload -> do
               let alternateNumber = if updatedState.props.isEditAlternateMobile then updatedState.data.driverAlternateMobile else Nothing
               if (errorPayload.code == 400 && ((decodeErrorCode errorPayload.response.errorMessage) == "INVALID_REQUEST")) then do
                modifyScreenState $ DriverDetailsScreenStateType $ \driverDetailsScreen -> updatedState { props {numberExistError = true,keyboardModalType = MOBILE__NUMBER }}
                driverDetailsFlow
               else do
                  pure $ toast $ getString SOMETHING_WENT_WRONG_TRY_AGAIN_LATER
                  modifyScreenState $ DriverDetailsScreenStateType $ \driverDetailsScreen -> updatedState { data {driverAlternateMobile = alternateNumber} , props {keyboardModalType = NONE,isEditAlternateMobile = false,checkAlternateNumber = (alternateNumber == Nothing)}}
                  driverDetailsFlow




    RESEND_ALTERNATE_OTP updatedState -> do
      let number =  if (updatedState.props.isEditAlternateMobile) then updatedState.data.driverEditAlternateMobile else updatedState.data.driverAlternateMobile
      getAlternateMobileResendOtpResp <- lift $ lift $ Remote.resendAlternateNumberOTP (makeResendAlternateNumberOtpRequest (fromMaybe "" (number)))
      case getAlternateMobileResendOtpResp of
            Right (AlternateNumberResendOTPResp resp) -> do
                pure $ toast $ getString OTP_HAS_BEEN_RESENT
                modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> updatedState)
                driverDetailsFlow
            Left errorPayload -> do
              if (errorPayload.code == 400 &&(decodeErrorCode errorPayload.response.errorMessage) == "AUTH_BLOCKED") then do
                  modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> updatedState)
                  pure $ toast $ getString OTP_RESENT_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER
                  driverDetailsFlow
              else do
                  pure $ toast $ getString SOMETHING_WENT_WRONG
                  modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> updatedState {data{ driverAlternateMobile = (if(updatedState.props.isEditAlternateMobile) then updatedState.data.driverAlternateMobile else Nothing), driverEditAlternateMobile = Nothing} , props{  otpIncorrect = false ,otpAttemptsExceeded = false ,keyboardModalType = NONE , alternateMobileOtp = "",checkAlternateNumber = not updatedState.props.isEditAlternateMobile }})
                  driverDetailsFlow


    VERIFY_OTP state -> do
       let toast_value = if not state.props.isEditAlternateMobile then (getString NUMBER_ADDED_SUCCESSFULLY) else (getString NUMBER_EDITED_SUCCESSFULLY)
           finalAlternateMobileNumber = state.data.driverEditAlternateMobile
       getVerifyAlternateMobileOtpResp <- lift $ lift $ Remote.verifyAlternateNumberOTP (makeVerifyAlternateNumberOtpRequest (state.props.alternateMobileOtp))
       case getVerifyAlternateMobileOtpResp of
         Right (API.ApiSuccessResult resp) -> do
              pure $ toast (toast_value)
              modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> state {data {driverAlternateMobile = finalAlternateMobileNumber , driverEditAlternateMobile = Nothing} , props {otpIncorrect = false ,otpAttemptsExceeded = false ,keyboardModalType = NONE , alternateMobileOtp = "",checkAlternateNumber = false,isEditAlternateMobile = false}})
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data = homeScreen.data {  driverAlternateMobile = finalAlternateMobileNumber  }})
              modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen {data = driverProfileScreen.data { driverAlternateNumber = finalAlternateMobileNumber}})
              driverDetailsFlow
         Left errorPayload -> do
            if (errorPayload.code == 400 && (decodeErrorCode errorPayload.response.errorMessage) == "INVALID_AUTH_DATA") then do
               if (state.data.otpLimit == 1)
               then do
                modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> state { props {otpAttemptsExceeded = true, keyboardModalType = NONE}})
                setValueToLocalStore SET_ALTERNATE_TIME ((getCurrentUTC ""))
                driverDetailsFlow
               else do
                let otpExceeded = ((state.data.otpLimit - 1) <= 0)
                modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> state {data  { otpLimit = state.data.otpLimit - 1 } , props {otpIncorrect = (if (otpExceeded) then false else true) ,otpAttemptsExceeded = otpExceeded,alternateMobileOtp = ""}})
                driverDetailsFlow
            else if (errorPayload.code == 429 && (decodeErrorCode errorPayload.response.errorMessage == "HITS_LIMIT_EXCEED"))
                then do
                  modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> state { props {otpAttemptsExceeded = true, keyboardModalType = NONE}})
                  setValueToLocalStore SET_ALTERNATE_TIME ((getCurrentUTC ""))
                  driverDetailsFlow
            else do
                pure $ toast $ getString SOMETHING_WENT_WRONG
                modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> state {data{ driverAlternateMobile = (if(state.props.isEditAlternateMobile) then state.data.driverAlternateMobile else Nothing), driverEditAlternateMobile = Nothing} , props{  otpIncorrect = false ,otpAttemptsExceeded = false ,keyboardModalType = NONE , alternateMobileOtp = "",checkAlternateNumber = not state.props.isEditAlternateMobile }})
                driverDetailsFlow


    ALTERNATE_NUMBER_REMOVE state -> do
       getAlternateMobileRemoveResp <- lift $ lift $ Remote.removeAlternateNumber (RemoveAlternateNumberRequest {} )
       case  getAlternateMobileRemoveResp of
          Right (API.ApiSuccessResult resp) -> do
                pure $ toast (getString NUMBER_REMOVED_SUCCESSFULLY)
                modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> state { data {driverAlternateMobile = Nothing}, props  { checkAlternateNumber = true}})
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data = homeScreen.data {  driverAlternateMobile = Nothing  }})
                modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen {data = driverProfileScreen.data { driverAlternateNumber = Nothing}})
                driverDetailsFlow
          Left errorPayload -> do
               void $ pure $ toast $ getString SOMETHING_WENT_WRONG
               modifyScreenState $ DriverDetailsScreenStateType $ \driverDetailsScreen -> state
               driverDetailsFlow

    GO_TO_HOMESCREEN state -> do
       modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> state {props {keyboardModalType = NONE}} )
       homeScreenFlow

    DRIVER_GENDER state -> do
        let genderSelected = state.data.driverGender
        let initialData = mkUpdateDriverInfoReq ""
            requiredData = initialData{gender = genderSelected}
        (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT (UpdateDriverInfoReq requiredData)
        pure $ toast (getString GENDER_UPDATED)
        modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> state { data {driverGender = genderSelected}})
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props = homeScreen.props {  showGenderBanner = false}})
        setValueToLocalStore IS_BANNER_ACTIVE "False"
        driverDetailsFlow 

  pure unit

vehicleDetailsFlow :: FlowBT String Unit
vehicleDetailsFlow = do
  action <- UI.vehicleDetailsScreen
  case action of
    UPDATE_VEHICLE_INFO  updatedState -> do
      (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT $ UpdateDriverInfoReq (mkUpdateDriverInfoReq "")
      vehicleDetailsFlow

aboutUsFlow :: FlowBT String Unit
aboutUsFlow = do
  action <- UI.aboutUsScreen
  case action of
    GO_TO_DRIVER_HOME_SCREEN -> homeScreenFlow
  pure unit

cancellationRateFlow :: FlowBT String Unit
cancellationRateFlow = UI.cancellationRateScreen

goToLocationFlow :: FlowBT String Unit
goToLocationFlow = do
  action <- UI.driverSavedLocationScreen
  case action of 
    EXIT_FROM_SCREEN -> homeScreenFlow
    AUTO_COMPLETE updatedState searchVal currentLat currentLon -> do
      resp <- lift $ lift $ Remote.autoComplete searchVal currentLat currentLon (EHC.getMapsLanguageFormat (getLanguageLocale languageKey))
      case resp of 
        Right (API.AutoCompleteResp autoCompleteResp) -> 
          modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> updatedState { data { predictions = 
            map (\(API.Prediction pred) -> {
            title : (fromMaybe "" ((split (Pattern ",") (pred.description)) !! 0)),
            description : drop ((fromMaybe 0 (indexOf (Pattern ",") (pred.description))) + 2) (pred.description),
            placeId : pred.placeId,
            distance : pred.distance
          }) autoCompleteResp.predictions
          }})
        Left _ -> pure unit
      goToLocationFlow
    GET_LOCATION_NAME state -> do
      resp <- lift $ lift $ Remote.placeName (Remote.makePlaceNameReq state.data.saveLocationObject.position.lat state.data.saveLocationObject.position.lon (EHC.getMapsLanguageFormat (getLanguageLocale languageKey)))
      case resp of
        Right (API.GetPlaceNameResp placeNameResp) ->
          case placeNameResp!!0 of 
            Just (API.PlaceName placeName) -> do
              let (API.LatLong latLong) = placeName.location
              modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> state { data { saveLocationObject { address = placeName.formattedAddress, position { lat = latLong.lat , lon = latLong.lon } } },  props { viewType = ST.LOCATE_ON_MAP}} )
            Nothing -> pure unit
        Left _ -> pure unit
      goToLocationFlow

    SAVE_LOCATION state -> do
      addDriverHomeLocationResp <- lift $ lift $ Remote.addDriverHomeLocation state.data.saveLocationObject.position.lat state.data.saveLocationObject.position.lon state.data.saveLocationObject.address state.data.saveLocationObject.tag
      case addDriverHomeLocationResp of
        Right _ -> do 
          void $ pure $ toast $ getString GOTO_LOC_ADDED
          let (GlobalState defaultEpassState) = defaultGlobalState
          if state.props.gotBackToHomeScreen then do
            getLocations <- lift $ lift $ Remote.getDriverHomeLocation ""
            case getLocations of
              Right locData -> modifyScreenState $ HomeScreenStateType (\ screenState -> screenState { data { driverGotoState {savedLocationsArray = getLocationArray locData}}})
              Left errorPayload -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
            void $ pure $ hideKeyboardOnNavigation true
            homeScreenFlow 
          else do
            modifyScreenState $ DriverSavedLocationScreenStateType (\_ ->  defaultEpassState.driverSavedLocationScreen)
            goToLocationFlow
        Left errorPayload -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
      goToLocationFlow

    GET_PLACE_NAME updatedState placeId -> do
      resp <- lift $ lift $ Remote.placeName (Remote.makePlaceNameReqByPlaceId placeId (EHC.getMapsLanguageFormat (getLanguageLocale languageKey)))
      case resp of
        Right (API.GetPlaceNameResp placeNameResp) ->
          case placeNameResp!!0 of 
            Just (API.PlaceName placeName) -> do
              let (API.LatLong latLong) = placeName.location
              liftFlowBT $ runEffectFn1 JB.locateOnMap JB.locateOnMapConfig { goToCurrentLocation = false, lat = latLong.lat, lon = latLong.lon, geoJson = "", points = [], zoomLevel = zoomLevel}
              modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> updatedState { data { saveLocationObject { address = placeName.formattedAddress, position { lat = latLong.lat , lon = latLong.lon } } },  props { viewType = ST.LOCATE_ON_MAP}} )
            Nothing -> pure unit
        Left _ -> pure unit
      goToLocationFlow
    
    DELETE_PLACE state id-> do
      deleteResp <- lift $ lift $ Remote.deleteDriverHomeLocation id
      case deleteResp of
        Right _ -> pure $ toast $ getString GOTO_LOC_REMOVED
        Left errorPayload -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
      modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> state { props { confirmDelete = false}} )
      goToLocationFlow
    
    UPDATE_HOME_LOCATION state -> do
      updateResp <- lift $ lift $ Remote.updateDriverHomeLocation state.data.saveLocationObject.position.place state.data.saveLocationObject.position.lat state.data.saveLocationObject.position.lon state.data.saveLocationObject.address state.data.saveLocationObject.tag
      case updateResp of
        Right _ -> pure $ toast $ getString GOTO_LOC_UPDATED
        Left errorPayload -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
      let (GlobalState defaultEpassState') = defaultGlobalState
      modifyScreenState $ DriverSavedLocationScreenStateType (\_ ->  defaultEpassState'.driverSavedLocationScreen)
      goToLocationFlow
    CHANGE_VIEW -> goToLocationFlow


selectLanguageFlow :: FlowBT String Unit
selectLanguageFlow = do
  let selectLang = getLanguageLocale languageKey
  modifyScreenState $ SelectLanguageScreenStateType (\selectLangState -> selectLangState{ props{ selectedLanguage = if selectLangState.props.onlyGetTheSelectedLanguage then selectLangState.props.selectedLanguage else if (selectLang == "__failed") then "EN_US" else selectLang}})
  action <- UI.selectLanguageScreen
  case action of
    CHANGE_LANGUAGE state -> do
      (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT $ UpdateDriverInfoReq $ mkUpdateDriverInfoReq ""
      if state.props.fromOnboarding then onBoardingFlow else driverProfileFlow
    LANGUAGE_CONFIRMED state -> do
      setValueToLocalStore LMS_SELECTED_LANGUAGE_CACHE state.props.selectedLanguage
      case state.props.selectLanguageForScreen of 
        "LMS_QUIZ_SCREEN" -> modifyScreenState $ LmsQuizScreenStateType (\lmsQuizScreen -> lmsQuizScreen { props {selectedLanguage = state.props.selectedLanguage, showShimmer = true}})
        "LMS_VIDEO_SCREEN" -> modifyScreenState $ LmsVideoScreenStateType (\lmsVideoScreen -> lmsVideoScreen { props {selectedLanguage = state.props.selectedLanguage, showShimmer = true}})
        _ -> pure unit
      modifyScreenState $ SelectLanguageScreenStateType (\selectLangState -> selectLangState{ props{ onlyGetTheSelectedLanguage = false, selectedLanguage = "", selectLanguageForScreen = ""}})
      pure unit

bookingOptionsFlow :: FlowBT String Unit
bookingOptionsFlow = do
  (API.DriverVehicleServiceTierResponse resp) <- HelpersAPI.callApiBT $ API.DriverVehicleServiceTierReq
  let (FeaturesConfigData featuresRemoteConfig) = CommonRC.featuresConfigData (getValueToLocalStore DRIVER_LOCATION)
  let filterByDeliveryBike = 
        if (not featuresRemoteConfig.enableDeliveryBike)
        then filter (\(API.DriverVehicleServiceTier tier) -> tier.serviceTierType /= DELIVERY_BIKE) resp.tiers
        else resp.tiers
      ridePreferences' = transfromRidePreferences filterByDeliveryBike
      canSwitchToInterCity' = resp.canSwitchToInterCity
      canSwitchToRental' = resp.canSwitchToRental
      canSwitchToIntraCity' = resp.canSwitchToIntraCity
      defaultRide = fromMaybe BookingOptionsScreenData.defaultRidePreferenceOption $ find (\item -> item.isDefault) ridePreferences'

  modifyScreenState $ BookingOptionsScreenType (\bookingOptions ->  bookingOptions
   { data { airConditioned = resp.airConditioned
           , vehicleType = HU.getVehicleMapping defaultRide.serviceTierType
           , vehicleName = defaultRide.name
           , ridePreferences = ridePreferences'
           , defaultRidePreference = defaultRide
          }, 
     props {
             canSwitchToRental = canSwitchToRental',
             canSwitchToInterCity = canSwitchToInterCity',
             canSwitchToIntraCity = canSwitchToIntraCity'
           } 
    })

  action <- UI.bookingOptions
  case action of
    UPDATE_AC_AVAILABILITY state toggleVal -> do
      (resp :: (Either ErrorResponse API.UpdateDriverVehicleServiceTierResp)) <- lift $ lift $ HelpersAPI.callApi $ Remote.mkUpdateAirConditionWorkingStatus toggleVal
      case resp of
        Right _ -> do
          let toastMessage = if toggleVal 
              then getString VARIANTS_ARE_SWITCHED 
              else getString NON_AC_ARE_SWITCHED
          pure $ toast toastMessage
          bookingOptionsFlow
        Left _ -> bookingOptionsFlow
    CHANGE_RIDE_PREFERENCE state service -> do
      (_ :: API.UpdateDriverVehicleServiceTierResp) <- HelpersAPI.callApiBT $ Remote.mkUpdateDriverVehiclesServiceTier service
      bookingOptionsFlow
    HOME_SCREEN_FROM_BOOKING_PREFS -> handleDeepLinksFlow Nothing Nothing Nothing
    SELECT_CAB state toggleDowngrade -> do
      void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      let canDowngradeToSedan = isJust $ (filter (\item -> item.vehicleVariant == "SEDAN" && item.isSelected) state.data.downgradeOptions) !! 0
          canDowngradeToHatchback = isJust $ (filter (\item -> item.vehicleVariant == "HATCHBACK" && item.isSelected) state.data.downgradeOptions) !! 0
          canDowngradeToTaxi = isJust $ (filter (\item -> item.vehicleVariant == "TAXI" && item.isSelected) state.data.downgradeOptions) !! 0
          
      let initialData = mkUpdateDriverInfoReq ""
          requiredData = initialData{canDowngradeToSedan = Just canDowngradeToSedan,canDowngradeToHatchback = Just canDowngradeToHatchback,canDowngradeToTaxi = Just canDowngradeToTaxi}
      (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT ((UpdateDriverInfoReq) requiredData)
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> driverProfile{ data{ vehicleSelected = transformSelectedVehicles state.data.downgradeOptions} })
      void $ lift $ lift $ toggleLoader false
      if toggleDowngrade then do
        modifyScreenState $ BookingOptionsScreenType (\bookingOptions -> bookingOptions{ props{ downgraded = not state.props.downgraded } })
        bookingOptionsFlow
      else
        driverProfileFlow
    ENABLE_RENTAL_INTERCITY_RIDE state -> do
      let initialData = mkUpdateDriverInfoReq ""
          requiredData = initialData{canSwitchToRental = state.props.canSwitchToRental, canSwitchToInterCity = state.props.canSwitchToInterCity, canSwitchToIntraCity = state.props.canSwitchToIntraCity}
      (UpdateDriverInfoResp (GetDriverInfoResp updateDriverResp)) <- Remote.updateDriverInfoBT ((UpdateDriverInfoReq) requiredData)
      modifyScreenState $ DriverProfileScreenStateType (\driverProfile -> driverProfile{ props{ canSwitchToRental = updateDriverResp.canSwitchToRental, canSwitchToInterCity = updateDriverResp.canSwitchToInterCity, canSwitchToIntraCity = updateDriverResp.canSwitchToIntraCity} })
      bookingOptionsFlow
    GO_TO_PROFILE -> driverProfileFlow
    EXIT_TO_RATE_CARD_SCREEN bopState -> do
      (response :: (Either ErrorResponse API.GetDriverRateCardRes)) <- lift $ lift $ HelpersAPI.callApi $ API.GetDriverRateCardReq Nothing (Just RC.defaultSliderDist)
      case response of
        Right resp -> do
          let 
            prefs = map (\item ->  HU.setPerKmRate resp item ) bopState.data.ridePreferences
            nonZeroPrefs = filter (\item -> fromMaybe 0.0 item.perKmRate > 0.0 ) prefs
          modifyScreenState $ RateCardScreenStateType $ \_ -> RateCardScreenData.initData{data{ ridePreferences = nonZeroPrefs, rateCard = bopState.data.rateCard}}
          rateCardScreenFlow
        Left errorPayload -> do
          void $ pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
          bookingOptionsFlow
  where 
    transfromRidePreferences :: Array API.DriverVehicleServiceTier -> Array ST.RidePreference
    transfromRidePreferences = 
      map (\(API.DriverVehicleServiceTier item) -> {
          airConditioned : item.airConditioned,
          driverRating : item.driverRating,
          isDefault : item.isDefault,
          isSelected : item.isSelected,
          longDescription : item.longDescription,
          luggageCapacity : item.luggageCapacity,
          name : item.name,
          seatingCapacity : item.seatingCapacity,
          serviceTierType : item.serviceTierType,
          shortDescription : item.shortDescription,
          vehicleRating : item.vehicleRating,
          isUsageRestricted : fromMaybe false item.isUsageRestricted,
          priority : fromMaybe 0 item.priority,
          rateCardData : Nothing,
          perKmRate : Nothing,
          farePolicyHour : Nothing
        }
      )

rateCardScreenFlow :: FlowBT String Unit
rateCardScreenFlow = do
  config <- getAppConfigFlowBT Constants.appConfig
  let cityConfig = getCityConfig config.cityConfig (getValueToLocalStore DRIVER_LOCATION)
  modifyScreenState $ RateCardScreenStateType (\rateCardScreen -> rateCardScreen{data {cityConfig = cityConfig}})
  action <- UI.rateCardScreen
  case action of
    TA.RATE_CARD_API updatedState dist -> do
      (response :: (Either ErrorResponse API.GetDriverRateCardRes)) <- lift $ lift $ HelpersAPI.callApi $ API.GetDriverRateCardReq Nothing (Just dist)
      case response of
        Left _ -> pure unit
        Right resp -> do
          let 
            prefs = map (\item ->  HU.setPerKmRate resp item ) updatedState.data.ridePreferences
            nonZeroPrefs = filter (\item -> fromMaybe 0.0 item.perKmRate > 0.0 ) prefs
          modifyScreenState $ RateCardScreenStateType (\rcsType -> rcsType { props { sliderLoading = false}, data { ridePreferences = nonZeroPrefs}})
      rateCardScreenFlow
    _ -> rateCardScreenFlow

helpAndSupportFlow :: FlowBT String Unit
helpAndSupportFlow = do
  config <- getAppConfigFlowBT Constants.appConfig
  let cityConfig = getCityConfig config.cityConfig (getValueToLocalStore DRIVER_LOCATION)
  modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen{data {cityConfig = cityConfig}})
  action <- UI.helpAndSupportScreen
  case action of
    WRITE_TO_US_SCREEN -> writeToUsFlow
    RIDE_SELECTION_SCREEN selectedCategory -> do
      modifyScreenState $ RideSelectionScreenStateType (\rideHistoryScreen -> rideHistoryScreen { offsetValue = 0, selectedCategory = selectedCategory } )
      rideSelectionScreenFlow
    REPORT_ISSUE_CHAT_SCREEN selectedCategory -> do
      let language = ( case getLanguageLocale languageKey of
                       "HI_IN" -> "hi"
                       "KN_IN" -> "kn"
                       "TA_IN" -> "ta"
                       "TE_IN" -> "te"
                       _       -> "en"
                     )
      (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT selectedCategory.categoryId language
      let getOptionsRes' = (mapWithIndex (\index (Option x) ->
        { option : x.option
        , issueOptionId : x.issueOptionId
        , label : x.label
        }
      ) getOptionsRes.options)
      let categoryName = getCategoryName $ fromMaybe "" selectedCategory.categoryAction
      modifyScreenState $ ReportIssueChatScreenStateType (\_ -> ReportIssueScreenData.initData { data { categoryName = categoryName, categoryAction = fromMaybe "" selectedCategory.categoryAction, categoryId = selectedCategory.categoryId, options = getOptionsRes' }, props { isReversedFlow = (fromMaybe "" selectedCategory.categoryAction) == "LOST_AND_FOUND" } })
      issueReportChatScreenFlow
    ISSUE_LIST_GO_BACK_SCREEN updatedState -> do
       modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> updatedState)
       helpAndSupportFlow
    ON_GOING_ISSUE_SCREEN updatedState -> do
       modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> updatedState)
       helpAndSupportFlow
    RESOLVED_ISSUE_SCREEN updatedState -> do
       modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> updatedState)
       helpAndSupportFlow
    REMOVE_ISSUE_SCREEN issueId updatedState -> do
       resp <- Remote.deleteIssueBT issueId
       pure $ toast $ getString ISSUE_REMOVED_SUCCESSFULLY
       modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> updatedState)
       helpAndSupportFlow
    DUMMY_RIDE_REQUEST updatedState -> do
      (resp :: API.ApiSuccessResult) <- HelpersAPI.callApiBT $ DummyRideRequestReq ""
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> updatedState)
      helpAndSupportFlow
    GO_BACK_TO_PROFILE_SCREEN updatedState -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\_ -> updatedState)
      driverProfileFlow
    GO_BACK_TO_HELP_AND_SUPPORT updatedState -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> updatedState)
      helpAndSupportFlow
    GO_BACK_TO_HOME_SCREEN_FROM_HELP updatedState -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> updatedState)
      homeScreenFlow
    GO_BACK_TO_TRIP_DETAILS updatedState -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> updatedState)
      tripDetailsScreenFlow

writeToUsFlow :: FlowBT String Unit
writeToUsFlow = do
  action <- UI.writeToUsScreen
  case action of
    GO_TO_HOME_SCREEN_FLOW -> homeScreenFlow

permissionsScreenFlow :: Maybe Event -> Maybe GetRidesHistoryResp -> Maybe Boolean -> FlowBT String Unit
permissionsScreenFlow event activeRideResp isActiveRide = do
  logField_ <- lift $ lift $ getLogFields
  hideSplashAndCallFlow $ pure unit
  void $ pure $ hideKeyboardOnNavigation true
  (GlobalState state) <- getState
  action <- UI.permissions
  case action of
    DRIVER_HOME_SCREEN -> do
      liftFlowBT $ logEvent logField_ "ny_driver_submit_permissions"
      handleDeepLinksFlow event activeRideResp isActiveRide
    LOGOUT_FROM_PERMISSIONS_SCREEN -> logoutFlow
    GO_TO_REGISTERATION_SCREEN state -> do
      let allChecked = state.props.isNotificationPermissionChecked && state.props.isOverlayPermissionChecked && state.props.isAutoStartPermissionChecked
          partialChecked = state.props.isNotificationPermissionChecked || state.props.isOverlayPermissionChecked || state.props.isAutoStartPermissionChecked
      modifyScreenState $ RegisterScreenStateType (\registerationScreen -> registerationScreen { data { permissionsStatus = case allChecked, partialChecked of
                                                                                                                                true, _ -> ST.COMPLETED
                                                                                                                                false, true -> ST.IN_PROGRESS
                                                                                                                                _, _ -> ST.NOT_STARTED } })
      onBoardingFlow

myRidesScreenFlow :: FlowBT String Unit
myRidesScreenFlow = do
  let (GlobalState defGlobalState) = defaultGlobalState
  flow <- UI.rideHistory
  case flow of
    REFRESH state -> do
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> state{offsetValue = 0})
      myRidesScreenFlow
    HOME_SCREEN -> homeScreenFlow
    PROFILE_SCREEN -> driverProfileFlow
    GO_TO_REFERRAL_SCREEN -> referralFlow
    LOADER_OUTPUT state -> do
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> state{offsetValue = state.offsetValue + 8})
      myRidesScreenFlow
    FILTER currTab-> do
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> rideHistoryScreen{currentTab = currTab})
      myRidesScreenFlow
    GO_TO_TRIP_DETAILS selectedCard -> do
      sourceMod <- translateString selectedCard.source 400
      destinationMod <- translateString selectedCard.destination 400
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {data {
      tripId = selectedCard.id,
      date = selectedCard.date,
      time = selectedCard.time,
      source = sourceMod,
      destination = destinationMod,
      totalAmount = selectedCard.total_amount,
      distance = selectedCard.rideDistance,
      status = selectedCard.status,
      vehicleType = selectedCard.vehicleType,
      rider = selectedCard.riderName,
      customerExtraFee = selectedCard.customerExtraFee,
      purpleTagVisibility = selectedCard.purpleTagVisibility,
      gotoTagVisibility = selectedCard.gotoTagVisibility,
      spLocTagVisibility = selectedCard.spLocTagVisibility,
      specialZoneLayoutBackground = selectedCard.specialZoneLayoutBackground,
      specialZoneImage = selectedCard.specialZoneImage,
      specialZoneText = selectedCard.specialZoneText,
      specialZonePickup = selectedCard.specialZonePickup, 
      stops = fst <<< HU.getStopName <$> selectedCard.stops
      }})

      tripDetailsScreenFlow
    NOTIFICATION_FLOW -> notificationFlow Nothing
    SELECTED_TAB state -> do
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> state{offsetValue = 0})
      myRidesScreenFlow
    OPEN_PAYMENT_HISTORY state-> do
      resp <- lift $ lift $ Remote.getPaymentHistory (getcurrentdate "") (getDatebyCount state.data.pastDays) Nothing
      case resp of
        Right (GetPaymentHistoryResp response) -> do
          let paymentHistoryArray = getPaymentHistoryItemList response
          modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> rideHistoryScreen{props {showPaymentHistory = true},data{paymentHistory {paymentHistoryList = paymentHistoryArray}}})
        Left err -> pure unit
      myRidesScreenFlow
    RIDE_HISTORY_NAV GoToSubscription -> updateAvailableAppsAndGoToSubs
    RIDE_HISTORY_NAV _ -> myRidesScreenFlow

rideSelectionScreenFlow :: FlowBT String Unit
rideSelectionScreenFlow = do
  flow <- UI.rideSelection
  case flow of
    REFRESH_RIDES state -> do
      modifyScreenState $ RideSelectionScreenStateType (\rideHistoryScreen -> state{offsetValue = 0})
      rideSelectionScreenFlow
    LOADER_RIDES_OUTPUT state -> do
      modifyScreenState $ RideSelectionScreenStateType (\rideHistoryScreen -> state{offsetValue = state.offsetValue + 8})
      rideSelectionScreenFlow
    SELECT_RIDE state -> do
      let language = ( case getLanguageLocale languageKey of
                         "HI_IN" -> "hi"
                         "KN_IN" -> "kn"
                         "TA_IN" -> "ta"
                         "TE_IN" -> "te"
                         _       -> "en"
                     )
      (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT state.selectedCategory.categoryId language
      let getOptionsRes' = (mapWithIndex (\index (Option x) ->
        { option : (show (index + 1)) <> ". " <> x.option
          , issueOptionId : x.issueOptionId
          , label : x.label
        }
      ) getOptionsRes.options)
      let tripId' = case state.selectedItem of
                      Just item -> Just item.id
                      _         -> Nothing
      let categoryName = getCategoryName $ fromMaybe "" state.selectedCategory.categoryAction
      modifyScreenState $ ReportIssueChatScreenStateType (\_ -> ReportIssueScreenData.initData { data { tripId = tripId', categoryName = categoryName, categoryId = state.selectedCategory.categoryId, categoryAction = fromMaybe "" state.selectedCategory.categoryAction, options = getOptionsRes' }, props { isReversedFlow = ((fromMaybe "" state.selectedCategory.categoryAction) == "LOST_AND_FOUND") } } )
      issueReportChatScreenFlow

issueReportChatScreenFlow :: FlowBT String Unit
issueReportChatScreenFlow = do
  flow <- UI.reportIssueChatScreen
  case flow of
    GO_TO_HELP_AND_SUPPORT -> do
      let language = ( case getLanguageLocale languageKey of
                         "HI_IN" -> "hi"
                         "KN_IN" -> "kn"
                         "TA_IN" -> "ta"
                         "TE_IN" -> "te"
                         _       -> "en"
                     )
      categories' <- HU.sortIssueCategories language <$> Remote.getCategoriesBT language
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen { data { categories = categories' } } )
      helpAndSupportFlow
    SUBMIT_ISSUE state -> do
      let mediaFiles' = case state.data.uploadedAudioId of
                          Just audioId -> cons audioId state.data.uploadedImagesIds
                          _            -> state.data.uploadedImagesIds
      let postIssueReq = PostIssueReq ({ mediaFiles  : mediaFiles'
                                       , categoryId  : state.data.categoryId
                                       , optionId    : state.data.selectedOptionId
                                       , description : trim state.data.messageToBeSent
                                       , rideId      : state.data.tripId
                                       , chats       : []
                                       })
      (PostIssueRes postIssueRes) <- Remote.postIssueBT postIssueReq
      (IssueInfoRes issueInfoRes) <- Remote.issueInfoBT postIssueRes.issueReportId
      void $ pure $ hideKeyboardOnNavigation true
      let showDescription = DS.length (trim issueInfoRes.description) > 0
          descMessages = if showDescription then snoc state.data.chatConfig.messages (makeChatComponent' issueInfoRes.description Nothing Nothing Nothing "Driver" (getCurrentUTC "") "Text" 500) else state.data.chatConfig.messages
          mediaMessages' = mapWithIndex (\index media -> makeChatComponent' media.url Nothing Nothing Nothing "Driver" (getCurrentUTC "") media._type ((index + if showDescription then 2 else 1) * 500)) issueInfoRes.mediaFiles
      if state.props.isReversedFlow
      then do
        let options'  = map (\x -> x.option) state.data.options
        let message = (getString SELECT_OPTION_REVERSED) <> "\n"
                      <> joinWith "\n" options'
        let messages' = concat [ descMessages, mediaMessages', [ (makeChatComponent' message Nothing Nothing Nothing "Bot" (getCurrentUTC "") "Text" (500 * (length mediaMessages' + 2))) ] ]
        modifyScreenState $ ReportIssueChatScreenStateType (\reportIssueScreen -> state { data { issueId = Just postIssueRes.issueReportId, chatConfig { enableSuggestionClick = false, messages = messages', chatSuggestionsList = options', suggestionDelay = 500 * (length mediaMessages' + 3) } }, props { showSubmitComp = false } })
        issueReportChatScreenFlow
      else do
        let message = makeChatComponent' (getString ISSUE_SUBMITTED_MESSAGE) Nothing Nothing Nothing "Bot" (getCurrentUTC "") "Text" (500 * (length mediaMessages' + 2))
        let messages' = concat [descMessages, mediaMessages', [message]]
        modifyScreenState $ ReportIssueChatScreenStateType (\reportIssueScreen -> state { data { issueId = Just postIssueRes.issueReportId, chatConfig { messages = messages' } }, props { showSubmitComp = false } })
        issueReportChatScreenFlow
    CALL_CUSTOMER state -> do
      case state.data.tripId of
        Just tripId -> do
                       void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
                       void $ lift $ lift $ toggleLoader true
                       resp <- Remote.callCustomerBT tripId
                       void $ lift $ lift $ toggleLoader false
                       let message = makeChatComponent' (getString ISSUE_SUBMITTED_MESSAGE) Nothing Nothing Nothing "Bot" (getCurrentUTC "") "Text" 500
                       let messages' = snoc state.data.chatConfig.messages message
                       modifyScreenState $ ReportIssueChatScreenStateType (\reportIssueScreen -> state { data  { chatConfig { messages = messages' } } })
                       issueReportChatScreenFlow
        _           -> do
                       pure $ toast ("Select different category or ride")
                       helpAndSupportFlow

referralScreenFlow :: FlowBT String Unit
referralScreenFlow = do
  let (GlobalState defGlobalState) = defaultGlobalState
  (GlobalState allState) <- getState
  let state = allState.referralScreen
  when (any (_ == "") [state.props.selectedDay.utcDate, state.props.selectedWeek.utcStartDate, state.props.selectedWeek.utcEndDate]) do
    let pastDates = getPastDays 7
        pastWeeks = getPastWeeks 4
        selectedDay = case last pastDates of
                        Just date -> date
                        Nothing -> state.props.selectedDay
        selectedWeek = case last pastWeeks of
                        Just week -> week
                        Nothing -> state.props.selectedWeek
    modifyScreenState $ ReferralScreenStateType (\ referralScreen -> referralScreen {props{ showShimmer = true, days = pastDates, weeks = pastWeeks, selectedDay = selectedDay, selectedWeek = selectedWeek }} )
  act <- UI.referralScreen
  case act of
    GO_TO_HOME_SCREEN_FROM_REFERRAL_SCREEN -> homeScreenFlow
    GO_TO_RIDES_SCREEN_FROM_REFERRAL_SCREEN -> myRidesScreenFlow
    GO_TO_PROFILE_SCREEN_FROM_REFERRAL_SCREEN -> driverProfileFlow
    GO_TO_NOTIFICATION_SCREEN_FROM_REFERRAL_SCREEN -> notificationFlow Nothing
    GO_TO_FLOW_AND_COME_BACK updatedState-> do
      response <-  lift $ lift $ Remote.linkReferralCode ( makeLinkReferralCodeReq updatedState.data.referralCode updatedState.data.password)
      case response of
        Right resp -> do
          modifyScreenState $ ReferralScreenStateType (\ referralScreen -> referralScreen{ data { driverInfo {referralCode = Just updatedState.data.referralCode} } ,  props { stage = SuccessScreen , firstTime = true}})
          referralScreenFlow
        Left error -> do
          void $ pure $ toast $ getString SOMETHING_WENT_WRONG
          referralScreenFlow
      referralScreenFlow
    REFRESH_LEADERBOARD -> referralScreenFlow
    REFERRAL_SCREEN_NAV GoToSubscription -> updateAvailableAppsAndGoToSubs
    REFERRAL_SCREEN_NAV (GoToEarningsScreen _) -> driverEarningsFlow
    REFERRAL_SCREEN_NAV _ -> referralScreenFlow
    _ -> referralScreenFlow

tripDetailsScreenFlow :: FlowBT String Unit
tripDetailsScreenFlow = do
  config <- getAppConfigFlowBT Constants.appConfig
  modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen { data {config = config}} )
  flow <- UI.tripDetailsScreen
  case flow of
    ON_SUBMIT  -> pure unit
    GO_TO_EARINING -> driverEarningsFlow
    GO_TO_HOME_SCREEN -> homeScreenFlow
    OPEN_HELP_AND_SUPPORT -> do
      let language = ( case getLanguageLocale languageKey of
                         "HI_IN" -> "hi"
                         "KN_IN" -> "kn"
                         "TA_IN" -> "ta"
                         "TE_IN" -> "te"
                         _       -> "en"
                     )
      categories' <- HU.sortIssueCategories language <$> Remote.getCategoriesBT language
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen { data { categories = categories', goBackTo = ScreenNames.TRIP_DETAILS_SCREEN} } )
      helpAndSupportFlow

currentRideFlow :: Maybe GetRidesHistoryResp -> Maybe Boolean -> FlowBT String Unit
currentRideFlow activeRideResp isActiveRide = do
  liftFlowBT $ markPerformance "CURRENT_RIDE_FLOW_START"
  let isRequestExpired = 
        if (getValueToLocalNativeStore RIDE_REQUEST_TIME) == "__failed" then false
          else ceil ((toNumber (rideRequestPollingData.duration - (getExpiryTime (getValueToLocalNativeStore RIDE_REQUEST_TIME) true)) * 1000.0)/rideRequestPollingData.delay) > 0
      onBoardingSubscriptionViewCount =  fromMaybe 0 (fromString (getValueToLocalNativeStore ONBOARDING_SUBSCRIPTION_SCREEN_COUNT))

  
  (isLocalStageOn RideRequested && (getValueToLocalNativeStore IS_RIDE_ACTIVE) == "false" && isRequestExpired) ? homeScreenFlow $ pure unit

  (GlobalState allState) <- getState
  setValueToLocalStore RIDE_STATUS_POLLING "False"
  setValueToLocalStore RENTAL_RIDE_STATUS_POLLING "False"
  case isActiveRide of
    Just false -> do
      noActiveRidePatch allState onBoardingSubscriptionViewCount
    _ -> do
      if isJust activeRideResp
        then do
          let (GetRidesHistoryResp activeRideResponse) = fromMaybe (GetRidesHistoryResp{list:[]}) activeRideResp
          (not (null activeRideResponse.list)) ? do
            void $ liftFlowBT $ startLocationPollingAPI
            activeRidePatch (GetRidesHistoryResp activeRideResponse) allState onBoardingSubscriptionViewCount
            $ noActiveRidePatch allState onBoardingSubscriptionViewCount
        else do
          (GetRidesHistoryResp activeRideResponse) <- Remote.getRideHistoryReqBT "2" "0" "true" "null" "null"
          (not (null activeRideResponse.list)) ?
            activeRidePatch (GetRidesHistoryResp activeRideResponse) allState onBoardingSubscriptionViewCount
            $ noActiveRidePatch allState onBoardingSubscriptionViewCount
  void $ pure $ setCleverTapUserProp [{key : "Driver On-ride", value : unsafeToForeign $ if getValueToLocalNativeStore IS_RIDE_ACTIVE == "false" then "No" else "Yes"}]
  -- Deprecated case for aadhaar popup shown after HV Integration
  when (allState.homeScreen.data.config.profileVerification.aadharVerificationRequired) $ do -- TODO :: Should be moved to global events as an async event
    (DriverRegistrationStatusResp resp) <- driverRegistrationStatusBT $ DriverRegistrationStatusReq true
    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {showlinkAadhaarPopup = (resp.aadhaarVerificationStatus == "INVALID" || resp.aadhaarVerificationStatus == "NO_DOC_AVAILABLE")}})
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {tobeLogged = true}})
  liftFlowBT $ markPerformance "CURRENT_RIDE_FLOW_END"
  homeScreenFlow
  where
    activeRidePatch (GetRidesHistoryResp activeRideResponse) allState onBoardingSubscriptionViewCount = do
      case activeRideResponse.list of
        [] -> do
          setValueToLocalNativeStore IS_RIDE_ACTIVE  "false"
          void $ updateStage $ HomeScreenStage HomeScreen
          updateDriverDataToStates
          if onBoardingSubscriptionScreenCheck onBoardingSubscriptionViewCount allState.homeScreen.data.config.subscriptionConfig.onBoardingSubscription then onBoardingSubscriptionScreenFlow onBoardingSubscriptionViewCount 
            else if onBoardingSubscriptionViewCount < 6 then  do 
              setValueToLocalStore ONBOARDING_SUBSCRIPTION_SCREEN_COUNT $ show (onBoardingSubscriptionViewCount + 1)
              pure unit
            else pure unit
        _ -> do
          for_ activeRideResponse.list $ \(RidesInfo ride) -> do
            let decodedSource = decodeAddress ride.fromLocation true
                decodedDestination = (\toLocation -> decodeAddress toLocation true) <$> ride.toLocation
                decodeNextStopAddress = (\(API.StopLocation {address,lat,lon}) -> decodeAddress (RC.getLocationInfoFromStopLocation address lat lon) true) <$> ride.nextStopLocation
                decodeLastStopAddress = (\(API.StopLocation {address,lat,lon}) -> decodeAddress (RC.getLocationInfoFromStopLocation address lat lon) true) <$> ride.lastStopLocation
                state = allState.homeScreen
                activeRide = (activeRideDetail state (RidesInfo ride))
                stage = (if activeRide.status == NEW then (if (any (\c -> c == ChatWithCustomer) [state.props.currentStage, state.props.advancedRideStage]) then ChatWithCustomer else RideAccepted) else RideStarted)
                voipConfig = getDriverVoipConfig $ DS.toLower $ getValueToLocalStore DRIVER_LOCATION
            sourceMod <- translateString decodedSource 500
            destinationMod <- maybe (pure Nothing) (\decodedDestination' -> do 
              destMod <- translateString decodedDestination' 500
              pure $ Just destMod)  decodedDestination
            nextStopAddressMod <- maybe (pure Nothing) (\decodedNextStopAddress' -> do 
              nextStopMod <- translateString decodedNextStopAddress' 500
              pure $ Just nextStopMod)  decodeNextStopAddress
            lastStopAddressMod <- maybe (pure Nothing) (\decodedLastStopAddress' -> do 
              lastStopMod <- translateString decodedLastStopAddress' 500
              pure $ Just lastStopMod)  decodeLastStopAddress
            if (voipConfig.driver.enableVoipFeature) then do
              void $ pure $ JB.initSignedCall activeRide.id true
            else pure unit
            setValueToLocalNativeStore IS_RIDE_ACTIVE  "true"
          
            -- Night Ride Safety PopUp 
            when (activeRide.disabilityTag == Just ST.SAFETY) $ do 
              let curr_time = convertUTCtoISC (getCurrentUTC "") "HH:mm:ss"
                  withInTime =JB.withinTimeRange curr_time (state.data.config.safetyRide.startTime) (state.data.config.safetyRide.endTime)
                  localVal =  getValueToLocalStore NIGHT_SAFETY_POP_UP
                  isShown = localVal == "true"
              if withInTime then do
                when (not isShown) $ setValueToLocalStore NIGHT_SAFETY_POP_UP "true"
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {showAccessbilityPopup = not isShown, safetyAudioAutoPlay = not isShown}})
              else
                setValueToLocalStore NIGHT_SAFETY_POP_UP "false"
          
            void $ updateStage $ HomeScreenStage stage
            void $ pure $ setCleverTapUserProp [{key : "Driver On-ride", value : unsafeToForeign "Yes"}]
            let stateChange = if (ride.bookingType == Just ADVANCED) 
                                  then (HomeScreenStateType (\homeScreen -> homeScreen { data{ advancedRideData = Just activeRide{source = sourceMod, destination = destinationMod}}, props{ silentPopUpView = false, goOfflineModal = false, retryRideList = false}})) 
                                  else (HomeScreenStateType (\homeScreen -> homeScreen { data{ activeRide = activeRide{source = sourceMod,destination = destinationMod, lastStopAddress = lastStopAddressMod, nextStopAddress = nextStopAddressMod}}, props{ silentPopUpView = false, goOfflineModal = false,isOdometerReadingsRequired = (activeRide.tripType == ST.Rental) && (maybe true (\val -> val) ride.isOdometerReadingsRequired), retryRideList  = false}}))
            modifyScreenState $ stateChange
            
    noActiveRidePatch allState onBoardingSubscriptionViewCount = do
      setValueToLocalNativeStore IS_RIDE_ACTIVE  "false"
      void $ pure $ setValueToLocalStore WAITING_TIME_STATUS (show ST.NoStatus)
      void $ pure $ setValueToLocalStore PARCEL_IMAGE_UPLOADED "false"
      when (allState.homeScreen.props.currentStage /= HomeScreen) $ do
        updateStage $ HomeScreenStage HomeScreen
      updateDriverDataToStates
      if onBoardingSubscriptionScreenCheck onBoardingSubscriptionViewCount allState.homeScreen.data.config.subscriptionConfig.onBoardingSubscription then onBoardingSubscriptionScreenFlow onBoardingSubscriptionViewCount 
        else if onBoardingSubscriptionViewCount < 6 then do 
          setValueToLocalStore ONBOARDING_SUBSCRIPTION_SCREEN_COUNT $ show (onBoardingSubscriptionViewCount + 1)
          pure unit
      else pure unit

checkDriverPaymentStatus :: GetDriverInfoResp -> FlowBT String Unit
checkDriverPaymentStatus (GetDriverInfoResp getDriverInfoResp) = when
  (getDriverInfoResp.paymentPending &&
    (getValueToLocalStore SHOW_PAYMENT_MODAL) /= "false" &&
    not any ( _ == true )[isLocalStageOn RideAccepted, isLocalStageOn RideStarted, isLocalStageOn ChatWithCustomer]
    ) do
    resp <- lift $ lift $ Remote.getPaymentHistory "" "" (Just "PAYMENT_PENDING")
    case resp of
      Right (GetPaymentHistoryResp resopnse) ->
        case resopnse!!0 of
          Just (PaymentDetailsEntity paymentDetailsEntity) ->
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { paymentState {
              makePaymentModal = true,
              rideCount = paymentDetailsEntity.totalRides,
              totalMoneyCollected = paymentDetailsEntity.totalEarnings,
              payableAndGST = paymentDetailsEntity.charges,
              date = convertUTCtoISC paymentDetailsEntity.date "Do MMM YYYY",
              invoiceId = paymentDetailsEntity.invoiceId,
              chargesBreakup = paymentDetailsEntity.chargesBreakup,
              dateObj = paymentDetailsEntity.date,
              laterButtonVisibility = getDriverInfoResp.subscribed
              }}})
          Nothing -> do
            overDueResp <- lift $ lift $ Remote.getPaymentHistory "" "" (Just "PAYMENT_OVERDUE")
            case overDueResp of
              Right (GetPaymentHistoryResp overDueResp') ->
                case overDueResp'!!0 of
                  Just (PaymentDetailsEntity paymentDetailsEntity) ->
                    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { paymentState {
                      makePaymentModal = true,
                      rideCount = paymentDetailsEntity.totalRides,
                      totalMoneyCollected = paymentDetailsEntity.totalEarnings,
                      payableAndGST = paymentDetailsEntity.charges,
                      date = convertUTCtoISC paymentDetailsEntity.date "Do MMM YYYY",
                      invoiceId = paymentDetailsEntity.invoiceId,
                      chargesBreakup = paymentDetailsEntity.chargesBreakup,
                      dateObj = paymentDetailsEntity.date,
                      laterButtonVisibility = getDriverInfoResp.subscribed
                      }}})
                  Nothing -> pure unit
              Left error -> pure unit
      Left error -> pure unit
    if getDriverInfoResp.paymentPending then pure unit else setValueToLocalStore PAYMENT_STATUS_POOLING "false"
    pure unit

onBoardingSubscriptionScreenFlow :: Int -> FlowBT String Unit
onBoardingSubscriptionScreenFlow onBoardingSubscriptionViewCount = do
  (GlobalState globalState) <- getState
  (GetDriverInfoResp getDriverInfoResp) <- getDriverInfoDataFromCache (GlobalState globalState) false
  setValueToLocalStore ONBOARDING_SUBSCRIPTION_SCREEN_COUNT $ show (onBoardingSubscriptionViewCount + 1)
  respBT <- lift $ lift $ Remote.getReelsVideo "reels_data_onboarding" $ HU.getLanguageTwoLetters $ Just $ getLanguageLocale languageKey
  let reelsData = case respBT of
                    Left err -> API.ReelsResp {reels : []}
                    Right resp -> resp
      driverVehicle = getValueToLocalStore VEHICLE_VARIANT
      driverCity = getValueToLocalStore DRIVER_LOCATION
      vehicleAndCityConfig = CommonRC.subscriptionsConfigVariantLevel driverCity driverVehicle
  modifyScreenState $ OnBoardingSubscriptionScreenStateType \onBoardingSubscriptionScreen ->
    onBoardingSubscriptionScreen
      { data
          { reelsData = transformReelsRespToReelsData reelsData,
            freeTrialDays = getDriverInfoResp.freeTrialDays,
            freeTrialRides = getDriverInfoResp.freeTrialRides,
            totalRidesTaken = getDriverInfoResp.totalRidesTaken,
            vehicleAndCityConfig = vehicleAndCityConfig
          }
      , props
          { isSelectedLangTamil = (getLanguageLocale languageKey) == "TA_IN"
          , screenCount = onBoardingSubscriptionViewCount + 1
          }
      }
  action <- UI.onBoardingSubscriptionScreen
  case action of 
    REGISTERATION_ONBOARDING state -> do
      setValueToLocalStore ONBOARDING_SUBSCRIPTION_SCREEN_COUNT "100"
      onBoardingFlow
    MAKE_PAYMENT_FROM_ONBOARDING state -> do
      case state.data.selectedPlanItem of 
        Just selectedPlan -> do
          setValueToLocalStore ONBOARDING_SUBSCRIPTION_SCREEN_COUNT "100"
          nyPaymentFlow selectedPlan "ONBOARDING"
        Nothing -> onBoardingSubscriptionScreenFlow (state.props.screenCount-1)
  pure unit

homeScreenFlow :: FlowBT String Unit
homeScreenFlow = do
  liftFlowBT $ markPerformance "HOME_SCREEN_FLOW"
  logField_ <- lift $ lift $ getLogFields
  setValueToLocalStore LOGS_TRACKING "false"
  Events.measureDurationFlowBT "Flow.homeScreenFlow" $ do    
    void $ pure $ cleverTapSetLocation unit
    if (getValueToLocalNativeStore IS_RIDE_ACTIVE) == "true" && (not $ any (\item -> isLocalStageOn item) [RideAccepted, RideStarted, ChatWithCustomer])
      then do  
        currentRideFlow Nothing Nothing
      else do 
        pure unit
    (GlobalState globalState) <- getState  
    getDriverInfoResp <- getDriverInfoDataFromCache (GlobalState globalState) false
    checkDriverBlockingStatus getDriverInfoResp globalState.homeScreen.data.config.subscriptionConfig.enableBlocking
    when globalState.homeScreen.data.config.subscriptionConfig.completePaymentPopup $ checkDriverPaymentStatus getDriverInfoResp
    updateBannerAndPopupFlags  
    void $ lift $ lift $ toggleLoader false
    liftFlowBT $ handleUpdatedTerms $ getString TERMS_AND_CONDITIONS_UPDATED        
  liftFlowBT $ Events.endMeasuringDuration "mainToHomeScreenDuration" 
  action <- UI.homeScreen 
  void $ lift $ lift $ fork $ Remote.pushSDKEvents
  case action of             
    GO_TO_PROFILE_SCREEN updatedState -> do 
      liftFlowBT $ logEvent logField_ "ny_driver_profile_click"
      modifyScreenState $ DriverProfileScreenStateType $ \driverProfileScreen -> driverProfileScreen { data {profileCompletedModules = updatedState.data.completingProfileRes.completed, cachedVehicleCategory = fromMaybe ST.UnKnown $ RC.getCategoryFromVariant updatedState.data.vehicleType, cancellationRate = updatedState.data.cancellationRate}}
      driverProfileFlow
    GO_TO_COMPLETE_PROFILE_SCREEN -> driverCompleteProfileFlow
    GO_TO_VEHICLE_DETAILS_SCREEN -> do 
      modifyScreenState $ DriverProfileScreenStateType $ \driverProfileScreen -> driverProfileScreen { props { screenType = ST.VEHICLE_DETAILS}}
      driverProfileFlow
    GO_TO_RIDES_SCREEN -> do
      liftFlowBT $ logEvent logField_ "ny_driver_my_rides"
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> rideHistoryScreen{offsetValue = 0 , currentTab = "COMPLETED"})
      myRidesScreenFlow
    GO_TO_REFERRAL_SCREEN_FROM_HOME_SCREEN -> do
      liftFlowBT $ logEvent logField_ "ny_driver_rankings"
      referralFlow
    GO_TO_CUSTOMER_REFERRAL_TRACKER state -> customerReferralTrackerFlow
    DRIVER_AVAILABILITY_STATUS state status -> do  
      void $ lift $ lift $ loaderText (getString PLEASE_WAIT) if status == Online then (getString SETTING_YOU_ONLINE) else if status == Silent then (getString SETTING_YOU_SILENT) else (getString SETTING_YOU_OFFLINE)
      void $ lift $ lift $ toggleLoader true
      let isGotoEnabled = state.data.driverGotoState.gotoEnabledForMerchant && state.data.config.gotoConfig.enableGoto      
      changeDriverStatus status
      gs@(GlobalState globalState) <- getState
      when (isGotoEnabled && (status == Online || status == Silent)) $ void $ getDriverInfoDataFromCache gs true
      (GetDriverInfoResp getDriverInfoRes) <- getDriverInfoDataFromCache gs false
      let (Vehicle linkedVehicle) = fromMaybe dummyVehicleObject getDriverInfoRes.linkedVehicle
      if globalState.globalProps.firstTimeOnboardingStatus
        then logDriverStatus linkedVehicle.category status 
        else pure unit
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props {showOffer = status == Online && state.props.driverStatusSet == Offline && getDriverInfoRes.autoPayStatus == Nothing }})
      homeScreenFlow
    GO_TO_HELP_AND_SUPPORT_SCREEN -> do
      let language = ( case getLanguageLocale languageKey of
                         "HI_IN" -> "hi"
                         "KN_IN" -> "kn"
                         "TA_IN" -> "ta"
                         "TE_IN" -> "te"
                         _       -> "en"
                     )
      categories' <- HU.sortIssueCategories language <$> Remote.getCategoriesBT language
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen { data { categories = categories', goBackTo = ScreenNames.HOME_SCREEN } } )
      helpAndSupportFlow
    GO_TO_EDIT_GENDER_SCREEN -> driverProfileFlow
    
    GO_TO_START_RIDE {id, otp ,startOdometerReading, startOdometerImage, lat, lon, ts} updatedState -> do
      void $ lift $ lift $ loaderText (getString START_RIDE) ""
      void $ lift $ lift $ toggleLoader true
      
      startRideResp <- lift $ lift $ Remote.startRide id (Remote.makeStartRideReq otp startOdometerReading (updatedState.props.odometerFileId) (fromMaybe 0.0 (Number.fromString lat)) (fromMaybe 0.0 (Number.fromString lon)) ts) -- driver's lat long during starting ride
      case startRideResp of
        Right startRideResp -> do
          let chargesOb = HU.getChargesOb updatedState.data.activeRide.tripType updatedState.data.cityConfig updatedState.data.activeRide.driverVehicle
          void $ pure $ setValueToLocalNativeStore RIDE_ID id
          _ <- pure $ hideKeyboardOnNavigation true
          liftFlowBT $ logEventWithMultipleParams logField_ "ny_driver_ride_start" $ [{key : "Service Tier", value : unsafeToForeign updatedState.data.activeRide.serviceTier},
                                                                                      {key : "Driver Vehicle", value : unsafeToForeign updatedState.data.activeRide.driverVehicle}]
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props {enterOtpModal = false,enterOdometerReadingModal = false, isInvalidOdometer = false, enterOdometerFocusIndex=0, chatServiceKilled = true}, data{ route = [], activeRide{status = INPROGRESS}}})
          void $ pure $ hideKeyboardOnNavigation true
          void $ pure $ JB.exitLocateOnMap ""
          void $ updateStage $ HomeScreenStage RideStarted
          void $ pure $ setValueToLocalStore TRIGGER_MAPS "true"
          void $ pure $ runFn2  EHC.updatePushInIdMap "PlayAudioAndLaunchMap" true
          void $ pure $ setValueToLocalStore TRIP_STATUS "started"
          void $ pure $ setValueToLocalStore WAITING_TIME_STATUS (show ST.NoStatus)
          void $ pure $ setValueToLocalStore PARCEL_IMAGE_UPLOADED "false"
          void $ pure $ setValueToLocalStore TOTAL_WAITED if updatedState.data.activeRide.waitTimeSeconds > chargesOb.freeSeconds then (updatedState.data.activeRide.id <> "<$>" <> show updatedState.data.activeRide.waitTimeSeconds) else "-1"
          void $ pure $ setValueToLocalStore RIDE_START_TIME (getCurrentUTC "")
          void $ pure $ clearTimerWithId updatedState.data.activeRide.waitTimerId
          void $ lift $ lift $ toggleLoader false
          void $ pure $ JB.destroySignedCall unit
          currentRideFlow Nothing Nothing
        Left errorPayload -> do
          let errResp = errorPayload.response
          let codeMessage = decodeErrorCode errResp.errorMessage
          
          liftFlowBT $ logEvent logField_ "incorrect flow"
          if ( errorPayload.code == 400 && codeMessage == "INCORRECT_OTP") then do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {otpIncorrect = true, enterOtpModal = true, otpAttemptsExceeded = false, rideOtp = "", enterOdometerReadingModal= false, enterOtpFocusIndex = 0, enterOdometerFocusIndex=0} })
              void $ lift $ lift $ toggleLoader false
          else if ( errorPayload.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then do
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {otpAttemptsExceeded = true, enterOtpModal = true, rideOtp = "", enterOtpFocusIndex = 0, enterOdometerFocusIndex=0} })
            void $ lift $ lift $ toggleLoader false
          else pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
          homeScreenFlow
    GO_TO_START_ZONE_RIDE {otp, lat, lon, ts} -> do
      void $ lift $ lift $ loaderText (getString PLEASE_WAIT) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      startZoneRideResp <- lift $ lift $ Remote.otpRide "" (Remote.makeOTPRideReq otp (fromMaybe 0.0 (Number.fromString lat)) (fromMaybe 0.0 (Number.fromString lon)) ts) -- driver's lat long during starting ride
      case startZoneRideResp of
        Right startZoneRideResp -> do
          liftFlowBT $ logEvent logField_ "ny_driver_special_zone_ride_start"
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props {enterOtpModal = false, mapRendered = true}, data{ route = [], activeRide{status = INPROGRESS}}})
          void $ pure $ hideKeyboardOnNavigation true
          void $ pure $ JB.exitLocateOnMap ""
          void $ updateStage $ HomeScreenStage RideStarted
          void $ pure $ setValueToLocalStore TRIGGER_MAPS "true"
          void $ lift $ lift $ toggleLoader false
          currentRideFlow Nothing Nothing
        Left errorPayload -> do
          let errResp = errorPayload.response
          let codeMessage = decodeErrorCode errResp.errorMessage
          let errorMessage = decodeErrorMessage errResp.errorMessage
          if ( errorPayload.code == 400 && (codeMessage == "BOOKING_NOT_FOUND_FOR_SPECIAL_ZONE_OTP")) then do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {wrongVehicleVariant = false, otpIncorrect = true, enterOtpModal = true, otpAttemptsExceeded = false, rideOtp = ""} })
            else if ( errorPayload.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {wrongVehicleVariant = false, otpAttemptsExceeded = true, enterOtpModal = true, rideOtp = ""} })
            else if ( errorPayload.code == 400 && (errorMessage == "Wrong Vehicle Variant")) then do
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {wrongVehicleVariant = true, otpIncorrect = true, enterOtpModal = true, otpAttemptsExceeded = false} })
              else pure $ toast (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
          void $ lift $ lift $ toggleLoader false
          homeScreenFlow
    GO_TO_END_RIDE {id, endOtp, endOdometerReading, endOdometerImage, lat, lon, ts} state -> do
      void $ lift $ lift $ loaderText (getString END_RIDE) ""
      void $ lift $ lift $ toggleLoader true
      let numDeviation = Just $ (fromMaybe 0 (fromString (getValueToLocalNativeStore RIDE_WAYPOINT_DEVIATION_COUNT))) >=3
          tripDistanceWithAcc = fromMaybe 0 $ fromString $ getValueToLocalNativeStore TRIP_DISTANCE_ACC
          tripDistance = fromMaybe 0 $ fromString $ getValueToLocalNativeStore TRIP_DISTANCE
          endRideOtp = if any (_ == state.data.activeRide.tripType)  [ST.Rental, ST.Intercity, ST.Delivery] then Just endOtp else Nothing
          endRideOtpModalOnError = if any (_ == state.data.activeRide.tripType)  [ST.Rental, ST.Intercity, ST.Delivery] then true else false

      void $ pure $ setValueToLocalStore RIDE_END_TIME (getCurrentUTC "")
      let fileId = if state.data.activeRide.tripType == ST.Rental then
          state.props.odometerFileId
        else Nothing 
      case Number.fromString lat, Number.fromString lon of 
        Just lt, Just ln | lt /= 0.0 && ln /= 0.0-> do
          (endRideResp) <- lift $ lift $ Remote.endRide id (Remote.makeEndRideReq endRideOtp endOdometerReading fileId lt ln numDeviation tripDistance tripDistanceWithAcc ts)
          case (endRideResp) of
            Right (API.EndRideResponse response) -> do
              when state.data.driverGotoState.isGotoEnabled do
                getDriverInfoResp <- Remote.getDriverInfoBT ""
                modifyScreenState $ GlobalPropsType (\globalProps -> globalProps 
                  { driverInformation = Just getDriverInfoResp,
                    gotoPopupType = case response.homeLocationReached of 
                      Nothing -> ST.NO_POPUP_VIEW
                      Just true -> ST.REACHED_HOME
                      Just false -> ST.MORE_GOTO_RIDES
                  })
              onSuccessEndRide
            Left errorPayload -> do
              (GetRidesHistoryResp rideHistoryResponse) <- Remote.getRideHistoryReqBT "2" "0" "true" "null" "null"
              let getCurrentRideInfo = head rideHistoryResponse.list
              case Tuple (DA.length $ rideHistoryResponse.list) (isJust state.data.advancedRideData)  of
                Tuple 1 true -> do 
                  case getCurrentRideInfo , state.data.advancedRideData of
                    Just (RidesInfo rideDataResp), Just advRide -> if (rideDataResp.id == advRide.id) then onSuccessEndRide else errorHandlerEndRide errorPayload endRideOtpModalOnError
                    _ , _ -> onSuccessEndRide
                Tuple 2 true -> errorHandlerEndRide errorPayload endRideOtpModalOnError
                Tuple 1 false -> errorHandlerEndRide errorPayload endRideOtpModalOnError
                Tuple _ _ ->  onSuccessEndRide
        _, _ -> do
          void $ lift $ lift $ toggleLoader false
          pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
          pure unit
      where
        errorHandlerEndRide errorPayload endRideOtpModalOnError = do
          void $ lift $ lift $ toggleLoader false
          let errResp = errorPayload.response
          let codeMessage = decodeErrorCode errResp.errorMessage
          liftFlowBT $ logEvent logField_ "incorrect flow"
          if ( errorPayload.code == 400 && codeMessage == "INCORRECT_OTP") then do
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {otpIncorrect = true, enterOtpModal = endRideOtpModalOnError, otpAttemptsExceeded = false, rideOtp = "",enterOdometerReadingModal= false, endRideOdometerReadingModal = false, enterOtpFocusIndex = 0, enterOdometerFocusIndex=0} })
          else if ( errorPayload.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then do
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {otpIncorrect = false,otpAttemptsExceeded = true, enterOtpModal = endRideOtpModalOnError, rideOtp = "",enterOdometerReadingModal= false,endRideOdometerReadingModal = false, enterOtpFocusIndex = 0, enterOdometerFocusIndex=0} })
          else do
            pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
            homeScreenFlow
        onSuccessEndRide = do
            void $ pure $ cleverTapCustomEvent "ny_driver_ride_ended"
            void $ pure $ metaLogEvent "ny_driver_ride_ended"
            liftFlowBT $ firebaseLogEvent "ny_driver_ride_ended"
            void $ pure $ removeAllPolylines ""
            void $ pure $ setValueToLocalStore RENTAL_RIDE_STATUS_POLLING "False"
            void $ pure $ setValueToLocalStore WAITING_TIME_STATUS (show ST.NoStatus)
            void $ pure $ setValueToLocalStore PARCEL_IMAGE_UPLOADED "false"
            void $ pure $ setValueToLocalNativeStore IS_RIDE_ACTIVE  "false"
            void $ pure $ setCleverTapUserProp [{key : "Driver On-ride", value : unsafeToForeign "No"}]
            void $ pure $ setValueToLocalStore DRIVER_STATUS_N "Online"
            void $ pure $ setValueToLocalNativeStore DRIVER_STATUS_N "Online"
            void $ lift $ lift $ Remote.driverActiveInactive "true" $ toUpper $ show Online
            void $ pure $ setValueToLocalNativeStore TRIP_STATUS "ended"
            void $ pure $ JB.destroySignedCall unit
            when (state.props.currentStage == RideStarted) $ for_  state.data.activeRide.stops $ \(API.Stop stop) -> do
              let (API.LocationInfo stopLocation) = stop.location
              pure $ removeMarker $ "stop" <> show stopLocation.lat <> show stopLocation.lon
            liftFlowBT $ logEventWithMultipleParams logField_ "ny_driver_ride_completed" $ [{key : "Service Tier", value : unsafeToForeign state.data.activeRide.serviceTier},
                                                                                            {key : "Driver Vehicle", value : unsafeToForeign state.data.activeRide.driverVehicle}]
            void $ pure $ setValueToLocalStore IS_DRIVER_STATS_CALLED "false"
            if getValueToLocalStore HAS_TAKEN_FIRST_RIDE == "true" then do
              getDriverInfoResp <- Remote.getDriverInfoBT ""

              let (GetDriverInfoResp getDriverInfoResp) = getDriverInfoResp
              modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{driverInformation = Just (GetDriverInfoResp getDriverInfoResp)}
              if (isJust getDriverInfoResp.numberOfRides && (fromMaybe 0 getDriverInfoResp.numberOfRides == 1))
                then do
                  let currdate = getcurrentdate ""
                  liftFlowBT $ logEventWithParams logField_ "ny_driver_first_ride_completed" "Date" currdate
                else pure unit
              setValueToLocalStore HAS_TAKEN_FIRST_RIDE "false"
              else pure unit
            (GetRidesHistoryResp rideHistoryResponse) <- Remote.getRideHistoryReqBT "1" "0" "false" "COMPLETED" "null"
            case (head rideHistoryResponse.list) of
              Nothing -> pure unit
              Just (RidesInfo response) -> do
                let specialZoneConfig = HU.getRideLabelData response.specialLocationTag
                    isSpecialPickUpZone = HU.checkSpecialPickupZone response.specialLocationTag
                modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {data {
                    tripId = response.shortRideId,
                    date = (convertUTCtoISC (response.createdAt) "D MMM"),
                    time = (convertUTCtoISC (response.createdAt )"h:mm A"),
                    source = (decodeAddress response.fromLocation false),
                    destination = if (RC.rideTypeConstructor response.tripCategory) == ST.Rental then "" else maybe "" (\toLocation -> decodeAddress toLocation false) response.toLocation,
                    vehicleType = response.vehicleVariant,
                    totalAmount = fromMaybe response.estimatedBaseFare response.computedFare,
                    distance = parseFloat (toNumber (fromMaybe 0 response.chargeableDistance) / 1000.0) 2,
                    status = response.status,
                    rider = (fromMaybe "" response.riderName),
                    customerExtraFee = response.customerExtraFee,
                    purpleTagVisibility = isJust response.disabilityTag,
                    gotoTagVisibility = isJust response.driverGoHomeRequestId,
                    spLocTagVisibility = isJust response.specialLocationTag && isJust (HU.getRequiredTag response.specialLocationTag),
                    specialZoneLayoutBackground = specialZoneConfig.backgroundColor,
                    specialZoneImage = specialZoneConfig.imageUrl,
                    specialZoneText = specialZoneConfig.text,
                    specialZonePickup = isSpecialPickUpZone,
                    tollCharge = fromMaybe 0.0 response.tollCharges,
                    vehicleModel = response.vehicleModel,
                    rideType = response.vehicleServiceTierName,
                    tripStartTime = response.tripStartTime,
                    tripEndTime = response.tripEndTime,
                    acRide = response.isVehicleAirConditioned,
                    vehicleServiceTier = response.vehicleServiceTier,
                    tripType = RC.rideTypeConstructor response.tripCategory 
                  , parkingCharge = fromMaybe 0.0 response.parkingCharge
                  , stops = (fst <<< HU.getStopName) <$> (fromMaybe [] response.stops)
                  }})
                let payerVpa = fromMaybe "" response.payerVpa
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen 
                  { data {
                      activeRide {endOdometerReading = (\(API.OdometerReading {value}) -> value) <$> response.endOdometerReading}, 
                      endRideData { 
                        actualRideDuration = response.actualDuration,
                        actualRideDistance = if state.data.activeRide.tripType == ST.Rental then round <$> response.actualRideDistance else response.chargeableDistance, 
                        finalAmount = fromMaybe response.estimatedBaseFare response.computedFare, 
                        riderName = fromMaybe "" response.riderName,
                        tripStartTime = maybe Nothing (\time -> Just (convertUTCtoISC time "hh:mm A")) response.tripStartTime ,
                        tripEndTime = maybe Nothing (\time -> Just (convertUTCtoISC time "hh:mm A")) response.tripEndTime,
                        rideId = response.id, 
                        tip = response.customerExtraFee, 
                        disability = response.disabilityTag, 
                        payerVpa = payerVpa, 
                        specialZonePickup = if isSpecialPickUpZone then Just true else Nothing,
                        capacity = response.vehicleCapacity,
                        serviceTier = response.vehicleServiceTierName,
                        specialLocationTag = response.specialLocationTag
                      }
                    , parking {
                        estimatedCharge = response.parkingCharge
                      }
                    , toll {
                        tollAmbigous = response.tollConfidence == Just CTA.Unsure
                      , finalCharge = fromMaybe 0.0 response.tollCharges
                      , estimatedCharge = fromMaybe 0.0 response.estimatedTollCharges
                      }                    
                      , coinsEarned = fromMaybe [] response.coinsEarned
                    },
                    props {
                      isFreeRide = fromMaybe false response.isFreeRide
                    }
                  })
                liftFlowBT $ logEventWithMultipleParams logField_ "ny_driver_ride_completed" $ [{key : "Service Tier", value : unsafeToForeign state.data.activeRide.serviceTier},
                                                                                            {key : "Driver Vehicle", value : unsafeToForeign state.data.activeRide.driverVehicle},
                                                                                            {key : "Actual Toll Charge", value : unsafeToForeign (fromMaybe 0.0 response.tollCharges)},
                                                                                            {key : "Estimated Toll Charge", value : unsafeToForeign (fromMaybe 0.0 response.estimatedTollCharges)},
                                                                                            {key : "Has Toll", value : unsafeToForeign (maybe false (\charge -> charge /= 0.0) response.tollCharges)}]
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props {enterOtpModal = false, endRideOdometerReadingModal = false, isInvalidOdometer = false,enterOdometerFocusIndex=0, showRideCompleted = true}})
            void $ updateStage $ HomeScreenStage RideCompleted
            void $ lift $ lift $ toggleLoader false
            updateDriverDataToStates
            void $ pure $ runFn2  EHC.updatePushInIdMap "PlayEndRideAudio" true
            homeScreenFlow
    GO_TO_CANCEL_RIDE {id, info , reason} state -> do
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_driver_ride_cancelled" $ [{key : "Reason code", value : unsafeToForeign reason},
                                                                                        {key : "Additional info", value : unsafeToForeign $ if info == "" then "null" else info},
                                                                                        {key : "Pickup", value : unsafeToForeign state.data.activeRide.source},
                                                                                        {key : "Estimated Ride Distance (meters)" , value : unsafeToForeign state.data.activeRide.distance},
                                                                                        {key : "Service Tier", value : unsafeToForeign state.data.activeRide.serviceTier},
                                                                                        {key : "Driver Vehicle", value : unsafeToForeign state.data.activeRide.driverVehicle}]
      API.DriverCancelRideResponse cancelRideResp <- Remote.cancelRide id (Remote.makeCancelRideReq info reason)
      void $ pure if state.data.driverGotoState.timerId /= "" then clearTimerWithId state.data.driverGotoState.timerId else unit
      void $ pure $ setValueToLocalStore WAITING_TIME_STATUS (show ST.NoStatus)
      void $ pure $ setValueToLocalStore PARCEL_IMAGE_UPLOADED "false"
      void $ pure $ clearTimerWithId state.data.activeRide.waitTimerId
      void $ pure $ removeAllPolylines ""
      void $ pure $ JB.exitLocateOnMap ""
      void $ pure $ setValueToLocalStore DRIVER_STATUS_N "Online"
      void $ pure $ setValueToLocalNativeStore DRIVER_STATUS_N "Online"
      resp <- lift $ lift $ Remote.driverActiveInactive "true" $ toUpper $ show Online
      handleDriverActivityResp resp
      void $ pure $ setValueToLocalStore RENTAL_RIDE_STATUS_POLLING "False"
      void $ updateStage $ HomeScreenStage HomeScreen
      void $ pure $ JB.destroySignedCall unit

      when state.data.driverGotoState.isGotoEnabled do
        driverInfoResp <- Remote.getDriverInfoBT ""
        modifyScreenState $ GlobalPropsType (\globalProps -> globalProps {driverInformation = Just driverInfoResp, gotoPopupType = if (fromMaybe false cancelRideResp.isGoHomeDisabled) then ST.REDUCED 0 else ST.NO_POPUP_VIEW})
      updateDriverDataToStates
      modifyScreenState $ GlobalPropsType (\globalProps -> globalProps { gotoPopupType = ST.NO_POPUP_VIEW })
      removeChatService ""
      homeScreenFlow
    FCM_NOTIFICATION notificationType state notificationBody -> do
      if (notificationType /= "EDIT_LOCATION") then void $ pure $ removeAllPolylines "" else pure unit
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props { showGenericAccessibilityPopUp = false}})
      case notificationType of
        "CANCELLED_PRODUCT" -> do
          resp <- lift $ lift $ Remote.driverActiveInactive "true" $ toUpper $ show Online
          handleDriverActivityResp resp
          void $ pure $ setValueToLocalStore WAITING_TIME_STATUS (show ST.NoStatus)
          void $ pure $ setValueToLocalStore PARCEL_IMAGE_UPLOADED "false"
          void $ pure $ clearTimerWithId state.data.activeRide.waitTimerId
          void $ pure $ JB.destroySignedCall unit
          removeChatService ""
          void $ updateStage $ HomeScreenStage HomeScreen
          updateDriverDataToStates
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props {rideRequestPill{isPillClickable =  true} , checkUpcomingRide = true}})
          modifyScreenState $ RideSummaryScreenStateType (\rideSummaryScreen -> rideSummaryScreen {props {throughBanner = false}})         
          homeScreenFlow
        "DRIVER_ASSIGNMENT" -> do
          let (GlobalState defGlobalState) = defaultGlobalState
          when (isJust defGlobalState.homeScreen.data.activeRide.disabilityTag) $ do
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props {showAccessbilityPopup = true, specialZoneProps{ currentGeoHash = "" }}})
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props {mapRendered = true,rentalInfoPopUp = homeScreen.data.activeRide.tripType == ST.Rental ,intercityInfoPopUp = homeScreen.data.activeRide.tripType == ST.Intercity , currentStage = RideAccepted , checkUpcomingRide = true, retryRideList = true}})
          currentRideFlow Nothing Nothing
        "RIDE_REQUESTED"    -> do
          void $ updateStage $ HomeScreenStage RideRequested
          homeScreenFlow
        "TRIP_STARTED" -> do
          if state.props.currentStage /= RideStarted then do
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{ activeRide{status = INPROGRESS}}})
            void $ updateStage $ HomeScreenStage RideStarted
            void $ pure $ setValueToLocalStore TRIGGER_MAPS "true"
            void $ pure $ setValueToLocalStore TRIP_STATUS "started"
            currentRideFlow Nothing Nothing
          else do
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props {chatServiceKilled = true}})
            homeScreenFlow
        "EDIT_LOCATION" -> do
          if isNothing state.data.advancedRideData then do
            void $ pure $ removeAllPolylines ""
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {data {route = []}})
            baseAppFlow false Nothing Nothing
          else pure unit
        "USER_FAVOURITE_DRIVER" -> do
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {data {favPopUp {visibility = true, title = if DS.null state.data.favPopUp.title then notificationBody.title else (fromMaybe "User" (DA.head (split (Pattern " ") notificationBody.title))) <> ", " <> state.data.favPopUp.title , message = reverseString $ DS.drop 4 (reverseString notificationBody.message)}}})
          homeScreenFlow
        _                   -> homeScreenFlow
    REFRESH_HOME_SCREEN_FLOW -> do
      void $ pure $ removeAllPolylines ""
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props {rideActionModal = false, cancelRideModalShow = false, enterOtpModal = false, routeVisible = false, refreshAnimation = false}})
      getDriverInfoApiResp <- lift $ lift $ Remote.getDriverInfoApi ""
      case getDriverInfoApiResp of
        Right resp -> modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{driverInformation = Just resp}
        Left _ -> pure unit
      updateDriverDataToStates
      homeScreenFlow
    RELOAD state -> homeScreenFlow
    GO_TO_NEW_STOP state -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ data { route = [] }, props{ mapRendered = true }})
      currentRideFlow Nothing Nothing
    GO_TO_ARRIVED_AT_STOP {id, lat, lon, ts} state -> do
      (resp :: (Either ErrorResponse API.ApiSuccessResult)) <- lift $ lift $ callApi $ API.ArrivedAtStopRequest id (Remote.makeArrivedAtStopReq lat lon)
      case resp of
        Left errorPayload -> do
          _ <- pure $ printLog "GO_TO_ARRIVED_AT_STOP" (show resp)
          let errResp = errorPayload.response
          let codeMessage = decodeErrorCode errResp.errorMessage
          if ( errorPayload.code /= 200) then do
            void $ pure $ toast $ getString $ if codeMessage == "INVALID_REQUEST" then YOU_ARE_NOT_AT_STOP_LOCATION else SOMETHING_WENT_WRONG
          else pure unit
        Right _ -> do
          _ <- pure $ printLog "GO_TO_ARRIVED_AT_STOP" "Arrived at stop"
          _ <- pure $ removeAllPolylines ""
          (GetRidesHistoryResp rideList) <- Remote.getRideHistoryReqBT "1" "0" "true" "null" "null"
          case (rideList.list DA.!! 0) of
            Just ( rideInfo) -> do
              let currActiveRideDetails = activeRideDetail state rideInfo
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data {route = [],activeRide = currActiveRideDetails}, props {routeVisible = true, arrivedAtStop = true}})
              pure unit
            Nothing -> pure unit
      homeScreenFlow
    NOTIFY_CUSTOMER state -> do
      if (getValueToLocalStore WAITING_TIME_STATUS == show ST.NoStatus) && (state.data.activeRide.tripType == ST.Rental || state.data.activeRide.tripType == ST.Intercity) then do
        setValueToLocalStore WAITING_TIME_STATUS (show ST.Scheduled)
      else do
        driverArrived <- lift $ lift $ Remote.driverArrived (state.data.activeRide.id) (DriverArrivedReq {
          "lat" : state.data.currentDriverLat
        , "lon" : state.data.currentDriverLon
        })
        case driverArrived of
          Right _ -> do
            setValueToLocalStore WAITING_TIME_STATUS $ if state.data.activeRide.tripType == ST.Delivery && state.data.activeRide.driverVehicle == "BIKE" then (show ST.NotTriggered) else (show ST.Triggered) 
            setValueToLocalStore WAITING_TIME_VAL (state.data.activeRide.id <> "<$>" <> getCurrentUTC "")
            void $ pure $ JB.sendMessage $ if EHC.isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion (getMerchant FunctionCall)) then (EHS.getMessageFromKey EHS.chatSuggestion "dis1AP" "EN_US") else "dis1AP"
            liftFlowBT $ logEventWithMultipleParams logField_ "ny_driver_i_have_arrived_clicked" $ [{key : "Service Tier", value : unsafeToForeign state.data.activeRide.serviceTier},
                                                                                                    {key : "Driver Vehicle", value : unsafeToForeign state.data.activeRide.driverVehicle},
                                                                                                    {key : "Estimated Toll Charge", value : unsafeToForeign (state.data.toll.estimatedCharge)},
                                                                                                    {key : "Has Toll", value : unsafeToForeign $ state.data.activeRide.estimatedTollCharges > 0.0}]
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{activeRide{notifiedCustomer = true}}})
          Left _ -> pure unit
      homeScreenFlow
    UPDATE_ROUTE state callDrawRoute -> do
      void $ pure $ JB.exitLocateOnMap ""   
      -- let srcDestConfig = HU.getSrcDestConfig state
      void $ pure $ JB.removeAllMarkers ""
      let srcDestConfig = HU.getSrcDestConfig state
          hasStops = not $ null state.data.activeRide.stops
          srcLat = srcDestConfig.srcLat
          srcLon = srcDestConfig.srcLon
          destLat = srcDestConfig.destLat
          destLon = srcDestConfig.destLon
          source = srcDestConfig.source
          destination = srcDestConfig.destination
          routeType = if state.props.currentStage == RideAccepted then "pickup" else "trip"
          city = EHU.getCityFromString $ getValueToLocalStore DRIVER_LOCATION
          driverVehicle = getValueToLocalStore VEHICLE_VARIANT
          sourcePointerIcon = if hasStops then EHU.getCitySpecificMarker city driverVehicle (Just $ show state.props.currentStage) else "ny_ic_src_marker"
          destinationMarkericon = if state.props.currentStage == RideAccepted && hasStops then "ny_ic_src_marker" else "ny_ic_dest_marker"
          srcMarkerConfig = JB.defaultMarkerConfig{ markerId = sourcePointerIcon, pointerIcon = sourcePointerIcon, primaryText = source }
          destMarkerConfig = JB.defaultMarkerConfig{ markerId = "ny_ic_dest_marker", pointerIcon = destinationMarkericon, primaryText = destination, anchorU = 0.5, anchorV = 1.0}
          drawRouteType = if hasStops then "DRIVER_LOCATION_UPDATE" else "NORMAL"
                
      if (state.data.activeRide.tripType == ST.Rental) && (state.props.currentStage == RideStarted ) && isNothing state.data.activeRide.nextStopAddress then do
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { routeVisible = true } })
          void $ pure $ removeAllPolylines ""
      else if state.data.config.feature.enableSpecialPickup && state.props.currentStage == RideAccepted && state.data.activeRide.specialLocationTag == Just "SpecialZonePickup" then do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { routeVisible = true } })
        let specialPickupZone = HU.findSpecialPickupZone destLat destLon
        case specialPickupZone of
          Just pickupZone -> do
            void $ pure $ removeAllPolylines ""
            let _ = unsafePerformEffect $ runEffectFn1 JB.locateOnMap JB.locateOnMapConfig{ lat = destLat
                                                                                          , lon = destLon
                                                                                          , geoJson = pickupZone.geoJson
                                                                                          , points = pickupZone.gates
                                                                                          , locationName = pickupZone.locationName
                                                                                          , navigateToNearestGate = false
                                                                                          , specialZoneMarkerConfig { showZoneLabel = true
                                                                                                                    , showLabelActionImage = true }
                                                                                          , locateOnMapPadding = { left : 2.0, top : 2.0, right : 2.0, bottom : 4.0 } }
            pure unit
          Nothing -> pure unit
      else if state.props.showDottedRoute then do
        let coors = (walkCoordinate srcLon srcLat destLon destLat)
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { routeVisible = true } })
        void $ pure $ removeAllPolylines ""
        let normalRoute = JB.mkRouteConfig coors srcMarkerConfig destMarkerConfig Nothing "NORMAL" "DOT" false JB.DEFAULT (mapRouteConfig "" "" false getPolylineAnimationConfig) 
        liftFlowBT $ drawRoute [normalRoute] (getNewIDWithTag "DriverTrackingHomeScreenMap")
        homeScreenFlow
      else if not null state.data.route && not callDrawRoute then do
        let shortRoute = (state.data.route !! 0)
        case shortRoute of
          Just (Route route) -> do
            let coor = walkCoordinates route.points
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { routeVisible = true } })
            pure $ removeMarker "ic_vehicle_side"
            void $ pure $ removeAllPolylines ""
            let normalRoute = JB.mkRouteConfig coor srcMarkerConfig destMarkerConfig Nothing drawRouteType "LineString" true JB.DEFAULT (mapRouteConfig "" "" false getPolylineAnimationConfig) 
            liftFlowBT $ drawRoute [normalRoute] (getNewIDWithTag "DriverTrackingHomeScreenMap")
            pure unit
          Nothing -> pure unit
      else do
        -- eRouteAPIResponse <- lift $ lift $ Remote.getRoute (makeGetRouteReq srcLat srcLon destLat destLon) routeType
        -- case eRouteAPIResponse of
        --   Right (GetRouteResp routeApiResponse) -> do
        --     let shortRoute = (routeApiResponse !! 0)
        --     case shortRoute of
        --       Just (Route route) -> do
        --         let coor = walkCoordinates route.points
        --         modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { activeRide { actualRideDistance = if state.props.currentStage == RideStarted then (toNumber route.distance) else state.data.activeRide.actualRideDistance , duration = route.duration } , route = routeApiResponse}, props { routeVisible = true } })
        --         pure $ removeMarker "ny_ic_auto"
        --         void $ pure $ removeAllPolylines ""
        --         let normalRoute = JB.mkRouteConfig coor srcMarkerConfig destMarkerConfig Nothing "NORMAL" "LineString" true JB.DEFAULT (mapRouteConfig "" "" false getPolylineAnimationConfig) 
        --         liftFlowBT $ drawRoute [normalRoute] (getNewIDWithTag "DriverTrackingHomeScreenMap")
        --         pure unit
        --       Nothing -> pure unit   
        --   Left err -> pure unit          
        let leftStops = DA.filter (\(API.Stop item) -> maybe true (\(API.StopInformation stopInfo) -> isNothing stopInfo.stopEndLatLng) item.stopInfo) state.data.activeRide.stops
            points = (DA.singleton $ API.LatLong {lat : srcLat, lon : srcLon}) 
                      <> (if state.props.currentStage == RideAccepted 
                            then [] 
                            else map (\(API.Stop item) -> getLatlon item.location ) leftStops)
                      <> (DA.singleton $ API.LatLong {lat : destLat, lon : destLon}) 
            getLatlon (API.LocationInfo location) = API.LatLong {lat : location.lat, lon : location.lon} 
        eRouteAPIResponse <- lift $ lift $ Remote.getRoute (makeGetRouteReq srcLat srcLon destLat destLon) routeType
        case eRouteAPIResponse of
          Right (GetRouteResp routeApiResponse) -> do
            let shortRoute = (routeApiResponse !! 0)
            case shortRoute of
              Just (Route route) -> do
                let coor = JB.getExtendedPath $ walkCoordinates route.points
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { activeRide { actualRideDistance = if state.props.currentStage == RideStarted then (toNumber route.distance) else state.data.activeRide.actualRideDistance , duration = route.duration } , route = routeApiResponse}, props { routeVisible = true } })
                pure $ removeMarker "ny_ic_auto"
                void $ pure $ removeAllPolylines ""
                let normalRoute = JB.mkRouteConfig coor srcMarkerConfig destMarkerConfig Nothing drawRouteType "LineString" true JB.DEFAULT (mapRouteConfig "" "" false getPolylineAnimationConfig) 
                liftFlowBT $ drawRoute [normalRoute] (getNewIDWithTag "DriverTrackingHomeScreenMap")
                pure unit
              Nothing -> pure unit   
          Left err -> pure unit        
      when (state.props.currentStage == RideStarted) $ for_  state.data.activeRide.stops $ \(API.Stop stop) -> do
        let (API.LocationInfo stopLocation) = stop.location
        pure $ removeMarker $ "stop" <> show stopLocation.lat <> show stopLocation.lon
        when (maybe true (\(API.StopInformation sInfo) -> isNothing sInfo.stopEndLatLng) stop.stopInfo) $ do
          let markerId = "stop" <> show stopLocation.lat <> show stopLocation.lon
              pt = {lat : stopLocation.lat, lng : stopLocation.lon}
              Tuple sourceArea _ = HU.getStopName (API.Stop stop)
          void $ liftFlowBT $ showMarker JB.defaultMarkerConfig{ markerId = markerId, pointerIcon = "ny_ic_stop_grey"} stopLocation.lat stopLocation.lon 40 0.5 0.9 (getNewIDWithTag "DriverTrackingHomeScreenMap")
          liftFlowBT $ runEffectFn1 upsertMarkerLabel  { id: markerId <> "label" , title: sourceArea, actionImage: "", actionCallBack: "", position: pt, markerImage : ""}
          pure unit
      homeScreenFlow
    UPDATE_STAGE stage -> do
      void $ updateStage $ HomeScreenStage stage
      homeScreenFlow
    GO_TO_NOTIFICATIONS -> notificationFlow Nothing
    ADD_ALTERNATE_HOME -> do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen{props{alternateNumberView = true, isEditAlternateMobile = true, mNumberEdtFocused = true}, data {fromHomeScreen = true}})
      driverProfileFlow
    ON_CALL state exophoneNumber -> do
      (API.ApiSuccessResult resp) <- Remote.onCallBT (Remote.makeOnCallReq state.data.activeRide.id exophoneNumber)
      homeScreenFlow
    OPEN_PAYMENT_PAGE state -> ysPaymentFlow
    HOMESCREEN_NAV GoToSubscription -> do
      let (GlobalState defGlobalState) = defaultGlobalState
      updateAvailableAppsAndGoToSubs
    HOMESCREEN_NAV (GoToEarningsScreen showCoinsView) -> do
      modifyScreenState $ DriverEarningsScreenStateType (\driverEarningsScreen -> driverEarningsScreen { props { subView = if showCoinsView then ST.YATRI_COINS_VIEW else ST.EARNINGS_VIEW }})
      driverEarningsFlow
    HOMESCREEN_NAV _ -> homeScreenFlow
    GO_TO_AADHAAR_VERIFICATION -> do
      modifyScreenState $ AadhaarVerificationScreenType (\aadhaarScreen -> aadhaarScreen { props { fromHomeScreen = true, currentStage = EnterAadhaar}})
      aadhaarVerificationFlow
    GO_TO_RIDE_DETAILS_SCREEN -> do 
      modifyScreenState $ TripDetailsScreenStateType $ \tripDetailsScreen -> tripDetailsScreen { data {goBackTo = ST.Home}}
      tripDetailsScreenFlow
    POST_RIDE_FEEDBACK state-> do 
      void $ lift $ lift $ Remote.postRideFeedback state.data.endRideData.rideId state.data.endRideData.rating state.data.endRideData.feedback
      when (state.data.endRideData.rating == 5) $ void $ pure $ JB.launchInAppRatingPopup unit
      (GlobalState globalstate) <- getState
      (GetDriverInfoResp getDriverInfoResp) <- getDriverInfoDataFromCache (GlobalState globalstate) false
      let (API.DriverGoHomeInfo driverGoHomeInfo) = getDriverInfoResp.driverGoHomeInfo
      when state.data.driverGotoState.isGotoEnabled do
        modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps { 
            gotoPopupType = case globalstate.globalProps.gotoPopupType of
              ST.REDUCED _ -> ST.REDUCED driverGoHomeInfo.cnt 
              _ -> globalstate.globalProps.gotoPopupType}
      void $ updateStage $ HomeScreenStage HomeScreen
      updateDriverDataToStates
      modifyScreenState $ GlobalPropsType (\globalProps -> globalProps { gotoPopupType = ST.NO_POPUP_VIEW })
      homeScreenFlow
    GO_TO_EARNINGS_SCREEN -> driverEarningsFlow
    CLEAR_PENDING_DUES -> clearPendingDuesFlow true
    GO_TO_RIDE_REQ_SCREEN state lat lon -> do
      modifyScreenState $ RideRequestScreenStateType (\rideRequestScreen -> (RideRequestData.initData "") {data{driverLat = Just lat , driverLong = Just lon}})
      rideRequestScreenFlow
    GO_TO_RIDE_SUMMARY_SCREEN state -> do 
     case state.data.upcomingRide of 
      Just ride  -> modifyScreenState $ RideSummaryScreenStateType (\rideSummaryScreen -> rideSummaryScreen { data{ rideDetails = RSC.transformer $ ride , activeRideData  = ride}  , props {throughBanner  = true}})  
      Nothing -> pure unit 

     
     rideSummaryScreenFlow
    GO_TO_RIDE_SUMMARY -> rideSummaryScreenFlow 
    ENABLE_GOTO_API state id currentLocation -> do
      when state.data.isSpecialLocWarrior $ updateWarriorSettings false
      activateResp <- lift $ lift $ Remote.activateDriverGoTo id currentLocation
      pure $ toggleBtnLoader "" false
      case activateResp of
        Right resp -> do 
          void $ pure $ toast $ getString GOTO_LOC_IS_ENABLED
          modifyScreenState $ HomeScreenStateType (\_ -> state { data { driverGotoState { showGoto = false}}})
          driverInfoResp <- Remote.getDriverInfoBT ""
          modifyScreenState $ GlobalPropsType (\globalProps -> globalProps {driverInformation = Just driverInfoResp})
          updateDriverDataToStates
        Left errorPayload ->if (decodeErrorCode errorPayload.response.errorMessage) == "DRIVER_CLOSE_TO_HOME_LOCATION" then modifyScreenState $ HomeScreenStateType (\_ -> state{data { driverGotoState {gotoLocInRange = true
                                , savedLocationsArray = getDisabledLocById id state.data.driverGotoState.savedLocationsArray, selectedGoTo = ""}}})
          else pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
      homeScreenFlow
    LOAD_GOTO_LOCATIONS state -> do
      resp <- lift $ lift $ Remote.getDriverHomeLocation ""
      case resp of
        Right response -> modifyScreenState $ HomeScreenStateType (\_ -> state{data { driverGotoState {showGoto = true, savedLocationsArray = getLocationArray response}}})
        Left errorPayload -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
      homeScreenFlow
    DISABLE_GOTO state -> do
      void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      deactivateResp <- lift $ lift $ Remote.deactivateDriverGoTo ""
      void $ lift $ lift $ toggleLoader false
      case deactivateResp of
        Right _ -> do
          void $ pure $ toast $ getString GOTO_LOC_IS_DISABLED
          void $ pure $ clearTimerWithId state.data.driverGotoState.timerId
          driverInfoResp <- Remote.getDriverInfoBT ""
          modifyScreenState $ GlobalPropsType (\globalProps -> globalProps {driverInformation = Just driverInfoResp})
          updateDriverDataToStates
        Left errorPayload -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
      homeScreenFlow
    GOTO_LOCATION_FLOW state addLocation -> do
      let (GlobalState defaultEpassState') = defaultGlobalState
      if addLocation then
        modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> defaultEpassState'.driverSavedLocationScreen { props { viewType = ST.SearchLocation, gotBackToHomeScreen = true}, data {savedLocationsArray = state.data.driverGotoState.savedLocationsArray}} )
      else modifyScreenState $ DriverSavedLocationScreenStateType (\_ -> defaultEpassState'.driverSavedLocationScreen)
      goToLocationFlow
    REFRESH_GOTO state -> do
      let defState = HomeScreenData.initData
      modifyScreenState $ HomeScreenStateType (\_ -> state { data { driverGotoState = defState.data.driverGotoState}})
      driverInfoResp <- Remote.getDriverInfoBT ""
      modifyScreenState $ GlobalPropsType (\globalProps -> globalProps {driverInformation = Just driverInfoResp, gotoPopupType = ST.NO_POPUP_VIEW})
      updateDriverDataToStates
      homeScreenFlow
    GOT_DRIVER_STATS driverStats -> do
      modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps { driverRideStats = Just $ driverStats }      
      updateDriverDataToStates      
      homeScreenFlow
    UPDATE_SPECIAL_LOCATION_LIST -> do
      (resp :: API.SpecialLocationFullRes) <- HelpersAPI.callApiBT $ API.SpecialLocationFullReq
      void $ pure $ HU.transformSpecialLocationList resp
      homeScreenFlow
    GO_TO_BOOKING_PREFERENCES -> bookingOptionsFlow
    UPDATE_AIR_CONDITIONED isAcWorking -> do
      (resp :: (Either ErrorResponse API.ApiSuccessResult)) <- lift $ lift $ HelpersAPI.callApi $ API.UpdateAirConditionUpdateRequest { isAirConditioned : isAcWorking }
      case resp of
        Right _ -> do
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { showAcWorkingPopup = Just false }})
          globalState <- getState
          (GetDriverInfoResp getDriverInfoResp) <- getDriverInfoDataFromCache globalState false
          let updatedResponse = getDriverInfoResp{checkIfACWorking = Just false}
          modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{driverInformation = Just (GetDriverInfoResp updatedResponse)}
        Left _ -> do
          pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
    UPDATE_ROUTE_ON_STAGE_SWITCH state -> do
      let srcDestConfig = HU.getSrcDestConfig state
          srcLat = srcDestConfig.srcLat
          srcLon = srcDestConfig.srcLon
          destLat = srcDestConfig.destLat
          destLon = srcDestConfig.destLon
          source = srcDestConfig.source
          destination = srcDestConfig.destination
          routeType = if state.props.currentStage == RideAccepted then "pickup" else "trip"
          srcMarkerConfig = JB.defaultMarkerConfig{ pointerIcon = "ny_ic_src_marker", primaryText = source }
          destMarkerConfig = JB.defaultMarkerConfig{ pointerIcon = "ny_ic_dest_marker", primaryText = destination, anchorU = 0.5, anchorV = 1.0 }
      eRouteAPIResponse <- lift $ lift $ Remote.getRoute (makeGetRouteReq srcLat srcLon destLat destLon) routeType
      case eRouteAPIResponse of
        Right (GetRouteResp routeApiResponse) -> do
          let shortRoute = (routeApiResponse !! 0)
          case shortRoute of
            Just (Route route) -> do
              let coor = walkCoordinates route.points
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { activeRide { actualRideDistance = if state.props.currentStage == RideStarted then (toNumber route.distance) else state.data.activeRide.actualRideDistance , duration = route.duration } , route = routeApiResponse}, props { routeVisible = true } })
              pure $ removeMarker "ny_ic_auto"
              void $ pure $ removeAllPolylines ""
              let normalRoute = JB.mkRouteConfig coor srcMarkerConfig destMarkerConfig Nothing "NORMAL" "LineString" true JB.DEFAULT (mapRouteConfig "" "" false getPolylineAnimationConfig) 
              liftFlowBT $ drawRoute [normalRoute] (getNewIDWithTag "DriverTrackingHomeScreenMap")
              pure unit
            Nothing -> pure unit   
        Left err -> pure unit  
      homeScreenFlow
    GO_TO_BENEFITS_SCREEN_FROM_HOME -> referralFlow
    GO_TO_ADD_UPI_SCREEN -> do
      let (GlobalState defGlobalState) = defaultGlobalState
      let customerReferralTrackerScreenState = defGlobalState.customerReferralTrackerScreen
      modifyScreenState $ CustomerReferralTrackerScreenStateType (\_ -> customerReferralTrackerScreenState{props {openPP = true}})
      customerReferralTrackerFlow
    VERIFY_MANUAL_UPI state -> do 
      response <- lift $ lift $ Remote.verifyVpaID ""
      case response of 
        Right val -> pure unit
        Left (errorPayload) -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
    SWITCH_PLAN_FROM_HS plan state -> do
      void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT)
      void $ lift $ lift $ toggleLoader true
      selectPlanResp <- lift $ lift $ Remote.selectPlan plan.id
      case selectPlanResp of 
        Right resp -> do 
          getDriverInfoResp <- Remote.getDriverInfoBT ""
          void $ lift $ lift $ toggleLoader false
          modifyScreenState $ GlobalPropsType (\globalProps -> globalProps {driverInformation = Just getDriverInfoResp})
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { plansState { showSwitchPlanModal = false} } })
          updateDriverDataToStates
        Left errorPayload -> do
          void $ lift $ lift $ toggleLoader false
          pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
      homeScreenFlow
    GOTO_HOTSPOT_SCREEN state -> hotspotScreenFlow
    GO_TO_UPLOAD_PARCEL_IMAGE state -> do
      modifyScreenState $ UploadParcelImageScreenStateType (\uploadParcelImageScreen -> uploadParcelImageScreen { data {rideId = state.data.activeRide.id}, props { showConfirmAndUploadButton = false, uploading = false, isStartRideActive = (state.props.currentStage == ST.RideAccepted || (state.props.currentStage == ST.ChatWithCustomer && (Const.getHomeStageFromString $ getValueToLocalStore PREVIOUS_LOCAL_STAGE) /= ST.RideStarted ) ) }})
      uploadParcelImageFlow
    NOTIFY_DRIVER_REACHED_DESTINATION state -> do
      when (state.data.activeRide.tripType == ST.Delivery) do
        driverArrived <- lift $ lift $ Remote.driverReachedDestination (state.data.activeRide.id) (DriverReachedReq {
          "lat" : state.data.currentDriverLat
        , "lon" : state.data.currentDriverLon
        })
        case driverArrived of
          Right _ -> do
            setValueToLocalStore WAITING_TIME_STATUS $ show ST.DestinationReachedTriggered
            
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{activeRide{notifiedReachedDestination = true}}})
          Left _ -> pure unit
        homeScreenFlow
    UPDATE_METRO_WARRIOR state -> do
      updateWarriorSettings $ not state.data.isSpecialLocWarrior
      homeScreenFlow
    GO_TO_METRO_WARRIOR state -> metroWarriorsScreenFlow
    UPDATE_STOPS_STATUS state -> do
      let driverLocation = 
            API.LatLong 
            { lat : state.data.currentDriverLat
            , lon : state.data.currentDriverLon
            }
          stopToDepart = HU.getStopToDepart state.data.activeRide.stops
          upcomingStop = HU.getUpcomingStop state.data.activeRide.stops
          locationId = case stopToDepart of 
                          Just (API.Stop stop) -> fromMaybe "" (unwrap stop.location).id
                          Nothing -> maybe "" (\(API.Stop stop) -> fromMaybe "" (unwrap stop.location).id) upcomingStop
      void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT)
      void $ lift $ lift $ toggleLoader true
      (resp :: (Either ErrorResponse API.ApiSuccessResult)) <- lift $ lift $ HelpersAPI.callApi $ API.UpdateStopStatusReq state.data.activeRide.id locationId (isNothing stopToDepart) driverLocation
      case resp of
        Right _ -> do
          void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT)
          case stopToDepart of
            Just (API.Stop stop) -> do
              let (API.LocationInfo stopLocation) = stop.location
                  markerId = "stop" <> show stopLocation.lat <> show stopLocation.lon
              pure $ removeMarker markerId
              pure $ removeMarker $ markerId <> "label"
            Nothing -> pure unit
          (GetRidesHistoryResp activeRideResponse) <- Remote.getRideHistoryReqBT "2" "0" "true" "null" "null"
          case (DA.find (\(RidesInfo x) -> x.bookingType == Just CURRENT) activeRideResponse.list) of
            Just ride -> do
              let advancedRide = (DA.find (\(RidesInfo x) -> x.bookingType == Just ADVANCED) activeRideResponse.list)
                  currActiveRideDetails = activeRideDetail state ride
                  advancedRideDetails = activeRideDetail state <$> advancedRide
              modifyScreenState $ HomeScreenStateType (\homeScreen -> state{ data {activeRide = currActiveRideDetails, advancedRideData = advancedRideDetails}, props{triggerGMapsIntent = isJust stopToDepart}})
            Nothing -> do
              setValueToLocalStore IS_RIDE_ACTIVE "false"
              pure unit
        Left err -> pure unit
      void $ lift $ lift $ toggleLoader false
  homeScreenFlow

clearPendingDuesFlow :: Boolean -> FlowBT String Unit
clearPendingDuesFlow showLoader = do
  void $ lift $ lift $ toggleLoader showLoader
  liftFlowBT $ initiatePaymentPage
  clearduesResp' <- lift $ lift $ Remote.cleardues ""
  case clearduesResp' of
    Right (ClearDuesResp clearduesResp) -> do
      let (CreateOrderRes orderResp) = clearduesResp.orderResp
          (PaymentPagePayload sdk_payload) = orderResp.sdk_payload
          (PayPayload innerpayload) = sdk_payload.payload
      setValueToLocalStore DISABLE_WIDGET "true"
      void $ pure $ cleverTapCustomEvent "ny_driver_payment_page_opened"
      void $ pure $ metaLogEvent "ny_driver_payment_page_opened"
      liftFlowBT $ firebaseLogEvent "ny_driver_payment_page_opened"
      lift $ lift $ doAff $ makeAff \cb -> runEffectFn1 checkPPInitiateStatus (cb <<< Right) $> nonCanceler
      let sdkPayload = maybe (encodeJSON orderResp.sdk_payload) (addLanguageToPayload (getPaymentPageLangKey (getLanguageLocale languageKey))) orderResp.sdk_payload_json
      void $ paymentPageUI sdkPayload
      pure $ toggleBtnLoader "" false
      void $ lift $ lift $ toggleLoader false
      liftFlowBT $ runEffectFn1 consumeBP unit
      setValueToLocalStore DISABLE_WIDGET "false"
      orderStatus <- lift $ lift $ Remote.paymentOrderStatus $ clearduesResp.orderId
      case orderStatus of
        Right (OrderStatusRes statusResp) -> do
          when (statusResp.status == PS.CHARGED) $ do
            _<- pure $ cleverTapEvent "ny_driver_clear_dues" $ [ {key : "due_amount", value : unsafeToForeign innerpayload.amount},
                                                                                         {key : "clearence_type", value : unsafeToForeign "manual"}
                                                                                        ]
            pure unit
          let popUpState = if statusResp.status == PS.CHARGED then Just PaymentSuccessPopup
                            else if any ( _ == statusResp.status)[PS.AUTHORIZATION_FAILED, PS.AUTHENTICATION_FAILED, PS.JUSPAY_DECLINED] then Just FailedPopup
                            else Nothing
          case popUpState of
            Just popUpState' -> modifyScreenState $ SubscriptionScreenStateType (\subscribeScreenState -> subscribeScreenState { props {popUpState = Just popUpState', lastPaymentType = Just API.CLEAR_DUE}})
            Nothing -> pure unit
        Left err -> pure $ toast $ Remote.getCorrespondingErrorMessage err 
    Left errorPayload -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
  getDriverInfoApiResp <- lift $ lift $ Remote.getDriverInfoApi ""
  case getDriverInfoApiResp of
    Right resp -> modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{driverInformation = Just resp}
    Left _ -> pure unit
  updateDriverDataToStates
  void $ lift $ lift $ toggleLoader false
  pure $ toggleBtnLoader "" false
  subScriptionFlow

nyPaymentFlow :: PlanCardConfig -> String -> FlowBT String Unit
nyPaymentFlow planCardConfig fromScreen = do
  liftFlowBT $ initiatePaymentPage
  response <- lift $ lift $ Remote.subscribePlan planCardConfig.id
  case response of
    Right (SubscribePlanResp listResp) -> do
      if fromScreen /= "MYPLAN" then do
        void $ pure $ cleverTapCustomEventWithParams "ny_driver_selected_plan" "selected_plan" planCardConfig.title
        void $ pure $ cleverTapCustomEventWithParams "ny_driver_selected_plan" "offer" $ show $ map (\offer -> offer.title) planCardConfig.offers
        liftFlowBT $ metaLogEventWithTwoParams "ny_driver_selected_plan" "selected_plan" planCardConfig.title "offer" $ show $ map (\offer -> offer.title) planCardConfig.offers
        liftFlowBT $ firebaseLogEventWithTwoParams "ny_driver_selected_plan" "selected_plan" planCardConfig.title "offer" $ show $ map (\offer -> offer.title) planCardConfig.offers
        pure unit 
      else pure unit
      let (CreateOrderRes orderResp) = listResp.orderResp
      setValueToLocalStore DISABLE_WIDGET "true"
      void $ pure $ cleverTapCustomEvent "ny_driver_payment_page_opened"
      void $ pure $ metaLogEvent "ny_driver_payment_page_opened"
      liftFlowBT $ firebaseLogEvent "ny_driver_payment_page_opened"
      lift $ lift $ doAff $ makeAff \cb -> runEffectFn1 checkPPInitiateStatus (cb <<< Right) $> nonCanceler
      let sdkPayload = maybe (encodeJSON orderResp.sdk_payload) (addLanguageToPayload (getPaymentPageLangKey (getLanguageLocale languageKey))) orderResp.sdk_payload_json
      void $ paymentPageUI sdkPayload
      pure $ toggleBtnLoader "" false
      liftFlowBT $ runEffectFn1 consumeBP unit
      setValueToLocalStore DISABLE_WIDGET "false"
      orderStatus <- lift $ lift $ Remote.paymentOrderStatus listResp.orderId
      case orderStatus of
        Right (OrderStatusRes statusResp) ->
          case statusResp.status of
            PS.CHARGED -> do
                setSubscriptionStatus Success statusResp.status planCardConfig
            PS.AUTHORIZATION_FAILED -> setSubscriptionStatus Failed statusResp.status planCardConfig
            PS.AUTHENTICATION_FAILED -> setSubscriptionStatus Failed statusResp.status planCardConfig
            PS.JUSPAY_DECLINED -> setSubscriptionStatus Failed statusResp.status planCardConfig
            PS.NEW -> pure unit
            PS.PENDING_VBV -> setSubscriptionStatus Pending statusResp.status planCardConfig
            _ -> setSubscriptionStatus Pending statusResp.status planCardConfig
        Left err -> setSubscriptionStatus Pending PS.PENDING_VBV planCardConfig
    Left (errorPayload) -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
  if fromScreen == "ONBOARDING" then
    onBoardingFlow
  else
    subScriptionFlow

setSubscriptionStatus :: PaymentStatus -> PS.APIPaymentStatus -> PlanCardConfig -> FlowBT String Unit
setSubscriptionStatus paymentStatus apiPaymentStatus planCardConfig = do
  (GlobalState globalState) <- getState
  case paymentStatus of
    Success -> do 
      void $ pure $ cleverTapCustomEvent "ny_driver_subscription_success"
      void $ pure $ JB.metaLogEvent "ny_driver_subscription_success"
      liftFlowBT $ JB.firebaseLogEvent "ny_driver_subscription_success"
      getDriverInfoApiResp <- lift $ lift $ Remote.getDriverInfoApi ""
      case getDriverInfoApiResp of
        Right resp -> modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{driverInformation = Just resp}
        Left _ -> pure unit
      updateDriverDataToStates
      modifyScreenState $ SubscriptionScreenStateType (\subscribeScreenState -> subscribeScreenState { props {popUpState = Just SuccessPopup}})
    Failed -> do
      void $ pure $ cleverTapCustomEventWithParams "ny_driver_subscription_failure" "selected_plan" planCardConfig.title
      void $ pure $ cleverTapCustomEventWithParams "ny_driver_subscription_failure" "failure_code" (show apiPaymentStatus)
      liftFlowBT $ metaLogEventWithTwoParams "ny_driver_subscription_failure" "selected_plan" planCardConfig.title "failure_code" (show apiPaymentStatus)
      liftFlowBT $ firebaseLogEventWithTwoParams "ny_driver_subscription_failure" "selected_plan" planCardConfig.title "failure_code" (show apiPaymentStatus)
      modifyScreenState $ SubscriptionScreenStateType (\subscribeScreenState -> subscribeScreenState { props {popUpState = Just FailedPopup, lastPaymentType = Just API.AUTOPAY_REGISTRATION_TYPE}})
    Pending -> modifyScreenState $ SubscriptionScreenStateType (\subscribeScreenState -> subscribeScreenState { props {joinPlanProps {selectedPlanItem = Nothing}}})
    Scheduled -> pure unit

paymentHistoryFlow :: FlowBT String Unit
paymentHistoryFlow = do 
  appConfig <- getAppConfigFlowBT Constants.appConfig
  action <- UI.paymentHistory
  case action of 
    GoToSetupAutoPay state -> 
      if state.data.autoPayStatus == ST.SUSPENDED then do
        resumeMandate <- lift $ lift $ Remote.resumeMandate ""
        case resumeMandate of 
          Right resp -> do
            getDriverInfoResp <- Remote.getDriverInfoBT ""
            modifyScreenState $ GlobalPropsType (\globalProps -> globalProps {driverInformation = Just getDriverInfoResp})
            updateDriverDataToStates
            let (GlobalState defGlobalState) = defaultGlobalState
            modifyScreenState $ SubscriptionScreenStateType (\_ -> defGlobalState.subscriptionScreen)
            pure $ toast $ getString RESUMED_AUTOPAY
          Left errorPayload -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
        subScriptionFlow
      else nyPaymentFlow state.data.planData "MYPLAN"
    EntityDetailsAPI state id -> do
      paymentEntityDetails <- lift $ lift $ Remote.paymentEntityDetails id
      let cityConfig = HU.getCityConfig appConfig.cityConfig (getValueToLocalStore DRIVER_LOCATION)
      case paymentEntityDetails of
        Right (HistoryEntryDetailsEntityV2Resp resp) ->
            modifyScreenState $ PaymentHistoryScreenStateType (\paymentHistoryScreen -> paymentHistoryScreen{props{subView = ST.TransactionDetails},
              data{ transactionDetails = (buildTransactionDetails (HistoryEntryDetailsEntityV2Resp resp) state.data.gradientConfig appConfig.subscriptionConfig.showFeeBreakup cityConfig.gstPercentage)}
            })
        Left errorPayload -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
      paymentHistoryFlow
    SWITCH_TAB -> paymentHistoryFlow
    LOAD_MORE_ITEMS state -> do
      modifyScreenState $ PaymentHistoryScreenStateType (\paymentHistoryScreen -> paymentHistoryScreen{props{offset = state.props.offset + 15}})
      paymentHistoryFlow
  pure unit 

hotspotScreenFlow :: FlowBT String Unit 
hotspotScreenFlow = do
  logField_ <- lift $ lift $ getLogFields
  liftFlowBT $ logEvent logField_ "ny_driver_open_hotspots_screen_event"
  action <- UI.hotspotScreen
  case action of
    REFRESH_HOTSPOTS -> hotspotScreenFlow
    _ -> homeScreenFlow
  pure unit

ysPaymentFlow :: FlowBT String Unit
ysPaymentFlow = do
  liftFlowBT $ initiatePaymentPage
  (GlobalState state) <- getState
  let homeScreenState = state.homeScreen
  response <- lift $ lift $ Remote.createPaymentOrder homeScreenState.data.paymentState.invoiceId
  case response of
    Right (CreateOrderRes listResp) -> do
      let (PaymentPagePayload sdk_payload) = listResp.sdk_payload
      setValueToLocalStore DISABLE_WIDGET "true"
      lift $ lift $ doAff $ makeAff \cb -> runEffectFn1 checkPPInitiateStatus (cb <<< Right) $> nonCanceler
      let sdkPayload = maybe (encodeJSON listResp.sdk_payload) (addLanguageToPayload (getPaymentPageLangKey (getLanguageLocale languageKey))) listResp.sdk_payload_json
      paymentPageOutput <- paymentPageUI sdkPayload
      pure $ toggleBtnLoader "" false
      setValueToLocalStore DISABLE_WIDGET "false"
      liftFlowBT $ runEffectFn1 consumeBP unit
      if paymentPageOutput == "backpressed" then homeScreenFlow else pure unit-- backpressed FAIL
      orderStatus <- lift $ lift $ Remote.paymentOrderStatus homeScreenState.data.paymentState.invoiceId
      case orderStatus of
        Right (OrderStatusRes resp) ->
          case resp.status of
            PS.CHARGED -> setPaymentStatus Success sdk_payload.payload
            PS.AUTHORIZATION_FAILED -> setPaymentStatus Failed sdk_payload.payload
            PS.AUTHENTICATION_FAILED -> setPaymentStatus Failed sdk_payload.payload
            PS.JUSPAY_DECLINED -> setPaymentStatus Failed sdk_payload.payload
            PS.NEW -> setPaymentStatus Pending sdk_payload.payload
            PS.PENDING_VBV -> setPaymentStatus Pending sdk_payload.payload
            PS.AUTHORIZING -> setPaymentStatus Pending sdk_payload.payload
            PS.COD_INITIATED -> setPaymentStatus Pending sdk_payload.payload
            PS.STARTED -> setPaymentStatus Pending sdk_payload.payload
            PS.AUTO_REFUNDED -> setPaymentStatus Pending sdk_payload.payload
        Left error -> setPaymentStatus Failed sdk_payload.payload
    Left (error) -> do
      pure $ toast $ getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
      homeScreenFlow
  ackScreenFlow homeScreenFlow

setPaymentStatus :: PaymentStatus -> PayPayload -> FlowBT String Unit
setPaymentStatus paymentStatus (PayPayload payload) = do
      case paymentStatus of
        Success -> do
                  let currency = getCurrency Constants.appConfig
                  setValueToLocalStore SHOW_PAYMENT_MODAL "false"
                  modifyScreenState $ AcknowledgementScreenType (\a -> a { data {
                    title = Just ( case getLanguageLocale languageKey of
                          "EN_US" -> "Payment of "<> currency <> payload.amount <>" Successful!"
                          "HI_IN" -> currency <> payload.amount <> " का भुगतान सफल!"
                          "BN_IN" -> currency <> payload.amount <> " পেমেন্ট সফল!"
                          _       -> "Payment of " <> currency <> payload.amount <>" Successful!"
                      ),
                    description = Nothing,
                    primaryButtonText = Just (getString GO_TO_HOME) ,
                    illustrationAsset = "success_lottie.json",
                    orderId = payload.orderId,
                    amount = payload.amount
                    },
                    props{ paymentStatus = paymentStatus}})
                  modifyScreenState $ HomeScreenStateType (\homeScreenState -> homeScreenState { data {paymentState {
                    paymentStatus = paymentStatus,
                    paymentStatusBanner = not (paymentStatus == Success),
                    makePaymentModal = false
                    }}})


        Failed -> modifyScreenState $ AcknowledgementScreenType (\ackScreenState -> ackScreenState { data { title = Just (getString PAYMENT_FAILED), description = Just (getString (PAYMENT_FAILED_DESC "PAYMENT_FAILED_DESC")), primaryButtonText = Just  "Retry Payment" , illustrationAsset = "ny_failed,"}, props {illustrationType = ST.Image, paymentStatus = paymentStatus}})

        Pending -> do
                  setValueToLocalStore PAYMENT_STATUS_POOLING "true"
                  setValueToLocalStore SHOW_PAYMENT_MODAL "false"
                  let time2PmTo10Am = (withinTimeRange "14:00:00" "10:00:00" (convertUTCtoISC(getCurrentUTC "") "HH:mm:ss"))
                  modifyScreenState $ AcknowledgementScreenType (\ackScreenState -> ackScreenState { data { title = Just (getString PAYMENT_PENDING), description = Just (getString PAYMENT_PENDING_DESC), primaryButtonText = Just  (getString GO_TO_HOME) , illustrationAsset = "ny_ic_payment_pending,"}, props {illustrationType = ST.Image, paymentStatus = paymentStatus}})
                  modifyScreenState $ HomeScreenStateType (\homeScreenState -> homeScreenState { data {paymentState {
                    paymentStatus = paymentStatus,
                    paymentStatusBanner = not (paymentStatus == Success),
                    makePaymentModal = false,
                    bannerBG = if time2PmTo10Am then Color.pearl else Color.floralWhite,
                    bannerTitle = getString if time2PmTo10Am then YOUR_PREVIOUS_PAYMENT_IS_PENDING else WE_WILL_NOTIFY_WHEN_PAYMENT_SUCCESS,
                    bannerTitleColor = if time2PmTo10Am then Color.dustyRed else Color.selectiveYellow,
                    banneActionText = getString if time2PmTo10Am then CONTACT_SUPPORT else CONTINUE_TAKING_RIDES,
                    bannerImage = if time2PmTo10Am then "ny_ic_payment_failed_banner," else "ny_ic_payment_pending_banner,",
                    blockedDueToPayment = time2PmTo10Am,
                    actionTextColor = if time2PmTo10Am then Color.dustyRed else Color.selectiveYellow
                    }}})
        Scheduled -> pure unit


ackScreenFlow :: FlowBT String Unit -> FlowBT String Unit
ackScreenFlow postRunflow = do
  action <- UI.acknowledgementScreen
  postRunflow


subScriptionFlow :: FlowBT String Unit
subScriptionFlow = do
  liftFlowBT $ runEffectFn1 initiatePP unit
  reelsResp <- lift $ lift $ Remote.getReelsVideo "reels_data" $ HU.getLanguageTwoLetters $ Just (getLanguageLocale languageKey)
  globalState <- getState
  GetDriverInfoResp getDriverInfoResp <- getDriverInfoDataFromCache globalState false
  let reelsRespData = case reelsResp of
                        Left err -> API.ReelsResp {reels : []}
                        Right resp -> resp
      driverVehicle = getValueToLocalStore VEHICLE_VARIANT
      driverCity = getValueToLocalStore DRIVER_LOCATION
      vehicleAndCityConfig = CommonRC.subscriptionsConfigVariantLevel driverCity driverVehicle
      Vehicle linkedVehicle = fromMaybe dummyVehicleObject getDriverInfoResp.linkedVehicle

  modifyScreenState $ SubscriptionScreenStateType \subscriptionScreen ->
    subscriptionScreen
      { data
          { reelsData = transformReelsRespToReelsData reelsRespData
          , vehicleAndCityConfig = vehicleAndCityConfig
          , linkedVehicleVariant = linkedVehicle.variant
          , switchPlanModalState = SubscriptionScreenInitData.initData.data.switchPlanModalState
          , subscriptionDown = getDriverInfoResp.subscriptionDown
          }
      , props
          { offerBannerProps = fromMaybe CommonRC.defaultOfferBannerConfig vehicleAndCityConfig.offerBannerConfig
          , isSelectedLangTamil = (getLanguageLocale languageKey) == "TA_IN"
          }
      }

  void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
  uiAction <- UI.subscriptionScreen
  case uiAction of
    NAV HomeScreenNav -> homeScreenFlow
    NAV GoToRideHistory -> myRidesScreenFlow
    NAV GoToContest -> referralFlow
    NAV GoToAlerts -> notificationFlow Nothing
    GOTO_HOMESCREEN -> homeScreenFlow
    NAV (GoToEarningsScreen _) -> driverEarningsFlow
    MAKE_PAYMENT state -> do
      case state.props.joinPlanProps.selectedPlanItem of 
        Just selectedPlan -> do
          setValueToLocalStore DISABLE_WIDGET "true"
          nyPaymentFlow selectedPlan "JOINPLAN"
        Nothing -> subScriptionFlow
    GOTO_PAYMENT_HISTORY state -> do
      let (GlobalState defGlobalState) = defaultGlobalState
      modifyScreenState $ PaymentHistoryScreenStateType(\_ -> defGlobalState.paymentHistoryScreen{props{autoPaySetup = state.data.myPlanData.autoPayStatus == ACTIVE_AUTOPAY, subView = ST.PaymentHistory}, data{autoPayStatus = state.data.myPlanData.autoPayStatus, planData = state.data.myPlanData.planEntity, gradientConfig = state.data.config.subscriptionConfig.gradientConfig}})
      paymentHistoryFlow
    CANCEL_AUTOPAY state -> do
      suspendMandate <- lift $ lift $ Remote.suspendMandate state.data.driverId
      case suspendMandate of 
        Right resp -> do 
          getDriverInfoResp <- Remote.getDriverInfoBT ""
          modifyScreenState $ GlobalPropsType (\globalProps -> globalProps {driverInformation = Just getDriverInfoResp})
          updateDriverDataToStates
          let (GlobalState defGlobalState) = defaultGlobalState
          modifyScreenState $ SubscriptionScreenStateType (\_ -> defGlobalState.subscriptionScreen{props{isEndRideModal = state.props.isEndRideModal}})
          pure $ toast $ getString AUTOPAY_CANCELLED
          pure $ toggleBtnLoader "" false
          subScriptionFlow
        Left errorPayload -> do 
          pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
          modifyScreenState $ SubscriptionScreenStateType (\subScriptionScreenState -> subScriptionScreenState{ props{ showError = true, showShimmer = false }})
      subScriptionFlow
    SWITCH_PLAN state planId -> do
      selectPlanResp <- lift $ lift $ Remote.selectPlan planId
      case selectPlanResp of 
        Right resp -> do
          let (GlobalState defGlobalState) = defaultGlobalState
          void $ pure $ cleverTapCustomEvent "ny_driver_switch_plan"
          void $ pure $ cleverTapCustomEventWithParams "ny_driver_switch_plan" "new_plan" state.props.managePlanProps.selectedPlanItem.title
          void $ pure $ cleverTapCustomEventWithParams "ny_driver_switch_plan" "previous_plan" state.data.managePlanData.currentPlan.title
          liftFlowBT $ metaLogEventWithTwoParams "ny_driver_switch_plan" "new_plan" state.props.managePlanProps.selectedPlanItem.title "previous_plan" state.data.managePlanData.currentPlan.title
          liftFlowBT $ firebaseLogEventWithTwoParams "ny_driver_switch_plan" "new_plan" state.props.managePlanProps.selectedPlanItem.title "previous_plan" state.data.managePlanData.currentPlan.title
          modifyScreenState $ SubscriptionScreenStateType (\subScriptionScreenState -> subScriptionScreenState{props{popUpState = Just SwitchedPlan, isEndRideModal = state.props.isEndRideModal}, data{managePlanData{currentPlan {title = state.props.managePlanProps.selectedPlanItem.title}}}})
        Left errorPayload -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
      subScriptionFlow
    RESUME_AUTOPAY state -> do
      resumeMandate <- lift $ lift $ Remote.resumeMandate state.data.driverId
      case resumeMandate of 
        Right resp -> do
          getDriverInfoResp <- Remote.getDriverInfoBT ""
          modifyScreenState $ GlobalPropsType (\globalProps -> globalProps {driverInformation = Just getDriverInfoResp})
          updateDriverDataToStates
          let (GlobalState defGlobalState) = defaultGlobalState
          modifyScreenState $ SubscriptionScreenStateType (\_ -> defGlobalState.subscriptionScreen)
          pure $ toast $ getString RESUMED_AUTOPAY
        Left errorPayload -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
      subScriptionFlow
    RETRY_PAYMENT_AC state planId -> nyPaymentFlow state.data.myPlanData.planEntity "MYPLAN"
    CHECK_ORDER_STATUS state orderId-> do
      orderStatus <- lift $ lift $ Remote.paymentOrderStatus orderId
      case orderStatus of
        Right (OrderStatusRes statusResp) -> do
            let status = if statusResp.status == PS.CHARGED then Success else if any (_ == statusResp.status) [PS.AUTHORIZATION_FAILED, PS.AUTHENTICATION_FAILED, PS.JUSPAY_DECLINED] then Failed else Pending
                popupState = if status == Success then Just SuccessPopup else if status == Failed then Just FailedPopup else Nothing
            modifyScreenState $ SubscriptionScreenStateType (\subScriptionScreenState -> subScriptionScreenState{props{ popUpState = popupState}})
        Left errorPayload -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
      subScriptionFlow
    REFRESH_SUSCRIPTION -> do
      (GlobalState state) <- getState
      getDriverInfoResp <- Remote.getDriverInfoBT ""
      modifyScreenState $ GlobalPropsType (\globalProps -> globalProps {driverInformation = Just getDriverInfoResp})
      updateDriverDataToStates
      let (GlobalState defGlobalState) = defaultGlobalState
          isEndRideModal = state.subscriptionScreen.props.isEndRideModal
      modifyScreenState $ SubscriptionScreenStateType (\_ -> defGlobalState.subscriptionScreen{props{isEndRideModal = isEndRideModal}})
      subScriptionFlow
    GO_TO_MANAGE_PLAN state -> do
      uiPlans <- Remote.getUiPlansBT "null"
      modifyScreenState $ SubscriptionScreenStateType (\subScriptionScreenState -> subScriptionScreenState{ data { managePlanData { alternatePlans = alternatePlansTransformer uiPlans state}}, props {subView = ManagePlan, showShimmer = false}})
      subScriptionFlow
    GO_TO_FIND_HELP_CENTRE state -> do
      let currentDriverLat = fromMaybe 0.0 $ Number.fromString $ getValueToLocalNativeStore LAST_KNOWN_LAT
      let currentDriverLon = fromMaybe 0.0 $ Number.fromString $ getValueToLocalNativeStore LAST_KNOWN_LON
      (LatLon lat lon _) <- getCurrentLocation currentDriverLat currentDriverLon currentDriverLat currentDriverLon 750 false true
      modifyScreenState $ SubscriptionScreenStateType (\subScriptionScreenState -> subScriptionScreenState{props {subView = FindHelpCentre, showShimmer = false, currentLat = fromMaybe 0.0 $ Number.fromString lat, currentLon = fromMaybe 0.0 $ Number.fromString lon}})
      subScriptionFlow
    REFRESH_HELP_CENTRE state -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> state { props {subView = FindHelpCentre, showShimmer = true}})
      subScriptionFlow
    GO_TO_OPEN_GOOGLE_MAPS state -> do
      void $ pure $ openNavigation state.props.destLat state.props.destLon "DRIVE"
      subScriptionFlow
    SUBSCRIBE_API state -> nyPaymentFlow state.data.myPlanData.planEntity "MYPLAN"
    CLEAR_DUES_ACT -> clearPendingDuesFlow false
    SWITCH_PLAN_ON_CITY_VEHICLE_CHANGE plan state -> do
      void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT)
      void $ lift $ lift $ toggleLoader true
      selectPlanResp <- lift $ lift $ Remote.selectPlan plan.id
      case selectPlanResp of 
        Right resp -> do 
          getDriverInfoResp <- Remote.getDriverInfoBT ""
          void $ lift $ lift $ toggleLoader false
          modifyScreenState $ GlobalPropsType (\globalProps -> globalProps {driverInformation = Just getDriverInfoResp})
          modifyScreenState $ SubscriptionScreenStateType (\subScriptionScreenState -> subScriptionScreenState { data { switchPlanModalState { showSwitchPlanModal = false} } })
          updateDriverDataToStates
        Left errorPayload -> do
          void $ lift $ lift $ toggleLoader false
          pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
      subScriptionFlow
    _ -> subScriptionFlow

constructLatLong :: String -> String -> Location
constructLatLong lat lng =
  { lat: fromMaybe 0.0 (Number.fromString lat)
  , lon : fromMaybe 0.0 (Number.fromString lng)
  , place : ""
  , driverInsideThreshold : false
  }

updateCustomerMarker :: Location -> Effect Unit
updateCustomerMarker loc = pure unit

editBankDetailsFlow :: FlowBT String Unit
editBankDetailsFlow = do
  action <- UI.editBankDetailsScreen
  pure unit

-- TODO :: currently this flow is not in use
-- editAadhaarDetailsFlow :: FlowBT String Unit
-- editAadhaarDetailsFlow = do
--   action <- UI.editAadhaarDetailsScreen
--   pure unit

noInternetScreenFlow :: String -> FlowBT String Unit
noInternetScreenFlow triggertype = do
  config <- getAppConfigFlowBT Constants.appConfig
  action <- UI.noInternetScreen triggertype
  internetCondition <- lift $ lift $ liftFlow $ isInternetAvailable unit
  case action of
    REFRESH_INTERNET -> case ((ifNotRegistered unit) || (getValueToLocalStore IS_DRIVER_ENABLED == "false")) of
                        true -> pure unit
                        false ->  baseAppFlow false Nothing Nothing
    TURN_ON_GPS -> if not internetCondition then noInternetScreenFlow "INTERNET_ACTION"
                    else do
                      when (isTokenValid (getValueToLocalStore REGISTERATION_TOKEN)) $ do 
                        void $ lift $ lift $ Remote.driverActiveInactive "true" $ toUpper $ show Online
                      baseAppFlow false Nothing Nothing
    CHECK_INTERNET -> case ((ifNotRegistered unit) || (getValueToLocalStore IS_DRIVER_ENABLED == "false")) of
                      true  -> pure unit
                      false -> do
                        permissionsGiven <- checkAllPermissions true config.permissions.locationPermission
                        if permissionsGiven
                          then baseAppFlow false Nothing Nothing
                          else permissionsScreenFlow Nothing Nothing Nothing

checkAllPermissions :: Boolean -> Boolean -> FlowBT String Boolean
checkAllPermissions checkBattery checkLocation = do
  config <- getAppConfigFlowBT Constants.appConfig
  androidVersion <- lift $ lift $ liftFlow $ getAndroidVersion
  isNotificationPermission <- lift $ lift $ liftFlow $ isNotificationPermissionEnabled unit
  isOverlayPermission <- lift $ lift $ liftFlow $ isOverlayPermissionEnabled unit
  isBatteryUsagePermission <- lift $ lift $ liftFlow $ isBatteryPermissionEnabled unit
  isLocationPermission <- lift $ lift $ liftFlow $ isLocationPermissionEnabled unit
  pure $ (androidVersion < 13 || not config.permissions.notification || isNotificationPermission) && 
          isOverlayPermission && 
          (not checkBattery || isBatteryUsagePermission) && 
          (not checkLocation || isLocationPermission)

popUpScreenFlow :: AllocationData -> FlowBT String Unit
popUpScreenFlow entityPayload = do
  let availableRide = (transformAllocationData entityPayload)
  modifyScreenState $ PopUpScreenStateType (\popUpScreen -> popUpScreen{ data { availableRides = availableRide } })
  action <- UI.popUpScreen
  case action of
    POPUP_REQUEST_RIDE id extraFare -> do
      if extraFare > 0.0 then do
        (API.ApiSuccessResult resp) <- Remote.offerRideBT (makeOfferRideReq id (Just extraFare))
        pure unit
        else do
          (API.ApiSuccessResult resp) <- Remote.offerRideBT (makeOfferRideReq id Nothing)
          pure unit
      homeScreenFlow
    _ -> homeScreenFlow

-- TODO :: use this case when on click of notification we want to go to alert section from app itself
-- alertNotification :: String -> FlowBT String Unit
-- alertNotification id = do
--   modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props{ selectedNotification = Just id } })
--   homeScreenFlow

changeDriverStatus :: DriverStatus -> FlowBT String Unit 
changeDriverStatus status = do
  globalState <- getState
  driverInfo@(GetDriverInfoResp getDriverInfoResp) <- getDriverInfoDataFromCache globalState false
  let API.DriverGoHomeInfo driverGoHomeInfo = getDriverInfoResp.driverGoHomeInfo
      qParam = toUpper $ show status
      isDriverActive = any ( _ == status) [Online, Silent]
  void $ Remote.driverActiveInactiveBT (show isDriverActive) qParam
  when (status == Offline) $ disableGoTo driverInfo -- disabling goto when driver is Offline
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {statusOnline = isDriverActive, driverStatusSet = status}})
  updateDriverStatusGlobal qParam isDriverActive
  void $ setValueToLocalStore DRIVER_STATUS if any ( _ == status) [Online, Silent] then "true" else "false"
  void $ setValueToLocalStore DRIVER_STATUS_N $ show status
  checkStatusAndStartLocationUpdates
  void $ setValueToLocalStore RIDE_T_FREQUENCY (if status == Online then "20000" else "30000")
  setValueToLocalStore DRIVER_MIN_DISPLACEMENT (if any ( _ == status) [Online, Silent] then "8.0" else "25.0")

getDriverInfoDataFromCache :: GlobalState -> Boolean -> FlowBT String GetDriverInfoResp
getDriverInfoDataFromCache (GlobalState globalState) mkCall = do
  if not mkCall && isJust globalState.globalProps.driverInformation then do 
    let driverInfoResp = fromMaybe dummyDriverInfo globalState.globalProps.driverInformation
    pure driverInfoResp
  else do
    driverInfoResp <- Remote.getDriverInfoBT ""
    modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{driverInformation = Just $ driverInfoResp}
    updateDriverDataToStates
    pure driverInfoResp

updateDriverStatusGlobal :: String -> Boolean -> FlowBT String Unit
updateDriverStatusGlobal mode active= do
  globalState <- getState
  (GetDriverInfoResp getDriverInfoResp) <- getDriverInfoDataFromCache globalState false
  let updatedResponse = getDriverInfoResp{mode = Just mode, active = active}
  modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{driverInformation = Just (GetDriverInfoResp updatedResponse)}

-- TODO :: currently this flow is not in use
-- driverRideRatingFlow :: FlowBT String Unit
-- driverRideRatingFlow = do
--   action <- UI.driverRideRatingScreen
--   case action of
--     SendCustomerFeedBack updatedState -> do
--       --TODO // API CALL for feedback
--       homeScreenFlow
--     CloseScreen -> homeScreenFlow

notificationFlow :: Maybe CTA.GlobalPayload -> FlowBT String Unit
notificationFlow mbPayload= do
  let (GlobalState defGlobalState) = defaultGlobalState
  case mbPayload of
    Nothing -> pure unit
    Just payload -> 
      case payload ^. _payload ^. _deepLinkJSON of
        Nothing -> pure unit
        Just (CTA.QueryParam queryParam) -> 
          case queryParam.messageId of 
            Nothing -> pure unit
            Just id -> do
              resp <- lift $ lift $ Remote.getMessageById id
              case resp of
                Left err -> do
                  pure $ toast $ "Message Not Found"
                Right resp -> modifyScreenState $ NotificationsScreenStateType (\notificationScreen -> notificationScreen{ notificationDetailModelState = notifisDetailStateTransformer $ notificationTransformer $ resp, notifsDetailModelVisibility = VISIBLE })
  screenAction <- UI.notifications
  case screenAction of
    REFRESH_SCREEN state -> do
      modifyScreenState $ NotificationsScreenStateType (\notificationScreen -> state{offsetValue = 0})
      notificationFlow Nothing
    LOAD_NOTIFICATIONS state -> do
      modifyScreenState $ NotificationsScreenStateType (\notificationScreen -> state{offsetValue = (length state.notificationList)})
      notificationFlow Nothing
    GO_HOME_SCREEN -> homeScreenFlow
    GO_REFERRAL_SCREEN -> referralFlow
    GO_EARNINGS_SCREEN -> driverEarningsFlow
    GO_RIDE_HISTORY_SCREEN -> myRidesScreenFlow
    GO_PROFILE_SCREEN -> driverProfileFlow
    CHECK_RIDE_FLOW_STATUS -> currentRideFlow Nothing Nothing
    NOTIFICATION_SCREEN_NAV GoToSubscription -> updateAvailableAppsAndGoToSubs
    NOTIFICATION_SCREEN_NAV _ -> notificationFlow Nothing

removeChatService :: String -> FlowBT String Unit
removeChatService _ = do
  void $ lift $ lift $ liftFlow $ stopChatListenerService
  void $ pure $ setValueToLocalNativeStore READ_MESSAGES "0"
  pure unit

setDriverStatusInLocal :: String -> String -> FlowBT String Unit
setDriverStatusInLocal status mode = do
  setValueToLocalStore DRIVER_STATUS status
  setValueToLocalNativeStore DRIVER_STATUS status
  setValueToLocalStore DRIVER_STATUS_N mode
  setValueToLocalNativeStore DRIVER_STATUS_N mode

updateDriverDataToStates :: FlowBT String Unit
updateDriverDataToStates = do
  appConfig <- getAppConfigFlowBT Constants.appConfig
  (GlobalState globalstate) <- getState  
  case globalstate.globalProps.driverRideStats of
    Just (DriverProfileStatsResp driverStats) -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen 
        { data { 
          totalRidesOfDay = driverStats.totalRidesOfDay, 
          totalEarningsOfDay = driverStats.totalEarningsOfDay, 
          coinBalance = driverStats.coinBalance, 
          bonusEarned = driverStats.bonusEarning, 
          earningPerKm = driverStats.totalEarningsOfDayPerKm,
          totalValidRidesOfDay = fromMaybe 0 driverStats.totalValidRidesOfDay,
          driverStats = true }})
      void $ pure $ setCleverTapUserProp [{key : "Driver Coin Balance", value : unsafeToForeign driverStats.coinBalance }]
    Nothing -> pure unit
  (GetDriverInfoResp getDriverInfoResp) <- getDriverInfoDataFromCache (GlobalState globalstate) false
  let (API.DriverGoHomeInfo driverGoHomeInfo) = getDriverInfoResp.driverGoHomeInfo
      (Vehicle linkedVehicle) = (fromMaybe dummyVehicleObject getDriverInfoResp.linkedVehicle)
      showGender = not (isJust (getGenderValue getDriverInfoResp.gender))
      dbClientVersion = getDriverInfoResp.clientVersion
      dbBundleVersion = getDriverInfoResp.bundleVersion
  modifyScreenState $ BookingOptionsScreenType $ \bookingOptionsScreen -> bookingOptionsScreen { data{ vehicleNumber = linkedVehicle.registrationNo }}
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data {driverName = getDriverInfoResp.firstName
        , vehicleType = linkedVehicle.variant
        , driverAlternateMobile =getDriverInfoResp.alternateNumber
        , profileImg = getDriverInfoResp.aadhaarCardPhoto
        , linkedVehicleCategory = fromMaybe linkedVehicle.variant linkedVehicle.serviceTierType
        , linkedVehicleVariant = linkedVehicle.variant
        , gender = fromMaybe "UNKNOWN" getDriverInfoResp.gender
        , payoutVpa = getDriverInfoResp.payoutVpa
        , payoutVpaStatus = getDriverInfoResp.payoutVpaStatus
        , isPayoutEnabled = getDriverInfoResp.isPayoutEnabled
        , payoutRewardAmount = getDriverInfoResp.payoutRewardAmount
        , payoutVpaBankAccount = getDriverInfoResp.payoutVpaBankAccount
        , driverGotoState { gotoCount = driverGoHomeInfo.cnt,
                            gotoValidTill = fromMaybe "-" driverGoHomeInfo.validTill,
                            isGotoEnabled = driverGoHomeInfo.status == Just "ACTIVE",
                            gotoEnabledForMerchant = getDriverInfoResp.isGoHomeEnabled,
                            goToPopUpType = case globalstate.globalProps.gotoPopupType of
                                            ST.REDUCED _ -> ST.REDUCED driverGoHomeInfo.cnt 
                                            _ -> globalstate.globalProps.gotoPopupType }
        , isSpecialLocWarrior = fromMaybe false getDriverInfoResp.isSpecialLocWarrior
        }, 
                                            
    props {
      statusOnline = if (isJust getDriverInfoResp.mode) then any ( _ == updateDriverStatus getDriverInfoResp.active) [Online, Silent] else getDriverInfoResp.active
    , driverStatusSet = case getDriverInfoResp.mode of
                          Just mode -> getDriverStatusFromMode mode
                          Nothing -> getDriverStatus ""
    , showGenderBanner = showGender
    , checkUpcomingRide = true
    }})
  setValueToLocalStore DRIVER_SUBSCRIBED $ show $ isJust getDriverInfoResp.autoPayStatus
  setValueToLocalStore VEHICLE_VARIANT linkedVehicle.variant
  setValueToLocalStore NEGOTIATION_UNIT $ getNegotiationUnit linkedVehicle.variant appConfig.rideRequest.negotiationUnit
  setValueToLocalStore USER_NAME getDriverInfoResp.firstName
  setValueToLocalStore REFERRAL_CODE (fromMaybe "" getDriverInfoResp.referralCode)
  setValueToLocalStore FREE_TRIAL_DAYS (show (fromMaybe 0 getDriverInfoResp.freeTrialDaysLeft))
  setValueToLocalStore IS_ON_FREE_TRIAL $ maybe "Nothing" show getDriverInfoResp.isOnFreeTrial
  modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen { data {  driverName = getDriverInfoResp.firstName
    , driverVehicleType = linkedVehicle.variant
    , driverRating = getDriverInfoResp.rating
    , driverAlternateNumber = getDriverInfoResp.alternateNumber
    , driverGender = getGenderState getDriverInfoResp.gender
    , capacity = fromMaybe 2 linkedVehicle.capacity
    , downgradeOptions = getDowngradeOptions linkedVehicle.variant
    , vehicleSelected = getDowngradeOptionsSelected (GetDriverInfoResp getDriverInfoResp)
    , profileImg = getDriverInfoResp.aadhaarCardPhoto},props {canSwitchToRental =  (getDriverInfoResp.canSwitchToRental),canSwitchToInterCity = getDriverInfoResp.canSwitchToInterCity, canSwitchToIntraCity = getDriverInfoResp.canSwitchToIntraCity}})
  modifyScreenState $ ReferralScreenStateType (\ referralScreen -> referralScreen{ data { driverInfo  
    {  driverName = getDriverInfoResp.firstName
    , driverMobile = getDriverInfoResp.mobileNumber
    , vehicleVariant = linkedVehicle.variant
    , vehicleRegNumber = linkedVehicle.registrationNo
    , referralCode = getDriverInfoResp.referralCode }}})
  if (isJust getDriverInfoResp.numberOfRides) then do
    setValueToLocalStore HAS_TAKEN_FIRST_RIDE $ show $ fromMaybe 0 getDriverInfoResp.numberOfRides == 0
    else setValueToLocalStore HAS_TAKEN_FIRST_RIDE "false"
  void $ checkAndUpdateRCStatus
  updateDriverVersion dbClientVersion dbBundleVersion

updateInitialCleverTapUserProps :: GetDriverInfoResp -> Effect Unit 
updateInitialCleverTapUserProps  (GetDriverInfoResp getDriverInfoResp) = do
  let middleName = case getDriverInfoResp.middleName of
                    Just ""  -> ""
                    Just name -> " " <> name
                    Nothing -> ""
      lastName   = case getDriverInfoResp.lastName of
                    Just "" -> ""
                    Just name -> " " <> name
                    Nothing -> ""
      name = getDriverInfoResp.firstName <> middleName <> lastName
  void $ pure $ setCleverTapUserProp [{key :"Name" , value :unsafeToForeign name}]
  void $ pure $ getDriverInfoResp.mobileNumber >>= \mobileNumber -> void $ pure $ setCleverTapUserProp [{key : "Mobile_Number", value : unsafeToForeign $ "+91" <> mobileNumber}]
  void $ pure $ getDriverInfoResp.referralCode >>= \referralCode -> void $ pure $ setCleverTapUserProp [{key : "Referral Code", value : unsafeToForeign referralCode}] 


updateCleverTapUserProps :: GetDriverInfoResp -> Effect Unit
updateCleverTapUserProps (GetDriverInfoResp getDriverInfoResp)= do
  void $ pure $ getDriverInfoResp.freeTrialDaysLeft >>= \freeTrialDaysLeft ->void $ pure $ setCleverTapUserProp [{key : "Ny_Free_Trial_Days_Left", value : unsafeToForeign freeTrialDaysLeft}]
  void $ pure $ getDriverInfoResp.numberOfRides >>= \numberOfRides -> void $ pure $ setCleverTapUserProp [{key : "total_driver_trips", value : unsafeToForeign numberOfRides }]
  void $ pure $ getDriverInfoResp.rating >>= \rating -> void $ pure $ setCleverTapUserProp [{key : "Driver_rating", value : unsafeToForeign rating }]
  void $ pure $ getDriverInfoResp.currentDues >>= \currentDues -> void $ pure $ setCleverTapUserProp [{key : "Current Dues", value :unsafeToForeign currentDues}]
  void $ pure $ getDriverInfoResp.manualDues >>= \manualDues -> void $ pure $ setCleverTapUserProp [{key : "Manual Dues", value : unsafeToForeign manualDues}]
  void $ pure $ getDriverInfoResp.linkedVehicle >>= \ (Vehicle linkedVehicle) -> void $ pure $ setCleverTapUserProp [{key :  "Vehicle Variant", value : unsafeToForeign linkedVehicle.variant}]
  void $ pure $ setCleverTapUserProp [
    {key : "Blocked", value : unsafeToForeign $ fromMaybe false getDriverInfoResp.blocked},
    {key : "Mode", value : unsafeToForeign $ fromMaybe "" getDriverInfoResp.mode},
    {key : "First ride taken", value : unsafeToForeign $ fromMaybe 0 getDriverInfoResp.numberOfRides > 0 },
    {key : "Plan Subscription Status", value : unsafeToForeign $ isJust getDriverInfoResp.autoPayStatus },
    {key : "Subscribed", value : unsafeToForeign $ getDriverInfoResp.subscribed},
    {key : "Enabled", value : unsafeToForeign $ getDriverInfoResp.enabled},
    {key : "to_be_blocked", value : unsafeToForeign $ fromMaybe 0.0 getDriverInfoResp.currentDues >= 75.0},
    {key : "Driver Location", value : unsafeToForeign $ getValueToLocalStore DRIVER_LOCATION}
  ]



updateAvailableAppsAndGoToSubs :: FlowBT String Unit
updateAvailableAppsAndGoToSubs = do
  (GlobalState state) <- getState
  let isEndRideScreen = state.homeScreen.props.currentStage == ST.RideCompleted
  modifyScreenState $ SubscriptionScreenStateType (\subscriptionScreen -> subscriptionScreen{props{subView = NoSubView, showShimmer = true, isEndRideModal = isEndRideScreen}})
  void $ liftFlowBT $ launchAff $ flowRunner (GlobalState state) $ void $ runExceptT $ runBackT $ getUpiApps
  subScriptionFlow

getUpiApps :: FlowBT String Unit
getUpiApps = do
  resp <- lift $ lift $ doAff $ makeAff (\cb -> (runEffectFn1 getAvailableUpiApps (cb <<< Right) ) $> nonCanceler)
  let req = Remote.mkUpdateDriverInfoReq ""
  let appsSupportMandate = runFn1 stringifyJSON $ map _.appName $ filter (_.supportsMandate) resp
  let appsNotSupportMandate = runFn1 stringifyJSON $ map _.appName $ filter (not _.supportsMandate) resp
  pure $ setCleverTapUserProp [{key : "appsSupportMandate", value : unsafeToForeign appsSupportMandate},
                               {key : "appsNotSupportMandate", value : unsafeToForeign appsNotSupportMandate}]
  void $ Remote.updateDriverInfoBT (UpdateDriverInfoReq req{availableUpiApps = Just $ runFn1 stringifyJSON resp})

checkDriverBlockingStatus :: GetDriverInfoResp -> Boolean -> FlowBT String Unit
checkDriverBlockingStatus (GetDriverInfoResp getDriverInfoResp) enableJoinPlanBlocking =
  if (joinPlanBlockerConditions && enableJoinPlanBlocking) then do
    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data{ paymentState {driverBlocked = true, subscribed = getDriverInfoResp.subscribed, showShimmer = not getDriverInfoResp.subscribed }}})
    mkDriverOffline
  else if driverCityOrVehicleChanged then do
    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data{ plansState { cityOrVehicleChanged = true}}})
    mkDriverOffline
  else 
    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data {plansState {cityOrVehicleChanged = false}, paymentState {driverBlocked = false }}})
    where 
      notOnRide = not getDriverInfoResp.onRide
      onlineMode = any ( _ == getDriverInfoResp.mode) [Just "ONLINE", Just "SILENT"]
      joinPlanBlockerConditions = (isNothing getDriverInfoResp.autoPayStatus && not isOnFreeTrial FunctionCall && getValueToLocalStore SHOW_SUBSCRIPTIONS == "true") || not getDriverInfoResp.subscribed
      justTrue flag = flag == Just true
      driverCityOrVehicleChanged = isJust getDriverInfoResp.autoPayStatus && justTrue getDriverInfoResp.isSubscriptionEnabledAtCategoryLevel && justTrue getDriverInfoResp.subscriptionEnabledForVehicleCategory && (justTrue getDriverInfoResp.isSubscriptionVehicleCategoryChanged || justTrue getDriverInfoResp.isSubscriptionCityChanged)
      mkDriverOffline =
        when (notOnRide && onlineMode) do
          changeDriverStatus Offline
          homeScreenFlow


updateBannerAndPopupFlags :: FlowBT String Unit
updateBannerAndPopupFlags = do
  (GlobalState allState) <- getState
  (GetDriverInfoResp getDriverInfoResp) <- getDriverInfoDataFromCache (GlobalState allState) false
  appConfig <- getAppConfigFlowBT Constants.appConfig
  rideAndEarnPopup <- callGetPastDaysData appConfig allState.homeScreen
  let
    coinsConfig = getCoinsConfigData $ DS.toLower driverCity
    cityConfig = getCityConfig appConfig.cityConfig driverCity
    driverCity = getValueToLocalStore DRIVER_LOCATION
    driverVehicle = getValueToLocalStore VEHICLE_VARIANT
    vehicleAndCityConfig = CommonRC.subscriptionsConfigVariantLevel driverCity driverVehicle
    freeTrialRidesLeft = fromMaybe 0 $ CA.lift2 (-) getDriverInfoResp.freeTrialRides getDriverInfoResp.totalRidesTaken
    freeTrialPopupDaysList = fromMaybe [] vehicleAndCityConfig.freeTrialPopupDaysList
    freeTrialPopupOnRidesList = fromMaybe [] vehicleAndCityConfig.freeTrialPopupOnRidesList
    autoPayNotActive = isNothing getDriverInfoResp.autoPayStatus || getDriverInfoResp.autoPayStatus /= Just "ACTIVE"
    pendingTotalManualDues = fromMaybe 0.0 getDriverInfoResp.manualDues
    subscriptionConfig = appConfig.subscriptionConfig
    _ = runFn2 EHC.updatePushInIdMap "bannerCarousel" true
    subscriptionRemoteConfig = fromMaybe CommonRC.defSubscriptionDues vehicleAndCityConfig.duesConfig
    freeTrialDays = fromMaybe 0 getDriverInfoResp.freeTrialDaysLeft
    shouldShowPopup = getValueToLocalStore APP_SESSION_TRACK_COUNT == "true" 
                      && getValueToLocalNativeStore IS_RIDE_ACTIVE == "false" 
                      && (isOnFreeTrial FunctionCall || (pendingTotalManualDues /= 0.0)) 
                      && getDriverInfoResp.subscribed 
                      && appConfig.subscriptionConfig.enableSubscriptionPopups
                      && getValueToLocalStore SHOW_SUBSCRIPTIONS == "true"
    autoPayStatus = getAutopayStatus getDriverInfoResp.autoPayStatus
    isDriverBlocked = fromMaybe false getDriverInfoResp.blocked
    driverBlockedExpiryTime = fromMaybe "" getDriverInfoResp.blockExpiryTime
    shouldDriverbeOffline = (isDriverBlocked && (not $ DS.null driverBlockedExpiryTime) && (runFn2 JB.differenceBetweenTwoUTC (driverBlockedExpiryTime) (EHC.getCurrentUTC "") > 0)) || getValueToLocalStore DRIVER_STATUS_N == "Offline"
    status = getDriverStatus ""
    autopayBannerType =
      if subscriptionConfig.enableSubscriptionPopups && getValueToLocalStore SHOW_SUBSCRIPTIONS == "true" then 
        case autoPayNotActive, isOnFreeTrial FunctionCall, (pendingTotalManualDues /= 0.0) of
          true, true, _ -> FREE_TRIAL_BANNER
          _, false, true -> do
            if pendingTotalManualDues < subscriptionRemoteConfig.low_dues_warning_limit then LOW_DUES_BANNER
            else if pendingTotalManualDues >= subscriptionRemoteConfig.low_dues_warning_limit && pendingTotalManualDues < subscriptionRemoteConfig.high_due_warning_limit then CLEAR_DUES_BANNER
            else if pendingTotalManualDues >= subscriptionRemoteConfig.high_due_warning_limit && pendingTotalManualDues < subscriptionRemoteConfig.max_dues_limit then DUE_LIMIT_WARNING_BANNER
            else NO_SUBSCRIPTION_BANNER
          true, _, _ -> if isNothing getDriverInfoResp.autoPayStatus then NO_SUBSCRIPTION_BANNER else SETUP_AUTOPAY_BANNER
          _, _, _ -> NO_SUBSCRIPTION_BANNER
      else NO_SUBSCRIPTION_BANNER
    showFreeTrialPopupOnDays = any (_ == freeTrialDays) freeTrialPopupDaysList
    showFreeTrialPopupOnRides = any (_ == freeTrialRidesLeft) freeTrialPopupOnRidesList
    subscriptionPopupType = 
      case isOnFreeTrial FunctionCall, autoPayNotActive, shouldShowPopup of
        true, true , true | showFreeTrialPopupOnDays -> FREE_TRIAL_POPUP
        true, true , true | showFreeTrialPopupOnRides -> FREE_TRIAL_RIDES_POPUP
        false, _, true -> if pendingTotalManualDues >= subscriptionRemoteConfig.max_dues_limit then NO_SUBSCRIPTION_POPUP else LOW_DUES_CLEAR_POPUP
        _, _, _ -> NO_SUBSCRIPTION_POPUP

    shouldMoveDriverOffline = (withinTimeRange "12:00:00" "23:59:59" (convertUTCtoISC (getCurrentUTC "") "HH:mm:ss"))

    moveDriverToOffline =
      (getValueToLocalStore MOVED_TO_OFFLINE_DUE_TO_HIGH_DUE == "")
        && shouldMoveDriverOffline
        && appConfig.subscriptionConfig.moveDriverToOfflineInHighDueDaily
        && getValueToLocalNativeStore IS_RIDE_ACTIVE == "false"
        && pendingTotalManualDues >= subscriptionRemoteConfig.high_due_warning_limit
        && getDriverInfoResp.mode /= Just "OFFLINE"
    
    isCoinPopupNotShownToday = (isYesterday (getValueToLocalStore COINS_POPUP_SHOWN_DATE)) || (getValueToLocalStore COINS_POPUP_SHOWN_DATE == "__failed")
    showCoinPopup = fromMaybe 0 (fromString $ getValueToLocalStore VISITED_DRIVER_COINS_PAGE) == 0
        && appConfig.feature.enableYatriCoins
        && cityConfig.enableYatriCoins
        && toBool (getValueToLocalNativeStore IS_RIDE_ACTIVE)  == false 
        && fromMaybe 0 (fromString $ getValueToLocalStore INTRODUCING_YATRI_POINTS_POPUP_LIMIT) < coinsConfig.rideMoreEarnCoinPopupMaxLimit
        && isCoinPopupNotShownToday

    hsState = allState.homeScreen
    coinPopupType_ = case allState.homeScreen.data.totalValidRidesOfDay of
                      10 -> checkPopupShowToday ST.TEN_RIDE_COMPLETED appConfig hsState
                      -- 8 -> checkPopupShowToday ST.EIGHT_RIDE_COMPLETED appConfig hsState
                      -- 7 -> checkPopupShowToday ST.ONE_MORE_RIDE appConfig hsState  these will  be enabled after monsoon offer of coin
                      -- 6 -> checkPopupShowToday ST.TWO_MORE_RIDES appConfig hsState
                      -- 5 -> checkPopupShowToday ST.FIVE_RIDE_COMPLETED appConfig hsState
                      6 -> checkPopupShowToday ST.SIX_RIDE_COMPLETED appConfig hsState
                      -- 2 -> checkPopupShowToday ST.TWO_RIDE_COMPLETED appConfig hsState
                      _ -> rideAndEarnPopup

    coinPopupType__ = if (coinPopupType_ == ST.NO_COIN_POPUP) then (checkPopupShowToday ST.CONVERT_COINS_TO_CASH appConfig hsState) else coinPopupType_

    coinPopupType = if (coinPopupType__ == ST.NO_COIN_POPUP) then (checkPopupShowToday ST.REFER_AND_EARN_COIN appConfig hsState) else coinPopupType__

    isPayoutEnabled = fromMaybe false getDriverInfoResp.isPayoutEnabled

    payoutVpa = getDriverInfoResp.payoutVpa
    
    payoutVpaStatus = getDriverInfoResp.payoutVpaStatus

    showReferNowPopUp = isPayoutEnabled && showReferralPopUp REFER_NOW_LAST_SHOWN ST.ReferNow && isJust payoutVpa && case payoutVpaStatus of 
                                                                                                            Just status -> status == VIA_WEBHOOK || status == VERIFIED_BY_USER
                                                                                                            Nothing -> false
  
    showAddUPIPopUp = isPayoutEnabled && showReferralPopUp ADD_UPI_LAST_SHOWN ST.AddUPI && isNothing payoutVpa

    showVerifyUPIPopUp = isPayoutEnabled && showReferralPopUp VERIFY_UPI_LAST_SHOWN ST.VerifyUPI && isJust payoutVpa && case payoutVpaStatus of 
                                                                                                              Just status -> status == MANUALLY_ADDED
                                                                                                              Nothing -> false
  when shouldDriverbeOffline $ changeDriverStatus Offline
  when moveDriverToOffline $ do
      setValueToLocalStore MOVED_TO_OFFLINE_DUE_TO_HIGH_DUE (getCurrentUTC "")
      changeDriverStatus Offline
  setValueToLocalStore DISABLE_WIDGET if status == Offline then "true" else "false"
  modifyScreenState
    $ HomeScreenStateType
        ( \homeScreen ->
            homeScreen
              { data
                { paymentState
                  { totalPendingManualDues = pendingTotalManualDues
                  , autoPayStatus = autoPayStatus
                  , showShimmer = false
                  , subscribed = getDriverInfoResp.subscribed
                  },
                  driverGotoState 
                  { savedLocationCount = fromMaybe 0 $ fromString $ getValueToLocalStore SAVED_GOTO_COUNT
                  }
                , config = appConfig
                , isVehicleSupported = fromMaybe true getDriverInfoResp.isVehicleSupported
                , cityConfig = cityConfig
                , cancellationRate = fromMaybe 0 getDriverInfoResp.cancellationRateInWindow
                , subsRemoteConfig = subscriptionRemoteConfig
                , plansState {
                    freeTrialRides = getDriverInfoResp.freeTrialRides,
                    totalRidesTaken = getDriverInfoResp.totalRidesTaken
                }
                }
              , props
                { autoPayBanner = autopayBannerType
                , subscriptionPopupType = subscriptionPopupType
                , waitTimeStatus = RC.waitTimeConstructor $ getValueToLocalStore WAITING_TIME_STATUS
                , showCoinsPopup = showCoinPopup
                , coinPopupType = coinPopupType
                , showReferNowPopUp = showReferNowPopUp
                , showAddUPIPopUp = showAddUPIPopUp
                , showVerifyUPIPopUp = showVerifyUPIPopUp
                , showAcWorkingPopup = if isNothing allState.homeScreen.props.showAcWorkingPopup
                                          then getDriverInfoResp.checkIfACWorking
                                       else allState.homeScreen.props.showAcWorkingPopup
                }
              }
        )
  where 
    showReferralPopUp storeKey popUpType = runFn2 HU.isMoreThanXMs (getValueToLocalStore storeKey) (RC.getReferralPopUpDelays popUpType)

callGetPastDaysData :: AppConfig -> HomeScreenState -> FlowBT String ST.CoinEarnedPopupType
callGetPastDaysData appConfig hsState = do
  let cityConfig = getCityConfig appConfig.cityConfig (getValueToLocalStore DRIVER_LOCATION)
      coinsConfig = getCoinsConfigData $ DS.toLower $ getValueToLocalStore DRIVER_LOCATION
      coinPopupInfo = getValueFromCache "COIN_EARNED_POPUP_TYPE" getCoinPopupStatus
      checkCoinIsEnabled = appConfig.feature.enableYatriCoins && cityConfig.enableYatriCoins
      vehicleVariant = hsState.data.vehicleType 
      isAutoRicksaw = RC.getCategoryFromVariant vehicleVariant == Just ST.AutoCategory
  if ((isPopupShownToday coinPopupInfo.rideMoreEarnCoin && (STR.null coinPopupInfo.rideMoreEarnCoin || (runFn2 isDateNDaysAgo coinPopupInfo.rideMoreEarnCoin coinsConfig.rideMoreEarnCoinIntervalLimit))) && checkCoinIsEnabled && isAutoRicksaw && withinTimeRange RC.rideMoreEarnMorePopupStartTime RC.dayEndTime (convertUTCtoISC (getCurrentUTC "") "HH:mm:ss")) then do
      resp <- lift $ lift $ Remote.getRideStatusPastDays ""
      case resp of
        Right (API.RideStatusPastDaysRes response) ->
          if (response.rideCountPopupValue == false) 
            then pure ST.RIDE_MORE_EARN_COIN 
            else pure ST.NO_COIN_POPUP
        Left err -> pure ST.NO_COIN_POPUP
  else pure ST.NO_COIN_POPUP

logoutFlow :: FlowBT String Unit
logoutFlow = do
  logField_ <- lift $ lift $ getLogFields
  liftFlowBT $ logEvent logField_ "ny_driver_logout"
  (API.ApiSuccessResult resp) <- Remote.logOutBT LogOutReq
  removeChatService ""
  lift $ lift $ liftFlow $ stopLocationPollingAPI
  deleteValueFromLocalStore REGISTERATION_TOKEN
  deleteValueFromLocalStore VERSION_NAME
  deleteValueFromLocalStore TEST_FLOW_FOR_REGISTRATOION
  deleteValueFromLocalStore IS_DRIVER_ENABLED
  deleteValueFromLocalStore DRIVER_STATUS
  deleteValueFromLocalStore BUNDLE_VERSION
  deleteValueFromLocalStore DRIVER_ID
  deleteValueFromLocalStore SET_ALTERNATE_TIME
  deleteValueFromLocalStore ONBOARDING_SUBSCRIPTION_SCREEN_COUNT
  deleteValueFromLocalStore FREE_TRIAL_DAYS
  deleteValueFromLocalStore REFERRAL_CODE_ADDED
  deleteValueFromLocalStore VEHICLE_CATEGORY
  deleteValueFromLocalStore ENTERED_RC
  deleteValueFromLocalStore GULLAK_TOKEN
  pure $ factoryResetApp ""
  void $ lift $ lift $ liftFlow $ logEvent logField_ "logout"
  isLocationPermission <- lift $ lift $ liftFlow $ isLocationPermissionEnabled unit
  void $ pure $ JB.stopService "in.juspay.mobility.app.GRPCNotificationService"
  if getValueToLocalStore DRIVER_LOCATION == "__failed" || getValueToLocalStore DRIVER_LOCATION == "--" || not isLocationPermission  then do
    chooseCityFlow Nothing
  else authenticationFlow "" Nothing

chooseCityFlow :: Maybe Event -> FlowBT String Unit
chooseCityFlow mbEvent = do
  liftFlowBT $ markPerformance "CHOOSE_CITY_FLOW"
  hideSplashAndCallFlow $ pure unit
  (GlobalState globalstate) <- getState
  logField_ <- lift $ lift $ getLogFields
  chooseCityScreen <- UI.chooseCityScreen
  case chooseCityScreen of
    GoToWelcomeScreen -> authenticationFlow "" mbEvent
    REFRESH_SCREEN_CHOOSE_CITY _ -> chooseCityFlow mbEvent
    DETECT_CITY lat lon state -> do
      if state.data.config.chooseCity.straightLineDistLogic then straightLineDist lat lon state else detectCityAPI lat lon state
      chooseCityFlow mbEvent
  where 
    closestCity :: (Tuple String Number) -> CityConfig -> Number -> Number -> (Tuple String Number)
    closestCity (Tuple cityName distance) city driverLat driverLon = do
      let distanceFromCity = getDistanceBwCordinates driverLat driverLon city.cityLat city.cityLong
      if distanceFromCity < distance then (Tuple city.cityName distanceFromCity) else (Tuple cityName distance)
    
    straightLineDist :: Number -> Number -> ST.ChooseCityScreenState -> FlowBT String Unit
    straightLineDist lat lon state = do
      let distanceFromBangalore = getDistanceBwCordinates lat lon 12.9716 77.5946
          initialAccumulator = Tuple "Bangalore" distanceFromBangalore
          result = DA.foldl (\acc city -> closestCity acc city lat lon) initialAccumulator state.data.config.cityConfig
          insideThreshold = (snd result) <= state.data.config.unserviceableThreshold
      if insideThreshold && (not state.props.isMockLocation) then
        modifyScreenState $ ChooseCityScreenStateType \chooseCityScreenState -> chooseCityScreenState { data { locationSelected = Just $ fst result }, props { locationUnserviceable = false, locationDetectionFailed = false }}
      else
        modifyScreenState $ ChooseCityScreenStateType \chooseCityScreenState -> chooseCityScreenState { props { locationUnserviceable = true }}
    
    detectCityAPI :: Number -> Number -> ST.ChooseCityScreenState -> FlowBT String Unit
    detectCityAPI lat lon state = do
      resp <- lift $ lift $ Remote.detectCity lat lon $ if (SC.getMerchantId "") == "NA" then getValueToLocalNativeStore MERCHANT_ID else (SC.getMerchantId "" )
      case resp of
        Right (API.DetectCityResp resp') -> do
          let unserviceableState = ChooseCityScreenStateType \chooseCityScreenState -> chooseCityScreenState { props { locationUnserviceable = true },  data { locationSelected = Nothing }}
              compareStrings = on (==) (toLower <<< trim)
          case resp'.city of
            Just city -> do
              let cityInList = any (\cityOb -> compareStrings cityOb.cityName city) state.data.config.cityConfig
                  displayCityName = case city of 
                                      "TamilNaduCities" -> Just "Tamil Nadu"
                                      _ -> Just city
                  locationServiceableState = ChooseCityScreenStateType \chooseCityScreenState -> chooseCityScreenState { data { locationSelected = displayCityName }, props { locationUnserviceable = false, locationDetectionFailed = false }}
              modifyScreenState if cityInList then locationServiceableState else unserviceableState
            Nothing -> modifyScreenState unserviceableState
        Left _ -> do
          liftFlowBT $ firebaseLogEvent "ny_driver_detect_city_fallback"
          straightLineDist lat lon state

welcomeScreenFlow :: Maybe Event -> FlowBT String Unit
welcomeScreenFlow mbEvent = do
  liftFlowBT $ markPerformance "WELCOME_SCREEN_FLOW"
  hideSplashAndCallFlow $ pure unit
  logField_ <- lift $ lift $ getLogFields
  welcomeScreen <- UI.welcomeScreen
  case welcomeScreen of
    GoToMobileNumberScreen -> loginFlow mbEvent

benefitsScreenFlow :: FlowBT String Unit
benefitsScreenFlow = do
  logField_ <- lift $ lift $ getLogFields
  void $ lift $ lift $ liftFlow $ logEvent logField_ "benefitsScreenFlow"
  appConfig <- getAppConfigFlowBT Constants.appConfig
  (GlobalState globalstate) <- getState
  (GetDriverInfoResp getDriverInfoResp) <- getDriverInfoDataFromCache (GlobalState globalstate) false
  let referralCode = getValueToLocalStore REFERRAL_CODE
      cityConfig = getCityConfig appConfig.cityConfig (getValueToLocalStore DRIVER_LOCATION)
  modifyScreenState $ BenefitsScreenStateType (\benefitsScreen -> benefitsScreen { data {referralCode = referralCode, cityConfig = cityConfig, payoutRewardAmount = getDriverInfoResp.payoutRewardAmount}})
  benefitsScreen <- UI.benefitsScreen
  case benefitsScreen of
    DRIVER_REFERRAL_SCREEN_NAV GoToSubscription -> updateAvailableAppsAndGoToSubs
    DRIVER_REFERRAL_SCREEN_NAV HomeScreenNav -> homeScreenFlow
    DRIVER_REFERRAL_SCREEN_NAV GoToRideHistory -> myRidesScreenFlow
    DRIVER_REFERRAL_SCREEN_NAV GoToAlerts -> notificationFlow Nothing
    DRIVER_REFERRAL_SCREEN_NAV (GoToEarningsScreen _) -> driverEarningsFlow
    DRIVER_REFERRAL_SCREEN_NAV _ -> benefitsScreenFlow
    DRIVER_CONTEST_SCREEN -> referralScreenFlow
    GO_TO_LMS_VIDEO_SCREEN state -> do
      let cachedSelectedLanguage = (getValueToLocalStore LMS_SELECTED_LANGUAGE_CACHE)
          cityConfig = (getCityConfig state.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION))
          selectedLanguage = if (cachedSelectedLanguage == "__failed" || cachedSelectedLanguage == "null" || cachedSelectedLanguage == "") 
                              then if (DS.null cityConfig.languageKey) then (getLanguageLocale languageKey) else cityConfig.languageKey
                              else cachedSelectedLanguage
      modifyScreenState $ LmsVideoScreenStateType (\lmsVideoScreen -> lmsVideoScreen { props {selectedLanguage = selectedLanguage, selectedModule = state.props.selectedModule}})
      lmsVideoScreenFlow
    CUSTOMER_REFERRAL_TRACKER_NAV openPP -> do 
      modifyScreenState $ CustomerReferralTrackerScreenStateType (\customerReferralTrackerScreenState -> customerReferralTrackerScreenState{props {openPP = openPP}})
      customerReferralTrackerFlow

customerReferralTrackerFlow :: FlowBT String Unit 
customerReferralTrackerFlow = do 
  liftFlowBT $ runEffectFn1 initiatePP unit
  (GlobalState globalState) <- getState
  appConfig <- getAppConfigFlowBT Constants.appConfig
  logField_ <- lift $ lift $ getLogFields
  let customerReferralTrackerScreenState = globalState.customerReferralTrackerScreen
  modifyScreenState $ CustomerReferralTrackerScreenStateType (\customerReferralTracker -> customerReferralTracker{data{config = appConfig}})
  void $ pure $ toggleBtnLoader "" false
  void $ lift $ lift $ toggleLoader false
  customerReferralTrackerScreenAction <- UI.customerReferralTrackerScreen
  case customerReferralTrackerScreenAction of
    ADD_UPI_FLOW state -> addUPIFlow state
    DELETE_UPI_FLOW state -> do
      response <- lift $ lift $ Remote.deleteVPA $ fromMaybe "" state.data.upiID
      case response of 
        Right val -> modifyScreenState $ CustomerReferralTrackerScreenStateType (\customerReferralTracker -> customerReferralTracker{data{upiID = Nothing, currentStage = CRST.Tracker}, props{showDeleteUPIView = false, showUPIOptions = false}})
        Left (errorPayload) -> do 
          modifyScreenState $ CustomerReferralTrackerScreenStateType (\customerReferralTracker -> customerReferralTracker{props{showDeleteUPIView = false, showUPIOptions = false}})
          pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
      customerReferralTrackerFlow
    REFRESH_ORDER_STATUS state -> do 
      case state.data.orderId of  
        Just orderId -> do 
          void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT)
          void $ lift $ lift $ toggleLoader true
          checkPaymentStatus orderId
        Nothing -> pure unit 
      customerReferralTrackerFlow
    HOME_SCREEN_FROM_REFERRAL_TRACKER -> handleDeepLinksFlow Nothing Nothing Nothing

addUPIFlow :: CRST.CustomerReferralTrackerScreenState -> FlowBT String Unit 
addUPIFlow state = do 
  void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT)
  void $ lift $ lift $ toggleLoader true
  liftFlowBT $ initiatePaymentPage
  response <- lift $ lift $ Remote.payoutRegistration "dummy"
  case response of
    Right (API.PayoutRegisterRes payoutRegisterRes) -> do
      let (CreateOrderRes createOrderRes) = payoutRegisterRes.orderResp
      modifyScreenState $ CustomerReferralTrackerScreenStateType (\customerReferralTracker -> customerReferralTracker{data{orderId = Just payoutRegisterRes.orderId}})
      lift $ lift $ doAff $ makeAff \cb -> runEffectFn1 checkPPInitiateStatus (cb <<< Right) $> nonCanceler
      let sdkPayload = maybe (encodeJSON createOrderRes.sdk_payload) (addLanguageToPayload (getPaymentPageLangKey (getLanguageLocale languageKey))) createOrderRes.sdk_payload_json
      void $ paymentPageUI sdkPayload
      checkPaymentStatus payoutRegisterRes.orderId
    Left (errorPayload) -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
  customerReferralTrackerFlow

checkPaymentStatus :: String -> FlowBT String Unit
checkPaymentStatus orderId = do
  orderStatus <- lift $ lift $ Remote.paymentOrderStatus orderId
  case orderStatus of
    Right (OrderStatusRes statusResp) -> do
      let orderStatus = getOrderStatus statusResp.status
      setUPIPaymentStatus orderStatus
    Left err -> setUPIPaymentStatus CRST.PENDING

setUPIPaymentStatus :: CRST.OrderStatus -> FlowBT String Unit
setUPIPaymentStatus status = do
  (GlobalState globalState) <- getState
  case status of
    CRST.CHARGED -> modifyScreenState $ CustomerReferralTrackerScreenStateType (\customerReferralTracker -> customerReferralTracker{data{orderStatus = Just status}, props{showInfoPopUp = true, callEarningsAPI = true, showShimmer = true}})
    CRST.FAILED -> modifyScreenState $ CustomerReferralTrackerScreenStateType (\customerReferralTracker -> customerReferralTracker{data{orderStatus = Just status}, props{showInfoPopUp = true}})
    CRST.PENDING -> modifyScreenState $ CustomerReferralTrackerScreenStateType (\customerReferralTracker -> customerReferralTracker{data{orderStatus = Just status}})
    _ -> pure unit

referralFlow :: FlowBT String Unit
referralFlow = do
  appConfig <- getAppConfigFlowBT Constants.appConfig
  let cityConfig = getCityConfig appConfig.cityConfig (getValueToLocalStore DRIVER_LOCATION)
  modifyScreenState $ BenefitsScreenStateType (\benefitsScreen -> benefitsScreen { props {driverReferralType = ST.CUSTOMER}})
  benefitsScreenFlow

lmsVideoScreenFlow :: FlowBT String Unit
lmsVideoScreenFlow = do
  modifyScreenState $ LmsVideoScreenStateType (\lmsVideoScreen -> lmsVideoScreen { props {showShimmer = true}})
  lmsVideoScreenAction <- UI.lmsVideoScreen
  case lmsVideoScreenAction of
    GO_TO_QUIZ_SCREEN state-> do
      case state.props.selectedModule of
        Nothing -> pure unit
        Just selModule -> do
          res <- lift $ lift $ Remote.getAllLmsQuestions (selModule ^. _moduleId) $ HU.getLanguageTwoLetters $ Just state.props.selectedLanguage
          case res of
            Right resp -> do
                let (API.LmsGetQuizRes extractedData ) = resp
                    questionsData = transformQuizRespToQuestions resp
                    retryEnabled = getRetryEnabled $ questionsData !! 0
                modifyScreenState 
                 $ LmsQuizScreenStateType 
                    (\lmsQuizScreen -> lmsQuizScreen { data  {  questions = questionsData}, 
                                                       props {  selectedLanguage = state.props.selectedLanguage,
                                                                selectedTranslatedModule = Just $ extractedData.selectedModuleInfo,
                                                                currentQuestionIndex = 0,
                                                                isRetryEnabled = retryEnabled,
                                                                currentQuestionSelectedOptionsData = {selectedSingleOption : Nothing, selectedMultipleOptions : []}
                                                            }
                                                      }
                    )
            Left err -> do
                pure $ toast $ getString UNABLE_TO_LOAD_QUIZ_PLEASE_TRY_AGAIN
                lmsVideoScreenFlow
      modifyScreenState $ LmsVideoScreenStateType (\lmsVideoScreen -> lmsVideoScreen { props {isFetchingQuiz = false}})
      lmsQuizFlow
    REFRESH_LMS_VIDEO_SCREEN state -> lmsVideoScreenFlow
    GO_TO_BENEFITS_SCREEN -> do
      modifyScreenState $ BenefitsScreenStateType (\benefitsScreen -> benefitsScreen { props {showShimmer = true}, data {moduleList = {completed : [], remaining : []}}})
      benefitsScreenFlow
    SELECT_LANGUAGE_FOR_VIDEOS state -> do
      let genLanguageList = case state.props.selectedModule of
                              Nothing -> state.data.config.languageList
                              Just selModule ->  HU.generateLanguageList $ selModule ^. _languagesAvailableForVideos
      selectLanguageForScreenFlow "LMS_VIDEO_SCREEN" state.props.selectedLanguage genLanguageList
      lmsVideoScreenFlow


selectLanguageForScreenFlow :: String -> String -> Array Language -> FlowBT String Unit
selectLanguageForScreenFlow screenName selLanguage genLanguageList = do
  modifyScreenState 
    $ SelectLanguageScreenStateType 
      (\selectLangState -> selectLangState{ data  { languageList = genLanguageList},
                                            props { selectedLanguage = selLanguage,
                                                    onlyGetTheSelectedLanguage = true,
                                                    selectLanguageForScreen = screenName 
                                                  }
                                          }
      )
  selectLanguageFlow
  modifyScreenState $ SelectLanguageScreenStateType (\selectLangState -> selectLangState{ props{ onlyGetTheSelectedLanguage = false, selectedLanguage = "", selectLanguageForScreen = ""}})

lmsQuizFlow :: FlowBT String Unit
lmsQuizFlow = do
  quizScreenAction <- UI.lmsQuizScreen
  modifyScreenState $ LmsQuizScreenStateType (\lmsQuizScreen -> lmsQuizScreen {props {showShimmer = false}})
  case quizScreenAction of
    GO_TO_NEXT_QUESTION state -> goToQuizNextQuestionFlow state
    CONFIRM_QUESTION state -> confirmQuestionFlow state
    RETRY_QUESTION state -> lmsQuizFlow
    RETAKE_QUIZ_SO state -> retakeQuizFlow state
    SELECT_LANGUAGE_FOR_QUESTION state -> do
      let genLanguageList = case state.props.selectedTranslatedModule of
                              Nothing -> state.data.config.languageList
                              Just selModule ->  HU.generateLanguageList $ selModule ^. _languagesAvailableForVideos
      selectLanguageForScreenFlow "LMS_QUIZ_SCREEN" state.props.selectedLanguage genLanguageList
      modifyScreenState $ LmsQuizScreenStateType (\lmsQuizScreen -> lmsQuizScreen { props {languageUpdated = true}})
      lmsQuizFlow
    GO_TO_LMS_VIDEOS_SCREEN_FROM_QUIZ state -> do
       modifyScreenState $ LmsVideoScreenStateType (\lmsVideoScreen -> lmsVideoScreen { props {selectedLanguage = state.props.selectedLanguage}})
       lmsVideoScreenFlow
    GO_TO_BENEFITS_SCREEN_FROM_QUIZ _ -> benefitsScreenFlow
  lmsQuizFlow

goToQuizNextQuestionFlow :: ST.LmsQuizScreenState -> FlowBT String Unit
goToQuizNextQuestionFlow state = do
  let updatedQuestions = mapWithIndex (\index question -> 
      if (index ==  state.props.currentQuestionIndex + 1) 
        then question {questionStatusDuringQuiz = ST.QUESTION_ATTEMPTING}
      else if (index == state.props.currentQuestionIndex && question.questionStatusDuringQuiz == ST.QUESTION_ATTEMPTING) 
        then
            let updatedStatus = case question.previousHistory of
                                  Nothing -> ST.QUESTION_INCORRECT
                                  Just (API.LmsQuizHistory history) -> if history.status == API.CORRECT then ST.QUESTION_CORRECT else ST.QUESTION_INCORRECT
            in question {questionStatusDuringQuiz = updatedStatus}
      else question ) state.data.questions
  let retryEnabled = getRetryEnabled (state.data.questions !! (state.props.currentQuestionIndex + 1))
  modifyScreenState $ LmsQuizScreenStateType (\lmsQuizScreen -> lmsQuizScreen { data {questions = updatedQuestions}, 
      props {isRetryEnabled = retryEnabled, currentQuestionIndex = state.props.currentQuestionIndex + 1, currentQuestionSelectedOptionsData = {selectedSingleOption : Nothing, selectedMultipleOptions : []}}})
  pure $ toggleBtnLoader "QuizPrimaryButton_NEXT_QUESTION" false
  lmsQuizFlow

retakeQuizFlow :: ST.LmsQuizScreenState -> FlowBT String Unit
retakeQuizFlow state =
  case state.props.selectedTranslatedModule of
    Nothing -> pure unit
    Just selModule -> do
      res <- lift $ lift $ Remote.getAllLmsQuestions (selModule ^. _moduleId) (HU.getLanguageTwoLetters $  Just state.props.selectedLanguage)
      case res of
        Right resp -> do
            let (API.LmsGetQuizRes extractedResp) = resp
                questionsData = transformQuizRespToQuestions resp
                retryEnabled = getRetryEnabled (questionsData !! 0)
            modifyScreenState 
             $ LmsQuizScreenStateType 
               (\lmsQuizScreen -> lmsQuizScreen { data {questions = transformQuizRespToQuestions resp}, 
                                                  props { isRetryEnabled = retryEnabled,
                                                          showShimmer = false,
                                                          selectedTranslatedModule = Just $ extractedResp.selectedModuleInfo,
                                                          currentQuestionIndex = 0,
                                                          currentQuestionSelectedOptionsData = {selectedSingleOption : Nothing,
                                                          selectedMultipleOptions : []}}
                                                }
                )
            lmsQuizFlow
        Left err -> do
          pure $ toast $ getString SOMETHING_WENT_WRONG_TRY_AGAIN_LATER
          lmsVideoScreenFlow

getRetryEnabled :: Maybe ST.LmsQuestion -> Boolean
getRetryEnabled mbQuestion = maybe false  (\question -> maybe false (\(API.LmsQuizHistory history) -> history.status == API.INCORRECT ) question.previousHistory ) mbQuestion

confirmQuestionFlow :: ST.LmsQuizScreenState -> FlowBT String Unit
confirmQuestionFlow state = let moduleId = maybe "" (\selModule -> selModule ^. _moduleId) state.props.selectedTranslatedModule in
  case (state.data.questions !! state.props.currentQuestionIndex) of
    Nothing -> do
      modifyScreenState $ LmsQuizScreenStateType (\lmsQuizScreen -> lmsQuizScreen {props {isConfirming = false, isConfirmed = true}})
      lmsQuizFlow
    Just questionInfo -> do
      let req = case questionInfo.options of
                  API.SingleSelect _ -> case state.props.currentQuestionSelectedOptionsData.selectedSingleOption of
                                          Nothing -> generateQuestionConfirmReq questionInfo.questionId moduleId questionInfo.language $ API.SingleSelectedOption ""
                                          Just option -> generateQuestionConfirmReq  questionInfo.questionId moduleId questionInfo.language $ API.SingleSelectedOption option.optionId
                  API.MultiSelect _ -> generateQuestionConfirmReq questionInfo.questionId moduleId  questionInfo.language $ API.MultiSelectedOption getMultipleSelectedOptions
      resp <- lift $ lift $ Remote.confirmQuestion req
      case resp of
        Right (API.QuestionConfirmRes resp) -> do
          let updatedQuestions = map (\eQuestion -> if eQuestion.questionId == questionInfo.questionId 
                                                    then eQuestion {validationRes = Just (API.QuestionConfirmRes resp), questionStatusDuringQuiz = if resp.validation == API.CORRECT_ANSWER then ST.QUESTION_CORRECT else ST.QUESTION_INCORRECT}
                                                    else eQuestion
                                      ) state.data.questions
          modifyScreenState $ LmsQuizScreenStateType (\lmsQuizScreen -> lmsQuizScreen {data {questions = updatedQuestions}, props {isConfirming = false, isConfirmed = true}})
          lmsQuizFlow
        Left err -> do
          modifyScreenState $ LmsQuizScreenStateType (\lmsQuizScreen -> lmsQuizScreen {props {isConfirming = false, isConfirmed = true}})
          lmsQuizFlow
  where
    generateQuestionConfirmReq :: String -> String -> String -> API.SelectedOption ->  API.QuestionConfirmReq
    generateQuestionConfirmReq questionId moduleId language selectedOption = API.QuestionConfirmReq { 
      questionId : questionId
    , moduleId : moduleId
    , language : language
    , selectedOption : selectedOption
    }

    getMultipleSelectedOptions = (map (\eOption -> eOption.optionId) state.props.currentQuestionSelectedOptionsData.selectedMultipleOptions)

driverEarningsFlow :: FlowBT String Unit
driverEarningsFlow = do 
  (GlobalState globalState) <- getState
  appConfig <- getAppConfigFlowBT Constants.appConfig
  logField_ <- lift $ lift $ getLogFields
  let earningScreenState = globalState.driverEarningsScreen
  modifyScreenState $ DriverEarningsScreenStateType (\driverEarningsScreen -> driverEarningsScreen{data{hasActivePlan = globalState.homeScreen.data.paymentState.autoPayStatus /= NO_AUTOPAY, config = appConfig}, props{showShimmer = true}})
  uiAction <- UI.driverEarningsScreen
  case uiAction of
    EARNINGS_NAV HomeScreenNav state -> do
       updateDriverStats state homeScreenFlow  
    EARNINGS_NAV GoToSubscription state -> do
       updateDriverStats state updateAvailableAppsAndGoToSubs  
    EARNINGS_NAV GoToContest state -> do
       updateDriverStats state referralFlow  
    EARNINGS_NAV GoToAlerts state -> do
       updateDriverStats state $ notificationFlow Nothing
    EARNINGS_NAV _ state -> do
       updateDriverStats state driverEarningsFlow
    CHANGE_SUB_VIEW subView state -> do
      modifyScreenState $ DriverEarningsScreenStateType (\state -> state{props{subView = subView, date = getCurrentUTC ""}})
      driverEarningsFlow
    CONVERT_COIN_TO_CASH state -> do
      resp <- lift $ lift $ Remote.convertCoinToCash state.data.coinsToUse
      if isRight resp then liftFlowBT $ logEventWithMultipleParams logField_ "ny_driver_convert_to_cash_api_success" $ [{key : "Number of Coins", value : unsafeToForeign state.data.coinsToUse}] else pure unit
      let lottieFile = if isRight resp then "ny_ic_coins_redeemed_success.json" else "ny_ic_coins_redeemed_failure.json"
      modifyScreenState $ DriverEarningsScreenStateType (\driverEarningsScreen -> driverEarningsScreen{props{showCoinsRedeemedAnim = lottieFile, coinConvertedSuccess = isRight resp}})
      driverEarningsFlow
    REFRESH_EARNINGS_SCREEN state -> driverEarningsFlow
    EARNINGS_HISTORY _ -> driverEarningsFlow
    GOTO_PAYMENT_HISTORY_FROM_COINS -> paymentHistoryFlow
    GOTO_MY_PLAN_FROM_COINS -> updateAvailableAppsAndGoToSubs
    GOTO_TRIP_DETAILS  selectedCard -> do
      sourceMod <- translateString selectedCard.source 400
      destinationMod <- if selectedCard.tripType == ST.Rental then pure "" else translateString selectedCard.destination 400
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {
        "data" {
          tripId = selectedCard.id,
          date = selectedCard.date,
          time = selectedCard.time,
          source = sourceMod,
          destination = destinationMod,
          totalAmount = selectedCard.total_amount,
          distance = selectedCard.rideDistance,
          status = selectedCard.status,
          vehicleType = selectedCard.vehicleType,
          rider = selectedCard.riderName,
          customerExtraFee = selectedCard.customerExtraFee,
          purpleTagVisibility = selectedCard.purpleTagVisibility,
          gotoTagVisibility = selectedCard.gotoTagVisibility,
          spLocTagVisibility = selectedCard.spLocTagVisibility,
          specialZoneLayoutBackground = selectedCard.specialZoneLayoutBackground,
          specialZoneImage = selectedCard.specialZoneImage,
          specialZoneText = selectedCard.specialZoneText,
          specialZonePickup = selectedCard.specialZonePickup,
          tollCharge = selectedCard.tollCharge,
          goBackTo = ST.Earning,
          rideType = selectedCard.rideType,
          tripStartTime = selectedCard.tripStartTime,
          tripEndTime = selectedCard.tripEndTime,
          vehicleModel = selectedCard.vehicleModel,
          acRide = selectedCard.acRide,
          vehicleServiceTier = selectedCard.vehicleServiceTier,
          tripType = selectedCard.tripType
        , parkingCharge = selectedCard.parkingCharge
        , stops = fst <<< HU.getStopName <$> selectedCard.stops
        }
      })
      tripDetailsScreenFlow
    LOAD_MORE_HISTORY state -> do
      modifyScreenState $ DriverEarningsScreenStateType (\driverEarningsScreen -> driverEarningsScreen{props{offsetValue = state.props.offsetValue + 10}})
      driverEarningsFlow
    GOTO_COINS_EARNING_INFO _ -> do 
      (API.CoinInfoRes resp) <- Remote.getCoinInfoBT "lazy"
      let 
        filteredResp = filter (\(API.CoinInfo coinInfo) -> checkCoinsInfoConditions coinInfo) resp
        sortedResp = sortBy (\(API.CoinInfo coinInfo1) (API.CoinInfo coinInfo2) -> compare coinInfo2.coins coinInfo1.coins) filteredResp
      modifyScreenState $ DriverEarningsScreenStateType (\state -> state{data{coinInfoRes = Just $ [tableTitle] <> sortedResp}})
      driverEarningsFlow 
  where 
    updateDriverStats state flow = do
      when (state.props.subView == ST.YATRI_COINS_VIEW) $ do
        modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{driverRideStats = (<$>) (\(DriverProfileStatsResp stats) -> DriverProfileStatsResp stats{ coinBalance = state.data.coinBalance }) globalProps.driverRideStats}
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{coinBalance = state.data.coinBalance}})
      flow
    
    checkCoinsInfoConditions :: API.CoinInfoType -> Boolean
    checkCoinsInfoConditions coinInfo = 
      let 
        cond1 = not $ startsWith "BulkUploadEvent" coinInfo.key
        cond2 = not $ DS.null coinInfo.title
        cond3 = coinInfo.coins /= 0
      in 
        cond1 && cond2 && cond3

    tableTitle :: API.CoinInfo
    tableTitle = API.CoinInfo {
      coins : 0
    , title : getString TASK_COMPLETED
    , description : ""
    , key : "TABLE_TITILE"
    }
      
activateReferralCode :: ST.RegistrationScreenState -> String -> FlowBT String Unit
activateReferralCode state code = do
  referDriverResponse <- lift $ lift $ Remote.referDriver $ makeReferDriverReq code
  case referDriverResponse of
    Right (API.ApiSuccessResult res) -> do
      case res.result of
        "Success" -> do
          logField_ <- lift $ lift $ getLogFields
          liftFlowBT $ logEventWithMultipleParams logField_ "ny_driver_ref_code_submitted" $ [{key : "Referee Code", value : unsafeToForeign code}]
          void $ pure $ setCleverTapUserProp [{key : "Entered Referral Code", value : unsafeToForeign code}]
        _         -> do
          void $ pure $ cleverTapCustomEvent "ny_driver_ref_code_already_applied"
      referralCodeAppliedStage state
    Left errorPayload -> do
      if (decodeErrorCode errorPayload.response.errorMessage) == "ALREADY_REFFERED" then
        referralCodeAppliedStage state
      else  
        modifyScreenState $ RegistrationScreenStateType (\driverReferralScreen -> state{ props{isValidReferralCode = false}})
  where
    referralCodeAppliedStage :: ST.RegistrationScreenState -> FlowBT String Unit
    referralCodeAppliedStage state = do
      modifyScreenState $ RegistrationScreenStateType (\driverReferralScreen -> state{ props{isValidReferralCode = true, referralCodeSubmitted = true, enterReferralCodeModal = false}})
      setValueToLocalStore REFERRER_URL ""
      setValueToLocalStore REFERRAL_CODE_ADDED "true"

documentcaptureScreenFlow :: FlowBT String Unit 
documentcaptureScreenFlow = do 
  screenOutput <- UI.documentCaptureScreen
  case screenOutput of
    TA.LOGOUT_FROM_DOC_CAPTURE -> logoutFlow
    TA.CHANGE_VEHICLE_FROM_DOCUMENT_CAPTURE -> do
      deleteValueFromLocalStore VEHICLE_CATEGORY
      onBoardingFlow
    TA.CHANGE_LANG_FROM_DOCUMENT_CAPTURE -> do
      modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{onBoardingDocs = Nothing}
      modifyScreenState $ SelectLanguageScreenStateType (\selectLangState -> selectLangState{ props{ onlyGetTheSelectedLanguage = false, selectedLanguage = "", selectLanguageForScreen = "", fromOnboarding = true}})
      selectLanguageFlow
    TA.UPLOAD_DOC_API state imageType -> do
      validateImageResp <- lift $ lift $ Remote.validateImage $ makeValidateImageReq state.data.imageBase64 imageType state.data.linkedRc Nothing Nothing state.data.vehicleCategory
      case validateImageResp of
        Right (ValidateImageRes resp) -> do
          void $ pure $ toast $ getString DOCUMENT_UPLOADED_SUCCESSFULLY
          onBoardingFlow
        Left error -> do
          modifyScreenState $ DocumentCaptureScreenStateType $ \docCapScreenState -> docCapScreenState { props {validating = false}, data {errorMessage = Just $ Remote.getCorrespondingErrorMessage error}}
          documentcaptureScreenFlow

uploadParcelImageFlow :: FlowBT String Unit
uploadParcelImageFlow = do
  screenOutput <- UI.uploadParcelImageScreen
  case screenOutput of
    GOTO_HOME_SCREEN -> homeScreenFlow
    UPLOAD_IMAGE state -> do
      setValueToLocalStore PARCEL_IMAGE_UPLOADED "true"
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props {enterOtpModal = true, rideOtp = "", enterOtpFocusIndex = 0,  otpIncorrect = false, zoneRideBooking = false}})
      homeScreenFlow

rideRequestScreenFlow :: FlowBT String Unit 
rideRequestScreenFlow = do
  action <- UI.rideRequestScreen
  case action  of 
    TA.GOTO_HOME state -> do
      let (ScheduledBookingListResponse listResponse) = state.data.resp
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {data{scheduleRideCount =Just $ Tuple (DA.length listResponse.bookings) (getCurrentUTC "")} } )
      homeScreenFlow
    TA.RIDE_REQUEST_REFRESH_SCREEN state -> do
      modifyScreenState $ RideRequestScreenStateType (\rideRequestScreen -> state{data{offset = 0, resp = RideRequestData.dummyResp}, props{shouldCall = true}})
      rideRequestScreenFlow
    TA.LOADER__OUTPUT state -> do
      modifyScreenState $ RideRequestScreenStateType (\rideRequestScreen -> state{data{offset = state.data.offset + 10}})
      rideRequestScreenFlow
    TA.GOTO_RIDE_SUMMARY state -> do
      modifyScreenState $ RideSummaryScreenStateType (\rideSummaryScreen -> RideSummaryScreenData.initData{data {
        rideDetails = state.data.currCard,
        fareDetails = state.data.fareDetails
      }})
      rideSummaryScreenFlow 
    TA.NOTIFICATION_TYPE notificationType state -> do
      void $ pure $ removeAllPolylines ""
      case notificationType of
        "DRIVER_ASSIGNMENT" -> do
          currentRideFlow Nothing Nothing
        _ -> rideRequestScreenFlow
    TA.GO_BACK_TO_RIDEREQUEST_SCREEN state -> do
      modifyScreenState $ RideRequestScreenStateType (\rideRequestScreen -> state{data{filteredArr = [],resp = RideRequestData.dummyResp, offset =0}})
      rideRequestScreenFlow
      
    _                   -> rideRequestScreenFlow

handleDriverActivityResp :: (Either ErrorResponse DriverActiveInactiveResp) -> FlowBT String Unit
handleDriverActivityResp resp =
  case resp of
    Left err -> do
      let errResp = err.response
          codeMessage = decodeErrorCode errResp.errorMessage
          accountBlocked = err.code == 403 && codeMessage == "DRIVER_ACCOUNT_BLOCKED"
      when accountBlocked $ changeDriverStatus Offline
      pure unit
    Right _ -> changeDriverStatus Online

rideSummaryScreenFlow :: FlowBT String Unit
rideSummaryScreenFlow = do
  action <- UI.rideSummaryScreen
  case action of
    ACCEPT_SCHEDULED_RIDE bookingId -> do 
       acceptScheduledRideResp <- lift $ lift $ Remote.scheduleBookingAccept bookingId
       case acceptScheduledRideResp of
        Right (API.ScheduleBookingAcceptRes val)  -> do 
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{checkUpcomingRide = true  , homeScreenBannerVisibility = true ,rideRequestPill{isPillClickable =  false} }}  )
          rideAcceptScreenFlow
        Left (err) -> do
          modifyScreenState $ RideSummaryScreenStateType (\rideSummaryScreen -> RideSummaryScreenData.initData{props{errorPopUp = true}} )
          rideSummaryScreenFlow
    CANCEL_SCHEDULED_RIDE {id, info , reason}  -> do
      API.DriverCancelRideResponse cancelRideResp <- Remote.cancelRide id (Remote.makeCancelRideReq info reason)
      void $ pure $ clearTimerWithId "bannerTimer"
      modifyScreenState $ HomeScreenStateType(\homeScreen -> homeScreen{props{checkUpcomingRide = true  , homeScreenBannerVisibility = false , rideRequestPill{isPillClickable =  true} }}  )
      homeScreenFlow
    GO_TO_RIDE_REQUEST state ->  do 
     modifyScreenState $ RideSummaryScreenStateType (\rideSummaryScreen -> rideSummaryScreen {props {shimmerVisibility = true}}) 
     modifyScreenState $ RideRequestScreenStateType (\rideRequestScreen -> rideRequestScreen{props{shouldCall = false}})
     rideRequestScreenFlow
    BACK_HOME -> homeScreenFlow
    GO_TO_HOME_SCREEN_FROM_BANNER -> homeScreenFlow
    ON_CALLING state exophoneNumber -> do
      (API.ApiSuccessResult  resp) <- Remote.onCallBT (Remote.makeOnCallReq state.data.activeRideData.id exophoneNumber)
      rideSummaryScreenFlow
    GO_TO_OPEN_GOOGLE_MAP state -> do
      void $ pure $ openNavigation state.data.activeRideData.src_lat state.data.activeRideData.src_lon "DRIVE"
      rideSummaryScreenFlow

    FCM_NOTIFICATION_TYPE notificationType state -> do
      void $ pure $ removeAllPolylines ""
      case notificationType of
        "DRIVER_ASSIGNMENT" -> do
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{rideStartRemainingTime = -1}}) 
          currentRideFlow Nothing Nothing
        "CANCELLED_PRODUCT" -> do
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{checkUpcomingRide = true  }}  ) 
          homeScreenFlow
        _ -> rideSummaryScreenFlow
      rideSummaryScreenFlow 
    GO_BACK_TO_RIDE_REQUEST  -> rideRequestScreenFlow
    _  -> rideSummaryScreenFlow   

rideAcceptScreenFlow ::  FlowBT String Unit
rideAcceptScreenFlow = do
  action <- UI.scheduledRideAcceptedScreen
  case action of 
    GO_HOME_FROM_SCHEDULED_RIDE_ACCEPT_SCREEN -> homeScreenFlow
  rideAcceptScreenFlow
   
logDriverStatus :: Maybe String -> ST.DriverStatus -> FlowBT String Unit
logDriverStatus category status = do
  logField_ <- lift $ lift $ getLogFields
  let label = 
            "ny_driver_"
            <> case category of
                Just category -> category
                Nothing -> ""
            <> "_"
            <> toLower (show status)
            <> "_mode"
      currentTime = getCurrentUTC ""
  liftFlowBT $ logEventWithParams logField_ label "Timestamp" currentTime
  modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps { firstTimeOnboardingStatus = false } 
  
metroWarriorsScreenFlow :: FlowBT String Unit
metroWarriorsScreenFlow = do
  let config = RC.metroWarriorsConfig (getValueToLocalStore DRIVER_LOCATION) (getValueToLocalStore VEHICLE_VARIANT)
  modifyScreenState $ MetroWarriorsScreenStateType $ \metroWarriors -> metroWarriors{data{remoteConfigData = config }}
  action <- UI.metroWarriorsScreen
  case action of
    TA.GO_TO_HOME_SCREEN_FROM_WARRIOR state -> homeScreenFlow
    TA.UPDATE_WARRIOR_SETTINGS state req@(API.UpdateSpecialLocWarriorInfoReq request) -> do
      globalState <- getState
      getDriverInfoResp <- getDriverInfoDataFromCache globalState false
      when (request.isSpecialLocWarrior) $ disableGoTo getDriverInfoResp
      (response :: (Either ErrorResponse API.SpecialLocWarriorInfoRes)) <- lift $ lift $ HelpersAPI.callApi $ req
      case response of
        Right settings -> do
          let warriorSettings = makeStationsData settings state.data.stationList state.data.remoteConfigData
          modifyScreenState $ MetroWarriorsScreenStateType $ \metroWarriors -> state{data{stationData = warriorSettings }}
          modifyScreenState $ HomeScreenStateType $ \homeScreen -> homeScreen{data {isSpecialLocWarrior = warriorSettings.isSpecialLocWarrior}}
        Left err -> do
          let errResp = err.response
              codeMessage = HU.decodeErrorCode errResp.errorMessage
          void $ pure $ toast $ if err.code == 500 then getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN else codeMessage
          pure unit
      metroWarriorsScreenFlow
    _ -> metroWarriorsScreenFlow

disableGoTo :: GetDriverInfoResp -> FlowBT String Unit
disableGoTo (GetDriverInfoResp getDriverInfoResp) = do
  let (API.DriverGoHomeInfo driverGoHomeInfo) = getDriverInfoResp.driverGoHomeInfo
  when (driverGoHomeInfo.status == Just "ACTIVE") do
    void $ lift $ lift $ Remote.deactivateDriverGoTo "" -- disabling goto when driver is Offline
    modifyScreenState $ GlobalPropsType $ \globalProps -> globalProps{driverInformation = Just $ GetDriverInfoResp getDriverInfoResp {driverGoHomeInfo = API.DriverGoHomeInfo driverGoHomeInfo { status = Nothing} } }
    updateDriverDataToStates

updateWarriorSettings :: Boolean -> FlowBT String Unit
updateWarriorSettings newSpecialLocationWarriorValue = do
  globalState <- getState
  getDriverInfoResp <- getDriverInfoDataFromCache globalState false
  when (newSpecialLocationWarriorValue) $ disableGoTo getDriverInfoResp
  let driverid = getValueToLocalStore DRIVER_ID
  response <- lift $ lift $ HelpersAPI.callApi $ API.GetSpecialLocWarriorInfoReq driverid
  case response of
    Right (API.SpecialLocWarriorInfoRes settings) -> do
      when (isNothing settings.preferredPrimarySpecialLoc) $ metroWarriorsScreenFlow
      let req = API.UpdateSpecialLocWarriorInfoReq {
        isSpecialLocWarrior : newSpecialLocationWarriorValue,
        preferredPrimarySpecialLocId : maybe Nothing (\(API.SpecialLocationWarrior item) -> Just item.id) settings.preferredPrimarySpecialLoc,
        preferredSecondarySpecialLocIds : settings.preferredSecondarySpecialLocIds,
        driverId : driverid
      }
      (response :: (Either ErrorResponse API.SpecialLocWarriorInfoRes)) <- lift $ lift $ HelpersAPI.callApi req
      case response of
        Right settings -> do
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{isSpecialLocWarrior = newSpecialLocationWarriorValue}})
        Left err -> do
          let errResp = err.response
              codeMessage = HU.decodeErrorCode errResp.errorMessage
          void $ pure $ toast $ if err.code == 500 then getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN else codeMessage
          pure unit
    Left err -> do
      let errResp = err.response
          codeMessage = HU.decodeErrorCode errResp.errorMessage
      void $ pure $ toast $ if err.code == 500 then getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN else codeMessage
      pure unit