module Helpers.LogicUtils where

import Prelude
import Common.Types.App (Version(..), LazyCheck(..), Event)
import Types.App (RIDE_SUMMARY_SCREEN_OUTPUT(..), LMS_QUIZ_SCREEN_OUTPUT(..), LMS_VIDEO_SCREEN_OUTPUT(..), REPORT_ISSUE_CHAT_SCREEN_OUTPUT(..), RIDES_SELECTION_SCREEN_OUTPUT(..), ABOUT_US_SCREEN_OUTPUT(..), BANK_DETAILS_SCREENOUTPUT(..), ADD_VEHICLE_DETAILS_SCREENOUTPUT(..), APPLICATION_STATUS_SCREENOUTPUT(..), DRIVER_DETAILS_SCREEN_OUTPUT(..), DRIVER_PROFILE_SCREEN_OUTPUT(..), CHOOSE_CITY_SCREEN_OUTPUT(..), DRIVER_RIDE_RATING_SCREEN_OUTPUT(..), ENTER_MOBILE_NUMBER_SCREEN_OUTPUT(..), ENTER_OTP_SCREEN_OUTPUT(..), FlowBT, GlobalState(..), HELP_AND_SUPPORT_SCREEN_OUTPUT(..), HOME_SCREENOUTPUT(..), MY_RIDES_SCREEN_OUTPUT(..), NOTIFICATIONS_SCREEN_OUTPUT(..), NO_INTERNET_SCREEN_OUTPUT(..), PERMISSIONS_SCREEN_OUTPUT(..), POPUP_SCREEN_OUTPUT(..), REGISTRATION_SCREEN_OUTPUT(..), RIDE_DETAIL_SCREENOUTPUT(..), PAYMENT_HISTORY_SCREEN_OUTPUT(..), SELECT_LANGUAGE_SCREEN_OUTPUT(..), ScreenStage(..), ScreenType(..), TRIP_DETAILS_SCREEN_OUTPUT(..), UPLOAD_ADHAAR_CARD_SCREENOUTPUT(..), UPLOAD_DRIVER_LICENSE_SCREENOUTPUT(..), VEHICLE_DETAILS_SCREEN_OUTPUT(..), WRITE_TO_US_SCREEN_OUTPUT(..), NOTIFICATIONS_SCREEN_OUTPUT(..), REFERRAL_SCREEN_OUTPUT(..), BOOKING_OPTIONS_SCREEN_OUTPUT(..), ACKNOWLEDGEMENT_SCREEN_OUTPUT(..), defaultGlobalState, SUBSCRIPTION_SCREEN_OUTPUT(..), NAVIGATION_ACTIONS(..), AADHAAR_VERIFICATION_SCREEN_OUTPUT(..), ONBOARDING_SUBSCRIPTION_SCREENOUTPUT(..), APP_UPDATE_POPUP(..), DRIVE_SAVED_LOCATION_OUTPUT(..), WELCOME_SCREEN_OUTPUT(..), DRIVER_EARNINGS_SCREEN_OUTPUT(..), BENEFITS_SCREEN_OUTPUT(..), CUSTOMER_REFERRAL_TRACKER_SCREEN_OUTPUT(..), HOTSPOT_SCREEN_OUTPUT(..), SCHEDULED_RIDE_ACCEPTED_SCREEN_OUTPUT(..), UPLOAD_PARCEL_IMAGE_SCREEN_OUTPUT(..))
import Types.ModifyScreenState (modifyScreenState, updateStage)
import Control.Alt ((<|>))
import Screens.Types (AadhaarStage(..), ActiveRide, AllocationData, AutoPayStatus(..), DriverStatus(..), HomeScreenStage(..), HomeScreenState, UpdateRouteSrcDestConfig(..), KeyboardModalType(..), Location, PlanCardConfig, PromoConfig, ReferralType(..), StageStatus(..), SubscribePopupType(..), SubscriptionBannerType(..), SubscriptionPopupType(..), SubscriptionSubview(..), UpdatePopupType(..), ChooseCityScreenStage(..))
import Screens.Types as ST
import Data.String (Pattern(..), split, toUpper, drop, indexOf, toLower, take)
import Control.Monad.Except (runExceptT, runExcept)
import Control.Monad.Except.Trans (lift)
import Data.Functor (map)
import Data.Either (Either(..), either, isRight)
import Data.Int (ceil, fromString, round, toNumber)
import Data.Lens ((^.))
import Services.API as API
import Data.Array (any, concat, cons, elem, elemIndex, filter, find, foldl, head, last, length, mapWithIndex, null, snoc, sortBy, (!!))
import Services.API (AlternateNumberResendOTPResp(..), Category(Category), CreateOrderRes(..), CurrentDateAndTimeRes(..), DriverActiveInactiveResp(..),  DriverAlternateNumberResp(..), DriverArrivedReq(..), DriverProfileStatsReq(..), DriverProfileStatsResp(..), DriverRegistrationStatusReq(..), DriverRegistrationStatusResp(..), GenerateAadhaarOTPResp(..), GetCategoriesRes(GetCategoriesRes), DriverInfoReq(..), GetDriverInfoResp(..), GetOptionsRes(GetOptionsRes), GetPaymentHistoryResp(..), GetPaymentHistoryResp(..), GetPerformanceReq(..), GetPerformanceRes(..), GetRidesHistoryResp(..), GetRouteResp(..), IssueInfoRes(IssueInfoRes), LogOutReq(..), Option(Option), OrderStatusRes(..), OrganizationInfo(..), PaymentDetailsEntity(..), PostIssueReq(PostIssueReq), PostIssueRes(PostIssueRes),  RemoveAlternateNumberRequest(..), ResendOTPResp(..), RidesInfo(..), Route(..),  Status(..), SubscribePlanResp(..), TriggerOTPResp(..), UpdateDriverInfoReq(..), UpdateDriverInfoResp(..), ValidateImageReq(..), ValidateImageRes(..), Vehicle(..), VerifyAadhaarOTPResp(..), VerifyTokenResp(..), GenerateReferralCodeReq(..), GenerateReferralCodeRes(..), FeeType(..), ClearDuesResp(..), HistoryEntryDetailsEntityV2Resp(..), DriverProfileSummaryRes(..), DummyRideRequestReq(..), BookingTypes(..), UploadOdometerImageResp(UploadOdometerImageResp), GetRidesSummaryListResp(..), PayoutVpaStatus(..), ScheduledBookingListResponse (..), DriverReachedReq(..), ServiceTierType(..), ActiveTripTransaction(..), TripTransactionDetails(..), BusTripStatus(..), AvailableRoutes(..), AvailableRoutesList(..), RouteInfo(..), StopInfo(..), BusVehicleDetails(..))
import JBridge (RecentBusTrip)
import Helpers.Utils as HU
import Helpers.Utils (isYesterday, LatLon(..), decodeErrorCode, decodeErrorMessage, getCurrentLocation, getDatebyCount, getDowngradeOptions, getGenderIndex, getNegotiationUnit, getPastDays, getPastWeeks, getTime, getcurrentdate, isDateGreaterThan, onBoardingSubscriptionScreenCheck, parseFloat, secondsLeft, toStringJSON, translateString, getDistanceBwCordinates, getCityConfig, getDriverStatus, getDriverStatusFromMode, updateDriverStatus, getLatestAndroidVersion, isDateNDaysAgo, getHvErrorMsg)
import Engineering.Helpers.LogEvent (logEvent, logEventWithParams, logEventWithMultipleParams)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, isNothing, maybe)
import Data.Number (fromString) as Number
import Services.Backend as Remote
import Effect.Class (liftEffect)
import Engineering.Helpers.Utils (loaderText, toggleLoader, reboot, showSplash, (?), fetchLanguage, capitalizeFirstChar, getCityFromCode, handleUpdatedTerms, getReferralCode, whenJust, whenRight)
import Effect.Uncurried (runEffectFn1, runEffectFn5, runEffectFn2, runEffectFn3, runEffectFn9, runEffectFn10)
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Engineering.Helpers.Commons (flowRunner, liftFlow, getNewIDWithTag, getVersionByKey, os, getExpiryTime, stringToVersion, setText, convertUTCtoISC, getCurrentUTC, markPerformance, setEventTimestamp, getTimeStampObject)
import Foreign.Class (class Encode, encode, decode)
import Helpers.API (callApiBT, callApi)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Presto.Core.Types.API (ErrorResponse(..))
import MerchantConfig.Types (AppConfig(..), Language, CityConfig)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Storage (KeyStore(..), deleteValueFromLocalStore, getValueToLocalNativeStore, getValueToLocalStore, isLocalStageOn, isOnFreeTrial, setValueToLocalNativeStore, setValueToLocalStore)
import LocalStorage.Cache (getValueFromCache, setValueToCache)
import Presto.Core.Types.Language.Flow (delay, setLogField, getLogFields, doAff, fork, Flow)
import Services.Accessor (_lat, _lon, _id, _orderId, _moduleId, _languagesAvailableForQuiz , _languagesAvailableForVideos, _routeCode, _routeInfo, _busNumber, _routeInfo, _busNumber, _allowStartRideFromQR)
import Debug (spy)
import DecodeUtil (decodeForeignAny, getAnyFromWindow, parseJSON, setAnyInWindow, stringifyJSON, getFromWindowString)


getBusDriverCurrentLocation :: FlowBT String { lat :: String, lon :: String }
getBusDriverCurrentLocation = do
  let currentDriverLat = fromMaybe 0.0 $ Number.fromString $ getValueToLocalNativeStore LAST_KNOWN_LAT
      currentDriverLon = fromMaybe 0.0 $ Number.fromString $ getValueToLocalNativeStore LAST_KNOWN_LON
  (LatLon lat lon _) <- getCurrentLocation 
    currentDriverLat 
    currentDriverLon 
    currentDriverLat 
    currentDriverLon 
    500 
    false 
    true
  pure { lat, lon }


logBusRideStart :: FlowBT String Unit
logBusRideStart = do
  logField_ <- lift $ lift $ getLogFields
  liftFlowBT $ logEvent logField_ "ny_driver_bus_ride_start"
  void $ lift $ lift $ toggleLoader false

updateRecentBusRide :: API.TripTransactionDetails -> FlowBT String Unit
updateRecentBusRide tripDetails = do
  void $ pure $ setValueToCache (show RECENT_BUS_TRIPS) (HU.tripDetailsToRecentTrip tripDetails) (stringifyJSON <<< encode)

updateRecentBusView :: FlowBT String Unit
updateRecentBusView = do
  (GlobalState globalState) <- getState
  when (shouldUpdateRecentBus globalState.homeScreen) do
    processRecentBusTrip

shouldUpdateRecentBus :: HomeScreenState ->  Boolean
shouldUpdateRecentBus state = 
  let noActiveTrip = isNothing state.data.whereIsMyBusData.trip
      allowQRStartRide = maybe true (\fleetConfig -> 
          fleetConfig ^. _allowStartRideFromQR
        ) state.data.whereIsMyBusData.fleetConfig
    in (getValueToLocalStore LOCAL_STAGE == "HomeScreen") && allowQRStartRide

processRecentBusTrip :: FlowBT String Unit
processRecentBusTrip = do
  let tripsStr = getValueToLocalStore RECENT_BUS_TRIPS
      (recentTrip :: Maybe (JB.RecentBusTrip)) = (decodeForeignAny (parseJSON tripsStr) Nothing)
  whenJust recentTrip \tripDetails -> do
    availableRoutes <- fetchAvailableRoutes tripDetails
    whenRight availableRoutes \routes -> do
      updateStateWithRoutes routes tripDetails

updateStateWithRoutes :: API.AvailableRoutesList -> JB.RecentBusTrip -> FlowBT String Unit
updateStateWithRoutes (AvailableRoutesList availableRoutesList) tripDetails = do
  let selectedRoute = findMatchingRoute tripDetails availableRoutesList
  
  modifyScreenState $ HomeScreenStateType \homeScreen -> homeScreen
    { data {
        whereIsMyBusData { 
          availableRoutes = Just (AvailableRoutesList availableRoutesList)
        , lastCompletedTrip = Just $ HU.recentTripToTripDetails tripDetails
        }
      }
    , props {
        whereIsMyBusConfig {
          selectedRoute = selectedRoute
        }
      }
      }
fetchAvailableRoutes :: JB.RecentBusTrip -> FlowBT String (Either ErrorResponse API.AvailableRoutesList)
fetchAvailableRoutes tripDetails = do
  lift $ lift $ Remote.getAvailableRoutes $ getValueToLocalStore BUS_VEHICLE_NUMBER_HASH
  
findMatchingRoute :: JB.RecentBusTrip -> Array API.AvailableRoutes -> Maybe AvailableRoutes
findMatchingRoute tripDetails availableRoutesList = 
  findRoundRoute <|> findDirectRoute
  where
    findRoundRoute = find isMatchingRoundRoute availableRoutesList
    findDirectRoute = find isMatchingDirectRoute availableRoutesList
    
    isMatchingRoundRoute (AvailableRoutes route) = 
      maybe false (_ == tripDetails.routeCode) route.roundRouteCode
    
    isMatchingDirectRoute (AvailableRoutes route) = 
      (route.routeInfo ^. _routeCode) == tripDetails.routeCode

updateBusFleetConfig :: HomeScreenState -> FlowBT String Unit
updateBusFleetConfig state = do
  when (isNothing state.data.whereIsMyBusData.fleetConfig) $ do
    response <- lift $ lift $ Remote.getBusFleetConfig ""
    case response of
      Right config -> do
        modifyScreenState $ HomeScreenStateType \homeScreen -> homeScreen
          { data {
              whereIsMyBusData {
                fleetConfig = Just config
              }
            }
          }
      Left _ -> pure unit

giveFleetConsent :: FlowBT String Unit
giveFleetConsent = do
  _ <- lift $ lift $ Remote.postFleetConsent ""
  pure unit
  