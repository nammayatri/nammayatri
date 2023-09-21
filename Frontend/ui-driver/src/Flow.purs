{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Flow where

import Log

import Common.Styles.Colors as Color
import Common.Styles.Colors as Color
import Common.Types.App (APIPaymentStatus(..)) as PS
import Common.Types.App (Event, LazyCheck(..), PaymentStatus(..), Version(..))
import Components.ChatView.Controller (makeChatComponent')
import Constants as Constants
import Control.Monad.Except (runExceptT)
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (concat, filter, cons, elemIndex, head, length, mapWithIndex, null, snoc, sortBy, (!!), any, last)
import Data.Either (Either(..))
import Data.Functor (map)
import Data.Int (ceil, fromString, round, toNumber)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, isNothing)
import Data.Number (fromString) as Number
import Data.Ord (compare)
import Data.Ord (compare)
import Data.Semigroup ((<>))
import Data.Semigroup ((<>))
import Data.String (Pattern(..), split, toUpper)
import Data.String (length) as STR
import Data.String (length) as STR
import Data.String.CodeUnits (splitAt)
import Data.String.CodeUnits (splitAt)
import Data.String.Common (joinWith, split, toUpper, trim)
import Data.String.Common (joinWith, split, toUpper, trim)
import Data.Time.Duration (Milliseconds(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler, launchAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn1)
import Effect.Uncurried (runEffectFn1)
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Engineering.Helpers.Commons (flowRunner)
import Engineering.Helpers.Commons (liftFlow, getNewIDWithTag, bundleVersion, os, getExpiryTime, stringToVersion, setText, convertUTCtoISC, getCurrentUTC, getCurrentTimeStamp)
import Engineering.Helpers.LogEvent (logEvent)
import Engineering.Helpers.LogEvent (logEvent, logEventWithParams)
import Engineering.Helpers.Suggestions (suggestionsDefinitions, getSuggestions)
import Engineering.Helpers.Utils (loaderText, toggleLoader, getAppConfig)
import Foreign.Class (class Encode, encode, decode)
import Helpers.Utils (hideSplash, getTime, decodeErrorCode, toString, secondsLeft, decodeErrorMessage, parseFloat, getcurrentdate, getDowngradeOptions, getPastDays, getPastWeeks, getGenderIndex, paymentPageUI, consumeBP, getDatebyCount, getNegotiationUnit, initiatePP, killPP, checkPPInitiateStatus, getCurrentLocation, LatLon(..), getAvailableUpiApps, isDateGreaterThan)
import JBridge (cleverTapCustomEvent, cleverTapCustomEventWithParams, cleverTapSetLocation, drawRoute, factoryResetApp, firebaseLogEvent, firebaseUserID, generateSessionId, getCurrentLatLong, getCurrentPosition, getVersionCode, getVersionName, hideKeyboardOnNavigation, isBatteryPermissionEnabled, isInternetAvailable, isLocationEnabled, isLocationPermissionEnabled, isOverlayPermissionEnabled, metaLogEvent, openNavigation, removeAllPolylines, removeMarker, saveSuggestionDefs, saveSuggestions, setCleverTapUserData, setCleverTapUserProp, showMarker, startLocationPollingAPI, stopChatListenerService, stopLocationPollingAPI, toast, toggleBtnLoader, unregisterDateAndTime, withinTimeRange, metaLogEventWithTwoParams, firebaseLogEventWithTwoParams)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (getMerchant, Merchant(..), getValueFromConfig)
import Prelude (Unit, bind, discard, pure, unit, unless, negate, void, when, map, ($), (==), (/=), (&&), (||), (/), when, (+), show, (>), not, (<), (*), (-), (<=), (<$>), (>=), ($>), (<<<))
import Presto.Core.Types.Language.Flow (delay, setLogField)
import Presto.Core.Types.Language.Flow (doAff, fork)
import Presto.Core.Types.Language.Flow (getLogFields)
import Resource.Constants (decodeAddress)
import Screens (ScreenName(..)) as ScreenNames
import Screens.BookingOptionsScreen.Controller (downgradeOptionsConfig)
import Screens.BookingOptionsScreen.ScreenData as BookingOptionsScreenData
import Screens.DriverDetailsScreen.Controller (getGenderValue, genders, getGenderState)
import Screens.DriverProfileScreen.Controller (getDowngradeOptionsSelected)
import Screens.Handlers (homeScreen)
import Screens.Handlers as UI
import Screens.HomeScreen.ComponentConfig (mapRouteConfig)
import Screens.HomeScreen.Controller (activeRideDetail)
import Screens.HomeScreen.View (rideRequestPollingData)
import Screens.PaymentHistoryScreen.Controller (ScreenOutput(..))
import Screens.PaymentHistoryScreen.Transformer (buildTransactionDetails)
import Screens.PopUpScreen.Controller (transformAllocationData)
import Screens.ReportIssueChatScreen.Handler (reportIssueChatScreen) as UI
import Screens.ReportIssueChatScreen.ScreenData (initData) as ReportIssueScreenData
import Screens.ReportIssueChatScreen.ScreenData (initData) as ReportIssueScreenData
import Screens.RideHistoryScreen.Transformer (getPaymentHistoryItemList)
import Screens.RideSelectionScreen.Handler (rideSelection) as UI
import Screens.RideSelectionScreen.View (getCategoryName)
import Screens.RideSelectionScreen.View (getCategoryName)
import Screens.SubscriptionScreen.Transformer (alternatePlansTransformer)
import Screens.Types (AadhaarStage(..), ActiveRide, AllocationData, AutoPayStatus(..), DriverStatus(..), HomeScreenStage(..), HomeScreenState, KeyboardModalType(..), Location, PlanCardConfig, ReferralType(..), SubscribePopupType(..), SubscriptionSubview(..), UpdatePopupType(..), PromoConfig)
import Screens.Types as ST
import Services.API (AlternateNumberResendOTPResp(..), Category(Category), CreateOrderRes(..), CurrentDateAndTimeRes(..), DriverActiveInactiveResp(..), DriverAlternateNumberOtpResp(..), DriverAlternateNumberResp(..), DriverArrivedReq(..), DriverDLResp(..), DriverProfileStatsReq(..), DriverProfileStatsResp(..), DriverRCResp(..), DriverRegistrationStatusReq(..), DriverRegistrationStatusResp(..), FeeType(..), GenerateAadhaarOTPResp(..), GetCategoriesRes(GetCategoriesRes), GetDriverInfoReq(..), GetDriverInfoResp(..), GetOptionsRes(GetOptionsRes), GetPaymentHistoryResp(..), GetPaymentHistoryResp(..), GetPerformanceReq(..), GetPerformanceRes(..), GetRidesHistoryResp(..), GetRouteResp(..), IssueInfoRes(IssueInfoRes), LogOutReq(..), LogOutRes(..), MakeRcActiveOrInactiveResp(..), OfferRideResp(..), OnCallRes(..), Option(Option), OrderStatusRes(..), OrganizationInfo(..), PayPayload(..), PaymentDetailsEntity(..), PaymentPagePayload(..), PostIssueReq(PostIssueReq), PostIssueRes(PostIssueRes), ReferDriverResp(..), RemoveAlternateNumberRequest(..), RemoveAlternateNumberResp(..), ResendOTPResp(..), RidesInfo(..), Route(..), StartRideResponse(..), Status(..), SubscribePlanResp(..), TriggerOTPResp(..), UpdateDriverInfoReq(..), UpdateDriverInfoResp(..), ValidateImageReq(..), ValidateImageRes(..), Vehicle(..), VerifyAadhaarOTPResp(..), VerifyTokenResp(..))
import Services.API as API
import Services.Accessor (_lat, _lon, _id)
import Services.Backend (driverRegistrationStatusBT, dummyVehicleObject, makeDriverDLReq, makeDriverRCReq, makeGetRouteReq, makeLinkReferralCodeReq, makeOfferRideReq, makeReferDriverReq, makeResendAlternateNumberOtpRequest, makeTriggerOTPReq, makeValidateAlternateNumberRequest, makeValidateImageReq, makeVerifyAlternateNumberOtpRequest, makeVerifyOTPReq, mkUpdateDriverInfoReq, walkCoordinate, walkCoordinates)
import Services.Backend as Remote
import Services.Config (getBaseUrl)
import Storage (KeyStore(..), deleteValueFromLocalStore, getValueToLocalNativeStore, getValueToLocalStore, isLocalStageOn, setValueToLocalNativeStore, setValueToLocalStore)
import Types.App (AADHAAR_VERIFICATION_SCREEN_OUTPUT(..), ABOUT_US_SCREEN_OUTPUT(..), ACKNOWLEDGEMENT_SCREEN_OUTPUT(..), ADD_VEHICLE_DETAILS_SCREENOUTPUT(..), APPLICATION_STATUS_SCREENOUTPUT(..), APP_UPDATE_POPUP(..), BANK_DETAILS_SCREENOUTPUT(..), BOOKING_OPTIONS_SCREEN_OUTPUT(..), DRIVER_DETAILS_SCREEN_OUTPUT(..), DRIVER_PROFILE_SCREEN_OUTPUT(..), DRIVER_RIDE_RATING_SCREEN_OUTPUT(..), ENTER_MOBILE_NUMBER_SCREEN_OUTPUT(..), ENTER_OTP_SCREEN_OUTPUT(..), FlowBT, GlobalState(..), HELP_AND_SUPPORT_SCREEN_OUTPUT(..), HOME_SCREENOUTPUT(..), MY_RIDES_SCREEN_OUTPUT(..), NAVIGATION_ACTIONS(..), NOTIFICATIONS_SCREEN_OUTPUT(..), NOTIFICATIONS_SCREEN_OUTPUT(..), NO_INTERNET_SCREEN_OUTPUT(..), PAYMENT_HISTORY_SCREEN_OUTPUT(..), PERMISSIONS_SCREEN_OUTPUT(..), POPUP_SCREEN_OUTPUT(..), REFERRAL_SCREEN_OUTPUT(..), REGISTRATION_SCREENOUTPUT(..), REPORT_ISSUE_CHAT_SCREEN_OUTPUT(..), RIDES_SELECTION_SCREEN_OUTPUT(..), RIDE_DETAIL_SCREENOUTPUT(..), SELECT_LANGUAGE_SCREEN_OUTPUT(..), SUBSCRIPTION_SCREEN_OUTPUT(..), ScreenStage(..), ScreenType(..), TRIP_DETAILS_SCREEN_OUTPUT(..), UPLOAD_ADHAAR_CARD_SCREENOUTPUT(..), UPLOAD_DRIVER_LICENSE_SCREENOUTPUT(..), VEHICLE_DETAILS_SCREEN_OUTPUT(..), WRITE_TO_US_SCREEN_OUTPUT(..), defaultGlobalState)
import Types.ModifyScreenState (modifyScreenState, updateStage)
import Constants as Constants
import Data.Function.Uncurried (runFn1)
import Helpers.FileProvider.Utils (stringifyJSON)


baseAppFlow :: Boolean -> Maybe Event -> FlowBT String Unit
baseAppFlow baseFlow event = do
    versionCode <- lift $ lift $ liftFlow $ getVersionCode
    checkVersion versionCode
    -- checkDateAndTime -- Need To Refactor
    (GlobalState state) <- getState
    cacheAppParameters versionCode
    when baseFlow $ void $ UI.splashScreen state.splashScreen
    let regToken = getValueToLocalStore REGISTERATION_TOKEN
    _ <- pure $ saveSuggestions "SUGGESTIONS" (getSuggestions "")
    _ <- pure $ saveSuggestionDefs "SUGGESTIONS_DEFINITIONS" (suggestionsDefinitions "")
    setValueToLocalStore CURRENCY (getValueFromConfig "currency")
    if isTokenValid regToken
      then case event of -- TODO:: Need to handle in generic way for all screens. Could be part of flow refactoring
        Just e -> 
          case e.data of
            "plans" | getValueToLocalNativeStore IS_RIDE_ACTIVE /= "true" && getValueToLocalNativeStore DISABLE_WIDGET /= "true" -> do
              lift $ lift $ doAff do liftEffect hideSplash
              setValueToLocalNativeStore REGISTERATION_TOKEN regToken
              updateAvailableAppsAndGoToSubs
            _ -> do
                  setValueToLocalNativeStore REGISTERATION_TOKEN regToken
                  getDriverInfoFlow
        
        Nothing -> do
            setValueToLocalNativeStore REGISTERATION_TOKEN regToken
            getDriverInfoFlow
      else loginFlow
    where
    cacheAppParameters :: Int -> FlowBT String Unit
    cacheAppParameters versionCode = do
      let bundle = bundleVersion unit
          driverId = (getValueToLocalStore DRIVER_ID)
      versionName <- lift $ lift $ liftFlow $ getVersionName
      void $ pure $ setCleverTapUserProp "App Version" versionName
      void $ pure $ setCleverTapUserProp "Bundle version" bundle
      void $ pure $ setCleverTapUserProp "Platform" os
      setValueToLocalStore VERSION_NAME versionName
      setValueToLocalStore BUNDLE_VERSION bundle
      setValueToLocalStore BASE_URL (getBaseUrl "dummy")
      setValueToLocalStore RIDE_REQUEST_BUFFER "-3"
      setValueToLocalStore SUGGESTIONS_ENABLED "false"
      setValueToLocalStore IS_BANNER_ACTIVE "True"
      setValueToLocalStore MESSAGES_DELAY "0"
      setValueToLocalStore SHOW_PAYMENT_MODAL "true"
      setValueToLocalNativeStore BUNDLE_VERSION bundle
      setValueToLocalNativeStore GPS_METHOD "CURRENT"
      setValueToLocalNativeStore MAKE_NULL_API_CALL "NO"
      setValueToLocalStore IS_DRIVER_AT_PICKUP "false"
      setValueToLocalStore DISABLE_WIDGET "false"
      when ((getValueToLocalStore SESSION_ID == "__failed") || (getValueToLocalStore SESSION_ID == "(null)")) $ do
        setValueToLocalStore SESSION_ID (generateSessionId unit)
      if(driverId == "__failed") then void $ lift $ lift $ setLogField "driver_id" $ encode ("null")
        else void $ lift $ lift $ setLogField "driver_id" $ encode (driverId)
      void $ lift $ lift $ setLogField "app_version" $ encode (show versionCode)
      void $ lift $ lift $ setLogField "bundle_version" $ encode (bundle)
      void $ lift $ lift $ setLogField "platform" $ encode (os)

checkVersion :: Int -> FlowBT String Unit
checkVersion versioncode = do
  when (getValueToLocalNativeStore IS_RIDE_ACTIVE /= "true" && versioncode < (getLatestAndroidVersion (getMerchant FunctionCall))) $ do
    lift $ lift $ doAff do liftEffect hideSplash
    modifyScreenState $ AppUpdatePopUpScreenType (\appUpdatePopUpScreenState → appUpdatePopUpScreenState {updatePopup = AppVersion})
    fl <- UI.handleAppUpdatePopUp
    case fl of
      UpdateNow -> checkVersion versioncode
      Later -> pure unit

checkDateAndTime :: FlowBT String Unit
checkDateAndTime = do
  _ <- pure $ setValueToLocalStore LAUNCH_DATE_SETTING "false"
  (CurrentDateAndTimeRes current)  <- Remote.currentDateAndTimeBT ""
  if(current == {timestamp : Nothing}) then pure unit
  else do
    let currentTimeStamp = current.timestamp
    let deviceDateTimeStamp = getCurrentTimeStamp unit
    let timeDiff = (((fromMaybe ( toNumber 0) currentTimeStamp )) - deviceDateTimeStamp)
    let timeDiffInMins = (timeDiff) / toNumber (1000)
    let absTimeDiff = if timeDiffInMins < toNumber 0 then timeDiffInMins * toNumber (-1) else timeDiffInMins
    if (absTimeDiff < toNumber 10 ) then do
      setValueToLocalStore IS_VALID_TIME "true"
      liftFlowBT $ unregisterDateAndTime
      liftFlowBT $ stopLocationPollingAPI
      liftFlowBT $ startLocationPollingAPI
    else
      when (absTimeDiff >= toNumber 10 ) do
        _ <- pure $ setValueToLocalStore LAUNCH_DATE_SETTING "true"
        setValueToLocalStore IS_VALID_TIME "false"
        modifyScreenState $ AppUpdatePopUpScreenType (\appUpdatePopUpScreenState -> appUpdatePopUpScreenState { updatePopup =DateAndTime })
        lift $ lift $ doAff do liftEffect hideSplash
        _ <- UI.handleAppUpdatePopUp
        checkDateAndTime

getLatestAndroidVersion :: Merchant -> Int
getLatestAndroidVersion merchant =
  case merchant of
    NAMMAYATRI -> 85
    YATRI -> 48
    YATRISATHI -> 16
    MOBILITY_PM -> 1
    MOBILITY_RS -> 1
    PASSCULTURE -> 1

ifNotRegistered :: Unit -> Boolean
ifNotRegistered _ = getValueToLocalStore REGISTERATION_TOKEN == "__failed"

isTokenValid :: String -> Boolean
isTokenValid = (/=) "__failed"

loginFlow :: FlowBT String Unit
loginFlow = do
  lift $ lift $ doAff do liftEffect hideSplash
  logField_ <- lift $ lift $ getLogFields
  _ <- pure $ spy "before hideSplash" ""
  runInternetCondition
  void $ pure $ setCleverTapUserProp "Preferred Language" $ getValueFromConfig "defaultLanguage"
  setValueToLocalStore LANGUAGE_KEY $ getValueFromConfig "defaultLanguage"
  languageType <- UI.chooseLanguage
  _ <- pure $ spy "after chooseLanguage" ""
  mobileNo <- UI.enterMobileNumber
  case mobileNo of
    GO_TO_ENTER_OTP updateState -> do
      liftFlowBT $ logEvent logField_ "ny_driver_otp_trigger"
      TriggerOTPResp triggerOtpResp <- Remote.triggerOTPBT (makeTriggerOTPReq updateState.data.mobileNumber)
      modifyScreenState $ EnterOTPScreenType (\enterOTPScreen → enterOTPScreen { data { tokenId = triggerOtpResp.authId}})
      enterOTPFlow
  where
    runInternetCondition = do
      internetAvailable <- lift $ lift $ liftFlow $ isInternetAvailable unit
      unless internetAvailable $ noInternetScreenFlow "INTERNET_ACTION"

enterOTPFlow :: FlowBT String Unit
enterOTPFlow = do
  action <- UI.enterOTP
  logField_ <- lift $ lift $ getLogFields
  case action of
    DRIVER_INFO_API_CALL updatedState -> do
      void $ lift $ lift $ loaderText (getString SENDING_OTP) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      (VerifyTokenResp resp) <- Remote.verifyTokenBT (makeVerifyOTPReq updatedState.data.otp) updatedState.data.tokenId
      _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_driver_verify_otp"
      _ <- pure $ metaLogEvent "ny_driver_verify_otp"
      let driverId = ((resp.person)^. _id)
      if(driverId == "__failed") then do
        _ <- lift $ lift $ setLogField "driver_id" $ encode ("null")
        pure unit
        else do
          _ <- lift $ lift $ setLogField "driver_id" $ encode (driverId)
          pure unit
      setValueToLocalStore DRIVER_ID driverId
      void $ pure $ setCleverTapUserData "Identity" (getValueToLocalStore DRIVER_ID)
      setValueToLocalStore REGISTERATION_TOKEN resp.token -- add from response
      void $ lift $ lift $ toggleLoader false
      (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT $ mkUpdateDriverInfoReq ""
      getDriverInfoFlow
    RETRY updatedState -> do
      modifyScreenState $ EnterOTPScreenType (\enterOTPScreen -> updatedState)
      (ResendOTPResp resp_resend) <- Remote.resendOTPBT updatedState.data.tokenId
      pure $ toast $ getString OTP_HAS_BEEN_RESENT
      modifyScreenState $ EnterOTPScreenType (\enterOTPScreen → enterOTPScreen { data { tokenId = resp_resend.authId, attemptCount = resp_resend.attempts}})
      enterOTPFlow

-- TODO :: As per the api response check the floe for driver registration
registrationStatusFlow :: FlowBT String Unit
registrationStatusFlow = do
  let startTime = getTime unit
  -- resp <- Remote.driverRegistrationStatus (DriverResgistrationStatusReq {})
  let endTime = getTime unit
  let registrationStatus = driverRegistrationStatus
  if registrationStatus then currentRideFlow else permissionsScreenFlow

-- TODO :: modify this function as per the response. Currently DEFINED for testing purpose only
driverRegistrationStatus :: Boolean
driverRegistrationStatus = do
  (getValueToLocalStore TEST_FLOW_FOR_REGISTRATOION) == "COMPLETED"

getDriverInfoFlow :: FlowBT String Unit
getDriverInfoFlow = do
  _ <- pure $ delay $ Milliseconds 1.0
  _ <- pure $ printLog "Registration token" (getValueToLocalStore REGISTERATION_TOKEN)
  getDriverInfoApiResp <- lift $ lift $ Remote.getDriverInfoApi (GetDriverInfoReq{})
  case getDriverInfoApiResp of
    Right getDriverInfoResp -> do
      let (GetDriverInfoResp getDriverInfoResp) = getDriverInfoResp
      modifyScreenState $ GlobalPropsType (\globalProps -> globalProps {driverInformation = (GetDriverInfoResp getDriverInfoResp)})
      setValueToLocalStore DRIVER_SUBSCRIBED if isNothing getDriverInfoResp.autoPayStatus then "false" else "true"
      let (OrganizationInfo organization) = getDriverInfoResp.organization
      modifyScreenState $ ApplicationStatusScreenType (\applicationStatusScreen -> applicationStatusScreen {props{alternateNumberAdded = isJust getDriverInfoResp.alternateNumber}})
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props{autoPayBanner = isNothing getDriverInfoResp.autoPayStatus || getDriverInfoResp.autoPayStatus /= Just "ACTIVE"}})
      case getDriverInfoResp.mobileNumber of
          Just value -> do 
            _ <- pure $ setCleverTapUserData "Phone" ("+91" <> value)
            void $ pure $ setCleverTapUserProp "Mobile_Number" ("91" <> value)
          Nothing -> pure unit

      let middleName = case getDriverInfoResp.middleName of
                        Just ""  -> ""
                        Just name -> (" " <> name)
                        Nothing -> ""
          lastName   = case getDriverInfoResp.lastName of
                        Just "" -> ""
                        Just name -> (" " <> name)
                        Nothing -> ""
          name = getDriverInfoResp.firstName <> middleName <> lastName
      void $ pure $ setCleverTapUserData "Name" name

      when (fromMaybe "UNKNOWN" (getDriverInfoResp.gender) /= "UNKNOWN") $ do
              case getDriverInfoResp.gender of
                  Just value -> void $ pure $ setCleverTapUserData "gender" value
                  Nothing -> pure unit

      case getDriverInfoResp.alternateNumber of
        Just value -> void $ pure $ setCleverTapUserData "Alternate Number" ("+91" <> value)
        Nothing -> pure unit

      case getDriverInfoResp.numberOfRides of
        Just value -> void $ pure $ setCleverTapUserData "Total Rides" (show $ value)
        Nothing -> pure unit

      void $ pure $ setCleverTapUserData "Identity" (getValueToLocalStore DRIVER_ID)

      case getDriverInfoResp.rating of
        Just value -> void $ pure $ setCleverTapUserData "Rating" (show $ value)
        Nothing -> pure unit

      let (Vehicle linkedVehicle) = (fromMaybe dummyVehicleObject getDriverInfoResp.linkedVehicle)
      void $ pure $ setCleverTapUserProp "Vehicle Variant" linkedVehicle.variant
      setValueToLocalStore VEHICLE_VARIANT linkedVehicle.variant
      setValueToLocalStore NEGOTIATION_UNIT $ getNegotiationUnit linkedVehicle.variant
      case getDriverInfoResp.blocked of
        Just value -> void $ pure $ setCleverTapUserProp "Blocked" (show $ value)
        Nothing -> pure unit

      case getDriverInfoResp.mode of
        Just currentMode -> void $ pure $ setCleverTapUserProp "Mode" currentMode
        Nothing -> pure unit

      if fromMaybe 0 getDriverInfoResp.numberOfRides > 0 then void $ pure $ setCleverTapUserProp "First ride taken" "true"
        else void $ pure $ setCleverTapUserProp "First ride taken" "false"
      
      _ <- pure $ setCleverTapUserProp "Plan Subscription Status" if isNothing getDriverInfoResp.autoPayStatus then "FALSE" else "TRUE"

      let dbClientVersion = getDriverInfoResp.clientVersion
      let dbBundleVersion = getDriverInfoResp.bundleVersion
      updateDriverVersion dbClientVersion dbBundleVersion
      if getDriverInfoResp.enabled then do
        if(getValueToLocalStore IS_DRIVER_ENABLED == "false") then do
          _ <- pure $ firebaseLogEvent "ny_driver_enabled"
          _ <- pure $ metaLogEvent "ny_driver_enabled"
          pure unit
        else
          pure unit
        setValueToLocalStore IS_DRIVER_ENABLED "true"
        let (Vehicle linkedVehicle) = (fromMaybe dummyVehicleObject getDriverInfoResp.linkedVehicle)
        let optionArr = genders FunctionCall
            element = fromMaybe "UNKNOWN" getDriverInfoResp.gender
            reqIndex = getGenderIndex element optionArr
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data = homeScreen.data {driverName = getDriverInfoResp.firstName, vehicleType = linkedVehicle.variant ,  driverAlternateMobile =getDriverInfoResp.alternateNumber   }
                                                                           , props {statusOnline =  if (isJust getDriverInfoResp.mode) then
                                                                                                        (any( _ == (updateDriverStatus getDriverInfoResp.active))[Online, Silent])
                                                                                                    else getDriverInfoResp.active
                                                                                    }
                                                                            }
                                                )
        modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> driverDetailsScreen
         {data = driverDetailsScreen.data { driverName = getDriverInfoResp.firstName,
        driverVehicleType = linkedVehicle.variant,
        driverRating = getDriverInfoResp.rating,
       -- base64Image = updatedState.data.base64Image,
        driverMobile = getDriverInfoResp.mobileNumber,
        driverAlternateMobile = getDriverInfoResp.alternateNumber,
        driverGender = getGenderState getDriverInfoResp.gender,
        genderSelectionModal{activeIndex = reqIndex}
        }})
        modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen {data {driverName = getDriverInfoResp.firstName, driverVehicleType = linkedVehicle.variant, driverRating = getDriverInfoResp.rating , driverAlternateNumber = getDriverInfoResp.alternateNumber, driverGender = getGenderState getDriverInfoResp.gender,capacity = fromMaybe 2 linkedVehicle.capacity, downgradeOptions = getDowngradeOptions linkedVehicle.variant}})
        _ <- liftFlowBT $ runEffectFn1 consumeBP unit
        permissionsGiven <- checkAll3Permissions
        if permissionsGiven
          then currentRideFlow
          else permissionsScreenFlow
        else do
          setValueToLocalStore IS_DRIVER_ENABLED "false"
          if getDriverInfoResp.verified then do
            setValueToLocalStore IS_DRIVER_VERIFIED "true"
            applicationSubmittedFlow "ApprovedScreen"
            else do
              setValueToLocalStore IS_DRIVER_VERIFIED "false"
              onBoardingFlow
    Left errorPayload -> do
      if ((decodeErrorCode errorPayload.response.errorMessage) == "VEHICLE_NOT_FOUND" || (decodeErrorCode errorPayload.response.errorMessage) == "DRIVER_INFORMATON_NOT_FOUND")
        then onBoardingFlow
        else do
          _ <- pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
          if getValueToLocalStore IS_DRIVER_ENABLED == "true" then do
            permissionsGiven <- checkAll3Permissions
            if permissionsGiven then
              currentRideFlow
              else permissionsScreenFlow
            else do
              if getValueToLocalStore IS_DRIVER_VERIFIED == "true" then do
                applicationSubmittedFlow "ApprovedScreen"
                else do
                onBoardingFlow


onBoardingFlow :: FlowBT String Unit
onBoardingFlow = do
  _ <- pure $ hideKeyboardOnNavigation true
  (DriverRegistrationStatusResp resp ) <- driverRegistrationStatusBT (DriverRegistrationStatusReq { })
  lift $ lift $ doAff do liftEffect hideSplash
  GlobalState globalState <- getState
  if (resp.dlVerificationStatus == "NO_DOC_AVAILABLE" && resp.rcVerificationStatus == "NO_DOC_AVAILABLE") then do
    flow <- UI.registration
    case flow of
      UPLOAD_DRIVER_LICENSE -> do
        modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { data {rcVerificationStatus = resp.rcVerificationStatus}}
        uploadDrivingLicenseFlow
    else if(resp.dlVerificationStatus == "NO_DOC_AVAILABLE") then do
      modifyScreenState $ UploadDrivingLicenseScreenStateType (\uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { data {rcVerificationStatus = resp.rcVerificationStatus}})
      uploadDrivingLicenseFlow
      else if (resp.rcVerificationStatus == "NO_DOC_AVAILABLE") then addVehicleDetailsflow false
        else applicationSubmittedFlow "StatusScreen"

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
      let (UpdateDriverInfoReq initialData) = mkUpdateDriverInfoReq ""
          requiredData = initialData{clientVersion = Just (Version clientVersion),bundleVersion = Just (Version bundleVersion)}
      (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT (UpdateDriverInfoReq requiredData)
      pure unit
    else pure unit
  else pure unit

aadhaarVerificationFlow :: FlowBT String Unit
aadhaarVerificationFlow = do
  lift $ lift $ doAff do liftEffect hideSplash
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
              _ <- pure $ toast $ decodeErrorMessage errorPayload.response.errorMessage
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
          if resp.code == 200 then if state.props.fromHomeScreen then getDriverInfoFlow else onBoardingFlow
            else do
              _ <- pure $ toast $ getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
              modifyScreenState $ AadhaarVerificationScreenType (\_ -> state{props{currentStage = EnterAadhaar, btnActive = false}})
              aadhaarVerificationFlow
        Left errorPayload -> do
          let stage = if (decodeErrorCode errorPayload.response.errorMessage) == "INVALID_OTP" then VerifyAadhaar else AadhaarDetails
          _ <- pure if (decodeErrorCode errorPayload.response.errorMessage) == "INVALID_OTP" then toast $ getString INVALID_OTP else unit
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
              _ <- pure $ toast $ getString VERIFICATION_FAILED
              modifyScreenState $ AadhaarVerificationScreenType (\_ -> state{props{currentStage = EnterAadhaar}})
              aadhaarVerificationFlow
        Left errorPayload -> do
          let errorCode = decodeErrorCode errorPayload.response.errorMessage
          case errorCode of
            "INVALID_AADHAAR" -> do
              _ <- pure $ toast $ getString VERIFICATION_FAILED
              modifyScreenState $ AadhaarVerificationScreenType (\_ -> state{props{currentStage = EnterAadhaar,showErrorAadhaar = true, btnActive = false}})
            "GENERATE_AADHAAR_OTP_EXCEED_LIMIT" -> pure $ toast $ getString OTP_RESEND_LIMIT_EXCEEDED
            _ -> pure $ toast $ decodeErrorMessage errorPayload.response.errorMessage
          modifyScreenState $ AadhaarVerificationScreenType (\aadhaarVerification -> aadhaarVerification{props{currentStage = EnterAadhaar, btnActive = false}})
          aadhaarVerificationFlow
    GO_TO_HOME_FROM_AADHAAR -> do
      (GlobalState state) <- getState
      modifyScreenState $ AadhaarVerificationScreenType (\_ -> state.aadhaarVerificationScreen)
      getDriverInfoFlow
    LOGOUT_FROM_AADHAAR -> do
      (LogOutRes resp) <- Remote.logOutBT LogOutReq
      deleteValueFromLocalStore REGISTERATION_TOKEN
      deleteValueFromLocalStore LANGUAGE_KEY
      deleteValueFromLocalStore VERSION_NAME
      deleteValueFromLocalStore BASE_URL
      deleteValueFromLocalStore TEST_FLOW_FOR_REGISTRATOION
      deleteValueFromLocalStore IS_DRIVER_ENABLED
      deleteValueFromLocalStore BUNDLE_VERSION
      deleteValueFromLocalStore DRIVER_ID
      deleteValueFromLocalStore SET_ALTERNATE_TIME
      _ <- pure $ firebaseLogEvent "logout"
      pure $ factoryResetApp ""
      loginFlow
    SEND_UNVERIFIED_AADHAAR_DATA state -> do
      void $ lift $ lift $ toggleLoader true
      unVerifiedAadhaarDataResp <- lift $ lift $ Remote.unVerifiedAadhaarData state.data.driverName state.data.driverGender state.data.driverDob
      case unVerifiedAadhaarDataResp of
        Right resp -> do
          void $ lift $ lift $ toggleLoader false
          if state.props.fromHomeScreen then getDriverInfoFlow else onBoardingFlow
        Left errorPayload -> do
          void $ lift $ lift $ toggleLoader false
          _ <- pure $ toast $ decodeErrorMessage errorPayload.response.errorMessage
          aadhaarVerificationFlow


uploadDrivingLicenseFlow :: FlowBT String Unit
uploadDrivingLicenseFlow = do
  (GlobalState state) <- getState
  logField_ <- lift $ lift $ getLogFields
  flow <- UI.uploadDrivingLicense
  case flow of
    ADD_VEHICLE_DETAILS_SCREEN state -> do
      if (state.data.imageFront == "IMAGE_NOT_VALIDATED") then do
        modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { data {dateOfIssue = Just ""}}
        uploadDrivingLicenseFlow
        else do
          void $ lift $ lift $ loaderText (getString VALIDATING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
          void $ lift $ lift $ toggleLoader true
          registerDriverDLResp <- lift $ lift $ Remote.registerDriverDL (makeDriverDLReq state.data.driver_license_number state.data.dob state.data.dateOfIssue state.data.imageIDFront state.data.imageIDFront)
          case registerDriverDLResp of
            Right (DriverDLResp resp) -> do
              void $ lift $ lift $ toggleLoader false
              liftFlowBT $ logEvent logField_ "ny_driver_submit_dl_details"
              setValueToLocalStore DOCUMENT_UPLOAD_TIME (getCurrentUTC "")
              if state.data.rcVerificationStatus /= "NO_DOC_AVAILABLE" then applicationSubmittedFlow "StatusScreen" else addVehicleDetailsflow false
            Left errorPayload -> do
              void $ lift $ lift $ toggleLoader false
              modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { data {dateOfIssue = Just ""}}
              if errorPayload.code == 400 || (errorPayload.code == 500 && (decodeErrorCode errorPayload.response.errorMessage) == "UNPROCESSABLE_ENTITY") then do
                let correspondingErrorMessage =  Remote.getCorrespondingErrorMessage errorPayload
                modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { props {errorVisibility = true}, data {errorMessage = correspondingErrorMessage}}
                uploadDrivingLicenseFlow
                else do
                  _ <- pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
                  uploadDrivingLicenseFlow

    LOGOUT_ACCOUNT -> do
      (LogOutRes resp) <- Remote.logOutBT LogOutReq
      deleteValueFromLocalStore REGISTERATION_TOKEN
      deleteValueFromLocalStore VERSION_NAME
      deleteValueFromLocalStore BASE_URL
      deleteValueFromLocalStore TEST_FLOW_FOR_REGISTRATOION
      deleteValueFromLocalStore IS_DRIVER_ENABLED
      deleteValueFromLocalStore BUNDLE_VERSION
      deleteValueFromLocalStore DRIVER_ID
      deleteValueFromLocalStore SET_ALTERNATE_TIME

      pure $ factoryResetApp ""
      _ <- lift $ lift $ liftFlow $ logEvent logField_ "logout"
      loginFlow

    VALIDATE_IMAGE_API state -> do
      void $ lift $ lift $ loaderText (getString VALIDATING) (getString PLEASE_WAIT_WHILE_VALIDATING_THE_IMAGE)
      void $ lift $ lift $ toggleLoader true
      validateImageResp <- lift $ lift $ Remote.validateImage (makeValidateImageReq (if state.props.clickedButtonType == "front" then state.data.imageFront else state.data.imageBack) "DriverLicense")
      case validateImageResp of
       Right (ValidateImageRes resp) -> do
        void $ lift $ lift $ toggleLoader false
        liftFlowBT $ logEvent logField_ "ny_driver_dl_photo_confirmed"
        modifyScreenState $ UploadDrivingLicenseScreenStateType (\uploadDrivingLicenseScreen -> state{props{errorVisibility = false}})
        if state.props.clickedButtonType == "front" then
          modifyScreenState $ UploadDrivingLicenseScreenStateType (\uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { data {imageIDFront = resp.imageId}})
          else
            modifyScreenState $ UploadDrivingLicenseScreenStateType (\uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { data {imageIDBack = resp.imageId}})
        uploadDrivingLicenseFlow
       Left errorPayload -> do
        void $ lift $ lift $ toggleLoader false
        if errorPayload.code == 429 && (decodeErrorCode errorPayload.response.errorMessage) == "IMAGE_VALIDATION_EXCEED_LIMIT" then do
          modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { props { openGenericMessageModal = true}}
          uploadDrivingLicenseFlow
          else if errorPayload.code == 400 || (errorPayload.code == 500 && (decodeErrorCode errorPayload.response.errorMessage) == "UNPROCESSABLE_ENTITY") then do
            let correspondingErrorMessage =  Remote.getCorrespondingErrorMessage errorPayload
            modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { props {errorVisibility = true}, data {errorMessage = correspondingErrorMessage, imageFrontUrl = state.data.imageFront, imageFront = "IMAGE_NOT_VALIDATED"}}
            uploadDrivingLicenseFlow
            else do
              _ <- pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
              modifyScreenState $ UploadDrivingLicenseScreenStateType $ \uploadDrivingLicenseScreen -> uploadDrivingLicenseScreen { data {imageFrontUrl = state.data.imageFront, imageFront = "IMAGE_NOT_VALIDATED"}}
              uploadDrivingLicenseFlow

    GOTO_VEHICLE_DETAILS_SCREEN -> addVehicleDetailsflow false
    GOTO_ONBOARDING_FLOW -> onBoardingFlow


addVehicleDetailsflow :: Boolean -> FlowBT String Unit
addVehicleDetailsflow addRcFromProf = do
  logField_ <- lift $ lift $ getLogFields
  modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen  -> addVehicleDetailsScreen{props{addRcFromProfile = addRcFromProf }})
  flow <- UI.addVehicleDetails
  case flow of
    GO_TO_APPLICATION_SCREEN state -> do
      if (state.data.rcImageID == "IMAGE_NOT_VALIDATED") then do
        modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { data { dateOfRegistration = Just ""},props{ addRcFromProfile = addRcFromProf}}
        addVehicleDetailsflow state.props.addRcFromProfile
        else do
          void $ lift $ lift $ loaderText (getString VALIDATING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
          void $ lift $ lift $ toggleLoader true
          registerDriverRCResp <- lift $ lift $ Remote.registerDriverRC (makeDriverRCReq state.data.vehicle_registration_number state.data.rcImageID state.data.dateOfRegistration true)
          case registerDriverRCResp of
            Right (DriverRCResp resp) -> do
              void $ lift $ lift $ toggleLoader false
              _ <- pure $ toast $ getString RC_ADDED_SUCCESSFULLY
              liftFlowBT $ logEvent logField_ "ny_driver_submit_rc_details"
              setValueToLocalStore DOCUMENT_UPLOAD_TIME (getCurrentUTC "")
              (GlobalState state') <- getState
              let profileState = state'.driverProfileScreen
              if (null profileState.data.rcDataArray) then applicationSubmittedFlow "StatusScreen"
              else do
                modifyScreenState $ DriverProfileScreenStateType $ \driverProfileScreen -> driverProfileScreen { props { screenType = ST.VEHICLE_DETAILS}}
                driverProfileFlow
            Left errorPayload -> do
              void $ lift $ lift $ toggleLoader false
              modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { data { dateOfRegistration = Just ""}}
              if errorPayload.code == 400 || (errorPayload.code == 500 && (decodeErrorCode errorPayload.response.errorMessage) == "UNPROCESSABLE_ENTITY") then do
                let correspondingErrorMessage =  Remote.getCorrespondingErrorMessage errorPayload
                modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { props {errorVisibility = true}, data {errorMessage = correspondingErrorMessage}}
                addVehicleDetailsflow state.props.addRcFromProfile
                else do
                  _ <- pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
                  addVehicleDetailsflow state.props.addRcFromProfile

    VALIDATE_IMAGE_API_CALL state -> do
      void $ lift $ lift $ loaderText (getString VALIDATING) (getString PLEASE_WAIT_WHILE_VALIDATING_THE_IMAGE)
      void $ lift $ lift $ toggleLoader true
      validateImageResp <- lift $ lift $ Remote.validateImage (makeValidateImageReq state.data.rc_base64 "VehicleRegistrationCertificate")
      case validateImageResp of
       Right (ValidateImageRes resp) -> do
        void $ lift $ lift $ toggleLoader false
        liftFlowBT $ logEvent logField_ "ny_driver_rc_photo_confirmed"
        modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen -> state)
        modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen -> addVehicleDetailsScreen {data { rcImageID = resp.imageId}})
        addVehicleDetailsflow state.props.addRcFromProfile
       Left errorPayload -> do
        void $ lift $ lift $ toggleLoader false
        if errorPayload.code == 429 && (decodeErrorCode errorPayload.response.errorMessage) == "IMAGE_VALIDATION_EXCEED_LIMIT" then do
          modifyScreenState $ AddVehicleDetailsScreenStateType (\addVehicleDetailsScreen -> addVehicleDetailsScreen {props { limitExceedModal = true}})
          addVehicleDetailsflow state.props.addRcFromProfile
          else if errorPayload.code == 400 || (errorPayload.code == 500 && (decodeErrorCode errorPayload.response.errorMessage) == "UNPROCESSABLE_ENTITY") then do
            let correspondingErrorMessage =  Remote.getCorrespondingErrorMessage errorPayload
            modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { props {errorVisibility = true}, data {errorMessage = correspondingErrorMessage , rcImageID = "IMAGE_NOT_VALIDATED" }}
            addVehicleDetailsflow state.props.addRcFromProfile 
            else do
              _ <- pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
              modifyScreenState $ AddVehicleDetailsScreenStateType $ \addVehicleDetailsScreen -> addVehicleDetailsScreen { data {rcImageID = "IMAGE_NOT_VALIDATED" }}
              addVehicleDetailsflow state.props.addRcFromProfile

    LOGOUT_USER -> do
      (LogOutRes resp) <- Remote.logOutBT LogOutReq
      deleteValueFromLocalStore REGISTERATION_TOKEN
      deleteValueFromLocalStore VERSION_NAME
      deleteValueFromLocalStore BASE_URL
      deleteValueFromLocalStore TEST_FLOW_FOR_REGISTRATOION
      deleteValueFromLocalStore IS_DRIVER_ENABLED
      deleteValueFromLocalStore BUNDLE_VERSION
      deleteValueFromLocalStore DRIVER_ID
      deleteValueFromLocalStore SET_ALTERNATE_TIME
      pure $ factoryResetApp ""
      _ <- lift $ lift $ liftFlow $ logEvent logField_  "logout"
      loginFlow

    REFER_API_CALL state -> do
      void $ lift $ lift $ loaderText (getString VALIDATING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      referDriverResponse <- lift $ lift $ Remote.referDriver (makeReferDriverReq state.data.referral_mobile_number)
      case referDriverResponse of
        Right (ReferDriverResp resp) -> do
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

applicationSubmittedFlow :: String -> FlowBT String Unit
applicationSubmittedFlow screenType = do
  lift $ lift $ doAff do liftEffect hideSplash
  logField_ <- lift $ lift $ getLogFields
  action <- UI.applicationStatus screenType
  setValueToLocalStore TEST_FLOW_FOR_REGISTRATOION "COMPLETED"
  case action of
    GO_TO_HOME_FROM_APPLICATION_STATUS -> permissionsScreenFlow
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
        Right (DriverAlternateNumberOtpResp resp) -> do
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
    LOGOUT_ACCOUT -> do
      (LogOutRes resp) <- Remote.logOutBT LogOutReq
      deleteValueFromLocalStore REGISTERATION_TOKEN
      deleteValueFromLocalStore VERSION_NAME
      deleteValueFromLocalStore BASE_URL
      deleteValueFromLocalStore TEST_FLOW_FOR_REGISTRATOION
      deleteValueFromLocalStore IS_DRIVER_ENABLED
      deleteValueFromLocalStore BUNDLE_VERSION
      deleteValueFromLocalStore DRIVER_ID
      deleteValueFromLocalStore SET_ALTERNATE_TIME
      pure $ factoryResetApp ""
      _ <- lift $ lift $ liftFlow $ logEvent logField_ "logout"
      loginFlow

driverProfileFlow :: FlowBT String Unit
driverProfileFlow = do
  logField_ <- lift $ lift $ getLogFields
  _ <- pure $ delay $ Milliseconds 1.0
  _ <- pure $ printLog "Registration token" (getValueToLocalStore REGISTERATION_TOKEN)
  action <- UI.driverProfileScreen
  case action of
    GO_TO_HOME_FROM_PROFILE -> homeScreenFlow
    GO_TO_REFERRAL_SCREEN_FROM_DRIVER_PROFILE_SCREEN -> referralScreenFlow
    DRIVER_DETAILS_SCREEN -> driverDetailsFlow
    VEHICLE_DETAILS_SCREEN -> vehicleDetailsFlow
    ABOUT_US_SCREEN -> aboutUsFlow
    SELECT_LANGUAGE_SCREEN -> do
      liftFlowBT $ logEvent logField_ "ny_driver_language_select" 
      selectLanguageFlow
    ON_BOARDING_FLOW -> onBoardingFlow
    GO_TO_LOGOUT -> do
      liftFlowBT $ logEvent logField_ "ny_driver_logout"
      (LogOutRes resp) <- Remote.logOutBT LogOutReq
      removeChatService ""
      lift $ lift $ liftFlow $ stopLocationPollingAPI
      deleteValueFromLocalStore REGISTERATION_TOKEN
      deleteValueFromLocalStore VERSION_NAME
      deleteValueFromLocalStore BASE_URL
      deleteValueFromLocalStore TEST_FLOW_FOR_REGISTRATOION
      deleteValueFromLocalStore IS_DRIVER_ENABLED
      deleteValueFromLocalStore DRIVER_STATUS
      deleteValueFromLocalStore BUNDLE_VERSION
      deleteValueFromLocalStore DRIVER_ID
      deleteValueFromLocalStore SET_ALTERNATE_TIME
      pure $ factoryResetApp ""
      _ <- lift $ lift $ liftFlow $ logEvent logField_ "logout"
      loginFlow
    HELP_AND_SUPPORT_SCREEN -> do
      liftFlowBT $ logEvent logField_ "ny_driver_help"
      let language = ( case getValueToLocalStore LANGUAGE_KEY of
                         "HI_IN" -> "hi"
                         "KN_IN" -> "kn"
                         "TA_IN" -> "ta"
                         _       -> "en"
                     )
      let categoryOrder = ["LOST_AND_FOUND", "RIDE_RELATED", "APP_RELATED", "FARE"]
      let compareByOrder a b = compare (fromMaybe (length categoryOrder) $ elemIndex a.categoryAction categoryOrder) (fromMaybe (length categoryOrder) $ elemIndex b.categoryAction categoryOrder)
      (GetCategoriesRes response) <- Remote.getCategoriesBT language
      let temp = (map (\(Category x) ->
                          { categoryName :
                              if (language == "en")
                              then
                                joinWith " " (map (\catName ->
                                  let { before, after } = splitAt 1 catName
                                  in (toUpper before <> after)
                                ) (split (Pattern " ") x.category))
                              else x.category
                          , categoryId       : x.issueCategoryId
                          , categoryAction   : x.label
                          , categoryImageUrl : x.logoUrl
                          }) response.categories)
      let categories' = sortBy compareByOrder temp
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen { data { categories = categories' } } )
      helpAndSupportFlow
    GO_TO_DRIVER_HISTORY_SCREEN -> do
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> rideHistoryScreen{offsetValue = 0, currentTab = "COMPLETED"})
      myRidesScreenFlow
    GO_TO_EDIT_BANK_DETAIL_SCREEN -> editBankDetailsFlow
    NOTIFICATIONS_SCREEN -> notificationFlow
    GO_TO_BOOKING_OPTIONS_SCREEN state-> do
      modifyScreenState $ BookingOptionsScreenType (\bookingOptions -> bookingOptions{data{vehicleType = state.data.driverVehicleType, vehicleNumber = state.data.vehicleRegNumber, vehicleName = state.data.vehicleModelName, vehicleCapacity = state.data.capacity, downgradeOptions = ((downgradeOptionsConfig state.data.vehicleSelected) <$> state.data.downgradeOptions)}})
      bookingOptionsFlow
    GO_TO_ACTIVATE_OR_DEACTIVATE_RC state -> do
      res <- lift $ lift $ Remote.makeRcActiveOrInactive (Remote.makeRcActiveOrInactiveReq (not state.data.isRCActive) (state.data.rcNumber))
      case res of
        Right (MakeRcActiveOrInactiveResp response) -> do
          pure $ toast $ if state.data.isRCActive then "RC-"<>state.data.rcNumber<>" "<> (getString DEACTIVATED) else "RC-"<>state.data.rcNumber<> (getString IS_ACTIVE_NOW)
          if state.data.isRCActive then do 
            changeDriverStatus Offline 
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {statusOnline = false, driverStatusSet = Offline, showOffer = false, rcActive = false, rcDeactivePopup = true}})
          else modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {rcActive = true, rcDeactivePopup = false}}) 
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
            getDriverInfoApiResp <- lift $ lift $ Remote.getDriverInfoApi (GetDriverInfoReq{})
            case getDriverInfoApiResp of
              Right getDriverInfoResp -> do
                let (GetDriverInfoResp getDriverInfoResp) = getDriverInfoResp
                let (Vehicle linkedVehicle) = (fromMaybe dummyVehicleObject getDriverInfoResp.linkedVehicle)
                modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen {data {driverVehicleType = linkedVehicle.variant, capacity = fromMaybe 2 linkedVehicle.capacity, downgradeOptions = getDowngradeOptions linkedVehicle.variant, vehicleSelected = getDowngradeOptionsSelected (GetDriverInfoResp getDriverInfoResp)}})
              Left errorPayload -> do
                pure unit
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
      modifyScreenState $ AddVehicleDetailsScreenStateType (\_ -> defaultEpassState.addVehicleDetailsScreen)
      addVehicleDetailsflow true
    GO_TO_CALL_DRIVER state -> do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> state {props = driverProfileScreen.props { alreadyActive = false}})
      _ <- Remote.callDriverToDriverBT  state.data.rcNumber
      pure $ toast $ (getString CALL_REQUEST_HAS_BEEN_PLACED)
      driverProfileFlow


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
       let toast_value = if (state.props.isEditAlternateMobile == false) then (getString NUMBER_ADDED_SUCCESSFULLY) else (getString NUMBER_EDITED_SUCCESSFULLY)
           finalAlternateMobileNumber = state.data.driverEditAlternateMobile
       getVerifyAlternateMobileOtpResp <- lift $ lift $ Remote.verifyAlternateNumberOTP (makeVerifyAlternateNumberOtpRequest (state.props.alternateMobileOtp))
       case getVerifyAlternateMobileOtpResp of
         Right (DriverAlternateNumberOtpResp resp) -> do
              pure $ toast (toast_value)
              liftFlowBT $ logEvent logField_ "ny_driver_added_alternate_number"
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
          Right (RemoveAlternateNumberResp resp) -> do
                pure $ toast (getString NUMBER_REMOVED_SUCCESSFULLY)
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data = homeScreen.data {  driverAlternateMobile = Nothing  }})
                modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen {data {driverAlternateNumber = Nothing}, props{checkAlternateNumber = true, numberExistError = false, alternateNumberView = false }})
                driverProfileFlow
          Left errorPayload -> do
               _ <- pure $ toast $ (decodeErrorCode errorPayload.response.errorMessage)
               modifyScreenState $ DriverProfileScreenStateType $ \driverProfileScreen -> state
               driverProfileFlow

    DRIVER_GENDER1 state -> do
      let genderSelected = state.data.driverGender
      let (UpdateDriverInfoReq initialData) = mkUpdateDriverInfoReq ""
          requiredData = initialData{gender = genderSelected}
      (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT (UpdateDriverInfoReq requiredData)
      pure $ toast (getString GENDER_UPDATED)
      modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> state { data {driverGender = genderSelected}})
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props = homeScreen.props {  showGenderBanner = false}})
      setValueToLocalStore IS_BANNER_ACTIVE "False"
      driverProfileFlow

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
       let toast_value = if (state.props.isEditAlternateMobile == false) then (getString NUMBER_ADDED_SUCCESSFULLY) else (getString NUMBER_EDITED_SUCCESSFULLY)
           finalAlternateMobileNumber = state.data.driverEditAlternateMobile
       getVerifyAlternateMobileOtpResp <- lift $ lift $ Remote.verifyAlternateNumberOTP (makeVerifyAlternateNumberOtpRequest (state.props.alternateMobileOtp))
       case getVerifyAlternateMobileOtpResp of
         Right (DriverAlternateNumberOtpResp resp) -> do
              pure $ toast (toast_value)
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
          Right (RemoveAlternateNumberResp resp) -> do
                pure $ toast (getString NUMBER_REMOVED_SUCCESSFULLY)
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data = homeScreen.data {  driverAlternateMobile = Nothing  }})
                modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen {data {driverAlternateNumber = Nothing}, props{checkAlternateNumber = true, numberExistError = false, alternateNumberView = false }})
                driverProfileFlow
          Left errorPayload -> do
               _ <- pure $ toast $ (decodeErrorCode errorPayload.response.errorMessage)
               modifyScreenState $ DriverProfileScreenStateType $ \driverProfileScreen -> state
               driverProfileFlow

    DRIVER_GENDER1 state -> do
      let genderSelected = state.data.driverGender
      let (UpdateDriverInfoReq initialData) = mkUpdateDriverInfoReq ""
          requiredData = initialData{gender = genderSelected}
      (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT (UpdateDriverInfoReq requiredData)
      pure $ toast (getString GENDER_UPDATED)
      modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> state { data {driverGender = genderSelected}})
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props = homeScreen.props {  showGenderBanner = false}})
      setValueToLocalStore IS_BANNER_ACTIVE "False"
      driverProfileFlow
    UPDATE_LANGUAGES language -> do
      let (UpdateDriverInfoReq initialData) = mkUpdateDriverInfoReq ""
          requiredData = initialData{languagesSpoken = Just language}
      (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT (UpdateDriverInfoReq requiredData)
      modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen { props {updateLanguages = false}})
      driverProfileFlow


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
                  modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> updatedState {data{ driverAlternateMobile = (if(updatedState.props.isEditAlternateMobile) then updatedState.data.driverAlternateMobile else Nothing), driverEditAlternateMobile = Nothing} , props{  otpIncorrect = false ,otpAttemptsExceeded = false ,keyboardModalType = NONE , alternateMobileOtp = "",checkAlternateNumber =(updatedState.props.isEditAlternateMobile == false) }})
                  driverDetailsFlow


    VERIFY_OTP state -> do
       let toast_value = if (state.props.isEditAlternateMobile == false) then (getString NUMBER_ADDED_SUCCESSFULLY) else (getString NUMBER_EDITED_SUCCESSFULLY)
           finalAlternateMobileNumber = state.data.driverEditAlternateMobile
       getVerifyAlternateMobileOtpResp <- lift $ lift $ Remote.verifyAlternateNumberOTP (makeVerifyAlternateNumberOtpRequest (state.props.alternateMobileOtp))
       case getVerifyAlternateMobileOtpResp of
         Right (DriverAlternateNumberOtpResp resp) -> do
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
                modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> state {data{ driverAlternateMobile = (if(state.props.isEditAlternateMobile) then state.data.driverAlternateMobile else Nothing), driverEditAlternateMobile = Nothing} , props{  otpIncorrect = false ,otpAttemptsExceeded = false ,keyboardModalType = NONE , alternateMobileOtp = "",checkAlternateNumber =(state.props.isEditAlternateMobile == false) }})
                driverDetailsFlow


    ALTERNATE_NUMBER_REMOVE state -> do
       getAlternateMobileRemoveResp <- lift $ lift $ Remote.removeAlternateNumber (RemoveAlternateNumberRequest {} )
       case  getAlternateMobileRemoveResp of
          Right (RemoveAlternateNumberResp resp) -> do
                pure $ toast (getString NUMBER_REMOVED_SUCCESSFULLY)
                modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> state { data {driverAlternateMobile = Nothing}, props  { checkAlternateNumber = true}})
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data = homeScreen.data {  driverAlternateMobile = Nothing  }})
                modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen {data = driverProfileScreen.data { driverAlternateNumber = Nothing}})
                driverDetailsFlow
          Left errorPayload -> do
               _ <- pure $ toast $ getString SOMETHING_WENT_WRONG
               modifyScreenState $ DriverDetailsScreenStateType $ \driverDetailsScreen -> state
               driverDetailsFlow

    GO_TO_HOMESCREEN state -> do
       modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> state {props {keyboardModalType = NONE}} )
       homeScreenFlow

    DRIVER_GENDER state -> do
        let genderSelected = state.data.driverGender
        let (UpdateDriverInfoReq initialData) = mkUpdateDriverInfoReq ""
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
      (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT $ mkUpdateDriverInfoReq ""
      vehicleDetailsFlow

aboutUsFlow :: FlowBT String Unit
aboutUsFlow = do
  action <- UI.aboutUsScreen
  case action of
    GO_TO_DRIVER_HOME_SCREEN -> homeScreenFlow
  pure unit

selectLanguageFlow :: FlowBT String Unit
selectLanguageFlow = do
  action <- UI.selectLanguageScreen
  case action of
    CHANGE_LANGUAGE -> do
      (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT $ mkUpdateDriverInfoReq ""
      driverProfileFlow

bookingOptionsFlow :: FlowBT String Unit
bookingOptionsFlow = do
  action <- UI.bookingOptions
  case action of
    SELECT_CAB state -> do
      let toSedan = (filter (\item -> item.vehicleVariant == "SEDAN" && item.isSelected) state.data.downgradeOptions) !! 0
          toHatchBack = (filter (\item -> item.vehicleVariant == "HATCHBACK" && item.isSelected) state.data.downgradeOptions) !! 0
          toTaxi = (filter (\item -> item.vehicleVariant == "TAXI" && item.isSelected) state.data.downgradeOptions) !! 0
      let (UpdateDriverInfoReq initialData) = mkUpdateDriverInfoReq ""
          requiredData = initialData{canDowngradeToSedan = Just (isJust toSedan),canDowngradeToHatchback = Just (isJust toHatchBack) ,canDowngradeToTaxi = Just (isJust toTaxi)}
      (UpdateDriverInfoResp updateDriverResp) <- Remote.updateDriverInfoBT ((UpdateDriverInfoReq) requiredData)
      modifyScreenState $ BookingOptionsScreenType (\bookingOptions -> BookingOptionsScreenData.initData)
      driverProfileFlow
    GO_TO_PROFILE -> driverProfileFlow

helpAndSupportFlow :: FlowBT String Unit
helpAndSupportFlow = do
  action <- UI.helpAndSupportScreen
  case action of
    WRITE_TO_US_SCREEN -> writeToUsFlow
    RIDE_SELECTION_SCREEN selectedCategory -> do
      modifyScreenState $ RideSelectionScreenStateType (\rideHistoryScreen -> rideHistoryScreen { offsetValue = 0, selectedCategory = selectedCategory } )
      rideSelectionScreenFlow
    REPORT_ISSUE_CHAT_SCREEN selectedCategory -> do
      let language = ( case getValueToLocalStore LANGUAGE_KEY of
                       "HI_IN" -> "hi"
                       "KN_IN" -> "kn"
                       "TA_IN" -> "ta"
                       _       -> "en"
                     )
      (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT selectedCategory.categoryId language
      let getOptionsRes' = (mapWithIndex (\index (Option x) ->
        { option : (show (index + 1)) <> ". " <>
                   if (language == "en")
                   then
                     joinWith " " (map (\optName ->
                       let {before, after} = splitAt 1 optName
                       in (toUpper before <> after)
                     ) (split (Pattern " ") x.option))
                   else x.option
        , issueOptionId : x.issueOptionId
        , label : x.label
        }
      ) getOptionsRes.options)
      let categoryName = getCategoryName selectedCategory.categoryAction
      modifyScreenState $ ReportIssueChatScreenStateType (\_ -> ReportIssueScreenData.initData { data { categoryName = categoryName, categoryAction = selectedCategory.categoryAction, categoryId = selectedCategory.categoryId, options = getOptionsRes' }, props { isReversedFlow = selectedCategory.categoryAction == "LOST_AND_FOUND" } })
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



writeToUsFlow :: FlowBT String Unit
writeToUsFlow = do
  action <- UI.writeToUsScreen
  case action of
    GO_TO_HOME_SCREEN_FLOW -> homeScreenFlow

permissionsScreenFlow :: FlowBT String Unit
permissionsScreenFlow = do
  logField_ <- lift $ lift $ getLogFields
  lift $ lift $ doAff do liftEffect hideSplash
  _ <- pure $ hideKeyboardOnNavigation true
  action <- UI.permissions
  case action of
    DRIVER_HOME_SCREEN -> do
      liftFlowBT $ logEvent logField_ "ny_driver_submit_permissions"
      setValueToLocalStore TEST_FLOW_FOR_REGISTRATOION "COMPLETED"
      setValueToLocalStore TEST_FLOW_FOR_PERMISSIONS "COMPLETED"
      currentRideFlow

myRidesScreenFlow :: FlowBT String Unit
myRidesScreenFlow = do
  let (GlobalState defGlobalState) = defaultGlobalState
  flow <- UI.rideHistory
  case flow of
    REFRESH state -> do
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> state{offsetValue = 0})
      myRidesScreenFlow
    MY_RIDE state -> tripDetailsScreenFlow
    HOME_SCREEN -> homeScreenFlow
    PROFILE_SCREEN -> driverProfileFlow
    GO_TO_REFERRAL_SCREEN -> referralScreenFlow
    LOADER_OUTPUT state -> do
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> state{offsetValue = state.offsetValue + 8})
      myRidesScreenFlow
    FILTER currTab-> do
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> rideHistoryScreen{currentTab = currTab})
      myRidesScreenFlow
    GO_TO_TRIP_DETAILS selectedCard -> do
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {data {
      tripId = selectedCard.id,
      date = selectedCard.date,
      time = selectedCard.time,
      source = selectedCard.source,
      destination = selectedCard.destination,
      totalAmount = selectedCard.total_amount,
      distance = selectedCard.rideDistance,
      status = selectedCard.status,
      vehicleType = selectedCard.vehicleType
      }})
      tripDetailsScreenFlow
    NOTIFICATION_FLOW -> notificationFlow
    SELECTED_TAB state -> do
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> state{offsetValue = 0})
      myRidesScreenFlow
    OPEN_PAYMENT_HISTORY state-> do
      let paymentHistory = spy "history" getPaymentHistoryItemList []
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> rideHistoryScreen{props {showPaymentHistory = true},data{paymentHistory {paymentHistoryList = paymentHistory}}})
      resp <- lift $ lift $ Remote.getPaymentHistory (getcurrentdate "") (getDatebyCount state.data.pastDays) Nothing
      case resp of
        Right (GetPaymentHistoryResp response) -> do
          let paymentHistory = getPaymentHistoryItemList response
          modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> rideHistoryScreen{props {showPaymentHistory = true},data{paymentHistory {paymentHistoryList = paymentHistory}}})
        Left err -> pure unit
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
      let language = ( case getValueToLocalStore LANGUAGE_KEY of
                         "HI_IN" -> "hi"
                         "KN_IN" -> "kn"
                         "TA_IN" -> "ta"
                         _       -> "en"
                     )
      (GetOptionsRes getOptionsRes) <- Remote.getOptionsBT state.selectedCategory.categoryId language
      let getOptionsRes' = (mapWithIndex (\index (Option x) ->
        { option : (show (index + 1)) <> ". " <>
                   if (language == "en")
                   then
                     joinWith " " (map (\optName ->
                       let {before, after} = splitAt 1 optName
                       in (toUpper before <> after)
                     ) (split (Pattern " ") x.option))
                   else x.option
          , issueOptionId : x.issueOptionId
          , label : x.label
        }
      ) getOptionsRes.options)
      let tripId' = case state.selectedItem of
                      Just item -> Just item.id
                      _         -> Nothing
      let categoryName = getCategoryName state.selectedCategory.categoryAction
      modifyScreenState $ ReportIssueChatScreenStateType (\_ -> ReportIssueScreenData.initData { data { tripId = tripId', categoryName = categoryName, categoryId = state.selectedCategory.categoryId, categoryAction = state.selectedCategory.categoryAction, options = getOptionsRes' }, props { isReversedFlow = (state.selectedCategory.categoryAction == "LOST_AND_FOUND") } } )
      issueReportChatScreenFlow

issueReportChatScreenFlow :: FlowBT String Unit
issueReportChatScreenFlow = do
  flow <- UI.reportIssueChatScreen
  case flow of
    GO_TO_HELP_AND_SUPPORT -> do
      let language = ( case getValueToLocalStore LANGUAGE_KEY of
                         "HI_IN" -> "hi"
                         "KN_IN" -> "kn"
                         "TA_IN" -> "ta"
                         _       -> "en"
                     )
      let categoryOrder = ["LOST_AND_FOUND", "RIDE_RELATED", "APP_RELATED", "FARE"]
      let compareByOrder a b = compare (fromMaybe (length categoryOrder) $ elemIndex a.categoryAction categoryOrder) (fromMaybe (length categoryOrder) $ elemIndex b.categoryAction categoryOrder)
      (GetCategoriesRes response) <- Remote.getCategoriesBT language
      let temp = (map (\(Category x) ->
                          { categoryName :
                              if (language == "en")
                              then
                                joinWith " " (map (\catName ->
                                  let { before, after } = splitAt 1 catName
                                  in (toUpper before <> after)
                                ) (split (Pattern " ") x.category))
                              else x.category
                          , categoryId       : x.issueCategoryId
                          , categoryAction   : x.label
                          , categoryImageUrl : x.logoUrl
                          }) response.categories)
      let categories' = sortBy compareByOrder temp
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
                                       })
      (PostIssueRes postIssueRes) <- Remote.postIssueBT postIssueReq
      (IssueInfoRes issueInfoRes) <- Remote.issueInfoBT postIssueRes.issueReportId
      _ <- pure $ hideKeyboardOnNavigation true
      let showDescription = STR.length (trim issueInfoRes.description) > 0
      let descMessages = if showDescription then snoc state.data.chatConfig.messages (makeChatComponent' issueInfoRes.description "Driver" (if (length issueInfoRes.mediaFiles) == 0 then (convertUTCtoISC (getCurrentUTC "") "hh:mm A") else "") "Text" 500) else state.data.chatConfig.messages
      let mediaMessages' = mapWithIndex (\index media -> do
                        if index == length issueInfoRes.mediaFiles - 1
                        then
                          makeChatComponent' media.url "Driver" (convertUTCtoISC (getCurrentUTC "") "hh:mm A") media._type ((index + if showDescription then 2 else 1) * 500)
                        else
                          makeChatComponent' media.url "Driver" "" media._type ((index + if showDescription then 2 else 1) * 500)
                    ) (issueInfoRes.mediaFiles)
      if state.props.isReversedFlow
      then do
        let options'  = map (\x -> x.option) state.data.options
        let message = (getString SELECT_OPTION_REVERSED) <> "\n"
                      <> joinWith "\n" options'
        let messages' = concat [ descMessages, mediaMessages', [ (makeChatComponent' message "Bot" (convertUTCtoISC (getCurrentUTC "") "hh:mm A") "Text" (500 * (length mediaMessages' + 2))) ] ]
        modifyScreenState $ ReportIssueChatScreenStateType (\reportIssueScreen -> state { data { issueId = Just postIssueRes.issueReportId, chatConfig { enableSuggestionClick = false, messages = messages', suggestionsList = options', suggestionDelay = 500 * (length mediaMessages' + 3) } }, props { showSubmitComp = false } })
        issueReportChatScreenFlow
      else do
        let message = makeChatComponent' (getString ISSUE_SUBMITTED_MESSAGE) "Bot" (convertUTCtoISC (getCurrentUTC "") "hh:mm A") "Text" (500 * (length mediaMessages' + 2))
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
                       let message = makeChatComponent' (getString ISSUE_SUBMITTED_MESSAGE) "Bot" (convertUTCtoISC (getCurrentUTC "") "hh:mm A") "Text" 500
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
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ ReferralScreenStateType (\ referralScreen -> referralScreen { data { config = config}} )
  when (any (_ == "") [state.props.selectedDay.utcDate, state.props.selectedWeek.utcStartDate, state.props.selectedWeek.utcEndDate]) do
    let pastDates = getPastDays 7
        pastWeeks = getPastWeeks 4
        selectedDay = case last pastDates of
                        Just date -> date
                        Nothing -> state.props.selectedDay
        selectedWeek = case last pastWeeks of
                        Just week -> week
                        Nothing -> state.props.selectedWeek
    modifyScreenState $ ReferralScreenStateType (\ referralScreen -> referralScreen {props{ days = pastDates, weeks = pastWeeks, selectedDay = selectedDay, selectedWeek = selectedWeek }} )
  if isJust state.data.driverInfo.referralCode then do
    (GetPerformanceRes getPerformanceres) <- Remote.getPerformanceBT (GetPerformanceReq {} )
    modifyScreenState $ ReferralScreenStateType (\ referralScreen -> referralScreen { data { driverPerformance { referrals = getPerformanceres.referrals}}} )
    else pure unit
  act <- UI.referralScreen
  case act of
    GO_TO_HOME_SCREEN_FROM_REFERRAL_SCREEN -> homeScreenFlow
    GO_TO_RIDES_SCREEN_FROM_REFERRAL_SCREEN -> myRidesScreenFlow
    GO_TO_PROFILE_SCREEN_FROM_REFERRAL_SCREEN -> driverProfileFlow
    GO_TO_NOTIFICATION_SCREEN_FROM_REFERRAL_SCREEN -> notificationFlow
    GO_TO_FLOW_AND_COME_BACK updatedState-> do
      response <-  lift $ lift $ Remote.linkReferralCode ( makeLinkReferralCodeReq updatedState.data.referralCode updatedState.data.password)
      case response of
        Right resp -> do
          modifyScreenState $ ReferralScreenStateType (\ referralScreen -> referralScreen{ data { driverInfo {referralCode = Just updatedState.data.referralCode} } ,  props { stage = SuccessScreen , firstTime = true}})
          referralScreenFlow
        Left error -> do
          _ <- pure $ toast $ getString SOMETHING_WENT_WRONG
          referralScreenFlow
      referralScreenFlow
    REFRESH_LEADERBOARD -> referralScreenFlow
    REFERRAL_SCREEN_NAV GoToSubscription -> updateAvailableAppsAndGoToSubs
    REFERRAL_SCREEN_NAV _ -> referralScreenFlow
    _ -> referralScreenFlow

tripDetailsScreenFlow :: FlowBT String Unit
tripDetailsScreenFlow = do
  flow <- UI.tripDetailsScreen
  case flow of
    ON_SUBMIT  -> pure unit
    GO_TO_HOME_SCREEN -> do
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> rideHistoryScreen{offsetValue = 0, currentTab = "COMPLETED"})
      myRidesScreenFlow
    OPEN_HELP_AND_SUPPORT -> do
      let language = ( case getValueToLocalStore LANGUAGE_KEY of
                         "HI_IN" -> "hi"
                         "KN_IN" -> "kn"
                         "TA_IN" -> "ta"
                         _       -> "en"
                     )
      let categoryOrder = ["LOST_AND_FOUND", "RIDE_RELATED", "APP_RELATED", "FARE"]
      let compareByOrder a b = compare (fromMaybe (length categoryOrder) $ elemIndex a.categoryAction categoryOrder) (fromMaybe (length categoryOrder) $ elemIndex b.categoryAction categoryOrder)
      (GetCategoriesRes response) <- Remote.getCategoriesBT language
      let temp = (map (\(Category x) ->
                          { categoryName :
                              if (language == "en")
                              then
                                joinWith " " (map (\catName ->
                                  let { before, after } = splitAt 1 catName
                                  in (toUpper before <> after)
                                ) (split (Pattern " ") x.category))
                              else x.category
                          , categoryId       : x.issueCategoryId
                          , categoryAction   : x.label
                          , categoryImageUrl : x.logoUrl
                          }) response.categories)
      let categories' = sortBy compareByOrder temp
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen { data { categories = categories' } } )
      helpAndSupportFlow

currentRideFlow :: FlowBT String Unit
currentRideFlow = do
  (GlobalState allState) <- getState
  case allState.notificationScreen.selectedNotification of
    Just _ -> do
      lift $ lift $ doAff do liftEffect hideSplash
      notificationFlow
    Nothing -> pure unit
  case allState.globalProps.callScreen of
    ScreenNames.SUBSCRIPTION_SCREEN -> do 
      lift $ lift $ doAff do liftEffect hideSplash
      updateAvailableAppsAndGoToSubs
    _ -> pure unit
  setValueToLocalStore RIDE_STATUS_POLLING "False"
  let isRequestExpired = if (getValueToLocalNativeStore RIDE_REQUEST_TIME) == "__failed" then false
    else ceil ((toNumber (rideRequestPollingData.duration - (getExpiryTime (getValueToLocalNativeStore RIDE_REQUEST_TIME) true)) * 1000.0)/rideRequestPollingData.delay) > 0
  if isLocalStageOn RideRequested && (getValueToLocalNativeStore IS_RIDE_ACTIVE) == "false" && isRequestExpired then
    homeScreenFlow
    else pure unit
  (GetRidesHistoryResp activeRideResponse) <- Remote.getRideHistoryReqBT "1" "0" "true" "null" "null"
  _ <- pure $ spy "activeRideResponse" activeRideResponse
  if not (null activeRideResponse.list) then do
    case (activeRideResponse.list !! 0 ) of
      Just ride -> do
        let state = allState.homeScreen
            activeRide = (activeRideDetail state ride)
            stage = (if activeRide.status == NEW then (if state.props.currentStage == ChatWithCustomer then ChatWithCustomer else RideAccepted) else RideStarted)
        setValueToLocalNativeStore IS_RIDE_ACTIVE  "true"
        _ <- updateStage $ HomeScreenStage stage
        void $ pure $ setCleverTapUserProp "Driver On-ride" "Yes"
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data{ activeRide = activeRide}, props{ silentPopUpView = false, goOfflineModal = false }})
      Nothing -> do
        setValueToLocalNativeStore IS_RIDE_ACTIVE  "false"
        _ <- updateStage $ HomeScreenStage HomeScreen
        pure unit
  else do
    setValueToLocalNativeStore IS_RIDE_ACTIVE  "false"
    _ <- updateStage $ HomeScreenStage HomeScreen
    pure unit
  void $ pure $ setCleverTapUserProp "Driver On-ride" if getValueToLocalNativeStore IS_RIDE_ACTIVE == "false" then "No" else "Yes"
  (DriverRegistrationStatusResp resp) <- driverRegistrationStatusBT (DriverRegistrationStatusReq { })
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {showlinkAadhaarPopup = (resp.aadhaarVerificationStatus == "INVALID" || resp.aadhaarVerificationStatus == "NO_DOC_AVAILABLE") && (getMerchant FunctionCall) == YATRISATHI}})
  homeScreenFlow

getDriverStatus :: String -> DriverStatus
getDriverStatus dummy = do
  case getValueToLocalNativeStore DRIVER_STATUS_N of
    "Online" -> Online
    "Offline" -> Offline
    "Silent" -> Silent
    _ -> Online

getDriverStatusFromMode :: String -> DriverStatus
getDriverStatusFromMode mode = do
  case mode of
    "ONLINE" -> Online
    "OFFLINE" -> Offline
    "SILENT" -> Silent
    _ -> Online

updateDriverStatus :: Boolean -> DriverStatus
updateDriverStatus status = do
  if status && getValueToLocalNativeStore DRIVER_STATUS_N == "Silent" then Silent
    else if status then Online
      else Offline

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
              driverFeeId = paymentDetailsEntity.driverFeeId,
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
                      driverFeeId = paymentDetailsEntity.driverFeeId,
                      chargesBreakup = paymentDetailsEntity.chargesBreakup,
                      dateObj = paymentDetailsEntity.date,
                      laterButtonVisibility = getDriverInfoResp.subscribed
                      }}})
                  Nothing -> pure unit
              Left error -> pure unit
      Left error -> pure unit
    pure unit

homeScreenFlow :: FlowBT String Unit
homeScreenFlow = do
  logField_ <- lift $ lift $ getLogFields
  _ <- pure $ printLog "HOME_SCREEN_FLOW" "."
  _ <- pure $ delay $ Milliseconds 1.0
  _ <- pure $ printLog "Registration token" (getValueToLocalStore REGISTERATION_TOKEN)
  _ <- pure $ firebaseUserID (getValueToLocalStore DRIVER_ID)
  _ <- pure $ cleverTapSetLocation unit
  if (getValueToLocalNativeStore IS_RIDE_ACTIVE) == "true" && not (isLocalStageOn RideAccepted) && not (isLocalStageOn RideStarted) && not (isLocalStageOn ChatWithCustomer) then
    currentRideFlow
  else pure unit
  getDriverInfoResp <- Remote.getDriverInfoBT (GetDriverInfoReq { })
  modifyScreenState $ GlobalPropsType (\globalProps -> globalProps {driverInformation = getDriverInfoResp})
  let (GetDriverInfoResp getDriverInfoResp) = getDriverInfoResp
  let (OrganizationInfo organization) = getDriverInfoResp.organization
  appConfig <- getAppConfig Constants.appConfig
  when appConfig.subscriptionConfig.enableBlocking $ checkDriverBlockingStatus (GetDriverInfoResp getDriverInfoResp)
  when appConfig.subscriptionConfig.completePaymentPopup $ checkDriverPaymentStatus (GetDriverInfoResp getDriverInfoResp)
  let showGender = not (isJust (getGenderValue getDriverInfoResp.gender))
  let (Vehicle linkedVehicle) = (fromMaybe dummyVehicleObject getDriverInfoResp.linkedVehicle)
  case getDriverInfoResp.linkedVehicle of
    Nothing -> do 
      changeDriverStatus Offline
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {statusOnline = false, driverStatusSet = Offline, showOffer = false, rcActive = false, rcDeactivePopup = true}}) 
    Just _ -> modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {rcActive = true, rcDeactivePopup = false}}) 
  setValueToLocalStore USER_NAME getDriverInfoResp.firstName
  if (isJust getDriverInfoResp.numberOfRides) then do
    setValueToLocalStore HAS_TAKEN_FIRST_RIDE $ show $ fromMaybe 0 getDriverInfoResp.numberOfRides == 0
    else setValueToLocalStore HAS_TAKEN_FIRST_RIDE "false"
  case getDriverInfoResp.mode of
    Just currentMode -> case currentMode of
                          "OFFLINE" -> do
                              void $ pure $ setCleverTapUserProp "Mode" currentMode
                              lift $ lift $ liftFlow $ stopLocationPollingAPI
                              setDriverStatusInLocal "false" (show $ getDriverStatusFromMode currentMode)
                              pure unit
                          _ ->        do
                                      void $ pure $ setCleverTapUserProp "Mode" currentMode
                                      setDriverStatusInLocal "true" (show $ getDriverStatusFromMode currentMode)
                                      lift $ lift $ liftFlow $ startLocationPollingAPI
                                      pure unit

    Nothing -> do
                setDriverStatusInLocal (show getDriverInfoResp.active) (show $ updateDriverStatus (getDriverInfoResp.active))
                lift $ lift $ liftFlow $ if getDriverInfoResp.active then  startLocationPollingAPI else stopLocationPollingAPI
                (DriverActiveInactiveResp resp) <- Remote.driverActiveInactiveBT (if any( _ == (updateDriverStatus getDriverInfoResp.active))[Online, Silent] then "true" else "false") $ toUpper $ show (updateDriverStatus getDriverInfoResp.active)
                pure unit


  _  <- pure $ spy "DRIVERRRR___STATUs" (getValueToLocalNativeStore DRIVER_STATUS)
  _  <- pure $ spy "DRIVERRRR___STATUS LOCAL" (getValueToLocalStore DRIVER_STATUS)
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {statusOnline = if (isJust getDriverInfoResp.mode) then
                                                                                                (any( _ == (updateDriverStatus (getDriverInfoResp.active)))[Online, Silent])
                                                                                             else getDriverInfoResp.active
                                                                            , driverStatusSet = getDriverStatus "" }
                                                                      , data{vehicleType = linkedVehicle.variant, driverAlternateMobile =getDriverInfoResp.alternateNumber, profileImg = getDriverInfoResp.aadhaarCardPhoto}})
  modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen {data {driverName = getDriverInfoResp.firstName, driverVehicleType = linkedVehicle.variant, driverRating = getDriverInfoResp.rating, capacity = fromMaybe 2 linkedVehicle.capacity, downgradeOptions = getDowngradeOptions linkedVehicle.variant, vehicleSelected = getDowngradeOptionsSelected (GetDriverInfoResp getDriverInfoResp), profileImg = getDriverInfoResp.aadhaarCardPhoto}})
  modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> driverDetailsScreen { data {driverAlternateMobile =getDriverInfoResp.alternateNumber}})
  modifyScreenState $ ReferralScreenStateType (\ referralScreen -> referralScreen{ data { driverInfo  {  driverName = getDriverInfoResp.firstName, driverMobile = getDriverInfoResp.mobileNumber,  vehicleRegNumber = linkedVehicle.registrationNo , referralCode = getDriverInfoResp.referralCode }}})
  let currdate = getcurrentdate ""
  (DriverProfileStatsResp resp) <- Remote.getDriverProfileStatsBT (DriverProfileStatsReq currdate)
  lift $ lift $ doAff do liftEffect hideSplash
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data{totalRidesOfDay = resp.totalRidesOfDay, totalEarningsOfDay = resp.totalEarningsOfDay, bonusEarned = resp.bonusEarning}, props{showGenderBanner = showGender, autoPayBanner = (isNothing getDriverInfoResp.autoPayStatus) || (getDriverInfoResp.autoPayStatus /= Just "ACTIVE")}})
  void $ lift $ lift $ toggleLoader false
  isGpsEnabled <- lift $ lift $ liftFlow $ isLocationEnabled unit
  if not isGpsEnabled then noInternetScreenFlow "LOCATION_DISABLED" else pure unit
  action <- UI.homeScreen
  case action of
    GO_TO_PROFILE_SCREEN -> do
      liftFlowBT $ logEvent logField_ "ny_driver_profile_click"
      driverProfileFlow 
    GO_TO_VEHICLE_DETAILS_SCREEN -> do 
      modifyScreenState $ DriverProfileScreenStateType $ \driverProfileScreen -> driverProfileScreen { props { screenType = ST.VEHICLE_DETAILS}}
      driverProfileFlow
    GO_TO_RIDES_SCREEN -> do
      _ <- pure $ printLog "HOME_SCREEN_FLOW GO_TO_RIDES_SCREEN" "."
      liftFlowBT $ logEvent logField_ "ny_driver_my_rides"
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> rideHistoryScreen{offsetValue = 0 , currentTab = "COMPLETED"})
      myRidesScreenFlow
    GO_TO_REFERRAL_SCREEN_FROM_HOME_SCREEN -> do
      liftFlowBT $ logEvent logField_ "ny_driver_rankings"
      referralScreenFlow
    DRIVER_AVAILABILITY_STATUS state status -> do
      void $ lift $ lift $ loaderText (getString PLEASE_WAIT) if status == Online then (getString SETTING_YOU_ONLINE) else if status == Silent then (getString SETTING_YOU_SILENT) else (getString SETTING_YOU_OFFLINE)
      void $ lift $ lift $ toggleLoader true
      let label = if status == Online then "ny_driver_online_mode" else if status == Silent then "ny_driver_silent_mode" else "ny_driver_offline_mode"
      let currentTime = (convertUTCtoISC (getCurrentUTC "") "HH:mm:ss")
      liftFlowBT $ logEventWithParams logField_ label "Timestamp" currentTime
      changeDriverStatus status
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {statusOnline = (if any( _ == status)[Online, Silent] then true else false), driverStatusSet = status, showOffer = (status == Online && state.props.driverStatusSet == Offline && getDriverInfoResp.autoPayStatus == Nothing )}})
      homeScreenFlow
    GO_TO_HELP_AND_SUPPORT_SCREEN -> do
      let language = ( case getValueToLocalStore LANGUAGE_KEY of
                         "HI_IN" -> "hi"
                         "KN_IN" -> "kn"
                         "TA_IN" -> "ta"
                         _       -> "en"
                     )
      let categoryOrder = ["LOST_AND_FOUND", "RIDE_RELATED", "APP_RELATED", "FARE"]
      let compareByOrder a b = compare (fromMaybe (length categoryOrder) $ elemIndex a.categoryAction categoryOrder) (fromMaybe (length categoryOrder) $ elemIndex b.categoryAction categoryOrder)
      (GetCategoriesRes response) <- Remote.getCategoriesBT language
      let temp = (map (\(Category x) ->
                          { categoryName :
                              if (language == "en")
                              then
                                joinWith " " (map (\catName ->
                                  let { before, after } = splitAt 1 catName
                                  in (toUpper before <> after)
                                ) (split (Pattern " ") x.category))
                              else x.category
                          , categoryId       : x.issueCategoryId
                          , categoryAction   : x.label
                          , categoryImageUrl : x.logoUrl
                          }) response.categories)
      let categories' = sortBy compareByOrder temp
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen { data { categories = categories' } } )
      helpAndSupportFlow
    GO_TO_EDIT_GENDER_SCREEN -> do
      modifyScreenState $ DriverDetailsScreenStateType (\driverDetailsScreen -> driverDetailsScreen { data {driverGender = Nothing,genderSelectionModal{selectionOptions = genders FunctionCall,activeIndex = Nothing,isSelectButtonActive = false}}, props  { genderSelectionModalShow = true}})
      driverDetailsFlow
    GO_TO_START_RIDE {id, otp , lat, lon} updatedState -> do
      _ <- pure $ printLog "HOME_SCREEN_FLOW GO_TO_START_RIDE" "."
      _ <- pure $ printLog "Lat in Floww: " lat
      _ <- pure $ printLog "Lon in Floww: " lon
      void $ lift $ lift $ loaderText (getString START_RIDE) ""
      void $ lift $ lift $ toggleLoader true
      startRideResp <- lift $ lift $ Remote.startRide id (Remote.makeStartRideReq otp (fromMaybe 0.0 (Number.fromString lat)) (fromMaybe 0.0 (Number.fromString lon))) -- driver's lat long during starting ride
      case startRideResp of
        Right startRideResp -> do
          if((getValueToLocalStore IS_WAIT_TIMER_STOP ) /= "NoView") then
            setValueToLocalStore IS_WAIT_TIMER_STOP (show ST.Stop)
          else
            pure unit
          _ <- pure $ setValueToLocalNativeStore RIDE_ID id
          liftFlowBT $ logEvent logField_ "ny_driver_ride_start"
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props {enterOtpModal = false, showDottedRoute = true,timerRefresh=false}, data{ route = [], activeRide{status = INPROGRESS}}})
          void $ lift $ lift $ toggleLoader false
          _ <- updateStage $ HomeScreenStage RideStarted
          _ <- pure $ setValueToLocalStore TRIGGER_MAPS "true"
          currentRideFlow
        Left errorPayload -> do
          let errResp = errorPayload.response
          let codeMessage = decodeErrorCode errResp.errorMessage
          if ( errorPayload.code == 400 && codeMessage == "INCORRECT_OTP") then do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {otpIncorrect = true, enterOtpModal = true, otpAttemptsExceeded = false, rideOtp = ""} })
              void $ lift $ lift $ toggleLoader false
            else if ( errorPayload.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {otpAttemptsExceeded = true, enterOtpModal = true, rideOtp = ""} })
              void $ lift $ lift $ toggleLoader false
              else pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
          homeScreenFlow
    GO_TO_START_ZONE_RIDE {otp, lat, lon} -> do
      void $ lift $ lift $ loaderText (getString PLEASE_WAIT) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      startZoneRideResp <- lift $ lift $ Remote.otpRide "" (Remote.makeOTPRideReq otp (fromMaybe 0.0 (Number.fromString lat)) (fromMaybe 0.0 (Number.fromString lon))) -- driver's lat long during starting ride
      case startZoneRideResp of
        Right startZoneRideResp -> do
          liftFlowBT $ logEvent logField_ "ny_driver_special_zone_ride_start"
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props {enterOtpModal = false, showDottedRoute = true}, data{ route = [], activeRide{status = INPROGRESS}}})
          void $ lift $ lift $ toggleLoader false
          void $ updateStage $ HomeScreenStage RideStarted
          _ <- pure $ setValueToLocalStore TRIGGER_MAPS "true"
          currentRideFlow
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
    GO_TO_END_RIDE {id, lat, lon} -> do
      _ <- pure $ printLog "HOME_SCREEN_FLOW GO_TO_END_RIDE" "."
      void $ lift $ lift $ loaderText (getString END_RIDE) ""
      void $ lift $ lift $ toggleLoader true
      _ <- pure $ spy "number_of_deviation" (getValueToLocalNativeStore RIDE_WAYPOINT_DEVIATION_COUNT)
      let numDeviation = Just $ (fromMaybe 0 (fromString (getValueToLocalNativeStore RIDE_WAYPOINT_DEVIATION_COUNT))) >=3
      endRideResp <- Remote.endRide id (Remote.makeEndRideReq (fromMaybe 0.0 (Number.fromString lat)) (fromMaybe 0.0 (Number.fromString lon)) numDeviation)-- driver's  lat long during ending ride
      _ <- pure $ cleverTapCustomEvent "ny_driver_ride_ended"
      _ <- pure $ metaLogEvent "ny_driver_ride_ended"
      liftFlowBT $ firebaseLogEvent "ny_driver_ride_ended"
      _ <- pure $ removeAllPolylines ""
      _ <- pure $ setValueToLocalStore IS_WAIT_TIMER_STOP "NoView"
      _ <- pure $ setValueToLocalNativeStore IS_RIDE_ACTIVE  "false"
      void $ pure $ setCleverTapUserProp "Driver On-ride" "No"
      _ <- pure $ setValueToLocalStore DRIVER_STATUS_N "Online"
      _ <- pure $ setValueToLocalNativeStore DRIVER_STATUS_N "Online"
      (DriverActiveInactiveResp resp) <- Remote.driverActiveInactiveBT "true" $ toUpper $ show Online
      liftFlowBT $ logEvent logField_ "ny_driver_ride_completed"

      if getValueToLocalStore HAS_TAKEN_FIRST_RIDE == "true" then do
        getDriverInfoResp <- Remote.getDriverInfoBT (GetDriverInfoReq { })
        let (GetDriverInfoResp getDriverInfoResp) = getDriverInfoResp
        if (isJust getDriverInfoResp.numberOfRides && (fromMaybe 0 getDriverInfoResp.numberOfRides == 1))
          then do
            let currdate = getcurrentdate ""
            liftFlowBT $ logEventWithParams logField_ "ny_driver_first_ride_completed" "Date" currdate
          else pure unit
        setValueToLocalStore HAS_TAKEN_FIRST_RIDE "false"
        else pure unit

      (GetRidesHistoryResp rideHistoryResponse) <- Remote.getRideHistoryReqBT "1" "0" "false" "null" "null"
      case (head rideHistoryResponse.list) of
        Nothing -> pure unit
        Just (RidesInfo response) -> do
          _ <- pure $ printLog "ride history response" response
          _ <- pure $ printLog "fromLocation lat" (response.fromLocation ^._lat)
          modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {data {
              tripId = response.shortRideId,
              date = (convertUTCtoISC (response.createdAt) "D MMM"),
              time = (convertUTCtoISC (response.createdAt )"h:mm A"),
              source = (decodeAddress response.fromLocation false),
              destination = (decodeAddress response.toLocation false),
              totalAmount = fromMaybe response.estimatedBaseFare response.computedFare,
              distance = parseFloat (toNumber (fromMaybe 0 response.chargeableDistance) / 1000.0) 2,
              status = response.status,
              rider = (fromMaybe "" response.riderName)
            }})
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {data { endRideData { finalAmount = fromMaybe response.estimatedBaseFare response.computedFare, riderName = fromMaybe "" response.riderName, rideId = response.id, tip = response.customerExtraFee, disability = response.disabilityTag}}})
        
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props { showRideCompleted = true}})
      _ <- updateStage $ HomeScreenStage RideCompleted
      void $ lift $ lift $ toggleLoader false
      homeScreenFlow
    GO_TO_CANCEL_RIDE {id, info , reason} -> do
      _ <- pure $ printLog "HOME_SCREEN_FLOW GO_TO_CANCEL_RIDE" "."
      cancelRideResp <- Remote.cancelRide id (Remote.makeCancelRideReq info reason)
      _ <- pure $ setValueToLocalStore IS_WAIT_TIMER_STOP "NoView"
      _ <- pure $ removeAllPolylines ""
      _ <- pure $ setValueToLocalStore DRIVER_STATUS_N "Online"
      _ <- pure $ setValueToLocalNativeStore DRIVER_STATUS_N "Online"
      (DriverActiveInactiveResp resp) <- Remote.driverActiveInactiveBT "true" $ toUpper $ show Online
      removeChatService ""
      _ <- updateStage $ HomeScreenStage HomeScreen
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props { chatcallbackInitiated = false}})
      homeScreenFlow
    FCM_NOTIFICATION notificationType -> do
      _ <- pure $ removeAllPolylines ""
      case notificationType of
        "CANCELLED_PRODUCT" -> do
          _ <- pure $ setValueToLocalStore DRIVER_STATUS_N "Online"
          _ <- pure $ setValueToLocalNativeStore DRIVER_STATUS_N "Online"
          (DriverActiveInactiveResp resp) <- Remote.driverActiveInactiveBT "true" $ toUpper $ show Online
          removeChatService ""
          _ <- updateStage $ HomeScreenStage HomeScreen
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props { chatcallbackInitiated = false}})
          homeScreenFlow
        "DRIVER_ASSIGNMENT" -> do
          let (GlobalState defGlobalState) = defaultGlobalState
          when (isJust defGlobalState.homeScreen.data.activeRide.disabilityTag) $ do
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props { showAccessbilityPopup = true}})
          currentRideFlow
        "RIDE_REQUESTED"    -> do
          _ <- updateStage $ HomeScreenStage RideRequested
          homeScreenFlow
        _                   -> homeScreenFlow
    REFRESH_HOME_SCREEN_FLOW -> do
      _ <- pure $ removeAllPolylines ""
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props {rideActionModal = false, cancelRideModalShow = false, enterOtpModal = false, routeVisible = false, refreshAnimation = false}})
      homeScreenFlow
    RELOAD state -> do
      _ <- pure $ printLog "HOME_SCREEN_FLOW RELOAD" state
      homeScreenFlow
    NOTIFY_CUSTOMER state -> do
      resp <- Remote.driverArrivedBT (state.data.activeRide.id) (DriverArrivedReq {
        "lat" : state.data.currentDriverLat
      , "lon" : state.data.currentDriverLon
      })
      liftFlowBT $ logEvent logField_ "ny_driver_i_have_arrived_clicked"
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{activeRide{notifiedCustomer = true}}, props{currentStage = ChatWithCustomer}})
      homeScreenFlow
    UPDATE_ROUTE state -> do
      _ <- pure $ printLog "HOME_SCREEN_FLOW UPDATE_ROUTE" state
      let srcLat = if state.props.currentStage == RideAccepted then state.data.currentDriverLat else state.data.activeRide.src_lat
          srcLon = if state.props.currentStage == RideAccepted then state.data.currentDriverLon else state.data.activeRide.src_lon
          destLat = if state.props.currentStage == RideAccepted then state.data.activeRide.src_lat else state.data.activeRide.dest_lat
          destLon = if state.props.currentStage == RideAccepted then state.data.activeRide.src_lon else state.data.activeRide.dest_lon
          source = if state.props.currentStage == RideAccepted then "" else state.data.activeRide.source
          destination = if state.props.currentStage == RideAccepted then state.data.activeRide.source else state.data.activeRide.destination
          routeType = if state.props.currentStage == RideAccepted then "pickup" else "trip"
      if state.props.showDottedRoute then do
        let coors = (walkCoordinate srcLon srcLat destLon destLat)
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { routeVisible = true } })
        _ <- pure $ removeAllPolylines ""
        _ <- lift $ lift $ doAff do liftEffect $ drawRoute coors "DOT" "#323643" false "ny_ic_src_marker" "ny_ic_dest_marker" 9 "NORMAL" source destination (mapRouteConfig "" "")
        homeScreenFlow
        else if not null state.data.route then do
          let shortRoute = (state.data.route !! 0)
          case shortRoute of
            Just (Route route) -> do
              let coor = walkCoordinates route.points
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props { routeVisible = true } })
              pure $ removeMarker "ic_vehicle_side"
              _ <- pure $ removeAllPolylines ""
              _ <- lift $ lift $ doAff do liftEffect $ drawRoute coor "LineString" "#323643" true "ny_ic_src_marker" "ny_ic_dest_marker" 9 "NORMAL" source destination (mapRouteConfig "" "")
              pure unit
            Nothing -> pure unit
          homeScreenFlow
          else do
            GetRouteResp routeApiResponse <- Remote.getRouteBT (makeGetRouteReq srcLat srcLon destLat destLon) routeType
            _ <- pure $ printLog "route Api response inside UPDATE_ROUTE" routeApiResponse
            _ <- pure $ printLog "state inside UPDATE_ROUTE" state
            let shortRoute = (routeApiResponse !! 0)
            case shortRoute of
              Just (Route route) -> do
                let coor = walkCoordinates route.points
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { data { activeRide { actualRideDistance = if state.props.currentStage == RideStarted then (toNumber route.distance) else state.data.activeRide.actualRideDistance , duration = route.duration } , route = routeApiResponse}, props { routeVisible = true } })
                pure $ removeMarker "ny_ic_auto"
                _ <- pure $ removeAllPolylines ""
                _ <- lift $ lift $ doAff do liftEffect $ drawRoute coor "ic_vehicle_side" "#323643" true "ny_ic_src_marker" "ny_ic_dest_marker" 9 "NORMAL" source destination (mapRouteConfig "" "")
                pure unit
              Nothing -> pure unit
            homeScreenFlow
    UPDATE_STAGE stage -> do
      _ <- updateStage $ HomeScreenStage stage
      homeScreenFlow
    GO_TO_NOTIFICATIONS -> notificationFlow
    ADD_ALTERNATE_HOME -> do
      modifyScreenState $ DriverProfileScreenStateType (\driverProfileScreen -> driverProfileScreen{props{alternateNumberView = true, isEditAlternateMobile = true, mNumberEdtFocused = true}, data {fromHomeScreen = true}})
      driverProfileFlow
    ON_CALL state -> do
      (OnCallRes resp) <- Remote.onCallBT (Remote.makeOnCallReq state.data.activeRide.id)
      homeScreenFlow
    OPEN_PAYMENT_PAGE state -> ysPaymentFlow
    HOMESCREEN_NAV GoToSubscription -> do
      let (GlobalState defGlobalState) = defaultGlobalState
      updateAvailableAppsAndGoToSubs
    HOMESCREEN_NAV _ -> homeScreenFlow
    GO_TO_AADHAAR_VERIFICATION -> do
      modifyScreenState $ AadhaarVerificationScreenType (\aadhaarScreen -> aadhaarScreen { props { fromHomeScreen = true, currentStage = EnterAadhaar}})
      aadhaarVerificationFlow
    GO_TO_RIDE_DETAILS_SCREEN -> do 
      tripDetailsScreenFlow
    POST_RIDE_FEEDBACK state-> do 
      resp <- lift $ lift $ Remote.postRideFeedback state.data.endRideData.rideId state.data.endRideData.rating state.data.endRideData.feedback
      _ <- updateStage $ HomeScreenStage HomeScreen 
      homeScreenFlow
      
  pure unit

nyPaymentFlow :: PlanCardConfig -> Boolean -> FlowBT String Unit
nyPaymentFlow planCardConfig fromJoinPlan = do
  liftFlowBT $ runEffectFn1 initiatePP unit
  response <- lift $ lift $ Remote.subscribePlan planCardConfig.id
  case response of
    Right (SubscribePlanResp listResp) -> do
      if fromJoinPlan then do
        _ <- pure $ cleverTapCustomEventWithParams "ny_driver_selected_plan" "selected_plan" planCardConfig.title
        _ <- pure $ cleverTapCustomEventWithParams "ny_driver_selected_plan" "offer" $ show $ map (\offer -> offer.title) planCardConfig.offers
        liftFlowBT $ metaLogEventWithTwoParams "ny_driver_selected_plan" "selected_plan" planCardConfig.title "offer" $ show $ map (\offer -> offer.title) planCardConfig.offers
        liftFlowBT $ firebaseLogEventWithTwoParams "ny_driver_selected_plan" "selected_plan" planCardConfig.title "offer" $ show $ map (\offer -> offer.title) planCardConfig.offers
        pure unit 
      else pure unit
      let (CreateOrderRes orderResp) = listResp.orderResp
          (PaymentPagePayload sdk_payload) = orderResp.sdk_payload
          (PayPayload innerpayload) = sdk_payload.payload
          finalPayload = PayPayload $ innerpayload{ language = Just (getPaymentPageLangKey (getValueToLocalStore LANGUAGE_KEY)) }
          sdkPayload = PaymentPagePayload $ sdk_payload{payload = finalPayload}
      setValueToLocalStore DISABLE_WIDGET "true"
      _ <- pure $ cleverTapCustomEvent "ny_driver_payment_page_opened"
      _ <- pure $ metaLogEvent "ny_driver_payment_page_opened"
      liftFlowBT $ firebaseLogEvent "ny_driver_payment_page_opened"
      lift $ lift $ doAff $ makeAff \cb -> runEffectFn1 checkPPInitiateStatus (cb <<< Right) $> nonCanceler
      _ <- paymentPageUI sdkPayload
      pure $ toggleBtnLoader "" false
      liftFlowBT $ runEffectFn1 consumeBP unit
      setValueToLocalStore DISABLE_WIDGET "false"
      orderStatus <- lift $ lift $ Remote.paymentOrderStatus listResp.orderId
      case orderStatus of
        Right (OrderStatusRes statusResp) ->
          case statusResp.status of
            PS.CHARGED -> setSubscriptionStatus Success statusResp.status planCardConfig
            PS.AUTHORIZATION_FAILED -> setSubscriptionStatus Failed statusResp.status planCardConfig
            PS.AUTHENTICATION_FAILED -> setSubscriptionStatus Failed statusResp.status planCardConfig
            PS.JUSPAY_DECLINED -> setSubscriptionStatus Failed statusResp.status planCardConfig
            PS.NEW -> pure unit
            PS.PENDING_VBV -> setSubscriptionStatus Pending statusResp.status planCardConfig
            _ -> setSubscriptionStatus Pending statusResp.status planCardConfig
        Left err -> setSubscriptionStatus Pending PS.PENDING_VBV planCardConfig
    Left (errorPayload) -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
  subScriptionFlow

setSubscriptionStatus :: PaymentStatus -> PS.APIPaymentStatus -> PlanCardConfig -> FlowBT String Unit
setSubscriptionStatus paymentStatus apiPaymentStatus planCardConfig = do
  case paymentStatus of
    Success -> do 
      _ <- pure $ cleverTapCustomEvent "ny_driver_subscription_success"
      _ <- pure $ JB.metaLogEvent "ny_driver_subscription_success"
      liftFlowBT $ JB.firebaseLogEvent "ny_driver_subscription_success"
      modifyScreenState $ SubscriptionScreenStateType (\subscribeScreenState -> subscribeScreenState { props {popUpState = Just SuccessPopup}})
    Failed -> do
      _ <- pure $ cleverTapCustomEventWithParams "ny_driver_subscription_failure" "selected_plan" planCardConfig.title
      _ <- pure $ cleverTapCustomEventWithParams "ny_driver_subscription_failure" "failure_code" (show apiPaymentStatus)
      liftFlowBT $ metaLogEventWithTwoParams "ny_driver_subscription_failure" "selected_plan" planCardConfig.title "failure_code" (show apiPaymentStatus)
      liftFlowBT $ firebaseLogEventWithTwoParams "ny_driver_subscription_failure" "selected_plan" planCardConfig.title "failure_code" (show apiPaymentStatus)
      modifyScreenState $ SubscriptionScreenStateType (\subscribeScreenState -> subscribeScreenState { props {popUpState = Just FailedPopup}})
    Pending -> modifyScreenState $ SubscriptionScreenStateType (\subscribeScreenState -> subscribeScreenState { props {joinPlanProps {selectedPlanItem = Nothing}}})
    Scheduled -> pure unit

paymentHistoryFlow :: FlowBT String Unit
paymentHistoryFlow = do 
  action <- UI.paymentHistory
  case action of 
    GoToSetupAutoPay state -> nyPaymentFlow state.data.planData true -- to discuss
    EntityDetailsAPI state id -> do
      paymentEntityDetails <- lift $ lift $ Remote.paymentEntityDetails id
      case paymentEntityDetails of
        Right (API.HistoryEntryDetailsEntityV2Resp resp) ->
            modifyScreenState $ PaymentHistoryScreenStateType (\paymentHistoryScreen -> paymentHistoryScreen{props{subView = ST.TransactionDetails},
              data { transactionDetails = (buildTransactionDetails (API.HistoryEntryDetailsEntityV2Resp resp))}
            })
        Left errorPayload -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
      paymentHistoryFlow
    SWITCH_TAB -> paymentHistoryFlow
  pure unit 

ysPaymentFlow :: FlowBT String Unit
ysPaymentFlow = do
  liftFlowBT $ runEffectFn1 initiatePP unit
  (GlobalState state) <- getState
  let homeScreenState = state.homeScreen
  response <- lift $ lift $ Remote.createPaymentOrder homeScreenState.data.paymentState.driverFeeId
  case response of
    Right (CreateOrderRes listResp) -> do
      let (PaymentPagePayload sdk_payload) = listResp.sdk_payload
      setValueToLocalStore DISABLE_WIDGET "true"
      lift $ lift $ doAff $ makeAff \cb -> runEffectFn1 checkPPInitiateStatus (cb <<< Right) $> nonCanceler
      paymentPageOutput <- paymentPageUI listResp.sdk_payload
      pure $ toggleBtnLoader "" false
      setValueToLocalStore DISABLE_WIDGET "false"
      liftFlowBT $ runEffectFn1 consumeBP unit
      if (paymentPageOutput == "backpressed") then homeScreenFlow else pure unit-- backpressed FAIL
      orderStatus <- lift $ lift $ Remote.paymentOrderStatus homeScreenState.data.paymentState.driverFeeId
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
  ackScreenFlow

setPaymentStatus :: PaymentStatus -> PayPayload -> FlowBT String Unit
setPaymentStatus paymentStatus (PayPayload payload) = do
      case paymentStatus of
        Success -> do
                  let currency = (getValueFromConfig "currency")
                  setValueToLocalStore SHOW_PAYMENT_MODAL "false"
                  modifyScreenState $ AcknowledgementScreenType (\a -> a { data {
                    title = Just ( case getValueToLocalStore LANGUAGE_KEY of
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


        Failed -> modifyScreenState $ AcknowledgementScreenType (\ackScreenState -> ackScreenState { data { title = Just (getString PAYMENT_FAILED), description = Just (getString PAYMENT_FAILED_DESC), primaryButtonText = Just  "Retry Payment" , illustrationAsset = "ny_failed,"}, props {illustrationType = ST.Image, paymentStatus = paymentStatus}})

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


ackScreenFlow :: FlowBT String Unit
ackScreenFlow = do
  action <- UI.acknowledgementScreen
  case action of
    EXIT_TO_HOME_SCREEN -> homeScreenFlow
    RETRY_PAYMENT -> ysPaymentFlow


subScriptionFlow :: FlowBT String Unit
subScriptionFlow = do
  modifyScreenState $ SubscriptionScreenStateType (\subscriptionScreen -> subscriptionScreen{props{isSelectedLangTamil = (getValueToLocalNativeStore LANGUAGE_KEY) == "TA_IN"}})
  void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
  uiAction <- UI.subscriptionScreen
  case uiAction of
    NAV HomeScreenNav -> homeScreenFlow
    NAV GoToRideHistory -> myRidesScreenFlow
    NAV GoToContest -> referralScreenFlow
    NAV GoToAlerts -> notificationFlow
    GOTO_HOMESCREEN -> homeScreenFlow
    MAKE_PAYMENT state -> do
      case state.props.joinPlanProps.selectedPlanItem of 
        Just selectedPlan -> do
          setValueToLocalStore DISABLE_WIDGET "true"
          nyPaymentFlow selectedPlan true
        Nothing -> subScriptionFlow
    GOTO_PAYMENT_HISTORY state -> do
      let (GlobalState defGlobalState) = defaultGlobalState
      modifyScreenState $ PaymentHistoryScreenStateType(\_ -> defGlobalState.paymentHistoryScreen{props{autoPaySetup = state.data.myPlanData.autoPayStatus == ACTIVE_AUTOPAY, subView = ST.PaymentHistory}, data{planData = state.data.myPlanData.planEntity}})
      paymentHistoryFlow
    CANCEL_AUTOPAY state -> do
      suspendMandate <- lift $ lift $ Remote.suspendMandate state.data.driverId
      case suspendMandate of 
        Right resp -> do 
          getDriverInfoResp <- Remote.getDriverInfoBT (GetDriverInfoReq { })
          modifyScreenState $ GlobalPropsType (\globalProps -> globalProps {driverInformation = getDriverInfoResp})
          let (GlobalState defGlobalState) = defaultGlobalState
          modifyScreenState $ SubscriptionScreenStateType (\_ -> defGlobalState.subscriptionScreen)
          pure $ toast $ getString AUTOPAY_CANCELLED
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
          _ <- pure $ cleverTapCustomEvent "ny_driver_switch_plan"
          _ <- pure $ cleverTapCustomEventWithParams "ny_driver_switch_plan" "new_plan" state.props.managePlanProps.selectedPlanItem.title
          _ <- pure $ cleverTapCustomEventWithParams "ny_driver_switch_plan" "previous_plan" state.data.managePlanData.currentPlan.title
          liftFlowBT $ metaLogEventWithTwoParams "ny_driver_switch_plan" "new_plan" state.props.managePlanProps.selectedPlanItem.title "previous_plan" state.data.managePlanData.currentPlan.title
          liftFlowBT $ firebaseLogEventWithTwoParams "ny_driver_switch_plan" "new_plan" state.props.managePlanProps.selectedPlanItem.title "previous_plan" state.data.managePlanData.currentPlan.title
          modifyScreenState $ SubscriptionScreenStateType (\subScriptionScreenState -> subScriptionScreenState{props{popUpState = Just SwitchedPlan}, data{managePlanData{currentPlan {title = state.props.managePlanProps.selectedPlanItem.title}}}})
        Left errorPayload -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
      subScriptionFlow
    RESUME_AUTOPAY state -> do
      resumeMandate <- lift $ lift $ Remote.resumeMandate state.data.driverId
      case resumeMandate of 
        Right resp -> do
          getDriverInfoResp <- Remote.getDriverInfoBT (GetDriverInfoReq { })
          modifyScreenState $ GlobalPropsType (\globalProps -> globalProps {driverInformation = getDriverInfoResp})
          let (GlobalState defGlobalState) = defaultGlobalState
          modifyScreenState $ SubscriptionScreenStateType (\_ -> defGlobalState.subscriptionScreen)
          pure $ toast $ getString RESUMED_AUTOPAY
        Left errorPayload -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
      subScriptionFlow
    RETRY_PAYMENT_AC state planId -> nyPaymentFlow state.data.myPlanData.planEntity false
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
      getDriverInfoResp <- Remote.getDriverInfoBT (GetDriverInfoReq { })
      modifyScreenState $ GlobalPropsType (\globalProps -> globalProps {driverInformation = getDriverInfoResp})
      let (GlobalState defGlobalState) = defaultGlobalState
      modifyScreenState $ SubscriptionScreenStateType (\_ -> defGlobalState.subscriptionScreen)
      subScriptionFlow
    GO_TO_MANAGE_PLAN state -> do
      uiPlans <- Remote.getUiPlansBT ""
      modifyScreenState $ SubscriptionScreenStateType (\subScriptionScreenState -> subScriptionScreenState{ data { managePlanData { alternatePlans = alternatePlansTransformer uiPlans state}}, props {subView = ManagePlan, showShimmer = false}})
      subScriptionFlow
    GO_TO_FIND_HELP_CENTRE state -> do
      let currentDriverLat = fromMaybe 0.0 $ Number.fromString $ getValueToLocalNativeStore LAST_KNOWN_LAT
      let currentDriverLon = fromMaybe 0.0 $ Number.fromString $ getValueToLocalNativeStore LAST_KNOWN_LON
      (LatLon lat lon) <- getCurrentLocation currentDriverLat currentDriverLon currentDriverLat currentDriverLon 750
      modifyScreenState $ SubscriptionScreenStateType (\subScriptionScreenState -> subScriptionScreenState{props {subView = FindHelpCentre, showShimmer = false, currentLat = fromMaybe 0.0 $ Number.fromString lat, currentLon = fromMaybe 0.0 $ Number.fromString lon}})
      subScriptionFlow
    REFRESH_HELP_CENTRE state -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> state { props {subView = FindHelpCentre, showShimmer = true}})
      subScriptionFlow
    GO_TO_OPEN_GOOGLE_MAPS state -> do
      _ <- lift $ lift $ fork $ liftFlow $ openNavigation state.props.currentLat state.props.currentLon state.props.destLat state.props.destLon "DRIVE"
      subScriptionFlow
    SUBSCRIBE_API state -> nyPaymentFlow state.data.myPlanData.planEntity false
    CLEAR_DUES_ACT -> do
      void $ lift $ lift $ toggleLoader true
      liftFlowBT $ runEffectFn1 initiatePP unit
      clearduesResp' <- lift $ lift $ Remote.cleardues ""
      case clearduesResp' of
        Right (API.ClearDuesResp clearduesResp) -> do
          let (CreateOrderRes orderResp) = clearduesResp.orderResp
              (PaymentPagePayload sdk_payload) = orderResp.sdk_payload
              (PayPayload innerpayload) = sdk_payload.payload
              finalPayload = PayPayload $ innerpayload{ language = Just (getPaymentPageLangKey (getValueToLocalStore LANGUAGE_KEY)) }
              sdkPayload = PaymentPagePayload $ sdk_payload{payload = finalPayload}
          setValueToLocalStore DISABLE_WIDGET "true"
          _ <- pure $ cleverTapCustomEvent "ny_driver_payment_page_opened"
          _ <- pure $ metaLogEvent "ny_driver_payment_page_opened"
          liftFlowBT $ firebaseLogEvent "ny_driver_payment_page_opened"
          lift $ lift $ doAff $ makeAff \cb -> runEffectFn1 checkPPInitiateStatus (cb <<< Right) $> nonCanceler
          void $ lift $ lift $ toggleLoader false
          _ <- paymentPageUI sdkPayload
          pure $ toggleBtnLoader "" false
          liftFlowBT $ runEffectFn1 consumeBP unit
          setValueToLocalStore DISABLE_WIDGET "false"
          orderStatus <- lift $ lift $ Remote.paymentOrderStatus $ clearduesResp.orderId
          case orderStatus of
            Right (OrderStatusRes statusResp) -> do
              let popUpState = if statusResp.status == PS.CHARGED then Just PaymentSuccessPopup
                                else if any ( _ == statusResp.status)[PS.AUTHORIZATION_FAILED, PS.AUTHENTICATION_FAILED, PS.JUSPAY_DECLINED] then Just FailedPopup
                                else Nothing
              case popUpState of
                Just popUpState' -> modifyScreenState $ SubscriptionScreenStateType (\subscribeScreenState -> subscribeScreenState { props {popUpState = Just popUpState'}})
                Nothing -> pure unit
            Left err -> pure $ toast $ Remote.getCorrespondingErrorMessage err 
        Left errorPayload -> pure $ toast $ Remote.getCorrespondingErrorMessage errorPayload
      void $ lift $ lift $ toggleLoader false
      pure $ toggleBtnLoader "" false
      subScriptionFlow
    _ -> subScriptionFlow

constructLatLong :: String -> String -> Location
constructLatLong lat lng =
  { lat: fromMaybe 0.0 (Number.fromString lat)
  , lon : fromMaybe 0.0 (Number.fromString lng)
  , place : ""
  }

updateCustomerMarker :: Location -> Effect Unit
updateCustomerMarker loc = pure unit

editBankDetailsFlow :: FlowBT String Unit
editBankDetailsFlow = do
  action <- UI.editBankDetailsScreen
  pure unit

editAadhaarDetailsFlow :: FlowBT String Unit
editAadhaarDetailsFlow = do
  action <- UI.editAadhaarDetailsScreen
  pure unit

noInternetScreenFlow :: String -> FlowBT String Unit
noInternetScreenFlow triggertype = do
  action <- UI.noInternetScreen triggertype
  internetCondition <- lift $ lift $ liftFlow $ isInternetAvailable unit
  case action of
    REFRESH_INTERNET -> case ((ifNotRegistered unit) || (getValueToLocalStore IS_DRIVER_ENABLED == "false")) of
                        true -> pure unit
                        false ->  currentRideFlow
    TURN_ON_GPS -> if not internetCondition then noInternetScreenFlow "INTERNET_ACTION"
                    else do
                      (DriverActiveInactiveResp resp) <- Remote.driverActiveInactiveBT "true" $ toUpper $ show Online
                      currentRideFlow
    CHECK_INTERNET -> case ((ifNotRegistered unit) || (getValueToLocalStore IS_DRIVER_ENABLED == "false")) of
                      true  -> pure unit
                      false -> do
                        permissionsGiven <- checkAll3Permissions
                        if permissionsGiven
                          then currentRideFlow
                          else permissionsScreenFlow

checkAll3Permissions :: FlowBT String Boolean
checkAll3Permissions = do
  isLocationPermission <- lift $ lift $ liftFlow $ isLocationPermissionEnabled unit
  isOverlayPermission <- lift $ lift $ liftFlow $ isOverlayPermissionEnabled unit
  isBatteryUsagePermission <- lift $ lift $ liftFlow $ isBatteryPermissionEnabled unit
  pure $ isLocationPermission && isOverlayPermission && isBatteryUsagePermission && getValueToLocalStore TEST_FLOW_FOR_PERMISSIONS == "COMPLETED"

popUpScreenFlow :: AllocationData -> FlowBT String Unit
popUpScreenFlow entityPayload = do
  _ <- pure $ printLog "popupscreen entity payload" entityPayload
  let availableRide = (transformAllocationData entityPayload)
  modifyScreenState $ PopUpScreenStateType (\popUpScreen -> popUpScreen{ data { availableRides = availableRide } })
  action <- UI.popUpScreen
  case action of
    POPUP_REQUEST_RIDE id extraFare -> do
      _ <- pure $ printLog "calling request ride api" "."
      if extraFare > 0.0 then do
        (OfferRideResp resp) <- Remote.offerRideBT (makeOfferRideReq id (Just extraFare))
        pure unit
        else do
          (OfferRideResp resp) <- Remote.offerRideBT (makeOfferRideReq id Nothing)
          pure unit
      homeScreenFlow
    _ -> do
      _ <- pure $ printLog "no action matched" "."
      homeScreenFlow

-- TODO :: use this case when on click of notification we want to go to alert section from app itself
-- alertNotification :: String -> FlowBT String Unit
-- alertNotification id = do
--   modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props{ selectedNotification = Just id } })
--   homeScreenFlow

changeDriverStatus :: DriverStatus -> FlowBT String Unit 
changeDriverStatus status = do 
  _ <- setValueToLocalStore DRIVER_STATUS if any( _ == status)[Online, Silent] then "true" else "false" --(show status)
  _ <- setValueToLocalStore DRIVER_STATUS_N $ show status
  (DriverActiveInactiveResp resp) <- Remote.driverActiveInactiveBT (if any( _ == status)[Online, Silent] then "true" else "false") $ toUpper $ show status
  _ <- setValueToLocalStore RIDE_T_FREQUENCY (if status == Online then "20000" else "30000")
  _ <- setValueToLocalStore DRIVER_MIN_DISPLACEMENT (if any( _ == status)[Online, Silent] then "8.0" else "25.0")
  pure unit 

driverRideRatingFlow :: FlowBT String Unit
driverRideRatingFlow = do
  action <- UI.driverRideRatingScreen
  case action of
    SendCustomerFeedBack updatedState -> do
      --TODO // API CALL for feedback
      homeScreenFlow
    CloseScreen -> homeScreenFlow

notificationFlow :: FlowBT String Unit
notificationFlow = do
  let (GlobalState defGlobalState) = defaultGlobalState
  screenAction <- UI.notifications
  case screenAction of
    REFRESH_SCREEN state -> do
      modifyScreenState $ NotificationsScreenStateType (\notificationScreen -> state{offsetValue = 0})
      notificationFlow
    LOAD_NOTIFICATIONS state -> do
      modifyScreenState $ NotificationsScreenStateType (\notificationScreen -> state{offsetValue = (length state.notificationList)})
      notificationFlow
    GO_HOME_SCREEN -> homeScreenFlow
    GO_REFERRAL_SCREEN -> referralScreenFlow
    GO_RIDE_HISTORY_SCREEN -> myRidesScreenFlow
    GO_PROFILE_SCREEN -> driverProfileFlow
    CHECK_RIDE_FLOW_STATUS -> currentRideFlow
    NOTIFICATION_SCREEN_NAV GoToSubscription -> updateAvailableAppsAndGoToSubs
    NOTIFICATION_SCREEN_NAV _ -> notificationFlow

removeChatService :: String -> FlowBT String Unit
removeChatService _ = do
  _ <- lift $ lift $ liftFlow $ stopChatListenerService
  _ <- pure $ setValueToLocalNativeStore READ_MESSAGES "0"
  pure unit

setDriverStatusInLocal :: String -> String -> FlowBT String Unit
setDriverStatusInLocal status mode = do
                                    setValueToLocalStore DRIVER_STATUS status
                                    setValueToLocalNativeStore DRIVER_STATUS status
                                    setValueToLocalStore DRIVER_STATUS_N mode
                                    setValueToLocalNativeStore DRIVER_STATUS_N mode

getPaymentPageLangKey :: String -> String 
getPaymentPageLangKey key = case key of 
  "EN_US" -> "english"
  "KN_IN" -> "kannada"
  "HI_IN" -> "hindi"
  "ML_IN" -> "malayalam"
  "BN_IN" -> "bengali"
  "TA_IN" -> "tamil"
  _       -> "english"

updateAvailableAppsAndGoToSubs :: FlowBT String Unit
updateAvailableAppsAndGoToSubs = do
  state <- getState
  modifyScreenState $ SubscriptionScreenStateType (\subscriptionScreen -> subscriptionScreen{props{subView = NoSubView, showShimmer = true}})
  void $ liftFlowBT $ launchAff $ flowRunner state $ void $ runExceptT $ runBackT $ getUpiApps
  subScriptionFlow

getUpiApps :: FlowBT String Unit
getUpiApps = do
  resp <- lift $ lift $ doAff $ makeAff (\cb -> (runEffectFn1 getAvailableUpiApps (cb <<< Right) ) $> nonCanceler)
  let (UpdateDriverInfoReq req) = Remote.mkUpdateDriverInfoReq ""
  let appsSupportMandate = runFn1 stringifyJSON $ map (\item -> item.appName) $ filter (\item -> item.supportsMandate) resp
  let appsNotSupportMandate = runFn1 stringifyJSON $ map (\item -> item.appName) $ filter (\item -> not item.supportsMandate) resp
  pure $ setCleverTapUserProp "appsSupportMandate" appsSupportMandate
  pure $ setCleverTapUserProp "appsNotSupportMandate" appsNotSupportMandate
  void $ Remote.updateDriverInfoBT (UpdateDriverInfoReq req{availableUpiApps = Just $ runFn1 stringifyJSON resp})

checkDriverBlockingStatus :: GetDriverInfoResp -> FlowBT String Unit
checkDriverBlockingStatus (GetDriverInfoResp getDriverInfoResp) = do
  if (  any ( _ == (getValueToLocalStore ENABLE_BLOCKING)) ["__failed", "disable"] &&
        isDateGreaterThan "2023-09-20T00:00:00" &&
        getDriverInfoResp.autoPayStatus == Nothing
    ) then do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {driverBlocked = true }})
      when (not getDriverInfoResp.onRide && any ( _ == getDriverInfoResp.mode) [Just "ONLINE", Just "SILENT"]) do
        void $ Remote.driverActiveInactiveBT "false" "OFFLINE"
        homeScreenFlow
  else modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen { props {driverBlocked = false }})
