{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Flow where

import Engineering.Helpers.LogEvent

import Accessor (_computedPrice, _contents, _formattedAddress, _id, _lat, _lon, _status, _toLocation, _signatureAuthData)
import Common.Types.App (GlobalPayload(..), SignatureAuthData(..), Payload(..), Version(..), LocationData(..), EventPayload(..), ClevertapEventParams)
import Common.Types.App (LazyCheck(..))
import Components.LocationListItem.Controller (dummyLocationListState)
import Components.SavedLocationCard.Controller (getCardType)
import Components.SettingSideBar.Controller as SettingSideBarController
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (lift)
import Data.Array (catMaybes, filter, length, null, snoc, (!!), any, sortBy, head, uncons, last)
import Data.Array as Arr
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn3)
import Data.Int as INT
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Number (fromString)
import Data.String (Pattern(..), drop, indexOf, split, toLower, trim, take)
import Debug (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn5, runEffectFn2)
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Engineering.Helpers.Commons (liftFlow, os, getNewIDWithTag, bundleVersion, getExpiryTime, stringToVersion, convertUTCtoISC, getCurrentUTC, getWindowVariable, flowRunner)
import Engineering.Helpers.Suggestions (suggestionsDefinitions, getSuggestions)
import Engineering.Helpers.Utils (loaderText, toggleLoader, saveObject)
import Foreign (MultipleErrors, unsafeToForeign)
import Foreign.Class (class Encode, encode)
import Foreign.Generic (decodeJSON, encodeJSON)
import Helpers.Utils (decodeError, addToPrevCurrLoc, addToRecentSearches, adjustViewWithKeyboard, checkPrediction, clearWaitingTimer, differenceOfLocationLists, drawPolygon, filterRecentSearches, getAssetStoreLink, getCurrentDate, getCurrentLocationMarker, getCurrentLocationsObjFromLocal, getDistanceBwCordinates, getGlobalPayload, getMobileNumber, getNewTrackingId, getObjFromLocal, getPrediction, getRecentSearches, getScreenFromStage, getSearchType, parseFloat, parseNewContacts, removeLabelFromMarker, requestKeyboardShow, saveCurrentLocations, seperateByWhiteSpaces, setText, showCarouselScreen, sortPredctionByDistance, toString, triggerRideStatusEvent, withinTimeRange, fetchDefaultPickupPoint)
import JBridge (metaLogEvent, currentPosition, drawRoute, enableMyLocation, factoryResetApp, firebaseLogEvent, firebaseLogEventWithParams, firebaseLogEventWithTwoParams, getVersionCode, getVersionName, hideKeyboardOnNavigation, isCoordOnPath, isInternetAvailable, isLocationEnabled, isLocationPermissionEnabled, locateOnMap, openNavigation, reallocateMapFragment, removeAllPolylines, toast, toggleBtnLoader, updateRoute, launchInAppRatingPopup, firebaseUserID, addMarker, generateSessionId, stopChatListenerService, updateRouteMarker, setCleverTapUserProp, setCleverTapUserData, cleverTapSetLocation, saveSuggestions, saveSuggestionDefs, hideLoader, emitJOSEvent, setFCMTokenWithTimeOut)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog)
import MerchantConfig.DefaultConfig as DC
import MerchantConfig.Utils (Merchant(..), getMerchant, getValueFromConfig)
import Engineering.Helpers.Utils (getAppConfig)
import Constants as Constants
import MerchantConfig.Utils as MU
import ModifyScreenState (modifyScreenState, updateRideDetails)
import Prelude (Unit, bind, discard, map, mod, negate, not, pure, show, unit, void, when, ($), (&&), (+), (-), (/), (/=), (<), (<=), (<>), (==), (>), (>=), (||), (<$>), (<<<), ($>))
import Presto.Core.Types.Language.Flow (doAff, fork, setLogField, delay)
import Presto.Core.Types.Language.Flow (getLogFields)
import Resources.Constants (DecodeAddress(..), decodeAddress, encodeAddress, getKeyByLanguage, getSearchRadius, getValueByComponent, getWard)
import Screens.AccountSetUpScreen.ScreenData as AccountSetUpScreenData
import Screens.AddNewAddressScreen.Controller (encodeAddressDescription, getSavedLocations, getSavedTags, getLocationList, calculateDistance, getSavedTagsFromHome, validTag, isValidLocation, getLocTag) as AddNewAddress
import Screens.AddNewAddressScreen.ScreenData (dummyLocation) as AddNewAddressScreenData
import Screens.ChooseLanguageScreen.Controller (ScreenOutput(..))
import Screens.EnterMobileNumberScreen.Controller (ScreenOutput(..))
import Screens.EnterMobileNumberScreen.ScreenData as EnterMobileNumberScreenData
import Screens.Handlers as UI
import Screens.HelpAndSupportScreen.ScreenData as HelpAndSupportScreenData
import Screens.EmergencyContactsScreen.ScreenData as EmergencyContactsScreenData
import Screens.HomeScreen.Controller (flowWithoutOffers, getSearchExpiryTime, isTipEnabled, getSpecialTag, findingQuotesSearchExpired, getZoneType)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.HomeScreen.Transformer (getLocationList, getDriverInfo, dummyRideAPIEntity, encodeAddressDescription, getPlaceNameResp, getUpdatedLocationList, transformContactList)
import Screens.InvoiceScreen.Controller (ScreenOutput(..)) as InvoiceScreenOutput
import Screens.HomeScreen.ScreenData (dummyRideBooking)
import Screens.MyProfileScreen.ScreenData as MyProfileScreenData
import Screens.ReferralScreen.ScreenData as ReferralScreen
import Screens.RideBookingFlow.HomeScreen.Config (getTipViewData, setTipViewData)
import Screens.RideBookingFlow.HomeScreen.Config (specialLocationIcons, specialLocationConfig, updateRouteMarkerConfig)
import Screens.SavedLocationScreen.Controller (getSavedLocationForAddNewAddressScreen)
import Screens.SelectLanguageScreen.ScreenData as SelectLanguageScreenData
import Screens.Types (CardType(..), AddNewAddressScreenState(..), SearchResultType(..), CurrentLocationDetails(..), CurrentLocationDetailsWithDistance(..), DeleteStatus(..), HomeScreenState, LocItemType(..), PopupType(..), SearchLocationModelType(..), Stage(..), LocationListItemState, LocationItemType(..), NewContacts, NotifyFlowEventType(..), FlowStatusData(..), ErrorType(..), ZoneType(..), TipViewData(..),TripDetailsGoBackType(..), Location, DisabilityT(..))
import Screens.Types (Gender(..)) as Gender
import Services.API (AddressGeometry(..), BookingLocationAPIEntity(..), CancelEstimateRes(..), ConfirmRes(..), ContactDetails(..), DeleteSavedLocationReq(..), FlowStatus(..), FlowStatusRes(..), GatesInfo(..), Geometry(..), GetDriverLocationResp(..), GetEmergContactsReq(..), GetEmergContactsResp(..), GetPlaceNameResp(..), GetProfileRes(..), LatLong(..), LocationS(..), LogOutReq(..), LogOutRes(..), PlaceName(..), ResendOTPResp(..), RideAPIEntity(..), RideBookingAPIDetails(..), RideBookingDetails(..), RideBookingListRes(..), RideBookingRes(..), Route(..), SavedLocationReq(..), SavedLocationsListRes(..), SearchLocationResp(..), SearchRes(..), ServiceabilityRes(..), SpecialLocation(..), TriggerOTPResp(..), UserSosRes(..), VerifyTokenResp(..), ServiceabilityResDestination(..), SelectEstimateRes(..), UpdateProfileReq(..), OnCallRes(..), Snapped(..), AddressComponents(..), FareBreakupAPIEntity(..), GetDisabilityListResp(..), Disability(..))
import Services.API (AuthType(..), AddressGeometry(..), BookingLocationAPIEntity(..), CancelEstimateRes(..), ConfirmRes(..), ContactDetails(..), DeleteSavedLocationReq(..), FlowStatus(..), FlowStatusRes(..), GatesInfo(..), Geometry(..), GetDriverLocationResp(..), GetEmergContactsReq(..), GetEmergContactsResp(..), GetPlaceNameResp(..), GetProfileRes(..), LatLong(..), LocationS(..), LogOutReq(..), LogOutRes(..), PlaceName(..), ResendOTPResp(..), RideAPIEntity(..), RideBookingAPIDetails(..), RideBookingDetails(..), RideBookingListRes(..), RideBookingRes(..), Route(..), SavedLocationReq(..), SavedLocationsListRes(..), SearchLocationResp(..), SearchRes(..), ServiceabilityRes(..), SpecialLocation(..), TriggerOTPResp(..), UserSosRes(..), VerifyTokenResp(..), ServiceabilityResDestination(..), TriggerSignatureOTPResp(..), User(..), OnCallRes(..))
import Services.Backend as Remote
import Services.Config (getBaseUrl)
import Storage (KeyStore(..), deleteValueFromLocalStore, getValueToLocalNativeStore, getValueToLocalStore, isLocalStageOn, setValueToLocalNativeStore, setValueToLocalStore, updateLocalStage)
import Types.App (ABOUT_US_SCREEN_OUTPUT(..), ACCOUNT_SET_UP_SCREEN_OUTPUT(..), ADD_NEW_ADDRESS_SCREEN_OUTPUT(..), GlobalState(..), CONTACT_US_SCREEN_OUTPUT(..), FlowBT, HELP_AND_SUPPORT_SCREEN_OUTPUT(..), HOME_SCREEN_OUTPUT(..), MY_PROFILE_SCREEN_OUTPUT(..), MY_RIDES_SCREEN_OUTPUT(..), PERMISSION_SCREEN_OUTPUT(..), REFERRAL_SCREEN_OUPUT(..), SAVED_LOCATION_SCREEN_OUTPUT(..), SELECT_LANGUAGE_SCREEN_OUTPUT(..), ScreenType(..), TRIP_DETAILS_SCREEN_OUTPUT(..), EMERGECY_CONTACTS_SCREEN_OUTPUT(..), WELCOME_SCREEN_OUTPUT(..), defaultGlobalState)
import Effect.Aff (makeAff, nonCanceler, launchAff)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Screens.AccountSetUpScreen.Transformer (getDisabilityList)

baseAppFlow :: GlobalPayload -> Boolean-> FlowBT String Unit
baseAppFlow (GlobalPayload gPayload) refreshFlow = do
  logField_ <- lift $ lift $ getLogFields
  _ <- pure $ printLog "Global Payload" gPayload
  (GlobalState state) <- getState
  let bundle = bundleVersion unit
      customerId = (getValueToLocalStore CUSTOMER_ID)
  versionCode <- lift $ lift $ liftFlow $ getVersionCode
  versionName <- lift $ lift $ liftFlow $ getVersionName
  void $ pure $ setCleverTapUserProp "App Version" versionName
  checkVersion versionCode versionName
  setValueToLocalStore VERSION_NAME $ concatString $ Arr.take 3 $ split (Pattern ".") versionName
  setValueToLocalStore BUNDLE_VERSION bundle
  void $ pure $ setCleverTapUserProp "Bundle version" bundle
  setValueToLocalNativeStore BUNDLE_VERSION bundle
  _ <- pure $ setValueToLocalStore TRACKING_DRIVER "False"
  _ <- pure $ setValueToLocalStore TRACKING_ENABLED "True"
  _ <- pure $ setValueToLocalStore RELOAD_SAVED_LOCATION "true"
  _ <- pure $ setValueToLocalStore TEST_MINIMUM_POLLING_COUNT if (flowWithoutOffers WithoutOffers) then "4" else "17"
  _ <- pure $ setValueToLocalStore TEST_POLLING_INTERVAL if (flowWithoutOffers WithoutOffers) then "8000.0" else "1500.0"
  _ <- pure $ setValueToLocalStore TEST_POLLING_COUNT if (flowWithoutOffers WithoutOffers) then "22" else "117"
  _ <- pure $ setValueToLocalStore RATING_SKIPPED "false"
  _ <- pure $ setValueToLocalStore POINTS_FACTOR "3"
  _ <- pure $ setValueToLocalStore BASE_URL (getBaseUrl "dummy")
  _ <- pure $ setValueToLocalStore ACCURACY_THRESHOLD "23.0"
  if ((getValueToLocalStore COUNTRY_CODE == "__failed") || (getValueToLocalStore COUNTRY_CODE == "(null)")) then do
    setValueToLocalStore COUNTRY_CODE "+91"
  else pure unit
  _ <- pure $ setValueToLocalStore MESSAGES_DELAY "0"
  _ <- pure $ saveSuggestions "SUGGESTIONS" (getSuggestions "")
  _ <- pure $ saveSuggestionDefs "SUGGESTIONS_DEFINITIONS" (suggestionsDefinitions "")
  when ((getValueToLocalStore SESSION_ID == "__failed") || (getValueToLocalStore SESSION_ID == "(null)")) $ do
    setValueToLocalStore SESSION_ID (generateSessionId unit)
  if (customerId == "__failed") then do
    _ <- lift $ lift $ setLogField "customer_id" $ encode ("null")
    pure unit
    else do
      _ <- lift $ lift $ setLogField "customer_id" $ encode (customerId)
      pure unit
  _ <- lift $ lift $ setLogField "app_version" $ encode (show versionCode)
  _ <- lift $ lift $ setLogField "bundle_version" $ encode (bundle)
  _ <- lift $ lift $ setLogField "platform" $ encode (os)
  when (not refreshFlow) $ void $ UI.splashScreen state.splashScreen
  _ <- lift $ lift $ liftFlow $ logEventWithParams logField_ "ny_user_app_version" "version" versionName
  _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_entered_app"
  if (getValueToLocalStore COUNTRY_CODE == "__failed") || (getValueToLocalStore COUNTRY_CODE == "(null)")
  then do
    setValueToLocalStore COUNTRY_CODE "+91"
  else pure unit
  if (getMerchant FunctionCall) == PASSCULTURE then setValueToLocalStore LANGUAGE_KEY $ getValueFromConfig "defaultLanguage"
    else pure unit
  if getValueToLocalStore REGISTERATION_TOKEN /= "__failed" && getValueToLocalStore REGISTERATION_TOKEN /= "(null)" &&  (isNothing $ (gPayload.payload)^._signatureAuthData)
    then currentFlowStatus
    else do
      let (Payload payload) = gPayload.payload
      case payload.signatureAuthData of
        Just (SignatureAuthData signatureAuth) -> do
          response <- lift $ lift $ Remote.triggerSignatureBasedOTP (SignatureAuthData signatureAuth)
          case response of
            Right (TriggerSignatureOTPResp triggerSignatureOtpResp) -> do
              case triggerSignatureOtpResp.person of
                Just (User person) -> do
                  lift $ lift $ setLogField "customer_id" $ encode (person.id)
                  setValueToLocalStore CUSTOMER_ID person.id
                  mobileNumber <- liftFlowBT $ runEffectFn2 getMobileNumber signatureAuth.authData (fromMaybe "" person.maskedMobileNumber)
                  setValueToLocalStore MOBILE_NUMBER mobileNumber
                _ -> pure unit
              case triggerSignatureOtpResp.token of
                Just token -> setValueToLocalStore REGISTERATION_TOKEN token
                Nothing -> pure unit
              currentFlowStatus
            Left err -> do
              liftFlowBT $ pure $ runFn3 emitJOSEvent "java" "onEvent" $ encode $  EventPayload { event : "signature_auth_failed", payload : Nothing}
              pure unit
        Nothing -> if (showCarouselScreen FunctionCall) then welcomeScreenFlow else enterMobileNumberScreenFlow

concatString :: Array String -> String
concatString arr = case uncons arr of
  Just { head: x, tail: xs } -> x <> (if length xs == 0 then "" else ".") <> concatString xs
  Nothing -> ""

-- IOS latest version : 1.2.4
type IosVersion = {
  majorUpdateIndex :: Int,
  minorUpdateIndex :: Int,
  patchUpdateIndex :: Int,
  enableForceUpdateIOS :: Boolean
}

getIosVersion :: MU.Merchant -> IosVersion
getIosVersion merchant =
  case merchant of
    MU.NAMMAYATRI -> { majorUpdateIndex : 1,
                    minorUpdateIndex : 2,
                    patchUpdateIndex : 4,
                    enableForceUpdateIOS : false
                  }
    MU.YATRI -> { majorUpdateIndex : 1,
               minorUpdateIndex : 1,
               patchUpdateIndex : 0,
               enableForceUpdateIOS : true
              }
    MU.YATRISATHI -> { majorUpdateIndex : 0,
                     minorUpdateIndex : 1,
                     patchUpdateIndex : 0,
                     enableForceUpdateIOS : false
                    }
    _ ->  { majorUpdateIndex : 0,
            minorUpdateIndex : 1,
            patchUpdateIndex : 0,
            enableForceUpdateIOS : false
          }


checkVersion :: Int -> String -> FlowBT String Unit
checkVersion versioncodeAndroid versionName= do
  void $ pure $ setCleverTapUserProp "Platform" os
  logField_ <- lift $ lift $ getLogFields
  let updatedIOSversion = getIosVersion (getMerchant FunctionCall)
  if os /= "IOS" && versioncodeAndroid < (getLatestAndroidVersion (getMerchant FunctionCall)) then do
    liftFlowBT $ hideLoader
    _ <- UI.handleAppUpdatePopUp
    _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_app_update_pop_up_view"
    checkVersion versioncodeAndroid versionName
    else if os == "IOS" && versionName /= "" && updatedIOSversion.enableForceUpdateIOS then do

      let versionArray = (split (Pattern ".") versionName)
          majorUpdateIndex = fromMaybe (-1) $ INT.fromString $ fromMaybe "NA" $ versionArray !! 0
          minorUpdateIndex = fromMaybe (-1) $ INT.fromString $ fromMaybe "NA" $ versionArray !! 1
          patchUpdateIndex = fromMaybe (-1) $ INT.fromString $ fromMaybe "NA" $ versionArray !! 2

      if any (_ == -1) [majorUpdateIndex, minorUpdateIndex, patchUpdateIndex] then pure unit
        else if forceIOSupdate majorUpdateIndex minorUpdateIndex patchUpdateIndex updatedIOSversion then do
          liftFlowBT $ hideLoader
          _ <- UI.handleAppUpdatePopUp
          _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_app_update_pop_up_view"
          checkVersion versioncodeAndroid versionName
          else pure unit
      else pure unit


getLatestAndroidVersion :: Merchant -> Int
getLatestAndroidVersion merchant =
  case merchant of
    NAMMAYATRI -> 31
    YATRI -> 49
    YATRISATHI -> 2
    _ -> 1

forceIOSupdate :: Int -> Int -> Int -> IosVersion -> Boolean
forceIOSupdate c_maj c_min c_patch updatedIOSversion=
  c_maj < updatedIOSversion.majorUpdateIndex ||
  c_min < updatedIOSversion.minorUpdateIndex ||
  c_patch < updatedIOSversion.patchUpdateIndex

currentRideFlow :: Boolean -> FlowBT String Unit
currentRideFlow rideAssigned = do
  logField_ <- lift $ lift $ getLogFields
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data {config = config}})
  rideBookingListResponse <- lift $ lift $ Remote.rideBookingList "1" "0" "true"
  (GlobalState state') <- getState
  let state = state'.homeScreen
  case rideBookingListResponse of
    Right (RideBookingListRes listResp) -> do
      if not (null listResp.list) then do
        when (not rideAssigned) $ lift $ lift $ liftFlow $ logEvent logField_ "ny_active_ride_with_idle_state"
        let (RideBookingRes resp) = (fromMaybe dummyRideBooking (listResp.list !! 0))
            status = (fromMaybe dummyRideAPIEntity ((resp.rideList) !! 0))^._status
            rideStatus = if status == "NEW" then RideAccepted else RideStarted
            newState = state{data{driverInfoCardState = getDriverInfo state.data.specialZoneSelectedVariant (RideBookingRes resp) ((length resp.rideList) == 0 )
                , finalAmount = fromMaybe 0 ((fromMaybe dummyRideAPIEntity (resp.rideList !!0) )^. _computedPrice)
                , currentSearchResultType = if (length resp.rideList) == 0 then QUOTES else ESTIMATES},
                  props{currentStage = rideStatus
                  , rideRequestFlow = true
                  , bookingId = resp.id
                  , isPopUp = NoPopUp
                  , zoneType = getSpecialTag resp.specialLocationTag
                  }}
        when (not rideAssigned) $ do
          void $ pure $ logEventWithTwoParams logField_ "ny_active_ride_with_idle_state" "status" status "bookingId" resp.id
        _ <- pure $ spy "Active api" listResp
        modifyScreenState $ HomeScreenStateType (\homeScreen → newState)
        updateLocalStage rideStatus
        let (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
        let (RideBookingDetails contents) = bookingDetails.contents
        let otpCode = contents.otpCode
        let rideList =  (resp.rideList !!0)
        case rideList of
          Nothing -> do
            case otpCode of
              Just otp' -> do
                _ <- pure $ setValueToLocalStore TRACKING_ENABLED "True"
                modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{props{isSpecialZone = true, isInApp = true}, data{driverInfoCardState{otp = otp'}}})
              Nothing -> pure unit
          Just (RideAPIEntity _) ->
            if otpCode /= Nothing then do
              _ <- pure $ setValueToLocalStore TRACKING_ENABLED "True"
              modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{props{isSpecialZone = true,isInApp = true }}) else
              pure unit
      else if ((getValueToLocalStore RATING_SKIPPED) == "false") then do
        updateLocalStage HomeScreen
        rideBookingListResponse <- lift $ lift $ Remote.rideBookingList "1" "0" "false"
        case rideBookingListResponse of
          Right (RideBookingListRes listResp) -> do
            let (RideBookingRes resp) = (fromMaybe dummyRideBooking (listResp.list !! 0))
            let (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
            let (RideBookingDetails contents) = bookingDetails.contents
            let (RideAPIEntity currRideListItem) = (fromMaybe dummyRideAPIEntity (resp.rideList !!0))
            _ <- pure $ spy "CurrentRideListItem" currRideListItem
            let differenceOfDistance = fromMaybe 0 contents.estimatedDistance - (fromMaybe 0 currRideListItem.chargeableRideDistance)
            let lastRideDate = (case currRideListItem.rideStartTime of
                                Just startTime -> (convertUTCtoISC startTime "DD/MM/YYYY")
                                Nothing        -> "")
                currentDate =  getCurrentDate ""
            if(lastRideDate /= currentDate) then do
              _ <- pure $ setValueToLocalStore FLOW_WITHOUT_OFFERS "true"
              _ <- pure $ setValueToLocalStore TEST_MINIMUM_POLLING_COUNT "4"
              _ <- pure $ setValueToLocalStore TEST_POLLING_INTERVAL "8000.0"
              _ <- pure $ setValueToLocalStore TEST_POLLING_COUNT "22"
              pure unit
              else pure unit
            when (isNothing currRideListItem.rideRating) $ do
              when (resp.status /= "CANCELLED" && length listResp.list > 0) $ do
                modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{
                    props { currentStage = RideCompleted
                          , estimatedDistance = contents.estimatedDistance
                          , zoneType = getSpecialTag resp.specialLocationTag}
                  , data { rideRatingState
                          { driverName = currRideListItem.driverName
                          , rideId = currRideListItem.id
                          , finalAmount = (fromMaybe 0 currRideListItem.computedPrice)
                          , source = decodeAddress (Booking resp.fromLocation)
                          , destination = (decodeAddress (Booking (resp.bookingDetails ^._contents^._toLocation)))
                          , vehicleNumber = (currRideListItem.vehicleNumber)
                          , status = (currRideListItem.status)
                          , shortRideId = currRideListItem.shortRideId
                          , rideEndTimeUTC = ""
                          , offeredFare = resp.estimatedTotalFare
                          , distanceDifference = differenceOfDistance
                          , bookingId = resp.id
                          , feedback = ""
                          , rideStartTime = case currRideListItem.rideStartTime of
                                              Just startTime -> (convertUTCtoISC startTime "h:mm A")
                                              Nothing        -> ""
                          , rideEndTime   = case currRideListItem.rideEndTime of
                                              Just endTime   -> " " <>(convertUTCtoISC endTime "h:mm A")
                                              Nothing        -> ""
                          , rideStartDate = case currRideListItem.rideStartTime of
                                              Just startTime ->( (fromMaybe "" ((split (Pattern ",") (convertUTCtoISC startTime "llll")) !!0 )) <> ", " <>  (convertUTCtoISC startTime "Do MMM") )
                                              Nothing        -> ""
                          , dateDDMMYY =  case currRideListItem.rideStartTime of
                                            Just startTime -> (convertUTCtoISC startTime "DD/MM/YYYY")
                                            Nothing        -> ""
                          }
                          , config = config
                          , finalAmount = (fromMaybe 0 currRideListItem.computedPrice)
                          , driverInfoCardState {
                            price = resp.estimatedTotalFare,
                            rideId = currRideListItem.id
                          }
                          , ratingViewState { rideBookingRes = (RideBookingRes resp)}
                          }
                })
                updateLocalStage RideCompleted
          Left err -> updateLocalStage HomeScreen
      else do
        updateLocalStage HomeScreen
    Left err -> updateLocalStage HomeScreen
  if not (isLocalStageOn RideAccepted) then removeChatService "" else pure unit

currentFlowStatus :: FlowBT String Unit
currentFlowStatus = do
  void $ lift $ lift $ toggleLoader false
  _ <- pure $ spy "currentFlowStatus" ":::"
  _ <- pure $ setValueToLocalStore DRIVER_ARRIVAL_ACTION "TRIGGER_DRIVER_ARRIVAL"
  verifyProfile "LazyCheck"
  (FlowStatusRes flowStatus) <- Remote.flowStatusBT "LazyCheck"
  void $ pure $ spy "flowStatus" flowStatus
  case flowStatus.currentStatus of
    WAITING_FOR_DRIVER_OFFERS currentStatus -> goToFindingQuotesStage currentStatus.estimateId false
    DRIVER_OFFERED_QUOTE currentStatus      -> goToFindingQuotesStage currentStatus.estimateId true
    RIDE_ASSIGNED _                         -> currentRideFlow true
    _                                       -> currentRideFlow false
  liftFlowBT $ hideLoader
  _ <- pure $ hideKeyboardOnNavigation true
  homeScreenFlow
  where
    verifyProfile :: String -> FlowBT String Unit
    verifyProfile dummy = do
      (GetProfileRes response) <- Remote.getProfileBT ""
      let dbBundleVersion = response.bundleVersion
          dbClientVersion = response.clientVersion
          maskedDeviceToken = response.maskedDeviceToken
      updateCustomerVersion dbClientVersion dbBundleVersion
      when (any (_ == maskedDeviceToken) [Just "__f...led", Just "..."]) do
        void $ liftFlowBT $ launchAff $ flowRunner defaultGlobalState $ do void $ runExceptT $ runBackT $ checkAndUpdateToken 5
      if isJust response.language then do
        when (getKeyByLanguage (fromMaybe "ENGLISH" response.language) /= (getValueToLocalNativeStore LANGUAGE_KEY)) $ do
          resp <- lift $ lift $ Remote.updateProfile (Remote.mkUpdateProfileRequest FunctionCall)
          pure unit
      else do
        resp <- lift $ lift $ Remote.updateProfile (Remote.mkUpdateProfileRequest FunctionCall)
        pure unit

      let middleName = case response.middleName of
                    Just ""  -> ""
                    Just name -> (" " <> name)
                    Nothing -> ""
          lastName   = case response.lastName of
                    Just "" -> ""
                    Just name -> (" " <> name)
                    Nothing -> ""
          name = (fromMaybe "" response.firstName) <> middleName <> lastName
      void $ pure $ setCleverTapUserData "Name" name

      when (fromMaybe "UNKNOWN" (response.gender) /= "UNKNOWN") $ do
          case response.gender of
              Just value -> void $ pure $ setCleverTapUserData "gender" value
              Nothing -> pure unit

      case response.language of
          Just value -> void $ pure $ setCleverTapUserData "Preferred Language" value
          Nothing -> pure unit

      void $ pure $ setCleverTapUserData "Identity" (getValueToLocalStore CUSTOMER_ID)
      void $ pure $ setCleverTapUserData "Phone" ("+91" <> (getValueToLocalStore MOBILE_NUMBER))
      setValueToLocalStore DISABILITY_UPDATED $ if (isNothing response.hasDisability) then "false" else "true"
      setValueToLocalStore REFERRAL_STATUS  $ if response.hasTakenRide then "HAS_TAKEN_RIDE" else if (response.referralCode /= Nothing && not response.hasTakenRide) then "REFERRED_NOT_TAKEN_RIDE" else "NOT_REFERRED_NOT_TAKEN_RIDE"
      setValueToLocalStore HAS_TAKEN_FIRST_RIDE if response.hasTakenRide then "true" else "false"
      void $ pure $ setCleverTapUserProp "First ride taken" if response.hasTakenRide then "true" else "false"
      if (((fromMaybe "" response.firstName) == "" ) && not (isJust response.firstName)) then do
        _ <- updateLocalStage HomeScreen
        liftFlowBT $ hideLoader
        accountSetUpScreenFlow
      else do
          modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{data{settingSideBar{name =fromMaybe "" response.firstName}}})
          setValueToLocalStore USER_NAME ((fromMaybe "" response.firstName) <> " " <> (fromMaybe "" response.middleName) <> " " <> (fromMaybe "" response.lastName))
      if (fromMaybe "UNKNOWN" (response.gender) /= "UNKNOWN") then do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{gender = Just (fromMaybe "" response.gender)}} , props {isBanner = false}})
        else pure unit
      if isJust response.email then do
        setValueToLocalStore USER_EMAIL $ fromMaybe "" response.email
        case response.email of
            Just value -> void $ pure $ setCleverTapUserData "Email" value
            Nothing -> pure unit
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{email = Just (fromMaybe "" response.email)}}})
        else pure unit

    goToFindingQuotesStage :: String -> Boolean -> FlowBT String Unit
    goToFindingQuotesStage estimateId driverOfferedQuote = do
      if any (_ == (getValueToLocalStore FINDING_QUOTES_START_TIME)) ["__failed", ""] then do
        updateFlowStatus SEARCH_CANCELLED
      else do
        let searchExpiryTime = getSearchExpiryTime "LazyCheck"
            secondsLeft = findingQuotesSearchExpired driverOfferedQuote
        if secondsLeft > 0 then do
          _ <- pure $ setValueToLocalStore RATING_SKIPPED "true"
          updateLocalStage FindingQuotes
          setValueToLocalStore AUTO_SELECTING ""
          setValueToLocalStore FINDING_QUOTES_POLLING "false"
          _ <- pure $ setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
          (GlobalState currentState) <- getState
          let tipViewData = case (getTipViewData "LazyCheck") of
                              Just (TipViewData tipView) -> do
                                currentState.homeScreen.props.tipViewProps{stage = tipView.stage , activeIndex = tipView.activeIndex , isVisible = tipView.isVisible }
                              Nothing -> do
                                currentState.homeScreen.props.tipViewProps
          case (getFlowStatusData "LazyCheck") of
            Just (FlowStatusData flowStatusData) -> do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{
                props{ sourceLat = flowStatusData.source.lat
                     , sourceLong = flowStatusData.source.lng
                     , destinationLat = flowStatusData.destination.lat
                     , destinationLong = flowStatusData.destination.lng
                     , currentStage = FindingQuotes
                     , searchExpire = secondsLeft
                     , estimateId = estimateId
                     , rideRequestFlow = true
                     , customerTip{
                        enableTips = (getValueToLocalStore ENABLE_TIPS) == "true"
                      }
                     , selectedQuote = Nothing
                     , tipViewProps = tipViewData
                     , findingQuotesProgress = 1.0 - (INT.toNumber secondsLeft)/(INT.toNumber searchExpiryTime)}
                , data { source = flowStatusData.source.place
                       , destination = flowStatusData.destination.place
                       , sourceAddress = flowStatusData.sourceAddress
                       , destinationAddress = flowStatusData.destinationAddress }
                })
            Nothing -> do
              updateFlowStatus SEARCH_CANCELLED
        else do
          updateFlowStatus SEARCH_CANCELLED


chooseLanguageScreenFlow :: FlowBT String Unit
chooseLanguageScreenFlow = do
  logField_ <- lift $ lift $ getLogFields
  liftFlowBT $ hideLoader
  setValueToLocalStore LANGUAGE_KEY $ getValueFromConfig "defaultLanguage"
  _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_choose_lang_scn_view"
  flow <- UI.chooseLanguageScreen
  case flow of
    NextScreen language -> do
                            setValueToLocalStore LANGUAGE_KEY language
                            void $ pure $ setCleverTapUserProp "Preferred Language" language
                            _ <- lift $ lift $ liftFlow $(logEventWithParams logField_ "ny_user_lang_choose" "language" (language))
                            enterMobileNumberScreenFlow
    Refresh state -> chooseLanguageScreenFlow

data FCMToken = FCMToken String
checkAndUpdateToken :: Int -> FlowBT String Unit
checkAndUpdateToken count = do
    if count > 0 then do
      (FCMToken newToken) <- lift $ lift $ doAff $ makeAff \cb -> setFCMTokenWithTimeOut 5000 (cb <<< Right) FCMToken $> nonCanceler
      if newToken == "NOT_FOUND" then checkAndUpdateToken (count - 1)
          else do
              let (UpdateProfileReq initialData) = Remote.mkUpdateProfileRequest FunctionCall
                  requiredData = initialData{deviceToken = Just newToken}
              void $ lift $ lift $ Remote.updateProfile (UpdateProfileReq requiredData)
    else pure unit

updateCustomerVersion :: Maybe Version -> Maybe Version -> FlowBT String Unit
updateCustomerVersion dbClientVersion dbBundleVersion = do
  if (isJust dbClientVersion
  && isJust dbBundleVersion) then do
    let versionName = getValueToLocalStore VERSION_NAME
        bundle = getValueToLocalStore BUNDLE_VERSION
        Version clientVersion = stringToVersion versionName
        Version bundleVersion = stringToVersion bundle
        Version bundleVersion' = fromMaybe (Version bundleVersion) dbBundleVersion
        Version clientVersion' = fromMaybe (Version clientVersion) dbClientVersion
    if any (_ == -1) [clientVersion.minor, clientVersion.major, clientVersion.maintenance,bundleVersion.minor,bundleVersion.major,bundleVersion.maintenance] then pure unit
      else if ( bundleVersion' /= bundleVersion || clientVersion' /= clientVersion)  then do
      let (UpdateProfileReq initialData) = Remote.mkUpdateProfileRequest FunctionCall
          requiredData = initialData{clientVersion = Just (Version clientVersion), bundleVersion = Just (Version bundleVersion)}
      resp <- lift $ lift $ Remote.updateProfile (UpdateProfileReq requiredData)
      pure unit
    else pure unit
  else pure unit

enterMobileNumberScreenFlow :: FlowBT String Unit
enterMobileNumberScreenFlow = do
  liftFlowBT $ hideLoader -- Removed initial choose langauge screen
  if(getValueToLocalStore LANGUAGE_KEY == "__failed") then setValueToLocalStore LANGUAGE_KEY $ getValueFromConfig "defaultLanguage" else pure unit
  void $ lift $ lift $ toggleLoader false
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen {data {config =  config }})
  logField_ <- lift $ lift $ getLogFields
  _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_enter_mob_num_scn_view"
  flow <- UI.enterMobileNumberScreen
  case flow of
    GoToAccountSetUp state -> do
            void $ lift $ lift $ loaderText (getString VERIFYING_OTP) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)  -- TODO : Handlde Loader in IOS Side
            void $ lift $ lift $ toggleLoader true
            let generatedID = "generated_" <> (generateSessionId unit)
            (resp) <- lift $ lift $  Remote.verifyToken (Remote.makeVerifyOTPReq state.data.otp generatedID) state.data.tokenId
            case resp of
              Right resp -> do
                    _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_verify_otp"
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen {props {enterOTP = false}})
                    let (VerifyTokenResp response) = resp
                        customerId = ((response.person)^. _id)
                    if (customerId == "__failed") then do
                      _ <- lift $ lift $ setLogField "customer_id" $ encode ("null")
                      pure unit
                      else do
                        _ <- lift $ lift $ setLogField "customer_id" $ encode (customerId)
                        pure unit
                    setValueToLocalStore CUSTOMER_ID customerId
                    void $ pure $ setCleverTapUserData "Identity" (getValueToLocalStore CUSTOMER_ID)
                    setValueToLocalStore REGISTERATION_TOKEN response.token
                    currentFlowStatus
              Left err -> do
                pure $ setText (getNewIDWithTag "EnterOTPNumberEditText") ""
                let errResp = err.response
                    codeMessage = decodeError errResp.errorMessage "errorCode"
                if ( err.code == 400 && codeMessage == "TOKEN_EXPIRED") then do
                    _ <- pure $ toast (getString OTP_PAGE_HAS_BEEN_EXPIRED_PLEASE_REQUEST_OTP_AGAIN)
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> enterMobileNumber{data{otp=""}, props{enterOTP = false, wrongOTP = false}})
                else if ( err.code == 400 && codeMessage == "INVALID_AUTH_DATA") then do
                    let attemptsLeft = decodeError errResp.errorMessage "errorPayload"
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> enterMobileNumber{props{wrongOTP = true, btnActiveOTP = false, attemptLeft = attemptsLeft}, data{otp=""}})
                else if ( err.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then do
                    pure $ toast (getString TOO_MANY_LOGIN_ATTEMPTS_PLEASE_TRY_AGAIN_LATER)
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen {props {enterOTP = false, wrongOTP = false}, data{otp=""}})
                else do
                    pure $ toast (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen {props {enterOTP = false,wrongOTP = false}, data{otp=""}})
                enterMobileNumberScreenFlow
    GoToOTP state -> do
            setValueToLocalStore MOBILE_NUMBER (state.data.mobileNumber)
            setValueToLocalStore COUNTRY_CODE (state.data.countryObj.countryCode)
            void $ pure $ setCleverTapUserData "Phone" (state.data.countryObj.countryCode <> (getValueToLocalStore MOBILE_NUMBER))
            (TriggerOTPResp triggerOtpResp) <- Remote.triggerOTPBT (Remote.makeTriggerOTPReq state.data.mobileNumber state.data.countryObj.countryCode (show state.data.otpChannel))
            modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen { data { tokenId = triggerOtpResp.authId, attempts = triggerOtpResp.attempts}, props {enterOTP = true,resendEnable = false}})
            modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{data{settingSideBar{number = state.data.mobileNumber}}})
            enterMobileNumberScreenFlow
    ResendOTP state -> do
            (ResendOTPResp resendResp) <- Remote.resendOTPBT state.data.tokenId
            modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen { data { tokenId = resendResp.authId, attempts = resendResp.attempts}})
            enterMobileNumberScreenFlow
    GoBack state  ->  do
            modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen { data {timer = 30 }, props {enterOTP = false,resendEnable = false}})
            enterMobileNumberScreenFlow
    GoToWelcomeScreen state -> welcomeScreenFlow

welcomeScreenFlow :: FlowBT String Unit
welcomeScreenFlow = do
  liftFlowBT $ hideLoader
  flow <- UI.welcomeScreen
  case flow of
    GoToMobileNumberScreen -> enterMobileNumberScreenFlow

accountSetUpScreenFlow :: FlowBT String Unit
accountSetUpScreenFlow = do
  logField_ <- lift $ lift $ getLogFields
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> accountSetUpScreen{data{config = config}})
  disabilityListT <- updateDisabilityList "Account_Set_Up_Screen"
  modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> accountSetUpScreen{data{disabilityOptions{disabilityOptionList = disabilityListT }}})
  flow <- UI.accountSetUpScreen
  case flow of
    GO_HOME state -> do
      void $ lift $ lift $ toggleLoader false
      let gender = getGenderValue state.data.gender
          selectedDisability = state.data.disabilityOptions.selectedDisability
          (UpdateProfileReq initialData) = Remote.mkUpdateProfileRequest FunctionCall
          requiredData = initialData{firstName = (Just state.data.name),gender = gender, hasDisability = Just (isJust selectedDisability), disability = case selectedDisability of 
            Just disability -> Just (Remote.mkDisabilityData disability (fromMaybe "" state.data.disabilityOptions.otherDisabilityReason))
            _ -> Nothing  }
      setValueToLocalStore DISABILITY_UPDATED "true"
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{showDisabilityPopUp = (isJust selectedDisability)} , data{disability = selectedDisability}})
      case gender of
          Just value -> void $ pure $ setCleverTapUserData "gender" value
          Nothing -> pure unit

      resp <- lift $ lift $ Remote.updateProfile (UpdateProfileReq requiredData)
      case resp of
        Right response -> do
          setValueToLocalStore USER_NAME state.data.name
          void $ pure $ setCleverTapUserData "Name" (getValueToLocalStore USER_NAME)
          case gender of
            Just value -> modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{gender = Just value}}, props{isBanner = false}})
            Nothing    -> pure unit
          _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_onboarded"
          _ <- pure $ metaLogEvent "ny_user_onboarded"
          pure unit
        Left err -> do
          _ <- pure $ toast (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
          modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> state{props{btnActive = true},data{name=state.data.name}})
          accountSetUpScreenFlow
    GO_BACK -> do
      _ <- pure $ deleteValueFromLocalStore REGISTERATION_TOKEN
      _ <- pure $ deleteValueFromLocalStore MOBILE_NUMBER
      modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> AccountSetUpScreenData.initData)
      modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen -> enterMobileNumberScreen{data{ otp = ""}})
      enterMobileNumberScreenFlow


updateDisabilityList :: String -> FlowBT String (Array DisabilityT)
updateDisabilityList screenType = do
  response <- Remote.disabilityList
  case response of 
    Right (GetDisabilityListResp resp) -> pure $ getDisabilityList resp
    Left err -> pure $ getDisabilityList []

homeScreenFlow :: FlowBT String Unit
homeScreenFlow = do
  logField_ <- lift $ lift $ getLogFields
  (GlobalState currentState) <- getState
  _ <- checkAndUpdateSavedLocations currentState.homeScreen
  _ <- pure $ cleverTapSetLocation unit
  -- TODO: REQUIRED ONCE WE NEED TO STORE RECENT CURRENTLOCATIONS
  -- resp <- lift $ lift $ getCurrentLocationsObjFromLocal currentState.homeScreen
  -- modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{previousCurrentLocations = resp}})

  -- TODO: HANDLE LOCATION LIST INITIALLY
  _ <- pure $ firebaseUserID (getValueToLocalStore CUSTOMER_ID)
  void $ lift $ lift $ toggleLoader false
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{hasTakenRide = if (getValueToLocalStore REFERRAL_STATUS == "HAS_TAKEN_RIDE") then true else false, isReferred = if (getValueToLocalStore REFERRAL_STATUS == "REFERRED_NOT_TAKEN_RIDE") then true else false }, data {config = config}})
  flow <- UI.homeScreen
  case flow of
    CHECK_FLOW_STATUS -> currentFlowStatus
    ON_RESUME_APP -> currentFlowStatus
    GO_TO_MY_RIDES -> do
      modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> myRidesScreen{data{offsetValue = 0}})
      myRidesScreenFlow true
    GO_TO_HELP -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> HelpAndSupportScreenData.initData)
      _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_help"
      helpAndSupportScreenFlow
    CHANGE_LANGUAGE ->  selectLanguageScreenFlow
    GO_TO_EMERGENCY_CONTACTS -> do
      modifyScreenState $  EmergencyContactsScreenStateType (\emergencyContactsScreen -> EmergencyContactsScreenData.initData)
      emergencyScreenFlow
    GO_TO_ABOUT -> aboutUsScreenFlow
    GO_TO_MY_PROFILE  updateProfile -> do
        _ <- lift $ lift $ liftFlow $ logEvent logField_ (if updateProfile then "safety_banner_clicked" else "ny_user_profile_click")
        modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState ->  MyProfileScreenData.initData{props{fromHomeScreen = updateProfile , updateProfile = updateProfile, isBtnEnabled = true , genderOptionExpanded = false , showOptions = false, expandEnabled = true }})
        myProfileScreenFlow
    GO_TO_FIND_ESTIMATES updatedState-> do
      let currentLocationText = getString CURRENT_LOCATION
      case updatedState.data.source of
        currentLocationText -> do
          PlaceName address <- getPlaceName updatedState.props.sourceLat updatedState.props.sourceLong HomeScreenData.dummyLocation
          modifyScreenState $ HomeScreenStateType (\homeScreen -> updatedState{ data{ source = address.formattedAddress, sourceAddress = encodeAddress address.formattedAddress [] Nothing } })
        _ -> pure unit
      (GlobalState globalState) <- getState
      let state = globalState.homeScreen
      liftFlowBT $  logEventWithTwoParams logField_ "ny_user_source_and_destination" "ny_user_enter_source" (take 99 (state.data.source)) "ny_user_enter_destination" (take 99 (state.data.destination))
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq state.props.sourceLat state.props.sourceLong)
      if (not sourceServiceabilityResp.serviceable) then do
        updateLocalStage SearchLocationModel
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = SearchLocationModel ,rideRequestFlow = false, isSearchLocation = SearchLocation, isSrcServiceable = false, isSource = Just true, isRideServiceable = false}})
        homeScreenFlow
        else pure unit
      (SearchRes rideSearchRes) <- Remote.rideSearchBT (Remote.makeRideSearchReq state.props.sourceLat state.props.sourceLong state.props.destinationLat state.props.destinationLong state.data.sourceAddress state.data.destinationAddress)
      routeResponse <- Remote.drawMapRoute state.props.sourceLat state.props.sourceLong state.props.destinationLat state.props.destinationLong (Remote.normalRoute "") "NORMAL" state.data.source state.data.destination rideSearchRes.routeInfo "pickup" (specialLocationConfig "" "")
      case rideSearchRes.routeInfo of
        Just (Route response) -> do
          let distance = if response.distance < 1000 then toString(response.distance)  <> " m" else parseFloat(INT.toNumber(response.distance) / 1000.0) 2 <> " km"
              duration = (show (response.duration / 60)) <> " min"
              tipEnabled = isTipEnabled response.distance
              Snapped points = response.points
          case head points, last points of
            Just (LatLong source), Just (LatLong dest) -> do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props{ routeEndPoints = Just ({ source : { lat : source.lat, lng : source.lon, place : state.data.source, address : Nothing }, destination : { lat : dest.lat, lng : dest.lon, place : state.data.destination, address : Nothing } }) } })
            _ , _ -> pure unit
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{rideDistance = distance, rideDuration = duration, source = state.data.source, sourceAddress = state.data.sourceAddress}, props{customerTip{enableTips = tipEnabled}}})
          _ <- setValueToLocalStore ENABLE_TIPS $ show tipEnabled
          if ((MU.getMerchant FunctionCall) /= MU.YATRI && response.distance >= 50000) then do
            updateLocalStage DistanceOutsideLimits
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = DistanceOutsideLimits ,rideRequestFlow = true, isSearchLocation = SearchLocation, findingQuotesProgress = 0.0}})
            homeScreenFlow
            else if response.distance < 500 && getValueToLocalStore LOCAL_STAGE /= "ShortDistance" then do
              updateLocalStage ShortDistance
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = ShortDistance ,rideRequestFlow = true, isSearchLocation = SearchLocation, distance = response.distance, findingQuotesProgress = 0.0}})
              homeScreenFlow
              else pure unit
          pure unit
        Nothing -> pure unit
      void $ pure $ setFlowStatusData (FlowStatusData { source : {lat : state.props.sourceLat, lng : state.props.sourceLong, place : state.data.source, address : Nothing}
                                                      , destination : {lat : state.props.destinationLat, lng : state.props.destinationLong, place : state.data.destination, address : Nothing}
                                                      , sourceAddress : state.data.sourceAddress
                                                      , destinationAddress : state.data.destinationAddress })
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{searchId = rideSearchRes.searchId,currentStage = FindingEstimate, rideRequestFlow = true, isSearchLocation = SearchLocation, sourcePlaceId = Nothing, destinationPlaceId = Nothing, findingQuotesProgress = 0.0}, data{nearByDrivers = Nothing}})
      updateLocalStage FindingEstimate
      homeScreenFlow
    RETRY_FINDING_QUOTES showLoader-> do
      void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)  -- TODO : Handlde Loader in IOS Side
      void $ lift $ lift $ toggleLoader showLoader
      (GlobalState newState) <- getState
      let state = newState.homeScreen
      liftFlowBT $ logEventWithParams logField_ "ny_user_tip_search" "Tip amount" ("₹ " <> (show $ state.props.customerTip.tipForDriver))
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_rider_retry_request_quote" $ [ {key : "Request Type", value : unsafeToForeign if(getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") then "Auto Assign" else "Manual Assign"},
                                                                                                      {key : "Estimate Fare", value : unsafeToForeign $ "₹" <> (show $ (state.data.suggestedAmount + state.data.rateCard.additionalFare))},
                                                                                                      {key : "Customer tip (Rs.)", value : unsafeToForeign $ "₹" <> (show $ state.props.customerTip.tipForDriver)},
                                                                                                      {key : "Estimated Ride Distance" , value : unsafeToForeign state.data.rideDistance},
                                                                                                      {key : "Night Ride", value : unsafeToForeign (show $ state.data.rateCard.nightCharges)}]
      if (not (isLocalStageOn QuoteList)) then do
        void $ pure $ firebaseLogEvent "ny_user_cancel_and_retry_request_quotes"
        cancelEstimate state.props.estimateId
      else do
        void $ pure $ firebaseLogEvent "ny_user_retry_request_quotes"
      setValueToLocalStore AUTO_SELECTING "false"
      setValueToLocalStore FINDING_QUOTES_POLLING "false"
      setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
      when (getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") do
        void $ pure $ firebaseLogEvent "ny_user_auto_confirm"

      let currentTime = (convertUTCtoISC (getCurrentUTC "") "HH:mm:ss")
          findingQuotesTime = convertUTCtoISC (getValueToLocalNativeStore FINDING_QUOTES_START_TIME) "HH:mm:ss"
      if withinTimeRange findingQuotesTime currentTime "22:00:00" || withinTimeRange findingQuotesTime currentTime "05:00:00" then do
        void $ pure $ toast (getString PLEASE_FIND_REVISED_FARE_ESTIMATE)
        void $ pure $ firebaseLogEvent "ny_user_new_estimate_after_night_charges_applicable"
        updateLocalStage FindEstimateAndSearch
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props { currentStage = FindEstimateAndSearch, searchAfterEstimate = false } })
      else do
        void $ pure $ setValueToLocalStore FINDING_QUOTES_START_TIME (getCurrentUTC "LazyCheck")
        response <- lift $ lift $ Remote.selectEstimate (Remote.makeEstimateSelectReq (flowWithoutOffers WithoutOffers) (if state.props.customerTip.enableTips && state.props.customerTip.isTipSelected then Just state.props.customerTip.tipForDriver else Nothing)) (state.props.estimateId)
        case response of
          Right res -> do
            updateLocalStage FindingQuotes
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props { currentStage = FindingQuotes, searchExpire = (getSearchExpiryTime "LazyCheck") } })
          Left err -> do
            void $ pure $ firebaseLogEvent "ny_user_estimate_expired"
            updateLocalStage FindEstimateAndSearch
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props { currentStage = FindEstimateAndSearch, searchAfterEstimate = true } })
        let tipViewData = if state.props.customerTip.isTipSelected then state.props.tipViewProps else HomeScreenData.initData.props.tipViewProps
        _ <- pure $ setTipViewData (TipViewData { stage : tipViewData.stage , activeIndex : tipViewData.activeIndex , isVisible : tipViewData.isVisible })
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props { customerTip = if homeScreen.props.customerTip.isTipSelected then homeScreen.props.customerTip else HomeScreenData.initData.props.customerTip{enableTips = homeScreen.props.customerTip.enableTips } , tipViewProps = tipViewData, findingQuotesProgress = 0.0 }})
      homeScreenFlow
    LOCATION_SELECTED item addToRecents-> do
        void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)  -- TODO : Handlde Loader in IOS Side
        void $ lift $ lift $ toggleLoader true
        (GlobalState newState) <- getState
        let state = newState.homeScreen

        case state.props.isSource of
          Just true -> do
            (GetPlaceNameResp sourceDetailResp) <- getPlaceNameResp (state.props.sourcePlaceId) (state.props.sourceLat) (state.props.sourceLong) (if state.props.isSource == Just false then dummyLocationListItemState else item)
            let (PlaceName sourceDetailResponse) = (fromMaybe HomeScreenData.dummyLocationName (sourceDetailResp !! 0))
                (LatLong sourceLocation) = sourceDetailResponse.location
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props{sourceLat = sourceLocation.lat, sourceLong = sourceLocation.lon} })
          Just false  -> do
            (GetPlaceNameResp destinationDetailResp) <- getPlaceNameResp (state.props.destinationPlaceId) (state.props.destinationLat) (state.props.destinationLong) (if state.props.isSource == Just true then dummyLocationListItemState else item)
            let (PlaceName destinationDetailResponse) = (fromMaybe HomeScreenData.dummyLocationName (destinationDetailResp!!0))
                (LatLong destinationLocation) = (destinationDetailResponse.location)
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props{destinationLat = destinationLocation.lat, destinationLong = destinationLocation.lon} })
          _          -> pure unit

        (GlobalState updatedState) <- getState
        let bothLocationChangedState = updatedState.homeScreen
        _ <- pure $ spy "destination prediction clicked state ---->>> " bothLocationChangedState
        (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq bothLocationChangedState.props.sourceLat bothLocationChangedState.props.sourceLong)
        let srcServiceable = sourceServiceabilityResp.serviceable
        let (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
        let pickUpPoints = map (\(GatesInfo item) -> {
                                                place: item.name,
                                                lat  : (item.point)^._lat,
                                                lng : (item.point)^._lon,
                                                address : item.address
                                              }) srcSpecialLocation.gates
        (ServiceabilityResDestination destServiceabilityResp) <- Remote.destServiceabilityBT (Remote.makeServiceabilityReqForDest bothLocationChangedState.props.destinationLat bothLocationChangedState.props.destinationLong)
        let destServiceable = destServiceabilityResp.serviceable
        let pickUpLoc = if length pickUpPoints > 0 then (if state.props.defaultPickUpPoint == "" then fetchDefaultPickupPoint pickUpPoints state.props.sourceLat state.props.sourceLong else state.props.defaultPickUpPoint) else (fromMaybe HomeScreenData.dummyLocation (state.data.nearByPickUpPoints!!0)).place
        modifyScreenState $ HomeScreenStateType (\homeScreen -> bothLocationChangedState{data{polygonCoordinates = fromMaybe "" sourceServiceabilityResp.geoJson,nearByPickUpPoints=pickUpPoints},props{isSpecialZone =  (sourceServiceabilityResp.geoJson) /= Nothing, confirmLocationCategory = if length pickUpPoints > 0 then state.props.confirmLocationCategory else "", defaultPickUpPoint = pickUpLoc, findingQuotesProgress = 0.0, sourceSelectedOnMap = false }})
        if (addToRecents) then
          addLocationToRecents item bothLocationChangedState sourceServiceabilityResp.serviceable destServiceabilityResp.serviceable
          else pure unit
        (GlobalState globalState) <- getState
        let updateScreenState = globalState.homeScreen
        if (not srcServiceable && (updateScreenState.props.sourceLat /= -0.1 && updateScreenState.props.sourceLong /= -0.1) && (updateScreenState.props.sourceLat /= 0.0 && updateScreenState.props.sourceLong /= 0.0)) then do
          modifyScreenState $ HomeScreenStateType (\homeScreen -> updateScreenState{props{isSrcServiceable = false, isRideServiceable= false, isSource = Just true}})
          homeScreenFlow
        else if ((not destServiceable) && (updateScreenState.props.destinationLat /= 0.0 && updateScreenState.props.destinationLat /= -0.1) && (updateScreenState.props.destinationLong /= 0.0 && bothLocationChangedState.props.destinationLong /= -0.1)) then do
          if (getValueToLocalStore LOCAL_STAGE == "HomeScreen") then do
            _ <- pure $ toast (getString LOCATION_UNSERVICEABLE)
            pure unit
            else pure unit
          modifyScreenState $ HomeScreenStateType (\homeScreen -> updateScreenState{props{isDestServiceable = false, isRideServiceable = false,isSource = Just false, isSrcServiceable = true}})
          homeScreenFlow
         else modifyScreenState $ HomeScreenStateType (\homeScreen -> updateScreenState{props{ isRideServiceable= true, isSrcServiceable = true, isDestServiceable = true}})
        rideSearchFlow "NORMAL_FLOW"

    SEARCH_LOCATION input state -> do
      (SearchLocationResp searchLocationResp) <- Remote.searchLocationBT (Remote.makeSearchLocationReq input ( state.props.sourceLat) ( state.props.sourceLong) getSearchRadius  (case (getValueToLocalStore LANGUAGE_KEY) of
                                                                                                                                                                                                    "HI_IN" -> "HINDI"
                                                                                                                                                                                                    "KN_IN" -> "KANNADA"
                                                                                                                                                                                                    "BN_IN" -> "BENGALI"
                                                                                                                                                                                                    "ML_IN" -> "MALAYALAM"
                                                                                                                                                                                                    _      -> "ENGLISH") "")
      let event =
            case state.props.isSource of
              Just true -> "ny_user_auto_complete_api_trigger_src"
              Just false -> "ny_user_auto_complete_api_trigger_dst"
              Nothing -> ""
      _ <- lift $ lift $ liftFlow $ logEvent logField_ event
      let sortedByDistanceList = sortPredctionByDistance searchLocationResp.predictions
      let predictionList = getLocationList sortedByDistanceList
      let recentLists = state.data.recentSearchs.predictionArray
      let filteredRecentsList = filterRecentSearches recentLists predictionList
      let filteredPredictionList = differenceOfLocationLists predictionList filteredRecentsList
      modifyScreenState $ HomeScreenStateType (\homeScreen -> state{data{locationList =
        map
        (\item -> do
                  let savedLocation  = getPrediction item state.data.savedLocations
                      locIsPresentInSavedLoc = checkPrediction item state.data.savedLocations
                  if not locIsPresentInSavedLoc then
                    item {
                      lat = savedLocation.lat,
                      lon = savedLocation.lon,
                      locationItemType = Just SAVED_LOCATION,
                      postfixImageUrl = "ny_ic_fav_red," <> (getAssetStoreLink FunctionCall) <> "ny_ic_fav_red.png" }
                    else
                      item {
                        lat = item.lat,
                        lon = item.lon,
                        locationItemType = item.locationItemType,
                        postfixImageUrl = "ny_ic_fav," <> (getAssetStoreLink FunctionCall) <> "ny_ic_fav.png" }
            ) ((filteredRecentsList) <> filteredPredictionList) }})
      homeScreenFlow
    GET_QUOTES state -> do
          _ <- pure $ setValueToLocalStore AUTO_SELECTING "false"
          setValueToLocalStore FINDING_QUOTES_POLLING "false"
          _ <- pure $ setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
          liftFlowBT $ logEvent logField_ "ny_user_request_quotes"
          liftFlowBT $ logEventWithMultipleParams logField_ "ny_rider_request_quote" $ [ {key : "Request Type", value : unsafeToForeign if(getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") then "Auto Assign" else "Manual Assign"},
                                                                                                          {key : "Estimate Fare", value : unsafeToForeign $ "₹" <> (show $ (state.data.suggestedAmount + state.data.rateCard.additionalFare))},
                                                                                                          {key : "Estimated Ride Distance" , value : unsafeToForeign state.data.rideDistance},
                                                                                                          {key : "Night Ride", value : unsafeToForeign (show $ state.data.rateCard.nightCharges)}]
          if(getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") then do
            _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_auto_confirm"
            pure unit
          else do
            pure unit
          void $ pure $ setValueToLocalStore FINDING_QUOTES_START_TIME (getCurrentUTC "LazyCheck")
          _ <- Remote.selectEstimateBT (Remote.makeEstimateSelectReq (flowWithoutOffers WithoutOffers) (if state.props.customerTip.enableTips && state.props.customerTip.isTipSelected then Just state.props.customerTip.tipForDriver else Nothing)) (state.props.estimateId)
          homeScreenFlow
    SELECT_ESTIMATE state -> do
        updateLocalStage SettingPrice
        let sourceSpecialTagIcon = specialLocationIcons state.props.zoneType.sourceTag
            destSpecialTagIcon = specialLocationIcons state.props.zoneType.destinationTag
            srcMarker = (Remote.normalRoute "").srcMarker
            destMarker = (Remote.normalRoute "").destMarker
        case state.props.routeEndPoints of
          Just points -> lift $ lift $ liftFlow $ updateRouteMarker $ updateRouteMarkerConfig (Remote.walkCoordinate points.source.lat points.source.lng points.destination.lat points.destination.lng) points.source.place points.destination.place srcMarker destMarker (specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon)
          Nothing -> pure unit
        homeScreenFlow
    GET_SELECT_LIST state -> do
      when (isLocalStageOn QuoteList) $ do
        updateFlowStatus SEARCH_CANCELLED
      homeScreenFlow
    CONFIRM_RIDE state -> do
          _ <- pure $ enableMyLocation false
          let selectedQuote = if state.props.isSpecialZone && state.data.currentSearchResultType == QUOTES then state.data.specialZoneSelectedQuote else state.props.selectedQuote
          _ <- pure $ spy "selected Quote " selectedQuote
          if isJust selectedQuote then do
            updateLocalStage ConfirmingRide
            response  <- lift $ lift $ Remote.rideConfirm (fromMaybe "" selectedQuote)
            case response of
              Right (ConfirmRes resp) -> do
                let bookingId = resp.bookingId
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = ConfirmingRide, bookingId = bookingId, isPopUp = NoPopUp}})
                homeScreenFlow
              Left err  -> do
                if not (err.code == 400 && (decodeError err.response.errorMessage "errorCode") == "QUOTE_EXPIRED") then pure $ toast (getString ERROR_OCCURED_TRY_AGAIN) else pure unit
                _ <- setValueToLocalStore AUTO_SELECTING "false"
                _ <- pure $ updateLocalStage QuoteList
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = QuoteList,selectedQuote = Nothing, expiredQuotes = snoc state.props.expiredQuotes (fromMaybe "" state.props.selectedQuote)}, data {quoteListModelState = []}})
                homeScreenFlow
            else homeScreenFlow
    ONGOING_RIDE state -> do
      _ <- pure $ setValueToLocalStore TRACKING_ENABLED "True"
      _ <- pure $ setValueToLocalStore TRACKING_DRIVER "False"
      _ <- pure $ setValueToLocalStore DRIVER_ARRIVAL_ACTION "TRIGGER_DRIVER_ARRIVAL"
      let srcLat = state.data.driverInfoCardState.sourceLat
          srcLon = state.data.driverInfoCardState.sourceLng
          dstLat = state.data.driverInfoCardState.destinationLat
          dstLon = state.data.driverInfoCardState.destinationLng
      updateLocalStage state.props.currentStage
      if spy "ONGOING_RIDEONGOING_RIDE CURRENT" state.props.currentStage == RideCompleted then
        do
          let sourceSpecialTagIcon = specialLocationIcons state.props.zoneType.sourceTag
              destSpecialTagIcon = specialLocationIcons state.props.zoneType.destinationTag
          _ <- pure $ spy "INSIDE IF OF ONGOING" state.props.currentStage
          _ <- Remote.drawMapRoute srcLat srcLon dstLat dstLon (Remote.normalRoute "") "DRIVER_LOCATION_UPDATE" "" "" Nothing "pickup" (specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon)
          homeScreenFlow
        else if state.props.currentStage == HomeScreen then
          do
            _ <- pure $ removeAllPolylines ""
            _ <- pure $ spy "INSIDE ELSE IF OF ONGOING" state.props.currentStage
            _ <- updateLocalStage HomeScreen
            modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData{data{settingSideBar{gender = state.data.settingSideBar.gender , email = state.data.settingSideBar.email}}})
            homeScreenFlow
          else do
            lift $ lift $ triggerRideStatusEvent "DRIVER_ASSIGNMENT" Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
            homeScreenFlow
    CANCEL_RIDE_REQUEST state -> do
      _ <- pure $ currentPosition ""
      _ <- updateLocalStage HomeScreen
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_rider_cancellation" $ [ {key : "Reason code", value : unsafeToForeign state.props.cancelReasonCode},
                                                                                                      {key : "Additional info", value : unsafeToForeign state.props.cancelDescription},
                                                                                                      {key : "Pickup", value : unsafeToForeign state.data.driverInfoCardState.source},
                                                                                                      {key : "Estimated Ride Distance" , value : unsafeToForeign state.data.rideDistance},
                                                                                                      {key : "Night Ride", value : unsafeToForeign (show $ state.data.rateCard.nightCharges)}]
      _ <- Remote.cancelRideBT (Remote.makeCancelRequest state) (state.props.bookingId)
      lift $ lift $ triggerRideStatusEvent "CANCELLED_PRODUCT" Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
      _ <- pure $ clearWaitingTimer <$> state.props.waitingTimeTimerIds
      liftFlowBT $ logEvent logField_ "ny_user_ride_cancelled_by_user"
      liftFlowBT $ logEvent logField_ $ "ny_user_cancellation_reason: " <> state.props.cancelReasonCode
      removeChatService ""
      modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData{data{settingSideBar{gender = state.data.settingSideBar.gender , email = state.data.settingSideBar.email}},props { isBanner = state.props.isBanner}})
      homeScreenFlow
    FCM_NOTIFICATION notification state-> do
        let rideID = state.data.driverInfoCardState.rideId
            srcLat = state.data.driverInfoCardState.sourceLat
            srcLon = state.data.driverInfoCardState.sourceLng
            dstLat = state.data.driverInfoCardState.destinationLat
            dstLon = state.data.driverInfoCardState.destinationLng
        _ <- pure $ setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
        _ <- pure $ setValueToLocalStore FINDING_QUOTES_POLLING "false"
        _ <- pure $ setValueToLocalStore TRACKING_DRIVER "False"
        if not state.props.isInApp then do
          _ <- pure $ setValueToLocalStore TRACKING_ENABLED "False"
          pure unit
          else do
            _ <- pure $ setValueToLocalStore TRACKING_ENABLED "True"
            pure unit
        case notification of
            "TRIP_STARTED"        -> do -- OTP ENTERED
                                      let shareAppCount = getValueToLocalStore SHARE_APP_COUNT
                                      if shareAppCount == "__failed" then do
                                        setValueToLocalStore SHARE_APP_COUNT "1"
                                      else if shareAppCount /= "-1" then do
                                        setValueToLocalStore SHARE_APP_COUNT (show ((INT.round $ (fromMaybe 0.0 (fromString (shareAppCount))))+1))
                                      else pure unit
                                      _ <- pure $ clearWaitingTimer <$> state.props.waitingTimeTimerIds
                                      let newState = state{data{route = Nothing},props{isCancelRide = false,waitingTimeTimerIds = [], currentStage = RideStarted, forFirst = true , showShareAppPopUp = (INT.round $ (fromMaybe 0.0 (fromString (getValueToLocalStore SHARE_APP_COUNT)))) `mod` 4 == 0, showChatNotification = false, cancelSearchCallDriver = false  }}
                                      _ <- updateLocalStage RideStarted
                                      modifyScreenState $ HomeScreenStateType (\homeScreen -> newState)
                                      lift $ lift $ triggerRideStatusEvent notification Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
                                      when state.props.isSpecialZone $ currentRideFlow true
                                      homeScreenFlow
            "TRIP_FINISHED"       -> do -- TRIP FINISHED
                                      if (getValueToLocalStore HAS_TAKEN_FIRST_RIDE == "false") then do
                                        _ <- pure $ metaLogEvent "ny_user_first_ride_completed"
                                        (GetProfileRes response) <- Remote.getProfileBT ""
                                        setValueToLocalStore HAS_TAKEN_FIRST_RIDE ( show response.hasTakenRide)
                                        else pure unit
                                      let sourceSpecialTagIcon = specialLocationIcons state.props.zoneType.sourceTag
                                          destSpecialTagIcon = specialLocationIcons state.props.zoneType.destinationTag
                                      _ <- pure $ metaLogEvent "ny_user_ride_completed"
                                      _ <- Remote.drawMapRoute srcLat srcLon dstLat dstLon (Remote.normalRoute "") "NORMAL" "" "" Nothing "pickup" (specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon)
                                      _ <- updateLocalStage HomeScreen
                                      if (state.props.bookingId /= "") then do
                                        (RideBookingRes resp) <- Remote.rideBookingBT (state.props.bookingId)
                                        let (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
                                        let (RideBookingDetails contents) = bookingDetails.contents
                                        let (RideAPIEntity ride) = fromMaybe dummyRideAPIEntity (resp.rideList !! 0)
                                        let finalAmount =  getFinalAmount (RideBookingRes resp)
                                        let differenceOfDistance = fromMaybe 0 contents.estimatedDistance - (fromMaybe 0 ride.chargeableRideDistance)
                                        lift $ lift $ triggerRideStatusEvent notification (Just finalAmount) (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
                                        setValueToLocalStore PICKUP_DISTANCE "0"
                                        liftFlowBT $ logEventWithMultipleParams logField_ "ny_rider_ride_completed" (rideCompletedDetails (RideBookingRes resp))
                                        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{startedAt = convertUTCtoISC (fromMaybe "" resp.rideStartTime ) "h:mm A", startedAtUTC = fromMaybe "" resp.rideStartTime ,endedAt = convertUTCtoISC (fromMaybe "" resp.rideEndTime ) "h:mm A", finalAmount = finalAmount, rideRatingState {driverName = ride.driverName, rideId = ride.id , distanceDifference = differenceOfDistance} , ratingViewState { rideBookingRes = (RideBookingRes resp)}, driverInfoCardState {initDistance = Nothing}},props{currentStage = RideCompleted, estimatedDistance = contents.estimatedDistance}})
                                        homeScreenFlow
                                        else homeScreenFlow
            "CANCELLED_PRODUCT"   -> do -- REMOVE POLYLINES
                                      _ <- pure $ removeAllPolylines ""
                                      _ <- updateLocalStage HomeScreen
                                      removeChatService ""
                                      setValueToLocalStore PICKUP_DISTANCE "0"
                                      lift $ lift $ triggerRideStatusEvent notification Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
                                      modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData{data{settingSideBar{gender = state.data.settingSideBar.gender , email = state.data.settingSideBar.email}, driverInfoCardState{initDistance = Nothing}},props { isBanner = state.props.isBanner, showChatNotification = false}})
                                      _ <- pure $ clearWaitingTimer <$> state.props.waitingTimeTimerIds
                                      permissionConditionA <- lift $ lift $ liftFlow $ isLocationPermissionEnabled unit
                                      permissionConditionB <- lift $ lift $ liftFlow $ isLocationEnabled unit
                                      when (not (permissionConditionA && permissionConditionB)) $ permissionScreenFlow "LOCATION_DISABLED"
                                      homeScreenFlow
            "DRIVER_ASSIGNMENT"   -> if (not (isLocalStageOn RideAccepted || isLocalStageOn RideStarted )) then do
                                        _ <- pure $ setValueToLocalStore DRIVER_ARRIVAL_ACTION "TRIGGER_DRIVER_ARRIVAL"
                                        _ <- liftFlowBT $ logEvent logField_ "ny_fs_driver_assignment"
                                        lift $ lift $ triggerRideStatusEvent notification Nothing (Just state.props.bookingId) $ getScreenFromStage state.props.currentStage
                                        currentRideFlow true
                                        homeScreenFlow
                                     else homeScreenFlow
            _                     -> homeScreenFlow

    LOGOUT -> do
      (LogOutRes resp) <- Remote.logOutBT LogOutReq
      removeChatService ""
      _ <- pure $ deleteValueFromLocalStore REGISTERATION_TOKEN
      _ <- pure $ deleteValueFromLocalStore REGISTRATION_APPROVED
      _ <- pure $ deleteValueFromLocalStore CUSTOMER_ID
      _ <- pure $ deleteValueFromLocalStore CONTACTS
      _ <- pure $ deleteValueFromLocalStore USER_EMAIL
      _ <- pure $ factoryResetApp ""
      _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_logout"
      _ <- pure $ (setText (getNewIDWithTag "EnterMobileNumberEditText") "" )
      modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> EnterMobileNumberScreenData.initData)
      modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData)
      enterMobileNumberScreenFlow -- Removed choose langauge screen
    SUBMIT_RATING state -> do
      liftFlowBT $ logEventWithParams logField_ "ny_user_ride_give_feedback" "Rating" (show $ state.data.rating)
      _ <- Remote.bookingFeedbackBT (Remote.makeRideFeedBackReq (state.data.rideRatingState.rideId) (state.data.rideRatingState.feedbackList))
      _ <- Remote.rideFeedbackBT (Remote.makeFeedBackReq (state.data.rideRatingState.rating) (state.data.rideRatingState.rideId) (state.data.rideRatingState.feedback))
      _ <- updateLocalStage HomeScreen
      let finalAmount = if state.data.finalAmount == 0 then state.data.rideRatingState.finalAmount else state.data.finalAmount
      let bookingId = if state.props.bookingId == "" then state.data.rideRatingState.bookingId else state.props.bookingId
      pure $ runFn3 emitJOSEvent "java" "onEvent" $ encode $ EventPayload {
                                          event : "process_result"
                                        , payload : Just {
                                          action : "feedback_submitted"
                                        , trip_amount : Just finalAmount
                                        , trip_id : Just bookingId
                                        , ride_status : Nothing
                                        , screen : Just $ getScreenFromStage state.props.currentStage
                                        , exit_app : false
                                        }
                                        }
      modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData{data{settingSideBar{gender = state.data.settingSideBar.gender , email = state.data.settingSideBar.email}},props {isBanner=state.props.isBanner}})
      if state.props.currentStage == RideCompleted then
        if (getSearchType unit) == "direct_search" then do
          _ <- updateLocalStage SearchLocationModel
          checkAndUpdateLocations
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = SearchLocationModel}})
          else pure unit
        else pure unit
      if state.data.rideRatingState.rating == 5 then do
        _ <- pure $ launchInAppRatingPopup unit
        pure unit
        else pure unit
      homeScreenFlow
    CANCEL -> homeScreenFlow
    RELOAD saveToCurrLocs -> do
      (GlobalState state) <- getState
      if state.homeScreen.props.currentStage == SearchLocationModel then do
        if (saveToCurrLocs && state.homeScreen.props.storeCurrentLocs) then addLocToCurrLoc state.homeScreen.props.sourceLat state.homeScreen.props.sourceLong state.homeScreen.data.source else pure unit
        _ <- pure $ toggleBtnLoader "" false
        homeScreenFlow
        else do
          if state.homeScreen.props.sourceLat/=0.0 && state.homeScreen.props.sourceLong /= 0.0 then do
            (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq state.homeScreen.props.sourceLat state.homeScreen.props.sourceLong)
            -- let srcServiceable = sourceServiceabilityResp.serviceable
            let (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
            let pickUpPoints = map (\(GatesInfo item) -> {
                                                    place: item.name,
                                                    lat  : (item.point)^._lat,
                                                    lng : (item.point)^._lon,
                                                    address : item.address
                                                  }) srcSpecialLocation.gates
            if (sourceServiceabilityResp.serviceable ) then do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{ polygonCoordinates = fromMaybe "" sourceServiceabilityResp.geoJson, nearByPickUpPoints = pickUpPoints } ,props{ isSrcServiceable = true, showlocUnserviceablePopUp = false}})
              if (saveToCurrLocs && state.homeScreen.props.storeCurrentLocs) then
                addLocToCurrLoc state.homeScreen.props.sourceLat state.homeScreen.props.sourceLong state.homeScreen.data.source
              else pure unit
            else do
              _ <- pure $ firebaseLogEvent "ny_loc_unserviceable"
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{ isSrcServiceable = false, showlocUnserviceablePopUp = true}})
          else do
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{ isSrcServiceable = true, showlocUnserviceablePopUp = false}})

      homeScreenFlow
    RETRY  -> homeScreenFlow
    CHECK_SERVICEABILITY updatedState lat long-> do
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq lat long)
      let sourceLat = if sourceServiceabilityResp.serviceable then lat else updatedState.props.sourceLat
          sourceLong = if sourceServiceabilityResp.serviceable then long else updatedState.props.sourceLong
      _ <- pure $ firebaseLogEvent $ "ny_loc_unserviceable_" <> show (not sourceServiceabilityResp.serviceable)
      modifyScreenState $ HomeScreenStateType (\homeScreen -> updatedState{data{ polygonCoordinates = fromMaybe "" sourceServiceabilityResp.geoJson } ,props{sourceLat = sourceLat, sourceLong = sourceLong, isSrcServiceable =sourceServiceabilityResp.serviceable , showlocUnserviceablePopUp = (not sourceServiceabilityResp.serviceable)}})
      homeScreenFlow
    HOME_SCREEN -> do
        (GlobalState state) <- getState
        when (isLocalStageOn FindingQuotes) $ do
          cancelEstimate state.homeScreen.props.estimateId
        _ <- pure $ removeAllPolylines ""
        _ <- lift $ lift $ liftFlow $ addMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) 9.9 9.9 160 0.5 0.9
        _ <- pure $ currentPosition ""
        _ <- updateLocalStage HomeScreen
        modifyScreenState $ HomeScreenStateType (\homeScreen ->  HomeScreenData.initData{data{settingSideBar{gender = state.homeScreen.data.settingSideBar.gender , email = state.homeScreen.data.settingSideBar.email}},props { isBanner = state.homeScreen.props.isBanner}})
        homeScreenFlow
    CHECK_CURRENT_STATUS -> do
      (GlobalState state) <- getState
      when (isLocalStageOn FindingQuotes) $ do
        cancelEstimate state.homeScreen.props.estimateId
      _ <- pure $ removeAllPolylines ""
      _ <- lift $ lift $ liftFlow $ addMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) 9.9 9.9 160 0.5 0.9
      _ <- pure $ currentPosition ""
      _ <- updateLocalStage HomeScreen
      modifyScreenState $ HomeScreenStateType (\homeScreen ->  HomeScreenData.initData{data{settingSideBar{gender = state.homeScreen.data.settingSideBar.gender , email = state.homeScreen.data.settingSideBar.email}},props { isBanner = state.homeScreen.props.isBanner}})
      currentFlowStatus
    UPDATE_LOCATION_NAME state lat lon -> do
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq lat lon)
      let srcServiceable = sourceServiceabilityResp.serviceable
      let (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
      let pickUpPoints = map (\(GatesInfo item) -> {
                                              place: item.name,
                                              lat  : (item.point)^._lat,
                                              lng : (item.point)^._lon,
                                              address : item.address
                                            }) srcSpecialLocation.gates
      let gateAddress = (fromMaybe HomeScreenData.dummyLocation ((filter( \ (item) -> (item.place == state.props.defaultPickUpPoint)) pickUpPoints) !! 0))
      if (fromMaybe "" sourceServiceabilityResp.geoJson) /= "" && (fromMaybe "" sourceServiceabilityResp.geoJson) /= state.data.polygonCoordinates && pickUpPoints /= state.data.nearByPickUpPoints then do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{polygonCoordinates = fromMaybe "" sourceServiceabilityResp.geoJson,nearByPickUpPoints=pickUpPoints},props{isSpecialZone =  (sourceServiceabilityResp.geoJson) /= Nothing , defaultPickUpPoint = (fromMaybe HomeScreenData.dummyLocation (state.data.nearByPickUpPoints!!0)).place, confirmLocationCategory = srcSpecialLocation.category}})
        _ <- pure $ removeAllPolylines ""
        liftFlowBT $ runEffectFn5 locateOnMap false lat lon (fromMaybe "" sourceServiceabilityResp.geoJson) pickUpPoints
        homeScreenFlow
      else do
        PlaceName placeDetails <- getPlaceName lat lon gateAddress
        _ <- liftFlowBT $ logEvent logField_ "ny_user_placename_api_lom_onDrag"
        modifyScreenState $ HomeScreenStateType (\homeScreen ->
        homeScreen {
          data {
            destination = if state.props.isSource == Just false && state.props.currentStage /= ConfirmingLocation then placeDetails.formattedAddress else homeScreen.data.destination,
            source = if state.props.isSource == Just true then placeDetails.formattedAddress else homeScreen.data.source,
            sourceAddress = case state.props.isSource , (state.props.currentStage /= ConfirmingLocation) of
              Just true, true -> encodeAddress placeDetails.formattedAddress placeDetails.addressComponents Nothing
              _ , _-> encodeAddress homeScreen.data.source [] state.props.sourcePlaceId,
            destinationAddress = case state.props.isSource,(state.props.currentStage /= ConfirmingLocation) of
              Just false , true -> encodeAddress placeDetails.formattedAddress placeDetails.addressComponents Nothing
              _ , _ -> encodeAddress homeScreen.data.destination [] state.props.destinationPlaceId
          },
          props {
            sourcePlaceId = if state.props.isSource == Just true then Nothing else homeScreen.props.sourcePlaceId,
            destinationPlaceId = if state.props.isSource == Just false then Nothing else homeScreen.props.destinationPlaceId,
            destinationLat = if state.props.isSource == Just false && state.props.currentStage /= ConfirmingLocation then lat else state.props.destinationLat,
            destinationLong = if state.props.isSource == Just false && state.props.currentStage /= ConfirmingLocation then lon else state.props.destinationLong,
            sourceLat = if state.props.isSource == Just true then lat else state.props.sourceLat,
            sourceLong = if state.props.isSource == Just true then lon else state.props.sourceLong,
            confirmLocationCategory = srcSpecialLocation.category
            }
          })
        let _ = spy "UPDATE_LOCATION_NAME" "UPDATE_LOCATION_NAME"
        homeScreenFlow
    UPDATE_PICKUP_NAME state lat lon -> do
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq lat lon)
      let srcServiceable = sourceServiceabilityResp.serviceable
      let (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
      let pickUpPoints = map (\(GatesInfo item) -> {
                                              place: item.name,
                                              lat  : (item.point)^._lat,
                                              lng : (item.point)^._lon,
                                              address : item.address
                                            }) srcSpecialLocation.gates
      let gateAddress = (fromMaybe HomeScreenData.dummyLocation ((filter( \ (item) -> (item.place == state.props.defaultPickUpPoint)) pickUpPoints) !! 0))
      if (fromMaybe "" sourceServiceabilityResp.geoJson) /= "" && (fromMaybe "" sourceServiceabilityResp.geoJson) /= state.data.polygonCoordinates && pickUpPoints /= state.data.nearByPickUpPoints then do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{polygonCoordinates = fromMaybe "" sourceServiceabilityResp.geoJson,nearByPickUpPoints=pickUpPoints},props{isSpecialZone =  (sourceServiceabilityResp.geoJson) /= Nothing , defaultPickUpPoint = (fromMaybe HomeScreenData.dummyLocation (pickUpPoints!!0)).place, confirmLocationCategory = srcSpecialLocation.category}})
        _ <- pure $ removeAllPolylines ""
        liftFlowBT $ runEffectFn5 locateOnMap false lat lon (fromMaybe "" sourceServiceabilityResp.geoJson) pickUpPoints
        homeScreenFlow
      else do
        PlaceName address <- getPlaceName lat lon gateAddress
        _ <- liftFlowBT $ logEvent logField_ "ny_user_placename_api_cpu_onDrag"
        modifyScreenState $ HomeScreenStateType (\homeScreen ->
        homeScreen {
          data {
            source = address.formattedAddress ,
            sourceAddress = encodeAddress address.formattedAddress address.addressComponents Nothing },
          props {
            sourceLat = lat ,
            sourceLong = lon,
            confirmLocationCategory = srcSpecialLocation.category
            }
          })
        let _ = spy "UPDATE_PICKUP_LOCATION_NAME" "UPDATE_PICKUP_LOCATION_NAME"
        homeScreenFlow
    GO_TO_FAVOURITES_  -> do
        _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_addresses"
        savedLocationFlow
    OPEN_GOOGLE_MAPS state -> do
      _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_ride_track_gmaps"
      (GetDriverLocationResp resp) <- Remote.getDriverLocationBT (state.data.driverInfoCardState.rideId)
      let sourceLat = (resp^._lat)
          sourceLng = (resp^._lon)
          destLat = if state.props.currentStage == RideAccepted then state.data.driverInfoCardState.sourceLat else state.data.driverInfoCardState.destinationLat
          destLng = if state.props.currentStage == RideAccepted then state.data.driverInfoCardState.sourceLng else state.data.driverInfoCardState.destinationLng
      _ <- lift $ lift $ fork $ liftFlow $ openNavigation sourceLat sourceLng destLat destLng "DRIVE"
      homeScreenFlow
    IN_APP_TRACK_STATUS state -> do
      case state.props.currentStage of
          RideAccepted -> do
                          _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_pickup_track_inapp"
                          pure unit
          RideStarted  -> do
                          _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_ride_track_inapp"
                          pure unit
          _           -> pure unit
      if (spy "driver current Stage "isLocalStageOn RideAccepted) || (spy "driver current Stage " isLocalStageOn RideStarted) then do
        _ <- pure $ setValueToLocalStore TRACKING_DRIVER "False"
        if not state.props.isInApp then do
          _ <- pure $ setValueToLocalStore TRACKING_ENABLED "False"
          homeScreenFlow
          else do
            _ <- pure $ setValueToLocalStore TRACKING_ENABLED "True"
            homeScreenFlow
        else
          homeScreenFlow
    UPDATE_SAVED_LOCATION -> do
      savedLocationResp <- lift $ lift $ Remote.getSavedLocationList ""
      case savedLocationResp of
        Right (SavedLocationsListRes listResp) -> do
          recentPredictionsObject <- lift $ lift $ getObjFromLocal currentState.homeScreen
          let savedLocationWithHomeOrWorkTag = (filter (\listItem ->  (listItem.prefixImageUrl == "ny_ic_home_blue," <> (getAssetStoreLink FunctionCall) <> "ny_ic_home_blue.png"|| (listItem.prefixImageUrl == "ny_ic_work_blue," <> (getAssetStoreLink FunctionCall) <> "ny_ic_work_blue.png"))) (AddNewAddress.getSavedLocations listResp.list))
              recents = (differenceOfLocationLists recentPredictionsObject.predictionArray savedLocationWithHomeOrWorkTag)
              savedLocationsWithOtherTag = (filter (\listItem -> not(listItem.prefixImageUrl == "ny_ic_home_blue," <> (getAssetStoreLink FunctionCall) <> "ny_ic_home_blue.png" || listItem.prefixImageUrl == "ny_ic_work_blue," <> (getAssetStoreLink FunctionCall) <> "ny_ic_work_blue.png")) (AddNewAddress.getSavedLocations listResp.list))
              updatedList = (map (\item ->  item { postfixImageUrl = if not (checkPrediction item savedLocationsWithOtherTag) then "ny_ic_fav_red," <> (getAssetStoreLink FunctionCall) <> "ny_ic_fav_red.png"
                                                                        else "ny_ic_fav," <> (getAssetStoreLink FunctionCall) <> "ny_ic_fav.png" }) (recents))
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen
                                                                    { data
                                                                      { savedLocations = (AddNewAddress.getSavedLocations listResp.list)
                                                                      , recentSearchs {predictionArray = updatedList}
                                                                      , locationList = updatedList
                                                                      }
                                                                    })
          homeScreenFlow
        Left (err) -> homeScreenFlow

    GO_TO_INVOICE_ updatedState -> do
      let prevRideState = updatedState.data.rideRatingState
      let finalAmount = show prevRideState.finalAmount
      modifyScreenState $ InvoiceScreenStateType (\invoiceScreen -> invoiceScreen {props{fromHomeScreen= true},data{totalAmount = ((getValueFromConfig "currency") <> " " <> finalAmount), date = prevRideState.dateDDMMYY, tripCharges = ((getValueFromConfig "currency") <> " " <> finalAmount), selectedItem {date = prevRideState.dateDDMMYY, bookingId = prevRideState.bookingId,rideStartTime = prevRideState.rideStartTime, rideEndTime = prevRideState.rideEndTime, rideId = prevRideState.rideId, shortRideId = prevRideState.shortRideId,vehicleNumber = prevRideState.vehicleNumber,time = prevRideState.rideStartTime,source = prevRideState.source,destination = prevRideState.destination,driverName = prevRideState.driverName,totalAmount = ((getValueFromConfig "currency") <> " " <> finalAmount)}, config = updatedState.data.config}})
      invoiceScreenFlow

    CHECK_FOR_DUPLICATE_SAVED_LOCATION state -> do
      let recents = map
                    (\item -> item{postfixImageVisibility = false, postfixImageUrl = ""}
                      ) (differenceOfLocationLists (state.data.recentSearchs.predictionArray) state.data.savedLocations)

      case state.data.selectedLocationListItem of
        Nothing -> do
          modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
            addNewAddressScreen
              { props
                { showSavePlaceView = false
                , fromHome = true
                , editLocation = false
                , editSavedLocation = false
                , isLocateOnMap = false
                , isBtnActive = true
                , isSearchedLocationServiceable = true
                , tagExists = false
                , placeNameExists = false }
              , data
                { addressSavedAs = ""
                , placeName = ""
                , savedLocations = state.data.savedLocations
                , locationList = recents
                , recentSearchs{predictionArray = recents}
                , selectedTag = state.props.tagType
                , savedTags = AddNewAddress.getSavedTagsFromHome state.data.savedLocations
                , address = ""
                , activeIndex = case state.props.tagType of
                                  Just tag -> case tag of
                                                HOME_TAG -> Just 0
                                                WORK_TAG -> Just 1
                                                _        -> Just 2
                                  Nothing  -> Nothing }})
          addNewAddressScreenFlow ""
        Just selectedLocationListItem -> do
          case selectedLocationListItem.locationItemType of
            Just RECENTS ->  getDistanceDiff state (fromMaybe 0.0 selectedLocationListItem.lat) (fromMaybe 0.0 selectedLocationListItem.lon)
            Nothing ->  getDistanceDiff state (fromMaybe 0.0 selectedLocationListItem.lat) (fromMaybe 0.0 selectedLocationListItem.lon)
            _ -> do
              (GetPlaceNameResp placeNameResp) <- getPlaceNameResp (selectedLocationListItem.placeId) (fromMaybe 0.0 selectedLocationListItem.lat) (fromMaybe 0.0 selectedLocationListItem.lon) selectedLocationListItem

              let (PlaceName placeName) = (fromMaybe HomeScreenData.dummyLocationName (placeNameResp!!0))
              let (LatLong placeLatLong) = (placeName.location)

              (ServiceabilityResDestination serviceabilityRes) <- Remote.destServiceabilityBT (Remote.makeServiceabilityReqForDest (placeLatLong.lat) (placeLatLong.lon))
              case (serviceabilityRes.serviceable) of
                false -> do
                          _ <- pure $ toast ("Location Unserviceable")
                          homeScreenFlow
                _     ->   modifyScreenState $ HomeScreenStateType (\homeScreen -> state{data{ selectedLocationListItem = Just selectedLocationListItem{lat = Just (placeLatLong.lat), lon = Just (placeLatLong.lon) }}})
              getDistanceDiff  state{data{ saveFavouriteCard{selectedItem{lat = Just (placeLatLong.lat), lon =Just (placeLatLong.lon) }},selectedLocationListItem = Just selectedLocationListItem{lat = Just (placeLatLong.lat), lon = Just (placeLatLong.lon) }}} (placeLatLong.lat) (placeLatLong.lon)
    GO_TO_CALL_EMERGENCY_CONTACT state -> do
        (UserSosRes res) <- Remote.userSosBT (Remote.makeUserSosReq (Remote.createUserSosFlow "EmergencyContact" state.props.emergencyHelpModelState.currentlySelectedContact.phoneNo) state.data.driverInfoCardState.rideId)
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state{props{emergencyHelpModelState{sosId = res.sosId}}})
        homeScreenFlow
    GO_TO_CALL_POLICE state -> do
        (UserSosRes res) <- Remote.userSosBT (Remote.makeUserSosReq (Remote.createUserSosFlow "Police" "") state.data.driverInfoCardState.rideId)
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state{props{emergencyHelpModelState{sosId = res.sosId}}})
        homeScreenFlow
    GO_TO_CALL_SUPPORT state -> do
        (UserSosRes res) <- Remote.userSosBT (Remote.makeUserSosReq (Remote.createUserSosFlow "CustomerCare" "") state.data.driverInfoCardState.rideId)
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state{props{emergencyHelpModelState{sosId = res.sosId}}})
        homeScreenFlow
    GO_TO_SOS_STATUS state -> do
        res <- Remote.userSosStatusBT state.props.emergencyHelpModelState.sosId (Remote.makeSosStatus state.props.emergencyHelpModelState.sosStatus)
        homeScreenFlow
    GO_TO_FETCH_CONTACTS state-> do
      (GetEmergContactsResp res) <- Remote.getEmergencyContactsBT GetEmergContactsReq
      let contacts = map (\(ContactDetails item) -> {
          number: item.mobileNumber,
          name: item.name,
          isSelected: true
        }) res.defaultEmergencyNumbers
      contactsInString <- pure $ toString contacts
      _ <- pure $ setValueToLocalStore CONTACTS (contactsInString)
      contactsInJson <- pure $ parseNewContacts contactsInString
      let newContacts = transformContactList contactsInJson
      modifyScreenState $ HomeScreenStateType (\homeScreen -> state{props{emergencyHelpModelState{emergencyContactData = newContacts}}})
      homeScreenFlow
    SAVE_FAVOURITE state -> do
      let tag = case  (toLower state.data.saveFavouriteCard.tag) of
                  "work" -> "Work"
                  "home" -> "Home"
                  _      -> state.data.saveFavouriteCard.tag
      _ <- setValueToLocalStore RELOAD_SAVED_LOCATION "true"
      case state.data.saveFavouriteCard.selectedItem.lat , state.data.saveFavouriteCard.selectedItem.lon of
        Nothing , Nothing -> fetchLatAndLong state tag
        _ , _ -> do
          resp <- Remote.addSavedLocationBT (encodeAddressDescription state.data.saveFavouriteCard.address tag state.data.saveFavouriteCard.selectedItem.placeId state.data.saveFavouriteCard.selectedItem.lat state.data.saveFavouriteCard.selectedItem.lon [])
          pure unit
      _ <-  pure $ toast (getString FAVOURITE_ADDED_SUCCESSFULLY)
      (savedLocationResp )<- lift $ lift $ Remote.getSavedLocationList ""
      case savedLocationResp of
        Right (SavedLocationsListRes listResp) -> do
          let updatedLocationList = getUpdatedLocationList state.data.locationList state.data.saveFavouriteCard.selectedItem.placeId
          let updatedRecents = getUpdatedLocationList state.data.recentSearchs.predictionArray  state.data.saveFavouriteCard.selectedItem.placeId
          modifyScreenState $ HomeScreenStateType (\homeScreen -> state{data{locationList = updatedLocationList, recentSearchs{predictionArray = updatedRecents},savedLocations = (AddNewAddress.getSavedLocations listResp.list)}})
          homeScreenFlow
        Left (err) -> homeScreenFlow
    GO_TO_REFERRAL -> referralScreenFlow
    ON_CALL state callType -> do
      (OnCallRes res) <- Remote.onCallBT (Remote.makeOnCallReq state.data.driverInfoCardState.rideId (show callType))
      homeScreenFlow
    TRIGGER_PERMISSION_FLOW flowType -> permissionScreenFlow flowType
    REPORT_ISSUE state -> do
       if isNothing state.data.ratingViewState.issueReason then do
        _ <- Remote.callbackRequestBT FunctionCall
        _ <- pure $ toast $ getString WE_WILL_GIVE_YOU_CALLBACK
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state{ data {ratingViewState { issueFacedView = false} }})
        homeScreenFlow
       else do
        _ <- Remote.sendIssueBT (Remote.makeSendIssueReq  Nothing (Just state.props.bookingId) (fromMaybe "" state.data.ratingViewState.issueReason) state.data.ratingViewState.issueDescription )
        _ <- pure $ toast $ getString YOUR_ISSUE_HAS_BEEN_REPORTED
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state{ data {ratingViewState { issueFacedView = false, openReportIssue = false} }})
        homeScreenFlow
    RIDE_DETAILS_SCREEN state -> do
      tripDetailsScreenFlow Home
    _ -> homeScreenFlow

getDistanceDiff :: HomeScreenState -> Number -> Number -> FlowBT String Unit
getDistanceDiff state lat lon = do
  distanceInfo <- getDistanceInfo (state.data.savedLocations) "" (lat) (lon) (fromMaybe "" state.data.saveFavouriteCard.selectedItem.placeId)
  case distanceInfo.locExistsAs of
    "" ->  modifyScreenState $ HomeScreenStateType (\homeScreen -> state{props{isSaveFavourite = true}})
    _  -> do
            _ <- pure $ toast  (getString ALREADY_EXISTS)
            modifyScreenState $ HomeScreenStateType (\homeScreen -> state{data{saveFavouriteCard{selectedItem = dummyLocationListState}}})
  homeScreenFlow


fetchLatAndLong :: HomeScreenState -> String -> FlowBT String Unit
fetchLatAndLong state tag  =
  case state.data.saveFavouriteCard.selectedItem.placeId of
    Just placeID -> do
      (GetPlaceNameResp placeNameResp) <- getPlaceNameResp (Just placeID) (fromMaybe 0.0 state.data.saveFavouriteCard.selectedItem.lat) (fromMaybe 0.0 state.data.saveFavouriteCard.selectedItem.lon) state.data.saveFavouriteCard.selectedItem
      let (PlaceName placeName) = (fromMaybe HomeScreenData.dummyLocationName (placeNameResp !! 0))
      let (LatLong placeLatLong) = (placeName.location)
      resp <- Remote.addSavedLocationBT (encodeAddressDescription state.data.saveFavouriteCard.address tag state.data.saveFavouriteCard.selectedItem.placeId (Just placeLatLong.lat) (Just placeLatLong.lon) placeName.addressComponents)
      pure unit
    Nothing -> pure unit

rideSearchFlow :: String -> FlowBT String Unit
rideSearchFlow flowType = do
  logField_ <- lift $ lift $ getLogFields
  (GlobalState homeScreenModifiedState) <- getState
  let finalState = homeScreenModifiedState.homeScreen -- bothLocationChangedState{props{isSrcServiceable =homeScreenModifiedState.homeScreen.props.isSrcServiceable, isDestServiceable = homeScreenModifiedState.homeScreen.props.isDestServiceable, isRideServiceable = homeScreenModifiedState.homeScreen.props.isRideServiceable }}
  if (finalState.props.sourceLat /= 0.0 && finalState.props.sourceLong /= 0.0) && (finalState.props.destinationLat /= 0.0 && finalState.props.destinationLong /= 0.0) && (finalState.data.source /= "") && (finalState.data.destination /= "")
    then do
      case finalState.props.sourceSelectedOnMap of
        false -> do
          pure $ removeAllPolylines ""
          liftFlowBT $ runEffectFn5 locateOnMap false finalState.props.sourceLat finalState.props.sourceLong finalState.data.polygonCoordinates finalState.data.nearByPickUpPoints
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = ConfirmingLocation,rideRequestFlow = true}})
          _ <- pure $ updateLocalStage ConfirmingLocation
          void $ lift $ lift $ toggleLoader false
        true -> do
          PlaceName address <- getPlaceName finalState.props.sourceLat finalState.props.sourceLong HomeScreenData.dummyLocation
          (SearchRes rideSearchRes) <- Remote.rideSearchBT (Remote.makeRideSearchReq finalState.props.sourceLat finalState.props.sourceLong finalState.props.destinationLat finalState.props.destinationLong (encodeAddress address.formattedAddress [] finalState.props.sourcePlaceId) finalState.data.destinationAddress)
          void $ pure $ setFlowStatusData (FlowStatusData { source : {lat : finalState.props.sourceLat, lng : finalState.props.sourceLong, place : address.formattedAddress, address : Nothing}
                                                          , destination : {lat : finalState.props.destinationLat, lng : finalState.props.destinationLong, place : finalState.data.destination, address : Nothing}
                                                          , sourceAddress : (encodeAddress address.formattedAddress [] finalState.props.sourcePlaceId)
                                                          , destinationAddress : finalState.data.destinationAddress })
          case finalState.props.currentStage of
            TryAgain -> do
              when (finalState.props.customerTip.enableTips) $ do
                cancelEstimate finalState.props.estimateId
              _ <- pure $ updateLocalStage TryAgain
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{searchId = rideSearchRes.searchId, currentStage = TryAgain, rideRequestFlow = true}, data{nearByDrivers = Nothing}})
            _        -> do
              let sourceSpecialTagIcon = specialLocationIcons finalState.props.zoneType.sourceTag
                  destSpecialTagIcon = specialLocationIcons finalState.props.zoneType.destinationTag
              routeResponse <- Remote.drawMapRoute finalState.props.sourceLat finalState.props.sourceLong finalState.props.destinationLat finalState.props.destinationLong (Remote.normalRoute "") "NORMAL" address.formattedAddress finalState.data.destination rideSearchRes.routeInfo "pickup" (specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon)
              case rideSearchRes.routeInfo of
                Just (Route response) -> do
                  let distance = if response.distance < 1000 then toString(response.distance)  <> " m" else parseFloat(INT.toNumber(response.distance) / 1000.0) 2 <> " km"
                      duration = (show (response.duration / 60)) <> " min"
                      tipEnabled = isTipEnabled response.distance
                      Snapped points = response.points
                  case head points, last points of
                    Just (LatLong source), Just (LatLong dest) -> do
                      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props{ routeEndPoints = Just ({ source : { lat : source.lat, lng : source.lon, place : address.formattedAddress, address : Nothing }, destination : { lat : dest.lat, lng : dest.lon, place : finalState.data.destination, address : Nothing } }) } })
                    _ , _ -> pure unit
                  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{rideDistance = distance, rideDuration = duration,source = address.formattedAddress, sourceAddress = encodeAddress address.formattedAddress [] finalState.props.sourcePlaceId}, props{customerTip{enableTips = tipEnabled}}})
                  _ <- setValueToLocalStore ENABLE_TIPS $ show tipEnabled
                  if ((MU.getMerchant FunctionCall) /= MU.YATRI && response.distance >= 50000 )then do
                    _ <- pure $ updateLocalStage DistanceOutsideLimits
                    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = DistanceOutsideLimits ,rideRequestFlow = true, isSearchLocation = SearchLocation}})
                    void $ lift $ lift $ toggleLoader false
                    else if response.distance < 500 && (getValueToLocalStore LOCAL_STAGE) /= "ShortDistance" then do
                      _ <- pure $ updateLocalStage ShortDistance
                      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = ShortDistance ,rideRequestFlow = true, isSearchLocation = SearchLocation, distance = response.distance}})
                      void $ lift $ lift $ toggleLoader false
                      else do
                        if flowType == "REPEAT_RIDE_FLOW" then liftFlowBT $ logEventWithParams logField_ "ny_user_repeat_ride_flow" "searchId" rideSearchRes.searchId else pure unit
                        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{searchId = rideSearchRes.searchId,currentStage = FindingEstimate, rideRequestFlow = true, isSearchLocation = SearchLocation, sourcePlaceId = Nothing, destinationPlaceId = Nothing}, data {source = address.formattedAddress, sourceAddress = encodeAddress address.formattedAddress [] finalState.props.sourcePlaceId, nearByDrivers = Nothing}})
                        _ <- pure $ updateLocalStage FindingEstimate
                        void $ lift $ lift $ toggleLoader false

                Nothing -> pure unit
    else modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{isSource = Just false, isRideServiceable = true, isSrcServiceable = true, isDestServiceable = true, currentStage = SearchLocationModel}})

  homeScreenFlow

dummyAddressGeometry :: AddressGeometry
dummyAddressGeometry = AddressGeometry {
  geometry : Geometry{
    location : LocationS{
      lat: 0.0,
      lng: 0.0
    }
  }
 }

getFinalAmount :: RideBookingRes -> Int
getFinalAmount (RideBookingRes resp) =
    let rideList = resp.rideList
        (RideAPIEntity ride) = (fromMaybe dummyRideAPIEntity (rideList !! 0))
    in INT.round $ fromMaybe 0.0 $ fromString (show (fromMaybe 0 ride.computedPrice))

tripDetailsScreenFlow :: TripDetailsGoBackType ->  FlowBT String Unit
tripDetailsScreenFlow fromMyRides = do
  (GlobalState state) <- getState
  config <- getAppConfig Constants.appConfig
  logField_ <- lift $ lift $ getLogFields
  expiryTime <- pure $ (getExpiryTime state.tripDetailsScreen.data.selectedItem.rideEndTimeUTC isForLostAndFound)
  modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {props{fromMyRides = fromMyRides, canConnectWithDriver = (expiryTime <= 86400)}, data{config = config}}) -- expiryTime < 24hrs or 86400 seconds
  flow <- UI.tripDetailsScreen
  case flow of
    GO_TO_HELPSCREEN -> helpAndSupportScreenFlow
    GO_TO_RIDES -> do
      (GlobalState newState) <- getState
      myRidesScreenFlow newState.myRidesScreen.props.fromNavBar
    ON_SUBMIT state -> do
      liftFlowBT $ logEventWithParams logField_ "ny_user_issue_reported" "Description" (state.data.message)
      _ <- Remote.sendIssueBT (Remote.makeSendIssueReq  Nothing (Just state.data.selectedItem.bookingId) state.data.message state.data.message )
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {props{issueReported = true}})
      tripDetailsScreenFlow state.props.fromMyRides
    GO_TO_INVOICE updatedState -> do
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_invoice_clicked" $ [ { key : "Pickup", value : unsafeToForeign updatedState.data.selectedItem.source},
                                                                                                          { key : "Destination", value : unsafeToForeign updatedState.data.selectedItem.destination},
                                                                                                          { key : "Fare", value : unsafeToForeign updatedState.data.selectedItem.totalAmount},
                                                                                                          { key : "Status", value : unsafeToForeign updatedState.data.selectedItem.status},
                                                                                                          { key : "Ride completion timestamp", value : unsafeToForeign updatedState.data.selectedItem.rideEndTime},
                                                                                                          { key : "Rating", value : (unsafeToForeign $ show $ updatedState.data.selectedItem.rating)}]
      modifyScreenState $ InvoiceScreenStateType (\invoiceScreen -> invoiceScreen {props{fromHomeScreen = false},data{totalAmount = updatedState.data.totalAmount, date = updatedState.data.date, tripCharges = updatedState.data.totalAmount, selectedItem = updatedState.data.selectedItem, config = updatedState.data.config}})
      invoiceScreenFlow
    GO_TO_HOME state -> do
      if state.props.fromMyRides == Home then
        modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData)
        else modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {  data{settingSideBar{opened = SettingSideBarController.CLOSED}}})
      homeScreenFlow
    CONNECT_WITH_DRIVER updatedState -> do
      void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      resp <- Remote.callDriverBT updatedState.data.selectedItem.rideId
      void $ lift $ lift $ toggleLoader false
      pure $ toast (getString REQUEST_RECEIVED_WE_WILL_CALL_YOU_BACK_SOON)
      _ <- Remote.sendIssueBT (Remote.makeSendIssueReq  (Just (MU.getValueFromConfig "SUPPORT_EMAIL")) (Just updatedState.data.selectedItem.rideId) "LOSTANDFOUND" "LOST AND FOUND" )
      tripDetailsScreenFlow updatedState.props.fromMyRides


invoiceScreenFlow :: FlowBT String Unit
invoiceScreenFlow = do
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ InvoiceScreenStateType (\invoiceScreen -> invoiceScreen{data{config = config}})
  flow <- UI.invoiceScreen
  (GlobalState newState) <- getState
  case flow of
    InvoiceScreenOutput.GoBack -> tripDetailsScreenFlow newState.tripDetailsScreen.props.fromMyRides
    InvoiceScreenOutput.GoToHome -> homeScreenFlow
  pure unit

contactUsScreenFlow :: FlowBT String Unit
contactUsScreenFlow = do
  flow <- UI.contactUsScreen
  case flow of
    GO_TO_HOME_FROM_CONTACT state -> do
      _ <- Remote.sendIssueBT (Remote.makeSendIssueReq (Just state.data.email) Nothing state.data.description state.data.subject )
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {  data{settingSideBar{opened = SettingSideBarController.CLOSED}}})
      homeScreenFlow
  pure unit

helpAndSupportScreenFlow :: FlowBT String Unit
helpAndSupportScreenFlow = do
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ ContactUsScreenStateType (\contactUsScreen -> contactUsScreen{data{config = config}})
  modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen{data{config = config}})
  flow <- UI.helpAndSupportScreen
  case flow of
    GO_TO_HOME_FROM_HELP -> homeScreenFlow
    GO_TO_SUPPORT_SCREEN bookingId'-> do
      modifyScreenState $ ContactUsScreenStateType (\contactUsScreen -> contactUsScreen {data{bookingId = bookingId'}})
      contactUsScreenFlow
    GO_TO_TRIP_DETAILS state -> do
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {data {tripId = state.data.tripId, vehicleVariant = state.data.vehicleVariant, selectedItem {status = state.data.status, faresList = state.data.faresList ,date = state.data.date, bookingId = state.data.bookingId,rideStartTime = state.data.rideStartTime, rideEndTime = state.data.rideEndTime, rideId = state.data.rideId, vehicleNumber = state.data.vehicleNumber,time = state.data.time,source = state.data.source,destination = state.data.destination,driverName = state.data.driverName,totalAmount = state.data.totalAmount, rating = state.data.rating, shortRideId = state.data.tripId},date = state.data.date, time = state.data.time, source = state.data.source, destination = state.data.destination, driverName = state.data.driverName, totalAmount = state.data.totalAmount,rating = state.data.rating}})
      tripDetailsScreenFlow HelpAndSupport
    VIEW_RIDES -> do
      modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> myRidesScreen { data{offsetValue = 0}})
      myRidesScreenFlow false
    UPDATE_STATE updatedState -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> updatedState)
      helpAndSupportScreenFlow
    DELETE_USER_ACCOUNT updatedState -> do
      _ <- Remote.sendIssueBT (Remote.makeSendIssueReq (Just updatedState.data.email) Nothing "Request To Delete Account" updatedState.data.description )
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen { props{showDeleteAccountView = true}, data {accountStatus = DEL_REQUESTED}})
      helpAndSupportScreenFlow

myRidesScreenFlow :: Boolean ->  FlowBT String Unit
myRidesScreenFlow fromNavBar = do
  config <- getAppConfig Constants.appConfig
  logField_ <- lift $ lift $ getLogFields
  (GlobalState globalState) <- getState
  modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> myRidesScreen {props{fromNavBar = fromNavBar}, data{config = config, isSrcServiceable = globalState.homeScreen.props.isSrcServiceable}})
  flow <- UI.myRidesScreen
  case flow of
    REFRESH state -> myRidesScreenFlow state.props.fromNavBar
    TRIP_DETAILS state -> do
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_my_rides_view_details" $ [ { key : "Pickup", value : unsafeToForeign state.data.selectedItem.source},
                                                                                                                  { key : "Destination", value : unsafeToForeign state.data.selectedItem.destination},
                                                                                                                  { key : "Fare", value : unsafeToForeign state.data.selectedItem.totalAmount},
                                                                                                                  { key : "Status", value : unsafeToForeign state.data.selectedItem.status},
                                                                                                                  { key : if state.data.selectedItem.status == "CANCELLED" then "Time" else "Ride completion timestamp",
                                                                                                                    value : unsafeToForeign $ if state.data.selectedItem.status == "CANCELLED" then state.data.selectedItem.time else state.data.selectedItem.rideEndTime},
                                                                                                                  { key : "Rating", value : unsafeToForeign (show $ state.data.selectedItem.rating)}]
      modifyScreenState $ TripDetailsScreenStateType (\tripDetails -> tripDetails{data{vehicleVariant = state.data.selectedItem.vehicleVariant}})
      tripDetailsScreenFlow MyRides
    LOADER_OUTPUT state -> do
      modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> state{data{offsetValue = state.data.offsetValue + 8}})
      myRidesScreenFlow state.props.fromNavBar
    BOOK_RIDE -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {  data{settingSideBar{opened = SettingSideBarController.CLOSED}}})
      homeScreenFlow
    GO_TO_NAV_BAR -> homeScreenFlow
    GO_TO_HELP_SCREEN -> helpAndSupportScreenFlow
    REPEAT_RIDE_FLOW state -> do
      updateRideDetails state
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_my_rides_repeat_ride" $ [{ key : "Pickup", value : unsafeToForeign state.data.selectedItem.source},
                                                                                                                {key : "Destination", value : unsafeToForeign state.data.selectedItem.destination}]
      let sourceLat = state.data.selectedItem.sourceLocation^._lat
      let sourceLong = state.data.selectedItem.sourceLocation^._lon
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq sourceLat sourceLong)
      let srcServiceable = sourceServiceabilityResp.serviceable
      let (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
      let pickUpPoints = map (\(GatesInfo item) -> {
                                              place: item.name,
                                              lat  : (item.point)^._lat,
                                              lng : (item.point)^._lon,
                                              address : item.address
                                            }) srcSpecialLocation.gates
      if(state.data.selectedItem.isSpecialZone) then do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{sourceSelectedOnMap = false},data{polygonCoordinates = fromMaybe "" sourceServiceabilityResp.geoJson, nearByPickUpPoints = pickUpPoints}})
        pure unit
        else pure unit
      rideSearchFlow "REPEAT_RIDE_FLOW"

selectLanguageScreenFlow :: FlowBT String Unit
selectLanguageScreenFlow = do
  appConfig <- getAppConfig Constants.appConfig
  logField_ <- lift $ lift $ getLogFields
  modifyScreenState $ SelectLanguageScreenStateType (\selectLanguageScreen -> selectLanguageScreen{data{config = appConfig}})
  flow <- UI.selectLanguageScreen
  case flow of
    UPDATE_LANGUAGE state -> do
                                liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_lang_selected" $[{ key : "Previous language", value : unsafeToForeign $ show $ getValueToLocalStore LANGUAGE_KEY},
                                                                                                                          { key : "New language", value : unsafeToForeign state.props.selectedLanguage}]
                                setValueToLocalStore LANGUAGE_KEY (state.props.selectedLanguage)
                                _ <- lift $ lift $ liftFlow $ logEventWithParams logField_ "ny_user_lang_selec" "language" (state.props.selectedLanguage)
                                let langVal =  case (state.props.selectedLanguage) of
                                                                                     "HI_IN" -> "HINDI"
                                                                                     "EN_US" -> "ENGLISH"
                                                                                     "KN_IN" -> "KANNADA"
                                                                                     "BN_IN" -> "BENGALI"
                                                                                     "ML_IN" -> "MALAYALAM"
                                                                                     _ -> getValueFromConfig "defaultLanguage"
                                void $ pure $ setCleverTapUserProp "Preferred Language" langVal
                                resp <- lift $ lift $ Remote.updateProfile (Remote.mkUpdateProfileRequest FunctionCall)
                                modifyScreenState $ SelectLanguageScreenStateType (\selectLanguageScreen -> SelectLanguageScreenData.initData)
                                homeScreenFlow
    GO_TO_HOME_SCREEN     -> homeScreenFlow

emergencyScreenFlow :: FlowBT String Unit
emergencyScreenFlow = do
  flow <- UI.emergencyContactsScreen
  case flow of
    GO_TO_HOME_FROM_EMERGENCY_CONTACTS -> homeScreenFlow
    POST_CONTACTS state -> do
      _ <- Remote.emergencyContactsBT (Remote.postContactsReq state.data.contactsList)
      if state.props.showInfoPopUp then pure $ toast $ getString CONTACT_REMOVED_SUCCESSFULLY
        else pure $ toast $ getString EMERGENCY_CONTACS_ADDED_SUCCESSFULLY
      modifyScreenState $  EmergencyContactsScreenStateType (\emergencyContactsScreen -> state{props{showInfoPopUp = false}})
      (GlobalState globalState) <- getState
      if globalState.homeScreen.props.emergencyHelpModelState.isSelectEmergencyContact
      then do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{emergencyHelpModelState{isSelectEmergencyContact = false, emergencyContactData = transformContactList state.data.contactsList}}})
        homeScreenFlow
      else emergencyScreenFlow
    GET_CONTACTS state -> do
      (GetEmergContactsResp res) <- Remote.getEmergencyContactsBT GetEmergContactsReq
      let contacts = map (\(ContactDetails item) -> {
          number: item.mobileNumber,
          name: item.name,
          isSelected: true
        }) res.defaultEmergencyNumbers
      contactsInString <- pure $ toString contacts
      _ <- pure $ setValueToLocalStore CONTACTS (contactsInString)
      modifyScreenState $  EmergencyContactsScreenStateType (\emergencyContactsScreen -> state{data{contactsList = contacts}})
      emergencyScreenFlow
    REFRESH_EMERGECY_CONTACTS_SCREEN state -> do
      modifyScreenState $  EmergencyContactsScreenStateType (\emergencyContactsScreen -> state)
      emergencyScreenFlow

aboutUsScreenFlow :: FlowBT String Unit
aboutUsScreenFlow = do
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ AboutUsScreenStateType (\aboutUsScreen -> aboutUsScreen {appConfig = config})
  flow <- UI.aboutUsScreen
  case flow of
    GO_TO_HOME_FROM_ABOUT -> homeScreenFlow

permissionScreenFlow :: String -> FlowBT String Unit
permissionScreenFlow triggertype = do
  _ <- pure $ hideKeyboardOnNavigation true
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ PermissionScreenStateType (\permissionScreen -> permissionScreen{appConfig = config})
  flow <- UI.permissionScreen triggertype
  permissionConditionA <- lift $ lift $ liftFlow $ isLocationPermissionEnabled unit
  permissionConditionB <- lift $ lift $ liftFlow $ isLocationEnabled unit
  internetCondition <- lift $ lift $ liftFlow $ isInternetAvailable unit
  case flow of
    REFRESH_INTERNET -> do
        if (os == "IOS") then pure unit
          else if not internetCondition then permissionScreenFlow "INTERNET_ACTION"
          else currentFlowStatus
    TURN_ON_GPS -> if not internetCondition then permissionScreenFlow "INTERNET_ACTION" else do
                      setValueToLocalStore PERMISSION_POPUP_TIRGGERED "true"
                      currentFlowStatus
    TURN_ON_INTERNET -> case (getValueToLocalStore USER_NAME == "__failed") of
                            true -> pure unit
                            _ -> if (os == "IOS") then currentFlowStatus
                                 else if (not (permissionConditionA && permissionConditionB) )then permissionScreenFlow "LOCATION_DISABLED"
                                 else currentFlowStatus
  pure unit

myProfileScreenFlow :: FlowBT String Unit
myProfileScreenFlow = do
  config <- getAppConfig Constants.appConfig
  disabilityListT <- updateDisabilityList "My_Profile_Screen"
  modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState -> myProfileScreenState{data{config = config, disabilityOptions{disabilityOptionList = disabilityListT }, editedDisabilityOptions{disabilityOptionList = disabilityListT}}})
  flow <- UI.myProfileScreen
  case flow of
    UPDATE_USER_PROFILE state -> do
      _ <- pure $ toggleBtnLoader "" false
      _ <- pure $ spy "profile_updated_state" state
      let stringName = seperateByWhiteSpaces(state.data.editedName)
          name = split (Pattern " ") stringName
          nameLength = length name
          gender = getGenderValue state.data.editedGender
          email = if state.data.editedEmailId == state.data.emailId || (state.data.editedEmailId == Just "") then Nothing else state.data.editedEmailId
          disability = case state.data.editedDisabilityOptions.selectedDisability of 
            Just disability -> if (state.data.editedDisabilityOptions.activeIndex == 1) 
                                  then Just (Remote.mkDisabilityData disability (fromMaybe "" state.data.editedDisabilityOptions.otherDisabilityReason))
                                  else Nothing
            _ -> Nothing
          hasDisability = if state.props.changeAccessibility then (Just (isJust disability)) else Nothing
      resp <- if nameLength > 2 then
                lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (name !! 0) (name !! 1) (name !! (nameLength - 1)) (email) gender hasDisability disability)
                else if nameLength == 2 then
                  lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (name !! 0) (Just "") (name !! 1) (email) gender hasDisability disability)
                  else if nameLength == 1 then
                    lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (name !! 0) (Just "") (Just "") (email) gender hasDisability disability)
                    else
                      lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (Just "") (Just "") (Just "") (email) gender hasDisability disability)
      case resp of
        Right response -> do
          setValueToLocalStore USER_NAME stringName
          case gender of
            Just gender -> modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{gender = Just gender}}, props{isBanner = false}})
            _ -> pure unit
          case email of
            Just email -> do
              setValueToLocalStore USER_EMAIL email
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{email = Just email}}})
            _ -> pure unit
          modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState ->  MyProfileScreenData.initData)
          myProfileScreenFlow
        Left err -> do
          let errResponse = err.response
          let codeMessage = decodeError errResponse.errorMessage "errorCode"
          case codeMessage of
            "PERSON_EMAIL_ALREADY_EXISTS" -> do
              pure $ setText (getNewIDWithTag "EmailEditText") ""
              modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState -> myProfileScreenState{props{isEmailValid = false, updateProfile = true}, data{emailErrorMessage = Just EMAIL_EXISTS, name = state.data.name, editedName = state.data.editedName, emailId = state.data.emailId, gender = state.data.gender, editedGender = state.data.editedGender}})
            _ -> pure $ toast (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
          myProfileScreenFlow
      myProfileScreenFlow
    GO_TO_HOME_ -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{opened = SettingSideBarController.CLOSED}}})
      homeScreenFlow

savedLocationFlow :: FlowBT String Unit
savedLocationFlow = do
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ SavedLocationScreenStateType (\savedLocationScreen -> savedLocationScreen{data{config = config}})
  void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
  flow <- UI.savedLocationScreen
  (SavedLocationsListRes savedLocationResp )<- Remote.getSavedLocationBT SavedLocationReq
  case flow of
    ADD_NEW_LOCATION state-> do
      (GlobalState newState) <- getState
      resp <- lift $ lift $ getRecentSearches newState.addNewAddressScreen
      let recents = map
                    (\item -> item{postfixImageUrl = "", postfixImageVisibility = false}) (differenceOfLocationLists (resp.predictionArray) ((AddNewAddress.getSavedLocations savedLocationResp.list)))
      modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> addNewAddressScreen{data{savedLocations = getSavedLocationForAddNewAddressScreen state.data.savedLocations ,locationList = recents ,placeName = "",address = "",addressSavedAs="", recentSearchs{predictionArray = recents},savedTags = (AddNewAddress.getSavedTags savedLocationResp.list)}, props{showSavePlaceView = false, editLocation = false, isLocationServiceable = true, isSearchedLocationServiceable = true, isLocateOnMap = false, fromHome = false}})
      case (AddNewAddress.validTag (AddNewAddress.getSavedTags savedLocationResp.list) "HOME" ""), (AddNewAddress.validTag (AddNewAddress.getSavedTags savedLocationResp.list) "WORK" "") of
          false   , false    -> modifyScreenState $ AddNewAddressScreenStateType(\addNewAddressScreen -> addNewAddressScreen{data{activeIndex = (Just 2), selectedTag = (Just OTHER_TAG) }, props{editSavedLocation = false}})
          _ , _ -> modifyScreenState $ AddNewAddressScreenStateType(\addNewAddressScreen -> addNewAddressScreen{data{activeIndex = Nothing, selectedTag = Nothing}, props{editSavedLocation = false}})
      addNewAddressScreenFlow "dummy"
    DELETE_LOCATION tagName -> do
      resp <- Remote.deleteSavedLocationBT (DeleteSavedLocationReq (trim tagName))
      pure $ toast (getString FAVOURITE_REMOVED_SUCCESSFULLY)
      _ <- pure $ setValueToLocalStore RELOAD_SAVED_LOCATION "true"
      savedLocationFlow
    EDIT_LOCATION cardState -> do
      (ServiceabilityRes serviceabilityRes) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq (fromMaybe 0.0 cardState.lat) (fromMaybe 0.0 cardState.lon))
      modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
        addNewAddressScreen
          { props
              { tagExists = false
              , showSavePlaceView = true
              , editLocation= true
              , editSavedLocation = true
              , isBtnActive = false
              , isLocateOnMap = false
              , isLocationServiceable = (serviceabilityRes.serviceable)
              , fromHome = false
              }
          , data
              { existsAs = ""
              , selectedTag = getCardType (fromMaybe "" (cardState.cardType))
              , placeName = cardState.tagName
              , savedLocations = (AddNewAddress.getSavedLocations savedLocationResp.list)
              , address = cardState.savedLocation
              , addressSavedAs = cardState.tagName
              , selectedItem
                  { title = (fromMaybe "" ((split (Pattern ",") (cardState.savedLocation)) !! 0))
                  , description = cardState.savedLocation
                  , lat = cardState.lat
                  , lon = cardState.lon
                  , placeId = cardState.placeId
                  , subTitle = (drop ((fromMaybe 0 (indexOf (Pattern ",") (cardState.savedLocation))) + 2) (cardState.savedLocation))
                  }
              , savedTags = (AddNewAddress.getSavedTags savedLocationResp.list)
              , lat = fromMaybe 0.0 cardState.lat
              , lon = fromMaybe 0.0 cardState.lon
              , latSelectedFromMap = fromMaybe 0.0 cardState.lat
              , lonSelectedFromMap = fromMaybe 0.0 cardState.lon
              , locSelectedFromMap = ""
              , activeIndex = case (getCardType (fromMaybe "" (cardState.cardType))) of
                                Just card -> case card of
                                                HOME_TAG -> Just 0
                                                WORK_TAG -> Just 1
                                                OTHER_TAG-> Just 2
                                Nothing   -> Nothing}})

      addNewAddressScreenFlow "edit Location"

    GO_BACK_FROM_SAVED_LOCATION -> do
      _ <- lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreenMap")
      homeScreenFlow
  pure unit

addNewAddressScreenFlow ::String -> FlowBT String Unit
addNewAddressScreenFlow input = do
  config <- getAppConfig Constants.appConfig
  logField_ <- lift $ lift $ getLogFields
  modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> addNewAddressScreen{data{config = config}})
  flow <- UI.addNewAddressScreen
  case flow of
    SEARCH_ADDRESS input state -> do
      (GlobalState newState) <- getState
      (SearchLocationResp searchLocationResp) <- Remote.searchLocationBT (Remote.makeSearchLocationReq input ( newState.homeScreen.props.sourceLat) ( newState.homeScreen.props.sourceLong) getSearchRadius (case (getValueToLocalStore LANGUAGE_KEY) of
                                                                                                                                                                                                                                "HI_IN" -> "HINDI"
                                                                                                                                                                                                                                "KN_IN" -> "KANNADA"
                                                                                                                                                                                                                                "BN_IN" -> "BENGALI"
                                                                                                                                                                                                                                "ML_IN" -> "MALAYALAM"
                                                                                                                                                                                                                                _      -> "ENGLISH") "")
      let sortedByDistanceList = sortPredctionByDistance searchLocationResp.predictions
          predictionList = AddNewAddress.getLocationList sortedByDistanceList
          recentLists = state.data.recentSearchs.predictionArray
          filteredRecentsList = filterRecentSearches recentLists predictionList
          filteredPredictionList = differenceOfLocationLists predictionList filteredRecentsList

      modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> state{  data  { locationList = map
                                                                                                                (\item -> item{ postfixImageVisibility = (not (checkPrediction item state.data.savedLocations))
                                                                                                                              , postfixImageUrl = if (checkPrediction item state.data.savedLocations) then "" else "ny_ic_fav_red," <> (getAssetStoreLink FunctionCall) <> "ny_ic_fav_red.png"
                                                                                                                              , isClickable = (checkPrediction item state.data.savedLocations)
                                                                                                                              , alpha = if (checkPrediction item state.data.savedLocations) then 1.0 else 0.5 }) (filteredPredictionList <> filteredRecentsList) }})
      addNewAddressScreenFlow ""

    ADD_LOCATION state -> do
      if (state.props.editSavedLocation) then do
        _ <- Remote.deleteSavedLocationBT (DeleteSavedLocationReq (trim state.data.placeName))
        pure unit
      else pure unit
      liftFlowBT $ logEventWithMultipleParams logField_ "ny_user_favourite_added" $ [{ key : "Address", value : unsafeToForeign state.data.address},
                                                                                                                { key : "Tag", value : unsafeToForeign $ show $ state.data.selectedTag}]
      (GetPlaceNameResp sourcePlace) <- getPlaceNameResp (state.data.selectedItem.placeId) (fromMaybe 0.0 state.data.selectedItem.lat) (fromMaybe 0.0 state.data.selectedItem.lon)  state.data.selectedItem
      let source = state.data.selectedItem.description
          (PlaceName sourceAddressGeometry) = (fromMaybe HomeScreenData.dummyLocationName (sourcePlace!!0))
          (LatLong sourceLocation) = (sourceAddressGeometry.location)
          lat = sourceLocation.lat
          lng = sourceLocation.lon
          newstate = state { data { lat =lat, lon=lng, selectedItem
                                                        { description = source
                                                        , lat = Just lat
                                                        , lon = Just lng
                                                        }
                                    , addressComponents = sourceAddressGeometry.addressComponents
                                    }
                              }

      resp <- Remote.addSavedLocationBT (AddNewAddress.encodeAddressDescription newstate)
      if state.props.editSavedLocation then pure $ toast (getString FAVOURITE_UPDATED_SUCCESSFULLY)
        else pure $ toast (getString FAVOURITE_ADDED_SUCCESSFULLY)

      _ <- pure $ setValueToLocalStore RELOAD_SAVED_LOCATION "true"
      _ <- lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreenMap")
      if state.props.fromHome then do
        (GlobalState globalState) <- getState
        (savedLocationResp )<- lift $ lift $ Remote.getSavedLocationList ""
        case savedLocationResp of
          Right (SavedLocationsListRes listResp) -> do
            let updatedLocationList = getUpdatedLocationList globalState.homeScreen.data.locationList state.data.selectedItem.placeId
            modifyScreenState $ HomeScreenStateType (\homeScreen ->
                                                        homeScreen
                                                          { data
                                                              { settingSideBar {opened = SettingSideBarController.CLOSED, appConfig = DC.config}
                                                              , locationList = updatedLocationList
                                                              , savedLocations = (AddNewAddress.getSavedLocations listResp.list)
                                                              }
                                                            } )

            homeScreenFlow
          Left (err) -> homeScreenFlow
        else savedLocationFlow

    UPDATE_LOCATION_NAME_ADDRESS state lat lon -> do
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq lat lon)
      let isServiceable = sourceServiceabilityResp.serviceable
      let (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
      let pickUpPoints = map (\(GatesInfo item) -> {
                                              place: item.name,
                                              lat  : (item.point)^._lat,
                                              lng : (item.point)^._lon,
                                              address : item.address
                                            }) srcSpecialLocation.gates
      let gateAddress = (fromMaybe HomeScreenData.dummyLocation ((filter( \ (item) -> (item.place == state.props.defaultPickUpPoint)) pickUpPoints) !! 0))
      if (fromMaybe "" sourceServiceabilityResp.geoJson) /= "" && (fromMaybe "" sourceServiceabilityResp.geoJson) /= state.data.polygonCoordinates && pickUpPoints /= state.data.nearByPickUpPoints then do
        modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> addNewAddressScreen{  data { polygonCoordinates = fromMaybe "" sourceServiceabilityResp.geoJson
                                                                                                             , nearByPickUpPoints = pickUpPoints
                                                                                                             }
                                                                                                      , props{ isSpecialZone =  (sourceServiceabilityResp.geoJson) /= Nothing
                                                                                                             , defaultPickUpPoint = (fromMaybe AddNewAddressScreenData.dummyLocation (state.data.nearByPickUpPoints!!0)).place
                                                                                                             , isServiceable = isServiceable
                                                                                                             }
                                                                                                      })
        _ <- pure $ removeAllPolylines ""
        liftFlowBT $ runEffectFn5 locateOnMap false lat lon (fromMaybe "" sourceServiceabilityResp.geoJson) pickUpPoints
        addNewAddressScreenFlow ""
      else do
        PlaceName address <- getPlaceName lat lon gateAddress
        modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> addNewAddressScreen{  data  { locSelectedFromMap = address.formattedAddress
                                                                                                              , latSelectedFromMap = lat
                                                                                                              , lonSelectedFromMap = lon
                                                                                                              }
                                                                                                      , props { isServiceable = isServiceable }
                                                                                                      } )
        addNewAddressScreenFlow ""
    GO_TO_FAVOURITES -> do
      _ <- lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreenMap")
      savedLocationFlow

    CHECK_LOCATION_SERVICEABILITY state locItemType-> do
      _ <- pure $ spy "Inside CHECK_LOCATION_SERVICEABILITY" state
      let item  = state.data.selectedItem
      if item.locationItemType /= Just RECENTS then do
        (GetPlaceNameResp placeNameResp) <- getPlaceNameResp (item.placeId) (fromMaybe 0.0 item.lat) (fromMaybe 0.0 item.lon) item
        let (PlaceName placeName) = (fromMaybe HomeScreenData.dummyLocationName (placeNameResp!!0))
        let (LatLong placeLatLong) = (placeName.location)
        (ServiceabilityRes serviceabilityRes) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq placeLatLong.lat placeLatLong.lon)
        case (serviceabilityRes.serviceable) , (state.props.editLocation) of
          true , isEditLocation ->  modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
            addNewAddressScreen
              { data
                  { address = item.description
                  , selectedItem = item
                  , selectedTag = if isEditLocation then addNewAddressScreen.data.selectedTag
                                  else Nothing
                  , addressSavedAs = case isEditLocation of
                                      true -> if (toLower state.data.placeName /= "home" && toLower state.data.placeName /= "work") then state.data.addressSavedAs
                                                else state.data.placeName
                                      _    -> addNewAddressScreen.data.addressSavedAs
                  }
              , props
                  { isSearchedLocationServiceable = true
                  , showSavePlaceView = true
                  , tagExists = false
                  , isLocateOnMap = false
                  , isBtnActive = isEditLocation
                  }
                } )
          _    ,  _     -> do
            pure $ setText (getNewIDWithTag "SavedLocationEditText") item.description
            _ <- pure $ hideKeyboardOnNavigation true
            modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
              addNewAddressScreen
                { props
                  { isSearchedLocationServiceable = false
                  , isLocateOnMap = false
                  , showSavePlaceView = false
                  }
                , data
                  { recentSearchs { predictionArray = state.data.recentSearchs.predictionArray }
                  , address = item.description
                  }
                } )
            addNewAddressScreenFlow ""
        updateDistanceInfo state (Just placeLatLong.lat) (Just placeLatLong.lon)
      else do
        let recentItem = (fromMaybe dummyLocationListItemState ( (filter (\ ( recent) -> (recent.placeId) == (item.placeId))(state.data.recentSearchs.predictionArray)) !! 0))
        modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
            addNewAddressScreen
              { data
                  { address = item.description
                  , selectedItem = item
                  , selectedTag = if state.props.editLocation then addNewAddressScreen.data.selectedTag
                                  else Nothing
                  , addressSavedAs = case state.props.editLocation of
                                      true -> if (toLower state.data.placeName /= "home" && toLower state.data.placeName /= "work") then state.data.addressSavedAs
                                                else state.data.placeName
                                      _    -> addNewAddressScreen.data.addressSavedAs
                  }
              , props
                  { isSearchedLocationServiceable = true
                  , showSavePlaceView = true
                  , tagExists = false
                  , isLocateOnMap = false
                  , isBtnActive = state.props.editLocation
                  }
                } )
        updateDistanceInfo state recentItem.lat recentItem.lon
    GO_TO_HOME_SCREEN_FLOW -> do
      _ <- lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreenMap")
      homeScreenFlow

  pure unit



referralScreenFlow :: FlowBT String Unit
referralScreenFlow = do
  config <- getAppConfig Constants.appConfig
  modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen { config = config })
  flow <- UI.referralScreen
  case flow of
    UPDATE_REFERRAL referralCode -> do
      let (UpdateProfileReq initialData) = Remote.mkUpdateProfileRequest FunctionCall
          requiredData = initialData{referralCode = (Just referralCode)}
      res <- lift $ lift $ Remote.updateProfile (UpdateProfileReq requiredData)
      case res of
        Right response -> do
          modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen { showThanks = true })
          setValueToLocalStore REFERRAL_STATUS "REFERRED_NOT_TAKEN_RIDE"
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props{isReferred = true} })
        Left err -> do
          if ((err.code == 500 && (decodeError err.response.errorMessage "errorCode") == "BPP_INTERNAL_API_ERROR")) then
            modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen { isInvalidCode = true })
          else do
            _ <- pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
            pure unit
      referralScreenFlow
    BACK_TO_HOME -> do
      modifyScreenState $ ReferralScreenStateType (\referralScreen -> ReferralScreen.initData)
      _ <- lift $ lift $ liftFlow $ adjustViewWithKeyboard "true"
      homeScreenFlow

drawDottedRoute :: HomeScreenState -> FlowBT String Unit
drawDottedRoute state = do
  _ <- pure $ removeAllPolylines ""
  let destMarker = if state.props.currentStage == RideAccepted then "src_marker" else "dest_marker"
      srcMarker = "ny_ic_auto_map"
      srcLat = state.data.driverInfoCardState.driverLat
      srcLng = state.data.driverInfoCardState.driverLng
      destLat = if state.props.currentStage == RideAccepted then state.data.driverInfoCardState.sourceLat else state.data.driverInfoCardState.destinationLat
      destLng = if state.props.currentStage == RideAccepted then state.data.driverInfoCardState.sourceLng else state.data.driverInfoCardState.destinationLng
  lift $ lift $ liftFlow $ drawRoute (Remote.walkCoordinate srcLat srcLng destLat destLng) "DOT" "#323643" false srcMarker destMarker 8 "DRIVER_LOCATION_UPDATE" "" "" (specialLocationConfig "" "")

isForLostAndFound :: Boolean
isForLostAndFound = true



checkAndUpdateSavedLocations :: HomeScreenState -> FlowBT String Unit
checkAndUpdateSavedLocations state = do
  if (getValueToLocalStore RELOAD_SAVED_LOCATION == "true") || (state.props.currentStage == HomeScreen)
    then do
      recentPredictionsObject <- lift $ lift $ getObjFromLocal state
      (savedLocationResp )<- lift $ lift $ Remote.getSavedLocationList ""
      case savedLocationResp of
        Right (SavedLocationsListRes listResp) -> do
          let savedLocationWithHomeOrWorkTag = (filter (\listItem ->  (listItem.prefixImageUrl == ("ny_ic_home_blue," <> (getAssetStoreLink FunctionCall) <> "ny_ic_home_blue.png") || (listItem.prefixImageUrl == ("ny_ic_work_blue," <> (getAssetStoreLink FunctionCall) <> "ny_ic_work_blue.png")))) (AddNewAddress.getSavedLocations listResp.list))
          let recent = (differenceOfLocationLists recentPredictionsObject.predictionArray savedLocationWithHomeOrWorkTag)
          let twoElements = catMaybes ([] <> [recent!!0] <> [recent!!1])
          _ <- pure $ setValueToLocalStore RELOAD_SAVED_LOCATION "false"
          modifyScreenState $
            HomeScreenStateType
              (\homeScreen ->
                homeScreen
                {
                  data
                  { recentSearchs
                    { predictionArray =
                        map
                          (\item -> item { postfixImageUrl =  if not (checkPrediction item (AddNewAddress.getSavedLocations listResp.list)) then "ny_ic_fav_red," <> (getAssetStoreLink FunctionCall) <> "ny_ic_fav_red.png" else "ny_ic_fav," <> (getAssetStoreLink FunctionCall) <> "ny_ic_fav.png" } ) twoElements},
                      savedLocations = (AddNewAddress.getSavedLocations listResp.list),
                      locationList =  map
                          (\item -> item { postfixImageUrl =  if not (checkPrediction item (AddNewAddress.getSavedLocations listResp.list)) then "ny_ic_fav_red," <> (getAssetStoreLink FunctionCall) <> "ny_ic_fav_red.png" else "ny_ic_fav," <> (getAssetStoreLink FunctionCall) <> "ny_ic_fav.png" } ) recent
                    }
                  }
                )
          pure unit
        Left (err) -> pure unit
      pure unit
    else pure unit

addLocationToRecents :: LocationListItemState -> HomeScreenState -> Boolean -> Boolean -> FlowBT String Unit
addLocationToRecents item state srcServiceable destServiceable = do
  let serviceable = if (state.props.isSource == Just false) then destServiceable else srcServiceable
  case item.locationItemType of
    Just PREDICTION -> do
        let lat = if (state.props.isSource == Just false) then state.props.destinationLat else state.props.sourceLat
        let lon = if (state.props.isSource == Just false) then state.props.destinationLong else state.props.sourceLong
        saveToRecents item lat lon serviceable
    _ -> saveToRecents item (fromMaybe 0.0 item.lat) (fromMaybe 0.0 item.lon) serviceable
  pure unit

saveToRecents :: LocationListItemState -> Number -> Number -> Boolean -> FlowBT String Unit
saveToRecents item lat lon serviceability = do
  (GlobalState currentState) <- getState
  recentPredictionsObject <- lift $ lift $ getObjFromLocal currentState.homeScreen
  if serviceability then do
    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{recentSearchs{predictionArray = addToRecentSearches item{lat = Just lat, lon = Just lon} recentPredictionsObject.predictionArray}, locationList = ((addToRecentSearches item{lat = Just lat, lon = Just lon} recentPredictionsObject.predictionArray) )}})
    (GlobalState modifiedState) <- getState
    _ <- pure $ saveObject "RECENT_SEARCHES" modifiedState.homeScreen.data.recentSearchs
    pure unit
    else pure unit
  pure unit


addLocToCurrLoc :: Number -> Number -> String -> FlowBT String Unit
addLocToCurrLoc lat lon name = do
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{previousCurrentLocations{ pastCurrentLocations = addToPrevCurrLoc {lat: lat, lon:lon, placeName : name} homeScreen.data.previousCurrentLocations.pastCurrentLocations}}})
  (GlobalState modifiedState) <- getState
  _ <- pure $ saveCurrentLocations "PREVIOUS_CURRENT_LOCATION" modifiedState.homeScreen.data.previousCurrentLocations
  pure unit

getDistanceInfo :: Array LocationListItemState -> String -> Number -> Number -> String -> FlowBT String {tagExists :: Boolean, locExistsAs :: String }
getDistanceInfo savedLocations excludeLocation lat lon placeId = do
  distArr <- pure $ ((AddNewAddress.calculateDistance savedLocations excludeLocation lat lon))
  rslt <- pure $ ((AddNewAddress.isValidLocation savedLocations excludeLocation placeId))
  let placeIdExists =(fromMaybe {locationName : "" , distanceDiff : 1.0} ((rslt)!!0))
      minDist = ((fromMaybe {locationName : "" , distanceDiff : 1.0} ((distArr)!!0)))
      locExistsAs = case placeIdExists.locationName /= "" , minDist.distanceDiff <= 0.020 of
                      true , _ -> placeIdExists.locationName
                      false    , true -> minDist.locationName
                      _ , _ -> ""
      tagExists = ((length rslt) > 0 || minDist.distanceDiff <= 0.020)
  pure $ { tagExists, locExistsAs }



updateDistanceInfo :: AddNewAddressScreenState ->Maybe Number ->Maybe Number -> FlowBT String Unit
updateDistanceInfo state lat lon = do

            distanceInfo <- getDistanceInfo state.data.savedLocations  (if state.props.editLocation then state.data.placeName else "") (fromMaybe 0.0 lat) (fromMaybe 0.0 lon) (fromMaybe "" state.data.selectedItem.placeId)
            modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen ->
              addNewAddressScreen
                { props
                  { tagExists = distanceInfo.tagExists
                  , isLocateOnMap = false
                  , showSavePlaceView = true
                  , isBtnActive = case state.data.activeIndex of
                                    Just 2 -> if state.data.addressSavedAs /= "" then true else false
                                    Just index -> true
                                    Nothing -> false
                  }
                , data
                  { selectedTag = state.data.selectedTag
                  , activeIndex = state.data.activeIndex
                  , existsAs = distanceInfo.locExistsAs
                  }
                } )
            addNewAddressScreenFlow ""

dummyLocationListItemState :: LocationListItemState
dummyLocationListItemState = dummyLocationListState{locationItemType = Just PREDICTION}

removeChatService :: String -> FlowBT String Unit
removeChatService _ = do
  _ <- lift $ lift $ liftFlow $ stopChatListenerService
  _ <- pure $ setValueToLocalNativeStore READ_MESSAGES "0"
  pure unit

setFlowStatusData :: Encode FlowStatusData => FlowStatusData -> Effect Unit
setFlowStatusData object = void $ pure $ setValueToLocalStore FLOW_STATUS_DATA (encodeJSON object)

getFlowStatusData :: String -> Maybe FlowStatusData
getFlowStatusData dummy =
  case runExcept (decodeJSON (getValueToLocalStore FLOW_STATUS_DATA) :: _ FlowStatusData) of
    Right res -> Just res
    Left err -> Nothing

updateFlowStatus :: NotifyFlowEventType -> FlowBT String Unit
updateFlowStatus eventType = do
  (FlowStatusRes flowStatus) <- Remote.flowStatusBT "LazyCheck"
  case flowStatus.currentStatus of
    RIDE_ASSIGNED _ -> do
      currentRideFlow true
      homeScreenFlow
    _               -> do
      res <- lift $ lift $ Remote.notifyFlowEvent (Remote.makeNotifyFlowEventReq (show eventType))
      case res of
        Right _  -> homeScreenFlow
        Left err -> do
          let errResp = err.response
              codeMessage = decodeError errResp.errorMessage "errorCode"
          when ( err.code == 400 && codeMessage == "ACTIVE_BOOKING_EXISTS") $ do
            void $ pure $ toast $ getString IT_SEEMS_LIKE_YOU_HAVE_AN_ONGOING_RIDE_
            currentRideFlow false

cancelEstimate :: String -> FlowBT String Unit
cancelEstimate bookingId = do
  logField_ <- lift $ lift $ getLogFields
  res <- lift $ lift $ Remote.cancelEstimate bookingId
  case res of
    Right res -> do
      -- TODO : to be removed after new bundle is 100% available (replace with pure unit)
      let (CancelEstimateRes resp) = res
      case resp.result of
        "Success" -> do
          if(getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") then do
            _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_cancel_waiting_for_driver_assign"
            pure unit
            else do
              _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_cancel_waiting_for_quotes"
              pure unit
        "BookingAlreadyCreated" -> do
          void $ pure $ toast $ getString IT_SEEMS_LIKE_YOU_HAVE_AN_ONGOING_RIDE_
          _ <- liftFlowBT $ logEvent logField_ "ny_fs_cancel_estimate_booking_exists_right"
          currentRideFlow true
          homeScreenFlow
        _ -> do
          void $ pure $ toast $ getString CANCELLATION_UNSUCCESSFULL_PLEASE_TRY_AGAIN
          _ <- liftFlowBT $ logEvent logField_ "ny_fs_cancel_estimate_failed_right"
          homeScreenFlow
    Left err -> do
      let errResp = err.response
          codeMessage = decodeError errResp.errorMessage "errorCode"
      if ( err.code == 400 && codeMessage == "ACTIVE_BOOKING_EXISTS") then do
        void $ pure $ toast $ getString IT_SEEMS_LIKE_YOU_HAVE_AN_ONGOING_RIDE_
        _ <- liftFlowBT $ logEvent logField_ "ny_fs_cancel_estimate_booking_exists_left"
        currentRideFlow true
        homeScreenFlow
      else do
        void $ pure $ toast $ getString CANCELLATION_UNSUCCESSFULL_PLEASE_TRY_AGAIN
        _ <- liftFlowBT $ logEvent logField_ "ny_fs_cancel_estimate_failed_left"
        homeScreenFlow

getGenderValue :: Maybe Gender.Gender -> Maybe String
getGenderValue gender =
  case gender of
    Just value -> case value of
      Gender.MALE -> Just "MALE"
      Gender.FEMALE -> Just "FEMALE"
      Gender.OTHER -> Just "OTHER"
      _ -> Just "PREFER_NOT_TO_SAY"
    Nothing -> Nothing

getPlaceName :: Number -> Number -> Location -> FlowBT String PlaceName
getPlaceName lat long location = do
  case location.address of
    Just address -> do
      let addressComponent = AddressComponents {
                                longName : location.place
                              , shortName : location.place
                              , types : ["sublocality"]
                            }
      pure (PlaceName {
                formattedAddress : address
              , location : LatLong { lat : location.lat, lon : location.lng }
              , plusCode : Nothing
              , addressComponents : [addressComponent]
            })
    Nothing -> do
      (GetPlaceNameResp locationName) <- Remote.placeNameBT (Remote.makePlaceNameReq lat long (case (getValueToLocalStore LANGUAGE_KEY) of
                                                                                                                                "HI_IN" -> "HINDI"
                                                                                                                                "KN_IN" -> "KANNADA"
                                                                                                                                "BN_IN" -> "BENGALI"
                                                                                                                                "ML_IN" -> "MALAYALAM"
                                                                                                                                _      -> "ENGLISH"))

      let (PlaceName address) = (fromMaybe HomeScreenData.dummyLocationName (locationName !! 0))
      pure (PlaceName address)


dummyLocationData :: LocationData
dummyLocationData = LocationData {
    lat : 0.0
  , lon : 0.0
  , name : Nothing
}

checkAndUpdateLocations :: FlowBT String Unit
checkAndUpdateLocations = do
  payload <- liftFlowBT $ getGlobalPayload unit
  _ <- pure $ spy "inside checkAndUpdateLocations" ""
  case payload of
    Just (GlobalPayload payload') -> do
      _ <- pure $ spy "inside right" payload'
      let (Payload innerPayload) = payload'.payload
      case isNothing innerPayload.search_type of
        true -> pure unit
        false -> do
          let searchType = fromMaybe "normal_search" $ innerPayload.search_type
          if searchType /= "normal_search" then do
            let (LocationData source) = fromMaybe dummyLocationData innerPayload.source
            let (LocationData destination) = fromMaybe dummyLocationData innerPayload.destination
            modifyScreenState $ HomeScreenStateType (\homescreen -> homescreen {
              data {
                source = (fromMaybe "" source.name)
              , destination = (fromMaybe "" destination.name)
              , sourceAddress = encodeAddress (fromMaybe "" source.name) [] Nothing
              , destinationAddress = encodeAddress (fromMaybe "" destination.name) [] Nothing
              }, props {
                  sourceLat = source.lat
                , sourceLong = source.lon
                , destinationLat = destination.lat
                , destinationLong = destination.lon
                , isSource = Just false
                , isSearchLocation = SearchLocation
              }
            })
          else pure unit
    Nothing ->do
      _ <- pure $ spy "inside left" "err"
      pure unit

rideCompletedDetails :: RideBookingRes -> Array ClevertapEventParams
rideCompletedDetails (RideBookingRes resp) = do
  let (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
      (RideBookingDetails contents) = bookingDetails.contents
      (RideAPIEntity ride) = fromMaybe dummyRideAPIEntity (resp.rideList !! 0)
      differenceOfDistance = fromMaybe 0 contents.estimatedDistance - (fromMaybe 0 ride.chargeableRideDistance)
      finalAmount =  getFinalAmount (RideBookingRes resp)
      timeVal = (convertUTCtoISC (fromMaybe ride.createdAt ride.rideStartTime) "HH:mm:ss")
      nightChargesVal = (withinTimeRange "22:00:00" "5:00:00" timeVal)

  [ {key : "Estimate ride distance", value : unsafeToForeign $ (show $ fromMaybe 0 contents.estimatedDistance/1000) <> " km"},
          {key : "Actual ride distance", value : unsafeToForeign $  (show $ (fromMaybe 0 ride.chargeableRideDistance)/1000) <> " km"},
          {key : "Difference between estimated and actual ride distance" , value : unsafeToForeign $  (show $ differenceOfDistance/1000) <> " km"},
          {key : "Total Estimated fare",value : unsafeToForeign $  "₹" <> (show $ resp.estimatedFare)},
          {key : "Total Actual fare",value : unsafeToForeign $  "₹" <> (show $ finalAmount)},
          {key : "Difference between estimated and actual fares",value : unsafeToForeign $  "₹" <> (show $ resp.estimatedFare - finalAmount)},
          {key : "Driver pickup charges",value : unsafeToForeign $  "₹ 10"},
          {key : "Night ride",value : unsafeToForeign $  show $ nightChargesVal}]
