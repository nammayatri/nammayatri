{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Flow where

import Accessor (_computedPrice, _contents, _formattedAddress, _id, _lat, _lon, _status, _toLocation, _signatureAuthData)
import Common.Types.App (GlobalPayload(..), SignatureAuthData(..), Payload(..), Version(..))
import Common.Types.App (LazyCheck(..))
import Common.Types.App (LazyCheck(..))
import Components.LocationListItem.Controller (dummyLocationListState)
import Components.SavedLocationCard.Controller (getCardType)
import Components.SettingSideBar.Controller as SettingSideBarController
import Control.Monad.Except (runExcept)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (lift)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (catMaybes, filter, length, null, snoc, (!!), any, sortBy, head, uncons)
import Data.Array as Arr
import Data.Either (Either(..))
import Data.Int as INT
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Number (fromString)
import Data.String (Pattern(..), drop, indexOf, split, toLower, trim, take)
import Debug (spy)
import Effect (Effect)
import Effect (Effect)
import Effect.Class (liftEffect)
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Engineering.Helpers.Commons (liftFlow, os, getNewIDWithTag, bundleVersion, getExpiryTime,stringToVersion, convertUTCtoISC, getCurrentUTC)
import Foreign.Class (class Encode)
import Foreign.Class (encode)
import Helpers.Utils (hideSplash, getDistanceBwCordinates, adjustViewWithKeyboard, decodeErrorCode, getObjFromLocal, differenceOfLocationLists, filterRecentSearches, setText', seperateByWhiteSpaces, getNewTrackingId, checkPrediction, getRecentSearches, addToRecentSearches, saveRecents, clearWaitingTimer, toString, parseFloat, getCurrentLocationsObjFromLocal, addToPrevCurrLoc, saveCurrentLocations, getCurrentDate, getPrediction, getCurrentLocationMarker, parseNewContacts, getMerchant, Merchant(..), drawPolygon,requestKeyboardShow, removeLabelFromMarker, sortPredctionByDistance, getMobileNumber, getAssetStoreLink, getCommonAssetStoreLink)
import JBridge (metaLogEvent, currentPosition, drawRoute, enableMyLocation, factoryResetApp, firebaseLogEvent, firebaseLogEventWithParams, firebaseLogEventWithTwoParams, getVersionCode, getVersionName, hideKeyboardOnNavigation, isCoordOnPath, isInternetAvailable, isLocationEnabled, isLocationPermissionEnabled, locateOnMap, openNavigation, reallocateMapFragment, removeAllPolylines, toast, toggleLoader, updateRoute, launchInAppRatingPopup, firebaseUserID, addMarker, generateSessionId, stopChatListenerService, updateRouteMarker)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog)
import MerchantConfig.DefaultConfig as DC
import MerchantConfig.Utils (Merchant(..), getAppConfig, getMerchant)
import MerchantConfig.Utils (getAppConfig)
import ModifyScreenState (modifyScreenState, updateRideDetails)
import Prelude (Unit, bind, discard, map, mod, negate, not, pure, show, unit, void, when, ($), (&&), (+), (-), (/), (/=), (<), (<=), (<>), (==), (>), (>=), (||), (<$>))
import Presto.Core.Types.Language.Flow (doAff, fork, setLogField, delay)
import Presto.Core.Types.Language.Flow (getLogFields)
import Resources.Constants (DecodeAddress(..), decodeAddress, encodeAddress, getKeyByLanguage, getValueByComponent, getWard, getSearchRadius)
import Screens.AccountSetUpScreen.ScreenData as AccountSetUpScreenData
import Screens.AddNewAddressScreen.Controller (encodeAddressDescription, getSavedLocations, getSavedTags, getLocationList, calculateDistance, getSavedTagsFromHome, validTag, isValidLocation, getLocTag) as AddNewAddress
import Screens.ChooseLanguageScreen.Controller (ScreenOutput(..))
import Screens.EnterMobileNumberScreen.Controller (ScreenOutput(..))
import Screens.Handlers as UI
import Screens.HelpAndSupportScreen.ScreenData as HelpAndSupportScreenData
import Screens.HomeScreen.Controller (flowWithoutOffers, getSearchExpiryTime, isTipEnabled, getSpecialTag)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.HomeScreen.Transformer (getLocationList, getDriverInfo, dummyRideAPIEntity, encodeAddressDescription, getPlaceNameResp, getUpdatedLocationList, transformContactList)
import Screens.InvoiceScreen.Controller (ScreenOutput(..)) as InvoiceScreenOutput
import Screens.MyProfileScreen.ScreenData as MyProfileScreenData
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails)
import Screens.ReferralScreen.ScreenData as ReferralScreen
import Screens.SavedLocationScreen.Controller (getSavedLocationForAddNewAddressScreen)
import Screens.SelectLanguageScreen.ScreenData as SelectLanguageScreenData
import Screens.Types (CardType(..), AddNewAddressScreenState(..), CurrentLocationDetails(..), CurrentLocationDetailsWithDistance(..), DeleteStatus(..), HomeScreenState, LocItemType(..), PopupType(..), SearchLocationModelType(..), Stage(..), LocationListItemState, LocationItemType(..), NewContacts, NotifyFlowEventType(..), FlowStatusData(..), EmailErrorType(..), ZoneType(..))
import Services.API (AddressGeometry(..), BookingLocationAPIEntity(..), CancelEstimateRes(..), ConfirmRes(..), ContactDetails(..), DeleteSavedLocationReq(..), FlowStatus(..), FlowStatusRes(..), GatesInfo(..), Geometry(..), GetDriverLocationResp(..), GetEmergContactsReq(..), GetEmergContactsResp(..), GetPlaceNameResp(..), GetProfileRes(..), LatLong(..), LocationS(..), LogOutReq(..), LogOutRes(..), PlaceName(..), ResendOTPResp(..), RideAPIEntity(..), RideBookingAPIDetails(..), RideBookingDetails(..), RideBookingListRes(..), RideBookingRes(..), Route(..), SavedLocationReq(..), SavedLocationsListRes(..), SearchLocationResp(..), SearchRes(..), ServiceabilityRes(..), SpecialLocation(..), TriggerOTPResp(..), UserSosRes(..), VerifyTokenResp(..), ServiceabilityResDestination(..), SelectEstimateRes(..),UpdateProfileReq(..), OnCallRes(..))
import Services.Backend as Remote
import Screens.Types (Gender(..)) as Gender
import Services.API (AuthType(..), AddressGeometry(..), BookingLocationAPIEntity(..), CancelEstimateRes(..), ConfirmRes(..), ContactDetails(..), DeleteSavedLocationReq(..), FlowStatus(..), FlowStatusRes(..), GatesInfo(..), Geometry(..), GetDriverLocationResp(..), GetEmergContactsReq(..), GetEmergContactsResp(..), GetPlaceNameResp(..), GetProfileRes(..), LatLong(..), LocationS(..), LogOutReq(..), LogOutRes(..), PlaceName(..), ResendOTPResp(..), RideAPIEntity(..), RideBookingAPIDetails(..), RideBookingDetails(..), RideBookingListRes(..), RideBookingRes(..), Route(..), SavedLocationReq(..), SavedLocationsListRes(..), SearchLocationResp(..), SearchRes(..), ServiceabilityRes(..), SpecialLocation(..), TriggerOTPResp(..), UserSosRes(..), VerifyTokenResp(..), ServiceabilityResDestination(..), TriggerSignatureOTPResp(..), User(..), OnCallRes(..))
import Services.Backend as Remote
import Storage (KeyStore(..), deleteValueFromLocalStore, getValueToLocalNativeStore, getValueToLocalStore, isLocalStageOn, setValueToLocalNativeStore, setValueToLocalStore, updateLocalStage)
import Types.App (ABOUT_US_SCREEN_OUTPUT(..), ACCOUNT_SET_UP_SCREEN_OUTPUT(..), ADD_NEW_ADDRESS_SCREEN_OUTPUT(..), GlobalState(..), CONTACT_US_SCREEN_OUTPUT(..), FlowBT, HELP_AND_SUPPORT_SCREEN_OUTPUT(..), HOME_SCREEN_OUTPUT(..), MY_PROFILE_SCREEN_OUTPUT(..), MY_RIDES_SCREEN_OUTPUT(..), PERMISSION_SCREEN_OUTPUT(..), REFERRAL_SCREEN_OUPUT(..), SAVED_LOCATION_SCREEN_OUTPUT(..), SELECT_LANGUAGE_SCREEN_OUTPUT(..), ScreenType(..), TRIP_DETAILS_SCREEN_OUTPUT(..), EMERGECY_CONTACTS_SCREEN_OUTPUT(..), WELCOME_SCREEN_OUTPUT(..))
import Effect (Effect)
import Control.Monad.Except (runExcept)
import Foreign.Class (class Encode)
import Foreign.Generic (decodeJSON, encodeJSON)
import Resources.Constants (getSearchRadius)
import Merchant.Utils as MU
import Screens.RideBookingFlow.HomeScreen.Config (specialLocationIcons, specialLocationConfig, updateRouteMarkerConfig)

baseAppFlow :: GlobalPayload -> Boolean-> FlowBT String Unit
baseAppFlow gPayload refreshFlow = do
  logField_ <- lift $ lift $ getLogFields
  _ <- pure $ printLog "Global Payload" gPayload
  (GlobalState state) <- getState
  let bundle = bundleVersion unit
      customerId = (getValueToLocalStore CUSTOMER_ID)
  versionCode <- lift $ lift $ liftFlow $ getVersionCode
  versionName <- lift $ lift $ liftFlow $ getVersionName
  -- checkVersion versionCode versionName
  setValueToLocalStore VERSION_NAME $ concatString $ Arr.take 3 $ split (Pattern ".") versionName
  setValueToLocalStore BUNDLE_VERSION bundle
  setValueToLocalNativeStore BUNDLE_VERSION bundle
  _ <- pure $ setValueToLocalStore TRACKING_DRIVER "False"
  _ <- pure $ setValueToLocalStore TRACKING_ENABLED "True"
  _ <- pure $ setValueToLocalStore RELOAD_SAVED_LOCATION "true"
  _ <- pure $ setValueToLocalStore TEST_MINIMUM_POLLING_COUNT "15"
  _ <- pure $ setValueToLocalStore TEST_POLLING_INTERVAL "1500.0"
  _ <- pure $ setValueToLocalStore TEST_POLLING_COUNT "113"
  _ <- pure $ setValueToLocalStore RATING_SKIPPED "false"
  _ <- pure $ setValueToLocalStore POINTS_FACTOR "3"
  _ <- pure $ setValueToLocalStore ACCURACY_THRESHOLD "23.0"
  when ((getValueToLocalStore SESSION_ID == "__failed") || (getValueToLocalStore SESSION_ID == "(null)")) $ do
    setValueToLocalStore SESSION_ID (generateSessionId unit)
  _ <- lift $ lift $ setLogField "customer_id" $ encode (customerId)
  _ <- lift $ lift $ setLogField "app_version" $ encode (show versionCode)
  _ <- lift $ lift $ setLogField "bundle_version" $ encode (bundle)
  _ <- lift $ lift $ setLogField "platform" $ encode (os)
  when (not refreshFlow) $ void $ UI.splashScreen state.splashScreen
  _ <- lift $ lift $ liftFlow $ logEventWithParams logField_ "ny_user_app_version" "version" versionName
  _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_entered_app"
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
              pure $ runFn3 emitJOSEvent "java" "onEvent" "event,signature_auth_failed"
              pure unit
        Nothing -> if (MU.showCarouselScreen FunctionCall) then welcomeScreenFlow else enterMobileNumberScreenFlow

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

getIosVersion :: Merchant -> IosVersion
getIosVersion merchant =
  case merchant of
    NAMMAYATRI -> { majorUpdateIndex : 1,
                    minorUpdateIndex : 2,
                    patchUpdateIndex : 4,
                    enableForceUpdateIOS : false
                  }
    YATRI -> { majorUpdateIndex : 1,
               minorUpdateIndex : 1,
               patchUpdateIndex : 0,
               enableForceUpdateIOS : true
              }
    JATRISAATHI -> { majorUpdateIndex : 0,
                     minorUpdateIndex : 1,
                     patchUpdateIndex : 0,
                     enableForceUpdateIOS : false
                    }
    PAYTM -> { majorUpdateIndex : 0,
                     minorUpdateIndex : 1,
                     patchUpdateIndex : 0,
                     enableForceUpdateIOS : false
                    }                  


checkVersion :: Int -> String -> FlowBT String Unit
checkVersion versioncodeAndroid versionName= do
  logField_ <- lift $ lift $ getLogFields
  let updatedIOSversion = getIosVersion (getMerchant FunctionCall)
  if os /= "IOS" && versioncodeAndroid < (getLatestAndroidVersion (getMerchant FunctionCall)) then do
    pure $ runFn3 emitJOSEvent "java" "onEvent" "event,event,hide_loader"
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
          pure $ runFn3 emitJOSEvent "java" "onEvent" "event,hide_loader"
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
    JATRISAATHI -> 2
    PAYTM -> 1

forceIOSupdate :: Int -> Int -> Int -> IosVersion -> Boolean
forceIOSupdate c_maj c_min c_patch updatedIOSversion=
  c_maj < updatedIOSversion.majorUpdateIndex ||
  c_min < updatedIOSversion.minorUpdateIndex ||
  c_patch < updatedIOSversion.patchUpdateIndex

currentRideFlow :: Boolean -> FlowBT String Unit
currentRideFlow rideAssigned = do
  logField_ <- lift $ lift $ getLogFields
  config <- getAppConfig
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
            newState = state{data{driverInfoCardState = getDriverInfo (RideBookingRes resp) state.props.isSpecialZone
                , finalAmount = fromMaybe 0 ((fromMaybe dummyRideAPIEntity (resp.rideList !!0) )^. _computedPrice)},
                  props{currentStage = rideStatus
                  , rideRequestFlow = true
                  , ratingModal = false
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
            if(lastRideDate /= currentDate) then
              setValueToLocalStore FLOW_WITHOUT_OFFERS "true"
              else pure unit
            when (isNothing currRideListItem.rideRating) $ do
              when (resp.status /= "CANCELLED" && length listResp.list > 0) $ do
                modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{
                    props { ratingModal= true
                          , estimatedDistance = contents.estimatedDistance
                          , zoneType = getSpecialTag resp.specialLocationTag }
                  , data { previousRideRatingState
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
                          , appConfig = config
                          }}
                })
            -- _ <- pure $ spy "CurrentRideListItem end" currRideListItem
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
  pure $ runFn3 emitJOSEvent "java" "onEvent" "event,hide_loader"
  _ <- pure $ hideKeyboardOnNavigation true
  homeScreenFlow
  where
    verifyProfile :: String -> FlowBT String Unit
    verifyProfile dummy = do
      (GetProfileRes response) <- Remote.getProfileBT ""
      let dbBundleVersion = response.bundleVersion
          dbClientVersion = response.clientVersion
      updateCustomerVersion dbClientVersion dbBundleVersion
      if isJust response.language then do
        when (getKeyByLanguage (fromMaybe "ENGLISH" response.language) /= (getValueToLocalNativeStore LANGUAGE_KEY)) $ do
          resp <- lift $ lift $ Remote.updateProfile (Remote.mkUpdateProfileRequest)
          pure unit
      else do
        resp <- lift $ lift $ Remote.updateProfile (Remote.mkUpdateProfileRequest)
        pure unit
      setValueToLocalStore REFERRAL_STATUS  $ if response.hasTakenRide then "HAS_TAKEN_RIDE" else if (response.referralCode /= Nothing && not response.hasTakenRide) then "REFERRED_NOT_TAKEN_RIDE" else "NOT_REFERRED_NOT_TAKEN_RIDE"
      setValueToLocalStore HAS_TAKEN_FIRST_RIDE if response.hasTakenRide then "true" else "false"
      if (((fromMaybe "" response.firstName) == "" ) && not (isJust response.firstName)) then do
        _ <- updateLocalStage HomeScreen
        pure $ runFn3 emitJOSEvent "java" "onEvent" "event,hide_loader"
        accountSetUpScreenFlow
      else do
          modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{data{settingSideBar{name =fromMaybe "" response.firstName}}})
          setValueToLocalStore USER_NAME ((fromMaybe "" response.firstName) <> " " <> (fromMaybe "" response.middleName) <> " " <> (fromMaybe "" response.lastName))
      if (fromMaybe "UNKNOWN" (response.gender) /= "UNKNOWN") then do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{gender = Just (fromMaybe "" response.gender)}} , props {isBanner = false}})
        else pure unit
      if isJust response.email then do
        setValueToLocalStore USER_EMAIL $ fromMaybe "" response.email
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{email = Just (fromMaybe "" response.email)}}})
        else pure unit

    goToFindingQuotesStage :: String -> Boolean -> FlowBT String Unit
    goToFindingQuotesStage estimateId driverOfferedQuote = do
      if any (_ == (getValueToLocalStore FINDING_QUOTES_START_TIME)) ["__failed", ""] then do
        updateFlowStatus SEARCH_CANCELLED
      else do
        let secondsPassed = spy "secondsPassed" (getExpiryTime (getValueToLocalStore FINDING_QUOTES_START_TIME) true)
        let searchExpiryTime = getSearchExpiryTime "LazyCheck"
        let secondsLeft = case driverOfferedQuote of
                            true  -> if (searchExpiryTime - secondsPassed) < 30 then (searchExpiryTime - secondsPassed) else 30
                            false -> (searchExpiryTime - secondsPassed)
        if secondsLeft > 0 then do
          _ <- pure $ setValueToLocalStore RATING_SKIPPED "true"
          updateLocalStage FindingQuotes
          setValueToLocalStore AUTO_SELECTING ""
          setValueToLocalStore FINDING_QUOTES_POLLING "false"
          _ <- pure $ setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
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
                     , selectedQuote = Nothing}
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
  pure $ runFn3 emitJOSEvent "java" "onEvent" "event,hide_loader"
  setValueToLocalStore LANGUAGE_KEY "EN_US"
  _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_choose_lang_scn_view"
  flow <- UI.chooseLanguageScreen
  case flow of
    NextScreen language -> do
                            setValueToLocalStore LANGUAGE_KEY language
                            _ <- lift $ lift $ liftFlow $(logEventWithParams logField_ "ny_user_lang_choose" "language" (language))
                            enterMobileNumberScreenFlow
    Refresh state -> chooseLanguageScreenFlow

updateCustomerVersion :: Maybe Version -> Maybe Version -> FlowBT String Unit
updateCustomerVersion dbClientVersion dbBundleVersion = do
  if (isJust dbClientVersion && isJust dbBundleVersion) then do
    let versionName = getValueToLocalStore VERSION_NAME
        bundle = getValueToLocalStore BUNDLE_VERSION
        Version clientVersion = stringToVersion versionName
        Version bundleVersion = stringToVersion bundle
        Version bundleVersion' = fromMaybe (Version bundleVersion) dbBundleVersion
        Version clientVersion' = fromMaybe (Version clientVersion) dbClientVersion
    if any (_ == -1) [clientVersion.minor, clientVersion.major, clientVersion.maintenance,bundleVersion.minor,bundleVersion.major,bundleVersion.maintenance] then pure unit
      else if ( bundleVersion' /= bundleVersion || clientVersion' /= clientVersion)  then do
      let (UpdateProfileReq initialData) = Remote.mkUpdateProfileRequest
          requiredData = initialData{clientVersion = Just (Version clientVersion), bundleVersion = Just (Version bundleVersion)}
      resp <- lift $ lift $ Remote.updateProfile (UpdateProfileReq requiredData)
      pure unit
    else pure unit
  else pure unit

enterMobileNumberScreenFlow :: FlowBT String Unit
enterMobileNumberScreenFlow = do
  pure $ runFn3 emitJOSEvent "java" "onEvent" "event,hide_loader" -- Removed initial choose langauge screen
  setValueToLocalStore LANGUAGE_KEY "EN_US"
  void $ lift $ lift $ toggleLoader false
  config <- getAppConfig
  modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen {data {config =  config }})
  logField_ <- lift $ lift $ getLogFields
  _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_enter_mob_num_scn_view"
  flow <- UI.enterMobileNumberScreen
  case flow of
    GoToAccountSetUp state -> do
            void $ lift $ lift $ loaderText (getString VERIFYING_OTP) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)  -- TODO : Handlde Loader in IOS Side
            void $ lift $ lift $ toggleLoader true
            (resp) <- lift $ lift $  Remote.verifyToken (Remote.makeVerifyOTPReq state.data.otp) state.data.tokenId
            case resp of
              Right resp -> do
                    _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_verify_otp"
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen {props {enterOTP = false}})
                    let (VerifyTokenResp response) = resp
                        customerId = ((response.person)^. _id)
                    _ <- lift $ lift $ setLogField "customer_id" $ encode (customerId)
                    setValueToLocalStore CUSTOMER_ID customerId
                    setValueToLocalStore REGISTERATION_TOKEN response.token
                    currentFlowStatus
              Left err -> do
                _ <- lift $ lift $ liftFlow (setText' (getNewIDWithTag "EnterOTPNumberEditText") "" )
                let errResp = err.response
                    codeMessage = decodeErrorCode errResp.errorMessage
                if ( err.code == 400 && codeMessage == "TOKEN_EXPIRED") then do
                    _ <- pure $ toast (getString REQUEST_TIMED_OUT)
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> enterMobileNumber{data{otp=""}, props{enterOTP = false, wrongOTP = false}})
                else if ( err.code == 400 && codeMessage == "INVALID_AUTH_DATA") then do
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> enterMobileNumber{props{wrongOTP = true}, data{otp=""}})
                    pure $ toast (getString WRONG_OTP)
                else if ( err.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then do
                    pure $ toast (getString LIMIT_EXCEEDED)
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen {props {enterOTP = false, wrongOTP = false}, data{otp=""}})
                else do
                    pure $ toast (getString ERROR_OCCURED)
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen {props {enterOTP = false,wrongOTP = false}, data{otp=""}})
                enterMobileNumberScreenFlow
    GoToOTP state -> do
            setValueToLocalStore MOBILE_NUMBER (state.data.mobileNumber)
            (TriggerOTPResp triggerOtpResp) <- Remote.triggerOTPBT (Remote.makeTriggerOTPReq state.data.mobileNumber)
            modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen { data { tokenId = triggerOtpResp.authId, attempts = triggerOtpResp.attempts}, props {enterOTP = true,resendEnable = true}})
            modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{data{settingSideBar{number = state.data.mobileNumber}}})
            enterMobileNumberScreenFlow
    ResendOTP state -> do
            (ResendOTPResp resendResp) <- Remote.resendOTPBT state.data.tokenId
            modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen { data { tokenId = resendResp.authId, attempts = resendResp.attempts}})
            enterMobileNumberScreenFlow
    GoBack state  ->  enterMobileNumberScreenFlow
    GoToWelcomeScreen state -> welcomeScreenFlow

welcomeScreenFlow :: FlowBT String Unit
welcomeScreenFlow = do
  lift $ lift $ doAff do liftEffect hideSplash
  flow <- UI.welcomeScreen
  case flow of
    GoToMobileNumberScreen -> enterMobileNumberScreenFlow

accountSetUpScreenFlow :: FlowBT String Unit
accountSetUpScreenFlow = do
  logField_ <- lift $ lift $ getLogFields
  config <- getAppConfig
  modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> accountSetUpScreen{data{config = config}})
  flow <- UI.accountSetUpScreen
  case flow of
    GO_HOME state -> do
      void $ lift $ lift $ toggleLoader false
      let gender = getGenderValue state.data.gender
      let (UpdateProfileReq initialData) = Remote.mkUpdateProfileRequest
          requiredData = initialData{firstName = (Just state.data.name),gender = gender}
      resp <- lift $ lift $ Remote.updateProfile (UpdateProfileReq requiredData)
      case resp of
        Right response -> do
          setValueToLocalStore USER_NAME state.data.name
          case gender of
            Just value -> modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{gender = Just value}}, props{isBanner = false}})
            Nothing    -> pure unit
          _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_onboarded"
          _ <- pure $ metaLogEvent "ny_user_onboarded"
          pure unit
        Left err -> do
          _ <- pure $ toast (getString ERROR_OCCURED)
          modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> state{props{btnActive = true},data{name=state.data.name}})
          accountSetUpScreenFlow
    GO_BACK -> do
      _ <- pure $ deleteValueFromLocalStore REGISTERATION_TOKEN
      _ <- pure $ deleteValueFromLocalStore MOBILE_NUMBER
      modifyScreenState $ AccountSetUpScreenStateType (\accountSetUpScreen -> AccountSetUpScreenData.initData)
      modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen -> enterMobileNumberScreen{data{ otp = ""}})
      enterMobileNumberScreenFlow

homeScreenFlow :: FlowBT String Unit
homeScreenFlow = do
  logField_ <- lift $ lift $ getLogFields
  (GlobalState currentState) <- getState
  _ <- checkAndUpdateSavedLocations currentState.homeScreen
  -- TODO: REQUIRED ONCE WE NEED TO STORE RECENT CURRENTLOCATIONS
  -- resp <- lift $ lift $ getCurrentLocationsObjFromLocal currentState.homeScreen
  -- modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{previousCurrentLocations = resp}})

  -- TODO: HANDLE LOCATION LIST INITIALLY
  _ <- pure $ firebaseUserID (getValueToLocalStore CUSTOMER_ID)
  void $ lift $ lift $ toggleLoader false
  config <- getAppConfig
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{hasTakenRide = if (getValueToLocalStore REFERRAL_STATUS == "HAS_TAKEN_RIDE") then true else false, isReferred = if (getValueToLocalStore REFERRAL_STATUS == "REFERRED_NOT_TAKEN_RIDE") then true else false }, data {config = config}})
  flow <- UI.homeScreen
  case flow of
    CHECK_FLOW_STATUS -> currentFlowStatus
    ON_RESUME_APP -> currentFlowStatus
    GO_TO_MY_RIDES -> do
      modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> myRidesScreen{data{offsetValue = 0}})
      _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_myrides_click"
      myRidesScreenFlow true
    GO_TO_HELP -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> HelpAndSupportScreenData.initData)
      _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_help"
      helpAndSupportScreenFlow
    CHANGE_LANGUAGE ->  selectLanguageScreenFlow
    GO_TO_EMERGENCY_CONTACTS -> emergencyScreenFlow
    GO_TO_ABOUT -> aboutUsScreenFlow
    GO_TO_MY_PROFILE  updateProfile -> do
        _ <- lift $ lift $ liftFlow $ logEvent logField_ (if updateProfile then "safety_banner_clicked" else "ny_user_profile_click")
        modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState ->  MyProfileScreenData.initData{props{updateProfile = updateProfile , fromHomeScreen = updateProfile , isBtnEnabled = not updateProfile , genderOptionExpanded = false , showOptions = false, expandEnabled = true }})
        myProfileScreenFlow
    GO_TO_FIND_ESTIMATES state-> do
      PlaceName address <- getPlaceName state.props.sourceLat state.props.sourceLong
      liftFlowBT $  logEventWithTwoParams logField_ "ny_user_source_and_destination" "ny_user_enter_source" (take 99 (state.data.source)) "ny_user_enter_destination" (take 99 (state.data.destination))
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq state.props.sourceLat state.props.sourceLong)
      if (not sourceServiceabilityResp.serviceable) then do
        updateLocalStage SearchLocationModel
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = SearchLocationModel ,rideRequestFlow = false, isSearchLocation = SearchLocation, isSrcServiceable = false, isSource = Just true, isRideServiceable = false}})
        homeScreenFlow
        else pure unit
      (SearchRes rideSearchRes) <- Remote.rideSearchBT (Remote.makeRideSearchReq state.props.sourceLat state.props.sourceLong state.props.destinationLat state.props.destinationLong (encodeAddress address.formattedAddress [] state.props.sourcePlaceId) state.data.destinationAddress)
      if (os == "IOS") then do
        routeResponse <- Remote.drawMapRoute state.props.sourceLat state.props.sourceLong state.props.destinationLat state.props.destinationLong (Remote.normalRoute "") "NORMAL" address.formattedAddress state.data.destination rideSearchRes.routeInfo "pickup" (specialLocationConfig "" "")
        pure unit
      else do
        routeResponse <- Remote.drawMapRoute state.props.sourceLat state.props.sourceLong state.props.destinationLat state.props.destinationLong (Remote.normalRoute "") "NORMAL" "" "" rideSearchRes.routeInfo "pickup" (specialLocationConfig "" "")
        pure unit
      case rideSearchRes.routeInfo of
        Just (Route response) -> do
          let distance = if response.distance < 1000 then toString(response.distance)  <> " m" else parseFloat(INT.toNumber(response.distance) / 1000.0) 2 <> " km"
              duration = (show (response.duration / 60)) <> " min"
              tipEnabled = isTipEnabled response.distance
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{rideDistance = distance, rideDuration = duration, source = address.formattedAddress, sourceAddress = encodeAddress address.formattedAddress [] state.props.sourcePlaceId}, props{customerTip{enableTips = tipEnabled}}})
          _ <- setValueToLocalStore ENABLE_TIPS $ show tipEnabled
          if ((getMerchant FunctionCall) /= YATRI && response.distance >= 50000) then do
            updateLocalStage DistanceOutsideLimits
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = DistanceOutsideLimits ,rideRequestFlow = true, isSearchLocation = SearchLocation}})
            homeScreenFlow
            else if response.distance < 500 && getValueToLocalStore LOCAL_STAGE /= "ShortDistance" then do
              updateLocalStage ShortDistance
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = ShortDistance ,rideRequestFlow = true, isSearchLocation = SearchLocation, distance = response.distance}})
              homeScreenFlow
              else pure unit
          pure unit
        Nothing -> pure unit
      void $ pure $ setFlowStatusData (FlowStatusData { source : {lat : state.props.sourceLat, lng : state.props.sourceLong, place : address.formattedAddress}
                                                      , destination : {lat : state.props.destinationLat, lng : state.props.destinationLong, place : state.data.destination}
                                                      , sourceAddress : (encodeAddress address.formattedAddress [] state.props.sourcePlaceId)
                                                      , destinationAddress : state.data.destinationAddress })
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{searchId = rideSearchRes.searchId,currentStage = FindingEstimate, rideRequestFlow = true, isSearchLocation = SearchLocation, sourcePlaceId = Nothing, destinationPlaceId = Nothing}})
      updateLocalStage FindingEstimate
      homeScreenFlow
    RETRY_FINDING_QUOTES -> do
      void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)  -- TODO : Handlde Loader in IOS Side
      void $ lift $ lift $ toggleLoader true
      (GlobalState newState) <- getState
      let state = newState.homeScreen
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
      void $ pure $ setValueToLocalStore FINDING_QUOTES_START_TIME (getCurrentUTC "LazyCheck")

      response <- lift $ lift $ Remote.selectEstimate (Remote.makeEstimateSelectReq (flowWithoutOffers WithoutOffers) (if state.props.customerTip.enableTips && state.props.customerTip.isTipSelected then Just state.props.customerTip.tipForDriver else Nothing)) (state.props.estimateId)
      case response of
        Right res -> do
          updateLocalStage FindingQuotes
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props { currentStage = FindingQuotes, searchExpire = (getSearchExpiryTime "LazyCheck") } })
        Left err -> do
          void $ pure $ firebaseLogEvent "ny_user_estimate_expired"
          updateLocalStage FindEstimateAndSearch
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{ props { currentStage = FindEstimateAndSearch } })
      homeScreenFlow
    LOCATION_SELECTED item addToRecents -> do
        void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)  -- TODO : Handlde Loader in IOS Side
        void $ lift $ lift $ toggleLoader true
        (GlobalState newState) <- getState
        let state = newState.homeScreen
        (GetPlaceNameResp sourceDetailResp) <- getPlaceNameResp (state.props.sourcePlaceId) (state.props.sourceLat) (state.props.sourceLong) (if state.props.isSource == Just false then dummyLocationListItemState else item)

        let (PlaceName sourceDetailResponse) = (fromMaybe HomeScreenData.dummyLocationName (sourceDetailResp !! 0))
            (LatLong sourceLocation) = sourceDetailResponse.location
            sourceChangedState = state {props{sourceLat = sourceLocation.lat, sourceLong = sourceLocation.lon}}
        (GetPlaceNameResp destinationDetailResp) <- getPlaceNameResp (state.props.destinationPlaceId) (state.props.destinationLat) (state.props.destinationLong) (if state.props.isSource == Just true then dummyLocationListItemState else item)

        let (PlaceName destinationDetailResponse) = (fromMaybe HomeScreenData.dummyLocationName (destinationDetailResp!!0))
            (LatLong destinationLocation) = (destinationDetailResponse.location)
            bothLocationChangedState = sourceChangedState {props{destinationLat = destinationLocation.lat, destinationLong = destinationLocation.lon}}

        _ <- pure $ spy "destination prediction clicked state ---->>> " bothLocationChangedState
        (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq bothLocationChangedState.props.sourceLat bothLocationChangedState.props.sourceLong)
        let srcServiceable = sourceServiceabilityResp.serviceable
        let (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
        let pickUpPoints = map (\(GatesInfo item) -> {
                                                place: item.name,
                                                lat  : (item.point)^._lat,
                                                lng : (item.point)^._lon
                                              }) srcSpecialLocation.gates
        (ServiceabilityResDestination destServiceabilityResp) <- Remote.destServiceabilityBT (Remote.makeServiceabilityReqForDest bothLocationChangedState.props.destinationLat bothLocationChangedState.props.destinationLong)
        let destServiceable = destServiceabilityResp.serviceable
        modifyScreenState $ HomeScreenStateType (\homeScreen -> bothLocationChangedState{data{polygonCoordinates = fromMaybe "" sourceServiceabilityResp.geoJson,nearByPickUpPoints=pickUpPoints},props{isSpecialZone =  (sourceServiceabilityResp.geoJson) /= Nothing && (getMerchant FunctionCall) == JATRISAATHI, defaultPickUpPoint = (fromMaybe HomeScreenData.dummyLocation (state.data.nearByPickUpPoints!!0)).place}})
        if (addToRecents) then
          addLocationToRecents item bothLocationChangedState sourceServiceabilityResp.serviceable destServiceabilityResp.serviceable
          else pure unit
        (GlobalState globalState) <- getState
        let updateScreenState = globalState.homeScreen
        if (not srcServiceable && (updateScreenState.props.sourceLat /= -0.1 && updateScreenState.props.sourceLong /= -0.1)) then do
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
          _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_request_quotes"
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
        lift $ lift $ liftFlow $ updateRouteMarker $ updateRouteMarkerConfig (Remote.walkCoordinate state.props.sourceLat state.props.sourceLong state.props.destinationLat state.props.destinationLong) state.data.source state.data.destination srcMarker destMarker (specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon)
        homeScreenFlow
    GET_SELECT_LIST state -> do
      when (isLocalStageOn QuoteList) $ do
        updateFlowStatus SEARCH_CANCELLED
      homeScreenFlow
    CONFIRM_RIDE state -> do
          _ <- pure $ enableMyLocation false
          let selectedQuote = if state.props.isSpecialZone then state.data.specialZoneSelectedQuote else state.props.selectedQuote
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
                if not (err.code == 400 && (decodeErrorCode err.response.errorMessage) == "QUOTE_EXPIRED") then pure $ toast (getString ERROR_OCCURED_TRY_AGAIN) else pure unit
                _ <- setValueToLocalStore AUTO_SELECTING "false"
                _ <- pure $ updateLocalStage QuoteList
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = QuoteList,selectedQuote = Nothing, expiredQuotes = snoc state.props.expiredQuotes (fromMaybe "" state.props.selectedQuote)}, data {quoteListModelState = []}})
                homeScreenFlow
            else homeScreenFlow
    ONGOING_RIDE state -> do
      _ <- pure $ setValueToLocalStore TRACKING_ENABLED "True"
      _ <- pure $ setValueToLocalStore TRACKING_DRIVER "False"
      _ <- pure $ setValueToLocalStore DRIVER_ARRIVAL_ACTION "TRIGGER_DRIVER_ARRIVAL"
      let rideID = state.data.driverInfoCardState.rideId
          srcLat = state.data.driverInfoCardState.sourceLat
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
            modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData{data{settingSideBar{gender = state.data.settingSideBar.gender , email = state.data.settingSideBar.email}},props { isBanner = state.props.isBanner}})
            homeScreenFlow
          else homeScreenFlow
    CANCEL_RIDE_REQUEST state -> do
      _ <- pure $ currentPosition ""
      _ <- updateLocalStage HomeScreen
      _ <- Remote.cancelRideBT (Remote.makeCancelRequest state) (state.props.bookingId)
      _ <- pure $ clearWaitingTimer <$> state.props.waitingTimeTimerIds
      liftFlowBT $ logEvent logField_ "ny_user_ride_cancelled_by_user" 
      liftFlowBT $ logEvent logField_ $ "ny_user_cancellation_reason: " <> state.props.cancelReasonCode
      modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData{data{settingSideBar{gender = state.data.settingSideBar.gender , email = state.data.settingSideBar.email}},props { isbanner = state.props.isbanner}})
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
                                      _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_ride_started"
                                      let shareAppCount = getValueToLocalStore SHARE_APP_COUNT
                                      if shareAppCount == "__failed" then do
                                        setValueToLocalStore SHARE_APP_COUNT "1"
                                      else if shareAppCount /= "-1" then do
                                        setValueToLocalStore SHARE_APP_COUNT (show ((INT.round $ (fromMaybe 0.0 (fromString (shareAppCount))))+1))
                                      else pure unit
                                      _ <- pure $ clearWaitingTimer <$> state.props.waitingTimeTimerIds
                                      let newState = state{data{route = Nothing},props{waitingTimeTimerIds = [], currentStage = RideStarted, forFirst = true , showShareAppPopUp = (INT.round $ (fromMaybe 0.0 (fromString (getValueToLocalStore SHARE_APP_COUNT)))) `mod` 4 == 0 }}
                                      _ <- updateLocalStage RideStarted
                                      modifyScreenState $ HomeScreenStateType (\homeScreen -> newState)
                                      when state.props.isSpecialZone $ currentRideFlow true
                                      homeScreenFlow
            "TRIP_FINISHED"       -> do -- TRIP FINISHED
                                      if (getValueToLocalStore HAS_TAKEN_FIRST_RIDE == "false") then do
                                        _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_first_ride_completed"
                                        _ <- pure $ metaLogEvent "ny_user_first_ride_completed"
                                        (GetProfileRes response) <- Remote.getProfileBT ""
                                        setValueToLocalStore HAS_TAKEN_FIRST_RIDE ( show response.hasTakenRide)
                                        else pure unit
                                      let sourceSpecialTagIcon = specialLocationIcons state.props.zoneType.sourceTag
                                          destSpecialTagIcon = specialLocationIcons state.props.zoneType.destinationTag
                                      _ <- lift $ lift $ liftFlow $ logEvent logField_  "ny_user_ride_completed"
                                      _ <- pure $ metaLogEvent "ny_user_ride_completed"
                                      _ <- Remote.drawMapRoute srcLat srcLon dstLat dstLon (Remote.normalRoute "") "NORMAL" "" "" Nothing "pickup" (specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon)
                                      _ <- updateLocalStage HomeScreen
                                      if (state.props.bookingId /= "") then do
                                        (RideBookingRes resp) <- Remote.rideBookingBT (state.props.bookingId)
                                        let (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
                                        let (RideBookingDetails contents) = bookingDetails.contents
                                        let (RideAPIEntity ride) = fromMaybe dummyRideAPIEntity (resp.rideList !! 0)
                                        let finalAmount =  INT.round $ fromMaybe 0.0 (fromString (getFinalAmount (RideBookingRes resp)))
                                        let differenceOfDistance = fromMaybe 0 contents.estimatedDistance - (fromMaybe 0 ride.chargeableRideDistance)
                                        setValueToLocalStore PICKUP_DISTANCE "0"
                                        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{startedAt = convertUTCtoISC (fromMaybe "" resp.rideStartTime ) "h:mm A", startedAtUTC = fromMaybe "" resp.rideStartTime ,endedAt = convertUTCtoISC (fromMaybe "" resp.rideEndTime ) "h:mm A", finalAmount = finalAmount, previousRideRatingState {distanceDifference = differenceOfDistance}, driverInfoCardState {initDistance = Nothing}},props {currentStage = RideCompleted, estimatedDistance = contents.estimatedDistance}})
                                        homeScreenFlow
                                        else homeScreenFlow
            "CANCELLED_PRODUCT"   -> do -- REMOVE POLYLINES
                                      _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_ride_cancelled"
                                      _ <- pure $ removeAllPolylines ""
                                      _ <- updateLocalStage HomeScreen
                                      removeChatService ""
                                      setValueToLocalStore PICKUP_DISTANCE "0"
                                      modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData{data{settingSideBar{gender = state.data.settingSideBar.gender , email = state.data.settingSideBar.email}, driverInfoCardState{initDistance = Nothing}},props { isBanner = state.props.isBanner}})
                                      _ <- pure $ clearWaitingTimer <$> state.props.waitingTimeTimerIds
                                      homeScreenFlow
            "DRIVER_ASSIGNMENT"   -> if (not (isLocalStageOn RideAccepted || isLocalStageOn RideStarted )) then do
                                        _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_fs_driver_assignment"                                
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
      _ <- pure $ deleteValueFromLocalStore LANGUAGE_KEY
      _ <- pure $ deleteValueFromLocalStore CONTACTS
      _ <- pure $ deleteValueFromLocalStore USER_EMAIL
      _ <- pure $ factoryResetApp ""
      _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_user_logout"
      modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData)
      enterMobileNumberScreenFlow -- Removed choose langauge screen
    SUBMIT_RATING state -> do
      _ <- Remote.rideFeedbackBT (Remote.makeFeedBackReq (state.data.previousRideRatingState.rating) (state.data.previousRideRatingState.rideId) (state.data.previousRideRatingState.feedback))
      _ <- updateLocalStage HomeScreen
      modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData{data{settingSideBar{gender = state.data.settingSideBar.gender , email = state.data.settingSideBar.email}},props { isBanner = state.props.isBanner}})
      if state.data.previousRideRatingState.rating == 5 then do
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
            if (sourceServiceabilityResp.serviceable ) then do
              if (isJust sourceServiceabilityResp.geoJson && (getMerchant FunctionCall) == JATRISAATHI ) then do
                let (SpecialLocation specialLocation) = (fromMaybe (HomeScreenData.specialLocation) sourceServiceabilityResp.specialLocation)
                lift $ lift $ doAff do liftEffect $ drawPolygon (fromMaybe "" sourceServiceabilityResp.geoJson) (specialLocation.locationName)
                else lift $ lift $ doAff do liftEffect $ removeLabelFromMarker unit
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{ isSrcServiceable = true, showlocUnserviceablePopUp = false}})
              if (saveToCurrLocs && state.homeScreen.props.storeCurrentLocs) then addLocToCurrLoc state.homeScreen.props.sourceLat state.homeScreen.props.sourceLong state.homeScreen.data.source else pure unit
              pure unit
              else do
                _ <- pure $ firebaseLogEvent "ny_loc_unserviceable"
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{ isSrcServiceable = false, showlocUnserviceablePopUp = true}})
                pure unit
            else do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{ isSrcServiceable = true, showlocUnserviceablePopUp = false}})
              pure unit

      homeScreenFlow
    RETRY  -> homeScreenFlow
    CHECK_SERVICEABILITY updatedState lat long-> do
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq lat long)
      let (SpecialLocation specialLocation) = (fromMaybe (HomeScreenData.specialLocation) sourceServiceabilityResp.specialLocation)
      if (sourceServiceabilityResp.serviceable && isJust sourceServiceabilityResp.geoJson && (getMerchant FunctionCall) == JATRISAATHI) then
        lift $ lift $ doAff do liftEffect $ drawPolygon (fromMaybe "" sourceServiceabilityResp.geoJson) (specialLocation.locationName)
        else lift $ lift $ doAff do liftEffect $ removeLabelFromMarker unit
      let sourceLat = if sourceServiceabilityResp.serviceable then lat else updatedState.props.sourceLat
          sourceLong = if sourceServiceabilityResp.serviceable then long else updatedState.props.sourceLong
      _ <- pure $ firebaseLogEvent $ "ny_loc_unserviceable_" <> show (not sourceServiceabilityResp.serviceable)
      modifyScreenState $ HomeScreenStateType (\homeScreen -> updatedState{props{sourceLat = sourceLat, sourceLong = sourceLong, isSrcServiceable =sourceServiceabilityResp.serviceable , showlocUnserviceablePopUp = (not sourceServiceabilityResp.serviceable)}})
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
      PlaceName placeDetails <- getPlaceName lat lon
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
          sourceLong = if state.props.isSource == Just true then lon else state.props.sourceLong
          }
        })
      let _ = spy "UPDATE_LOCATION_NAME" "UPDATE_LOCATION_NAME"
      homeScreenFlow
    UPDATE_PICKUP_NAME state lat lon -> do
      PlaceName address <- getPlaceName lat lon
      _ <- liftFlowBT $ logEvent logField_ "ny_user_placename_api_cpu_onDrag"
      modifyScreenState $ HomeScreenStateType (\homeScreen ->
      homeScreen {
        data {
          source = address.formattedAddress ,
          sourceAddress = encodeAddress address.formattedAddress address.addressComponents Nothing },
        props {
          sourceLat = lat ,
          sourceLong = lon
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
      _ <- lift $ lift $ fork $ liftFlow $ openNavigation sourceLat sourceLng destLat destLng
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
      let prevRideState = updatedState.data.previousRideRatingState
      let finalAmount = show prevRideState.finalAmount
      modifyScreenState $ InvoiceScreenStateType (\invoiceScreen -> invoiceScreen {props{fromHomeScreen= true},data{totalAmount = ("₹ " <> finalAmount), date = prevRideState.dateDDMMYY, tripCharges = ("₹ " <> finalAmount), selectedItem {date = prevRideState.dateDDMMYY, bookingId = prevRideState.bookingId,rideStartTime = prevRideState.rideStartTime, rideEndTime = prevRideState.rideEndTime, rideId = prevRideState.rideId, shortRideId = prevRideState.shortRideId,vehicleNumber = prevRideState.vehicleNumber,time = prevRideState.rideStartTime,source = prevRideState.source,destination = prevRideState.destination,driverName = prevRideState.driverName,totalAmount = ("₹ " <> finalAmount)}, config = updatedState.data.config}})
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
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state{props{emergencyHelpModelState{sosId = res.sosId, showCallSuccessfulPopUp = true}}})
        homeScreenFlow
    GO_TO_CALL_POLICE state -> do
        (UserSosRes res) <- Remote.userSosBT (Remote.makeUserSosReq (Remote.createUserSosFlow "Police" "") state.data.driverInfoCardState.rideId)
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state{props{emergencyHelpModelState{sosId = res.sosId, showCallSuccessfulPopUp = true}}})
        homeScreenFlow
    GO_TO_CALL_SUPPORT state -> do
        (UserSosRes res) <- Remote.userSosBT (Remote.makeUserSosReq (Remote.createUserSosFlow "CustomerCare" "") state.data.driverInfoCardState.rideId)
        modifyScreenState $ HomeScreenStateType (\homeScreen -> state{props{emergencyHelpModelState{sosId = res.sosId, showCallSuccessfulPopUp = true}}})
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
      case ((not finalState.props.isSpecialZone) && finalState.props.sourceSelectedOnMap) of
        false -> do
          _ <- liftFlowBT $ runEffectFn5 locateOnMap false finalState.props.sourceLat finalState.props.sourceLong (if (getMerchant FunctionCall == JATRISAATHI) then finalState.data.polygonCoordinates else "")  (if (getMerchant FunctionCall == JATRISAATHI) then finalState.data.nearByPickUpPoints else [])
          pure $ removeAllPolylines ""
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = ConfirmingLocation,rideRequestFlow = true}})
          _ <- pure $ updateLocalStage ConfirmingLocation
          void $ lift $ lift $ toggleLoader false
        true -> do
          PlaceName address <- getPlaceName finalState.props.sourceLat finalState.props.sourceLong
          (SearchRes rideSearchRes) <- Remote.rideSearchBT (Remote.makeRideSearchReq finalState.props.sourceLat finalState.props.sourceLong finalState.props.destinationLat finalState.props.destinationLong (encodeAddress address.formattedAddress [] finalState.props.sourcePlaceId) finalState.data.destinationAddress)
          void $ pure $ setFlowStatusData (FlowStatusData { source : {lat : finalState.props.sourceLat, lng : finalState.props.sourceLong, place : address.formattedAddress}
                                                          , destination : {lat : finalState.props.destinationLat, lng : finalState.props.destinationLong, place : finalState.data.destination}
                                                          , sourceAddress : (encodeAddress address.formattedAddress [] finalState.props.sourcePlaceId)
                                                          , destinationAddress : finalState.data.destinationAddress })
          case finalState.props.currentStage of
            TryAgain -> do
              when (finalState.props.customerTip.enableTips) $ do
                cancelEstimate finalState.props.estimateId
              _ <- pure $ updateLocalStage TryAgain
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{searchId = rideSearchRes.searchId, currentStage = TryAgain, rideRequestFlow = true}})
            _        -> do
              let sourceSpecialTagIcon = specialLocationIcons finalState.props.zoneType.sourceTag
                  destSpecialTagIcon = specialLocationIcons finalState.props.zoneType.destinationTag
              routeResponse <- Remote.drawMapRoute finalState.props.sourceLat finalState.props.sourceLong finalState.props.destinationLat finalState.props.destinationLong (Remote.normalRoute "") "NORMAL" address.formattedAddress finalState.data.destination rideSearchRes.routeInfo "pickup" (specialLocationConfig sourceSpecialTagIcon destSpecialTagIcon)
              case rideSearchRes.routeInfo of
                Just (Route response) -> do
                  let distance = if response.distance < 1000 then toString(response.distance)  <> " m" else parseFloat(INT.toNumber(response.distance) / 1000.0) 2 <> " km"
                      duration = (show (response.duration / 60)) <> " min"
                      tipEnabled = isTipEnabled response.distance
                  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{rideDistance = distance, rideDuration = duration,source = address.formattedAddress, sourceAddress = encodeAddress address.formattedAddress [] finalState.props.sourcePlaceId}, props{customerTip{enableTips = tipEnabled}}})
                  _ <- setValueToLocalStore ENABLE_TIPS $ show tipEnabled
                  if ((getMerchant FunctionCall) /= YATRI && response.distance >= 50000 )then do
                    _ <- pure $ updateLocalStage DistanceOutsideLimits
                    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = DistanceOutsideLimits ,rideRequestFlow = true, isSearchLocation = SearchLocation}})
                    void $ lift $ lift $ toggleLoader false
                    else if response.distance < 500 && (getValueToLocalStore LOCAL_STAGE) /= "ShortDistance" then do
                      _ <- pure $ updateLocalStage ShortDistance
                      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = ShortDistance ,rideRequestFlow = true, isSearchLocation = SearchLocation, distance = response.distance}})
                      void $ lift $ lift $ toggleLoader false
                      else do
                        if flowType == "REPEAT_RIDE_FLOW" then liftFlowBT $ logEventWithParams logField_ "ny_user_repeat_ride_flow" "searchId" rideSearchRes.searchId else pure unit
                        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{searchId = rideSearchRes.searchId,currentStage = FindingEstimate, rideRequestFlow = true, isSearchLocation = SearchLocation, sourcePlaceId = Nothing, destinationPlaceId = Nothing}, data {source = address.formattedAddress, sourceAddress = encodeAddress address.formattedAddress [] finalState.props.sourcePlaceId}})
                        _ <- pure $ updateLocalStage FindingEstimate
                        void $ lift $ lift $ toggleLoader false

                Nothing -> pure unit
    else modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{isSource = Just false, isRideServiceable = true, isSrcServiceable = true, isDestServiceable = true, currentStage = SearchLocationModel }})

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

getFinalAmount :: RideBookingRes -> String
getFinalAmount (RideBookingRes resp) =
    let rideList = resp.rideList
        (RideAPIEntity ride) = (fromMaybe dummyRideAPIEntity (rideList !! 0))
    in (show (fromMaybe 0 ride.computedPrice))

tripDetailsScreenFlow :: Boolean ->  FlowBT String Unit
tripDetailsScreenFlow fromMyRides = do
  (GlobalState state) <- getState
  config <- getAppConfig
  expiryTime <- pure $ (getExpiryTime state.tripDetailsScreen.data.selectedItem.rideEndTimeUTC isForLostAndFound)
  modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {props{fromMyRides = fromMyRides, canConnectWithDriver = (expiryTime <= 86400)}, data{config = config}}) -- expiryTime < 24hrs or 86400 seconds
  flow <- UI.tripDetailsScreen
  case flow of
    GO_TO_HELPSCREEN -> helpAndSupportScreenFlow
    GO_TO_RIDES -> do
      (GlobalState newState) <- getState
      myRidesScreenFlow newState.myRidesScreen.props.fromNavBar
    ON_SUBMIT state -> do
      _ <- Remote.sendIssueBT (Remote.makeSendIssueReq  Nothing (Just state.data.selectedItem.bookingId) state.data.message state.data.message )
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {props{issueReported = true}})
      tripDetailsScreenFlow state.props.fromMyRides
    GO_TO_INVOICE updatedState -> do
      modifyScreenState $ InvoiceScreenStateType (\invoiceScreen -> invoiceScreen {props{fromHomeScreen = false},data{totalAmount = updatedState.data.totalAmount, date = updatedState.data.date, tripCharges = updatedState.data.totalAmount, selectedItem = updatedState.data.selectedItem, config = updatedState.data.config}})
      invoiceScreenFlow
    GO_TO_HOME -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {  data{settingSideBar{opened = SettingSideBarController.CLOSED}}})
      homeScreenFlow
    CONNECT_WITH_DRIVER updatedState -> do
      void $ lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
      void $ lift $ lift $ toggleLoader true
      resp <- Remote.callDriverBT updatedState.data.selectedItem.rideId
      void $ lift $ lift $ toggleLoader false
      pure $ toast (getString REQUEST_RECEIVED_WE_WILL_CALL_YOU_BACK_SOON)
      _ <- Remote.sendIssueBT (Remote.makeSendIssueReq  (Just "nammayatri.support@juspay.in") (Just updatedState.data.selectedItem.rideId) "LOSTANDFOUND" "LOST AND FOUND" )
      tripDetailsScreenFlow updatedState.props.fromMyRides


invoiceScreenFlow :: FlowBT String Unit
invoiceScreenFlow = do
  config <- getAppConfig
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
  config <- getAppConfig
  modifyScreenState $ ContactUsScreenStateType (\contactUsScreen -> contactUsScreen{data{config = config}})
  modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> helpAndSupportScreen{data{config = config}})
  flow <- UI.helpAndSupportScreen
  case flow of
    GO_TO_HOME_FROM_HELP -> homeScreenFlow
    GO_TO_SUPPORT_SCREEN bookingId'-> do
      modifyScreenState $ ContactUsScreenStateType (\contactUsScreen -> contactUsScreen {data{bookingId = bookingId'}})
      contactUsScreenFlow
    GO_TO_TRIP_DETAILS state -> do
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen {data {tripId = state.data.tripId, selectedItem {faresList = state.data.faresList ,date = state.data.date, bookingId = state.data.bookingId,rideStartTime = state.data.rideStartTime, rideEndTime = state.data.rideEndTime, rideId = state.data.rideId, vehicleNumber = state.data.vehicleNumber,time = state.data.time,source = state.data.source,destination = state.data.destination,driverName = state.data.driverName,totalAmount = state.data.totalAmount, rating = state.data.rating},date = state.data.date, time = state.data.time, source = state.data.source, destination = state.data.destination, driverName = state.data.driverName, totalAmount = state.data.totalAmount,rating = state.data.rating}})
      tripDetailsScreenFlow false
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
  config <- getAppConfig
  modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> myRidesScreen {props{fromNavBar = fromNavBar}, data{config = config}})
  flow <- UI.myRidesScreen
  case flow of
    REFRESH state -> myRidesScreenFlow state.props.fromNavBar
    TRIP_DETAILS state -> do
      tripDetailsScreenFlow true
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
      let sourceLat = state.data.selectedItem.sourceLocation^._lat
      let sourceLong = state.data.selectedItem.sourceLocation^._lon
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq sourceLat sourceLong)
      let srcServiceable = sourceServiceabilityResp.serviceable
      let (SpecialLocation srcSpecialLocation) = fromMaybe HomeScreenData.specialLocation (sourceServiceabilityResp.specialLocation)
      let pickUpPoints = map (\(GatesInfo item) -> {
                                              place: item.name,
                                              lat  : (item.point)^._lat,
                                              lng : (item.point)^._lon
                                            }) srcSpecialLocation.gates
      if(state.data.selectedItem.isSpecialZone) then do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{polygonCoordinates = fromMaybe "" sourceServiceabilityResp.geoJson, nearByPickUpPoints = pickUpPoints}})
        pure unit
        else pure unit
      rideSearchFlow "REPEAT_RIDE_FLOW"

selectLanguageScreenFlow :: FlowBT String Unit
selectLanguageScreenFlow = do
  appConfig <- getAppConfig
  modifyScreenState $ SelectLanguageScreenStateType (\selectLanguageScreen -> selectLanguageScreen{data{config = appConfig}})
  flow <- UI.selectLanguageScreen
  case flow of
    UPDATE_LANGUAGE state -> do
                                setValueToLocalStore LANGUAGE_KEY (state.props.selectedLanguage)
                                logField_ <- lift $ lift $ getLogFields
                                _ <- lift $ lift $ liftFlow $(logEventWithParams logField_ "ny_user_lang_selec" "language" (state.props.selectedLanguage))
                                resp <- lift $ lift $ Remote.updateProfile (Remote.mkUpdateProfileRequest "")
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

aboutUsScreenFlow :: FlowBT String Unit
aboutUsScreenFlow = do
  config <- getAppConfig
  modifyScreenState $ AboutUsScreenStateType (\aboutUsScreen -> aboutUsScreen {appConfig = config})
  flow <- UI.aboutUsScreen
  case flow of
    GO_TO_HOME_FROM_ABOUT -> homeScreenFlow

permissionScreenFlow :: String -> FlowBT String Unit
permissionScreenFlow triggertype = do
  _ <- pure $ hideKeyboardOnNavigation true
  config <- getAppConfig
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
                            _ -> if (os == "IOS") then pure unit
                                 else if (not (permissionConditionA && permissionConditionB) )then permissionScreenFlow "LOCATION_DISABLED"
                                 else currentFlowStatus
  pure unit

myProfileScreenFlow :: FlowBT String Unit
myProfileScreenFlow = do
  config <- getAppConfig
  modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState -> myProfileScreenState{data{config = config}})
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
      resp <- if nameLength > 2 then
                lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (name !! 0) (name !! 1) (name !! (nameLength - 1)) (email) gender)
                else if nameLength == 2 then
                  lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (name !! 0) (Just "") (name !! 1) (email) gender)
                  else if nameLength == 1 then
                    lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (name !! 0) (Just "") (Just "") (email) gender)
                    else
                      lift $ lift $ Remote.updateProfile (Remote.editProfileRequest (Just "") (Just "") (Just "") (email) gender)
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
          let codeMessage = decodeErrorCode errResponse.errorMessage
          case codeMessage of
            "PERSON_EMAIL_ALREADY_EXISTS" -> do
              _ <- lift $ lift $ liftFlow (setText' (getNewIDWithTag "EmailEditText") "" )
              modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState -> myProfileScreenState{props{isEmailValid = false, updateProfile = true}, data{errorMessage = Just EMAIL_EXISTS, name = state.data.name, editedName = state.data.editedName, emailId = state.data.emailId, gender = state.data.gender, editedGender = state.data.editedGender}})
            _ -> pure $ toast (getString ERROR_OCCURED)
          myProfileScreenFlow
      myProfileScreenFlow
    GO_TO_HOME_ -> do
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{opened = SettingSideBarController.CLOSED}}})
      homeScreenFlow


dummyRideBooking :: RideBookingRes
dummyRideBooking = RideBookingRes
  {
  agencyNumber : "",
  status : "",
  rideStartTime : Nothing,
  rideEndTime : Nothing,
  duration : Nothing,
  fareBreakup :[],
  createdAt : "",
  discount : Nothing ,
  estimatedTotalFare : 0,
  agencyName : "",
  rideList :[] ,
  estimatedFare : 0,
  tripTerms : [],
  id : "",
  updatedAt : "",
  bookingDetails : dummyRideBookingAPIDetails ,
  fromLocation :  dummyBookingDetails,
  merchantExoPhone : "",
  specialLocationTag : Nothing
  }

dummyRideBookingAPIDetails ::RideBookingAPIDetails
dummyRideBookingAPIDetails= RideBookingAPIDetails{
  contents : dummyRideBookingDetails,
  fareProductType : ""
}

dummyRideBookingDetails :: RideBookingDetails
dummyRideBookingDetails =  RideBookingDetails{
  toLocation : dummyBookingDetails,
  estimatedDistance : Nothing,
  otpCode : Nothing
}

savedLocationFlow :: FlowBT String Unit
savedLocationFlow = do
  config <- getAppConfig
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
              , isBtnActive = (serviceabilityRes.serviceable)
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
  config <- getAppConfig
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
      PlaceName address <- getPlaceName lat lon
      modifyScreenState $ AddNewAddressScreenStateType (\addNewAddressScreen -> addNewAddressScreen{  data  { locSelectedFromMap = address.formattedAddress
                                                                                                            , latSelectedFromMap = lat
                                                                                                            , lonSelectedFromMap = lon
                                                                                                            }
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
            _ <- lift $ lift $ liftFlow (setText' (getNewIDWithTag "SavedLocationEditText") item.description )
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
  config <- getAppConfig 
  modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen { config = config })
  flow <- UI.referralScreen
  case flow of
    UPDATE_REFERRAL referralCode -> do
      let (UpdateProfileReq initialData) = Remote.mkUpdateProfileRequest
          requiredData = initialData{referralCode = (Just referralCode)}
      res <- lift $ lift $ Remote.updateProfile (UpdateProfileReq requiredData)
      case res of
        Right response -> do
          modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen { showThanks = true })
          setValueToLocalStore REFERRAL_STATUS "REFERRED_NOT_TAKEN_RIDE"
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props{isReferred = true} })
        Left err -> do
          if ((err.code == 500 && (decodeErrorCode err.response.errorMessage) == "BPP_INTERNAL_API_ERROR")) then
            modifyScreenState $ ReferralScreenStateType (\referralScreen -> referralScreen { isInvalidCode = true })
          else do
            _ <- pure $ toast (getString ERROR_OCCURED_TRY_AFTER_SOMETIME)
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
    _ <- pure $ saveRecents "RECENT_SEARCHES" modifiedState.homeScreen.data.recentSearchs
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
              codeMessage = decodeErrorCode errResp.errorMessage
          when ( err.code == 400 && codeMessage == "ACTIVE_BOOKING_EXISTS") $ do
            void $ pure $ toast "ACTIVE BOOKING EXISTS"
          currentFlowStatus

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
          void $ pure $ toast "ACTIVE BOOKING EXISTS"
          _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_fs_cancel_estimate_booking_exists_right"
          currentRideFlow true
          homeScreenFlow
        _ -> do
          void $ pure $ toast "CANCEL FAILED"
          _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_fs_cancel_estimate_failed_right"
          homeScreenFlow
    Left err -> do
      let errResp = err.response
          codeMessage = decodeErrorCode errResp.errorMessage
      if ( err.code == 400 && codeMessage == "ACTIVE_BOOKING_EXISTS") then do
        void $ pure $ toast "ACTIVE BOOKING EXISTS"
        _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_fs_cancel_estimate_booking_exists_left"
        currentRideFlow true
        homeScreenFlow
      else do
        void $ pure $ toast "CANCEL FAILED"
        _ <- lift $ lift $ liftFlow $ logEvent logField_ "ny_fs_cancel_estimate_failed_left"
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

getPlaceName :: Number -> Number -> FlowBT String PlaceName
getPlaceName lat long = do
  (GetPlaceNameResp locationName) <- Remote.placeNameBT (Remote.makePlaceNameReq lat long (case (getValueToLocalStore LANGUAGE_KEY) of
                                                                                                                            "HI_IN" -> "HINDI"
                                                                                                                            "KN_IN" -> "KANNADA"
                                                                                                                            "BN_IN" -> "BENGALI"
                                                                                                                            "ML_IN" -> "MALAYALAM"
                                                                                                                            _      -> "ENGLISH"))

  let (PlaceName address) = (fromMaybe HomeScreenData.dummyLocationName (locationName !! 0))
  pure (PlaceName address)