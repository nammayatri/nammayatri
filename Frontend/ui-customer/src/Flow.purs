{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Flow where

import Accessor (_computedPrice, _contents, _lat, _lon, _status, _toLocation)
import Common.Types.App (GlobalPayload)
import Common.Types.App (LazyCheck(..))
import Components.LocationListItem.Controller (dummyLocationListState)
import Components.SavedLocationCard.Controller (getCardType)
import Control.Monad.Except.Trans (lift)
import Data.Array (catMaybes, filter, length, null, snoc, (!!), any, head, uncons)
import Data.Array as Arr
import Data.Either (Either(..))
import Data.Int as INT
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Number (fromString)
import Data.String (Pattern(..), drop, indexOf, split, toLower, trim, take)
import Debug (spy)
import Effect.Class (liftEffect)
import Engineering.Helpers.BackTrack (getState)
import Engineering.Helpers.Commons (liftFlow, os, getNewIDWithTag, bundleVersion, getExpiryTime)
import Foreign.Class (encode)
import Helpers.Utils (hideSplash, adjustViewWithKeyboard, decodeErrorCode, getObjFromLocal, convertUTCtoISC, differenceOfLocationLists, filterRecentSearches, setText', getNewTrackingId, checkPrediction, getRecentSearches, addToRecentSearches, saveRecents, clearWaitingTimer, toString, parseFloat, getCurrentLocationsObjFromLocal, addToPrevCurrLoc, saveCurrentLocations, getCurrentDate, getPrediction, getCurrentLocationMarker, parseNewContacts, getCurrentUTC)
import JBridge (currentPosition, drawRoute, enableMyLocation, factoryResetApp, firebaseLogEvent, firebaseLogEventWithParams, firebaseLogEventWithTwoParams, getVersionCode, getVersionName, hideKeyboardOnNavigation, isInternetAvailable, isLocationEnabled, isLocationPermissionEnabled, loaderText, locateOnMap, openNavigation, reallocateMapFragment, removeAllPolylines, toast, toggleBtnLoader, toggleLoader, launchInAppRatingPopup, firebaseUserID, addMarker, generateSessionId, stopChatListenerService)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog)
import ModifyScreenState (modifyScreenState)
import Prelude (Unit, bind, discard, map, mod, negate, not, pure, show, unit, void, when, ($), (&&), (+), (-), (/), (/=), (<), (<=), (<>), (==), (>), (>=), (||))
import Presto.Core.Types.Language.Flow (doAff, fork, setLogField, delay)
import Resources.Constants (DecodeAddress(..), decodeAddress, encodeAddress, getKeyByLanguage, getValueByComponent, getWard)
import Screens.AddNewAddressScreen.Controller (encodeAddressDescription, getSavedLocations, getSavedTags, getLocationList, calculateDistance, getSavedTagsFromHome, validTag, isValidLocation) as AddNewAddress
import Screens.Handlers as UI
import Screens.HelpAndSupportScreen.ScreenData as HelpAndSupportScreenData
import Screens.HomeScreen.Controller (flowWithoutOffers, getSearchExpiryTime)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Screens.HomeScreen.Transformer (getLocationList, getDriverInfo, dummyRideAPIEntity, encodeAddressDescription, getPlaceNameResp, getUpdatedLocationList, transformContactList)
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails)
import Screens.ReferralScreen.ScreenData as ReferralScreen
import Screens.SavedLocationScreen.Controller (getSavedLocationForAddNewAddressScreen)
import Screens.Types (CardType(..), AddNewAddressScreenState(..),CurrentLocationDetails(..), CurrentLocationDetailsWithDistance(..), DeleteStatus(..), HomeScreenState, LocItemType(..), PopupType(..), SearchLocationModelType(..), Stage(..), LocationListItemState, LocationItemType(..), NewContacts, NotifyFlowEventType(..), FlowStatusData(..), EmailErrorType(..))
import Screens.Types (Gender(..)) as Gender
import Services.API (AddressGeometry(..), BookingLocationAPIEntity(..), ConfirmRes(..), DeleteSavedLocationReq(..), Geometry(..), GetDriverLocationResp(..), GetPlaceNameResp(..), GetProfileRes(..), LatLong(..), LocationS(..), LogOutReq(..), LogOutRes(..), PlaceName(..), ResendOTPResp(..), RideAPIEntity(..), RideBookingAPIDetails(..), RideBookingDetails(..), RideBookingListRes(..), RideBookingRes(..), Route(..), SavedLocationReq(..), SavedLocationsListRes(..), SearchLocationResp(..), SearchRes(..), ServiceabilityRes(..), TriggerOTPResp(..), VerifyTokenResp(..), UserSosRes(..),  GetEmergContactsReq(..), GetEmergContactsResp(..), ContactDetails(..), FlowStatusRes(..), FlowStatus(..), CancelEstimateRes(..))
import Services.Backend as Remote
import Storage (KeyStore(..), deleteValueFromLocalStore, getValueToLocalNativeStore, getValueToLocalStore, isLocalStageOn, setValueToLocalNativeStore, setValueToLocalStore, updateLocalStage)
import Types.App (ABOUT_US_SCREEN_OUTPUT(..), ACCOUNT_SET_UP_SCREEN_OUTPUT(..), ADD_NEW_ADDRESS_SCREEN_OUTPUT(..), GlobalState(..), CONTACT_US_SCREEN_OUTPUT(..), FlowBT, HELP_AND_SUPPORT_SCREEN_OUTPUT(..), HOME_SCREEN_OUTPUT(..), MY_PROFILE_SCREEN_OUTPUT(..), MY_RIDES_SCREEN_OUTPUT(..), PERMISSION_SCREEN_OUTPUT(..), REFERRAL_SCREEN_OUPUT(..), SAVED_LOCATION_SCREEN_OUTPUT(..), SELECT_LANGUAGE_SCREEN_OUTPUT(..), ScreenType(..), TRIP_DETAILS_SCREEN_OUTPUT(..), EMERGECY_CONTACTS_SCREEN_OUTPUT(..))
import Effect (Effect)
import Control.Monad.Except (runExcept)
import Foreign.Class (class Encode)
import Foreign.Generic (decodeJSON, encodeJSON)

import Screens.OnBoardingFlow.Proxy as OBP --(enterMobileNumberScreenFlow, accountSetUpScreenFlow, permissionScreenFlow)
import Screens.CustomerUtils.Proxy as CUP --(aboutUsScreenFlow, selectLanguageScreenFlow, helpAndSupportScreenFlow, invoiceScreenFlow, myProfileScreenFlow, emergencyScreenFlow, addNewAddressScreenFlow, tripDetailsScreenFlow, myRidesScreenFlow)

baseAppFlow :: GlobalPayload -> FlowBT String Unit
baseAppFlow gPayload = do
  _ <- pure $ printLog "Global Payload" gPayload
  (GlobalState state) <- getState
  let bundle = bundleVersion unit
      customerId = (getValueToLocalStore CUSTOMER_ID)
  versionCode <- lift $ lift $ liftFlow $ getVersionCode
  versionName <- lift $ lift $ liftFlow $ getVersionName
  checkVersion versionCode versionName
  setValueToLocalStore VERSION_NAME $ concatString $ Arr.take 3 $ split (Pattern ".") versionName
  setValueToLocalStore BUNDLE_VERSION bundle
  setValueToLocalNativeStore BUNDLE_VERSION bundle
  _ <- pure $ setValueToLocalStore TRACKING_DRIVER "False"
  _ <- pure $ setValueToLocalStore TRACKING_ENABLED "False"
  _ <- pure $ setValueToLocalStore RELOAD_SAVED_LOCATION "true"
  _ <- pure $ setValueToLocalStore TEST_MINIMUM_POLLING_COUNT "15"
  _ <- pure $ setValueToLocalStore TEST_POLLING_INTERVAL "1500.0"
  _ <- pure $ setValueToLocalStore TEST_POLLING_COUNT "113"
  _ <- pure $ setValueToLocalStore RATING_SKIPPED "false"
  _ <- pure $ setValueToLocalStore POINTS_FACTOR "3"
  when ((getValueToLocalStore SESSION_ID == "__failed") || (getValueToLocalStore SESSION_ID == "(null)")) $ do
    setValueToLocalStore SESSION_ID (generateSessionId unit)
  _ <- lift $ lift $ setLogField "customer_id" $ encode (customerId)
  _ <- lift $ lift $ setLogField "app_version" $ encode (show versionCode)
  _ <- lift $ lift $ setLogField "bundle_version" $ encode (bundle)
  _ <- lift $ lift $ setLogField "platform" $ encode (os)
  _ <- UI.splashScreen state.splashScreen
  _ <- lift $ lift $ liftFlow $(firebaseLogEventWithParams "ny_user_app_version" "version" (versionName))
  if getValueToLocalStore REGISTERATION_TOKEN /= "__failed" && getValueToLocalStore REGISTERATION_TOKEN /= "(null)" then currentFlowStatus else OBP.enterMobileNumberScreenFlow -- Removed choose langauge screen


concatString :: Array String -> String
concatString arr = case uncons arr of
  Just { head: x, tail: xs } -> x <> (if length xs == 0 then "" else ".") <> concatString xs
  Nothing -> ""

-- IOS latest version : 1.2.4
updatedIOSversion = {
  majorUpdateIndex : 1,
  minorUpdateIndex : 2,
  patchUpdateIndex : 4
}

enableForceUpdateIOS :: Boolean
enableForceUpdateIOS = false

checkVersion :: Int -> String -> FlowBT String Unit
checkVersion versioncodeAndroid versionName= do
  if os /= "IOS" && versioncodeAndroid < 31 then do
    lift $ lift $ doAff do liftEffect hideSplash
    _ <- UI.handleAppUpdatePopUp
    _ <- pure $ firebaseLogEvent "ny_user_app_update_pop_up_view"
    checkVersion versioncodeAndroid versionName
    else if os == "IOS" && versionName /= "" && enableForceUpdateIOS then do

      let versionArray = (split (Pattern ".") versionName)
          majorUpdateIndex = fromMaybe (-1) $ INT.fromString $ fromMaybe "NA" $ versionArray !! 0
          minorUpdateIndex = fromMaybe (-1) $ INT.fromString $ fromMaybe "NA" $ versionArray !! 1
          patchUpdateIndex = fromMaybe (-1) $ INT.fromString $ fromMaybe "NA" $ versionArray !! 2

      if any (_ == -1) [majorUpdateIndex, minorUpdateIndex, patchUpdateIndex] then pure unit
        else if forceIOSupdate majorUpdateIndex minorUpdateIndex patchUpdateIndex  then do
          lift $ lift $ doAff do liftEffect hideSplash
          _ <- UI.handleAppUpdatePopUp
          _ <- pure $ firebaseLogEvent "ny_user_app_update_pop_up_view"
          checkVersion versioncodeAndroid versionName
          else pure unit
      else pure unit

forceIOSupdate :: Int -> Int -> Int -> Boolean
forceIOSupdate c_maj c_min c_patch =
  c_maj < updatedIOSversion.majorUpdateIndex ||
  c_min < updatedIOSversion.minorUpdateIndex ||
  c_patch < updatedIOSversion.patchUpdateIndex

currentRideFlow :: Boolean -> FlowBT String Unit
currentRideFlow rideAssigned = do
  rideBookingListResponse <- lift $ lift $ Remote.rideBookingList "1" "0" "true"
  (GlobalState state') <- getState
  let state = state'.homeScreen
  case rideBookingListResponse of
    Right (RideBookingListRes listResp) -> do
      if not (null listResp.list) then do
        when (not rideAssigned) $ do
          void $ pure $ firebaseLogEvent "ny_active_ride_with_idle_state"
        let (RideBookingRes resp) = (fromMaybe dummyRideBooking (listResp.list !! 0))
            status = (fromMaybe dummyRideAPIEntity ((resp.rideList) !! 0))^._status
            rideStatus = if status == "NEW" then RideAccepted else RideStarted
            newState = state{data{driverInfoCardState = getDriverInfo (RideBookingRes resp)
                , finalAmount = fromMaybe 0 ((fromMaybe dummyRideAPIEntity (resp.rideList !!0) )^. _computedPrice)},
                  props{currentStage = rideStatus
                  , rideRequestFlow = true
                  , ratingModal = false
                  , bookingId = resp.id
                  }}
        _ <- pure $ spy "Active api" listResp
        modifyScreenState $ HomeScreenStateType (\homeScreen → newState)
        _ <- pure $ setValueToLocalStore TRACKING_ENABLED if status == "NEW" then "True" else "False"
        updateLocalStage rideStatus
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
            let differenceOfDistance = fromMaybe 0 contents.estimatedDistance - INT.round (fromMaybe 0.0 currRideListItem.chargeableRideDistance)
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
                          , estimatedDistance = contents.estimatedDistance }
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
                          }}
                })
          Left err -> updateLocalStage HomeScreen
      else do
        updateLocalStage HomeScreen
    Left err -> updateLocalStage HomeScreen

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
  lift $ lift $ doAff do liftEffect hideSplash
  permissionConditionA <- lift $ lift $ liftFlow $ isLocationPermissionEnabled unit
  permissionConditionB <- lift $ lift $ liftFlow $ isLocationEnabled unit
  internetCondition <- lift $ lift $ liftFlow $ isInternetAvailable unit
  if( not internetCondition) then OBP.permissionScreenFlow "INTERNET_ACTION"
  else if ( permissionConditionA && os == "IOS") then pure unit
  else if ( not (permissionConditionA && permissionConditionB)) then OBP.permissionScreenFlow "LOCATION_DISABLED"
  else pure unit
  _ <- pure $ hideKeyboardOnNavigation true
  homeScreenFlow
  where
    verifyProfile :: String -> FlowBT String Unit
    verifyProfile dummy = do
      (GetProfileRes response) <- Remote.getProfileBT ""
      if isJust response.language then do
        when (getKeyByLanguage (fromMaybe "ENGLISH" response.language) /= (getValueToLocalNativeStore LANGUAGE_KEY)) $ do
          resp <- lift $ lift $ Remote.updateProfile (Remote.makeUpdateLanguageRequest "")
          pure unit
      else do
        resp <- lift $ lift $ Remote.updateProfile (Remote.makeUpdateLanguageRequest "")
        pure unit
      setValueToLocalStore REFERRAL_STATUS  $ if response.hasTakenRide then "HAS_TAKEN_RIDE" else if (response.referralCode /= Nothing && not response.hasTakenRide) then "REFERRED_NOT_TAKEN_RIDE" else "NOT_REFERRED_NOT_TAKEN_RIDE"
      setValueToLocalStore HAS_TAKEN_FIRST_RIDE if response.hasTakenRide then "true" else "false"
      if (((fromMaybe "" response.firstName) == "" ) && not (isJust response.firstName)) then do
        _ <- updateLocalStage HomeScreen
        lift $ lift $ doAff do liftEffect hideSplash
        OBP.accountSetUpScreenFlow
      else do
          modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{data{settingSideBar{name =fromMaybe "" response.firstName}}})
          setValueToLocalStore USER_NAME ((fromMaybe "" response.firstName) <> " " <> (fromMaybe "" response.middleName) <> " " <> (fromMaybe "" response.lastName))
      if (fromMaybe "UNKNOWN" (response.gender) /= "UNKNOWN") then do
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{gender = Just (fromMaybe "" response.gender)}}})
        else pure unit
      if isJust response.email then do
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

homeScreenFlow :: FlowBT String Unit
homeScreenFlow = do
  (GlobalState currentState) <- getState
  _ <- checkAndUpdateSavedLocations currentState.homeScreen
  -- TODO: REQUIRED ONCE WE NEED TO STORE RECENT CURRENTLOCATIONS
  -- resp <- lift $ lift $ getCurrentLocationsObjFromLocal currentState.homeScreen
  -- modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{previousCurrentLocations = resp}})

  -- TODO: HANDLE LOCATION LIST INITIALLY
  _ <- pure $ firebaseUserID (getValueToLocalStore CUSTOMER_ID)
  void $ lift $ lift $ toggleLoader false
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{hasTakenRide = if (getValueToLocalStore REFERRAL_STATUS == "HAS_TAKEN_RIDE") then true else false, isReferred = if (getValueToLocalStore REFERRAL_STATUS == "REFERRED_NOT_TAKEN_RIDE") then true else false }})
  flow <- UI.homeScreen
  case flow of
    CHECK_FLOW_STATUS -> currentFlowStatus
    ON_RESUME_APP -> currentFlowStatus
    GO_TO_MY_RIDES -> do
      modifyScreenState $ MyRideScreenStateType (\myRidesScreen -> myRidesScreen{data{offsetValue = 0}})
      _ <- pure $ firebaseLogEvent "ny_user_myrides_click"
      CUP.myRidesScreenFlow true
    GO_TO_HELP -> do
      modifyScreenState $ HelpAndSupportScreenStateType (\helpAndSupportScreen -> HelpAndSupportScreenData.initData)
      _ <- pure $ firebaseLogEvent "ny_user_help"
      CUP.helpAndSupportScreenFlow
    CHANGE_LANGUAGE ->  CUP.selectLanguageScreenFlow
    GO_TO_EMERGENCY_CONTACTS -> CUP.emergencyScreenFlow
    GO_TO_ABOUT -> CUP.aboutUsScreenFlow
    GO_TO_MY_PROFILE -> do
        _ <- pure $ firebaseLogEvent "ny_user_profile_click"
        -- modifyScreenState $ MyProfileScreenStateType (\myProfileScreenState ->  MyProfileScreenData.initData)
        CUP.myProfileScreenFlow
    GO_TO_FIND_ESTIMATES state-> do
      _ <- lift $ lift $ liftFlow $ firebaseLogEventWithTwoParams "ny_user_source_and_destination" "ny_user_enter_source" (take 99 (state.data.source)) "ny_user_enter_destination" (take 99 (state.data.destination))
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq state.props.sourceLat state.props.sourceLong)
      if (not sourceServiceabilityResp.serviceable) then do
        updateLocalStage SearchLocationModel
        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = SearchLocationModel ,rideRequestFlow = false, isSearchLocation = SearchLocation, isSrcServiceable = false, isSource = Just true, isRideServiceable = false}})
        homeScreenFlow
        else pure unit
      (SearchRes rideSearchRes) <- Remote.rideSearchBT (Remote.makeRideSearchReq state.props.sourceLat state.props.sourceLong state.props.destinationLat state.props.destinationLong state.data.sourceAddress state.data.destinationAddress)
      routeResponse <- Remote.drawMapRoute state.props.sourceLat state.props.sourceLong state.props.destinationLat state.props.destinationLong (Remote.normalRoute "") "NORMAL" state.data.source state.data.destination rideSearchRes.routeInfo "pickup"
      case rideSearchRes.routeInfo of
        Just (Route response) -> do
          let distance = if response.distance < 1000 then toString(response.distance)  <> " m" else parseFloat(INT.toNumber(response.distance) / 1000.0) 2 <> " km"
              duration = (show (response.duration / 60)) <> " min"
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{rideDistance = distance, rideDuration = duration}, props{customerTip{enableTips = response.distance < 5000}}})
          _ <- setValueToLocalStore ENABLE_TIPS $ show (response.distance < 5000)
          if response.distance >= 50000 then do
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
      void $ pure $ setFlowStatusData (FlowStatusData { source : {lat : state.props.sourceLat, lng : state.props.sourceLong, place : state.data.source}
                                                      , destination : {lat : state.props.destinationLat, lng : state.props.destinationLong, place : state.data.destination}
                                                      , sourceAddress : state.data.sourceAddress
                                                      , destinationAddress : state.data.destinationAddress })
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{searchId = rideSearchRes.searchId,currentStage = FindingEstimate, rideRequestFlow = true, isSearchLocation = SearchLocation, sourcePlaceId = Nothing, destinationPlaceId = Nothing}})
      updateLocalStage FindingEstimate
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
        (ServiceabilityRes destServiceabilityResp) <- Remote.destServiceabilityBT (Remote.makeServiceabilityReq bothLocationChangedState.props.destinationLat bothLocationChangedState.props.destinationLong)
        let destServiceable = destServiceabilityResp.serviceable
        modifyScreenState $ HomeScreenStateType (\homeScreen -> bothLocationChangedState)
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
      (SearchLocationResp searchLocationResp) <- Remote.searchLocationBT (Remote.makeSearchLocationReq input ( state.props.sourceLat) ( state.props.sourceLong) 50000  (case (getValueToLocalStore LANGUAGE_KEY) of
                                                                                                                                                                                                    "HI_IN" -> "HINDI"
                                                                                                                                                                                                    "KN_IN" -> "KANNADA"
                                                                                                                                                                                                    _      -> "ENGLISH") "")
      let event =
            case state.props.isSource of
              Just true -> "ny_user_auto_complete_api_trigger_src"
              Just false -> "ny_user_auto_complete_api_trigger_dst"
              Nothing -> ""
      _ <- pure $ firebaseLogEvent event
      let predictionList = getLocationList searchLocationResp.predictions
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
                      postfixImageUrl = "ny_ic_fav_red,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav_red.png" }
                    else
                      item {
                        lat = item.lat,
                        lon = item.lon,
                        locationItemType = item.locationItemType,
                        postfixImageUrl = "ny_ic_fav,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav.png" }
            ) ((filteredRecentsList) <> filteredPredictionList) }})
      homeScreenFlow
    GET_QUOTES state -> do
          _ <- pure $ setValueToLocalStore AUTO_SELECTING "false"
          setValueToLocalStore FINDING_QUOTES_POLLING "false"
          _ <- pure $ setValueToLocalStore TRACKING_ID (getNewTrackingId unit)
          _ <- pure $ firebaseLogEvent "ny_user_request_quotes"
          if(getValueToLocalStore FLOW_WITHOUT_OFFERS == "true") then do
            _ <- pure $ firebaseLogEvent "ny_user_auto_confirm"
            pure unit
          else do
            pure unit
          void $ pure $ setValueToLocalStore FINDING_QUOTES_START_TIME (getCurrentUTC "LazyCheck")
          _ <- Remote.selectEstimateBT (Remote.makeEstimateSelectReq (flowWithoutOffers WithoutOffers) (if state.props.customerTip.enableTips && state.props.customerTip.isTipSelected then Just state.props.customerTip.tipForDriver else Nothing)) (state.props.estimateId)
          homeScreenFlow
    SELECT_ESTIMATE state -> do
        updateLocalStage SettingPrice
        homeScreenFlow
    GET_SELECT_LIST state -> do
      when (isLocalStageOn QuoteList) $ do
        updateFlowStatus SEARCH_CANCELLED
      homeScreenFlow
    CONFIRM_RIDE state -> do
          _ <- pure $ enableMyLocation false
          if isJust state.props.selectedQuote then do
            updateLocalStage ConfirmingRide
            response  <- lift $ lift $ Remote.rideConfirm (fromMaybe "" state.props.selectedQuote)
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
          _ <- pure $ spy "INSIDE IF OF ONGOING" state.props.currentStage
          _ <- Remote.drawMapRoute srcLat srcLon dstLat dstLon (Remote.normalRoute "") "DRIVER_LOCATION_UPDATE" "" "" Nothing "pickup"
          homeScreenFlow
        else if state.props.currentStage == HomeScreen then
          do
            _ <- pure $ removeAllPolylines ""
            _ <- pure $ spy "INSIDE ELSE IF OF ONGOING" state.props.currentStage
            _ <- updateLocalStage HomeScreen
            modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData)
            homeScreenFlow
          else homeScreenFlow
    CANCEL_RIDE_REQUEST state -> do
      _ <- pure $ currentPosition ""
      _ <- pure $ enableMyLocation true
      _ <- updateLocalStage HomeScreen
      _ <- Remote.cancelRideBT (Remote.makeCancelRequest state) (state.props.bookingId)
      _ <- pure $ clearWaitingTimer state.props.waitingTimeTimerId
      removeChatService ""
      _ <- pure $ firebaseLogEvent "ny_user_ride_cancelled_by_user"
      modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData)
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
                                      _ <- pure $ firebaseLogEvent "ny_user_ride_started"
                                      let shareAppCount = getValueToLocalStore SHARE_APP_COUNT
                                      if shareAppCount == "__failed" then do
                                        setValueToLocalStore SHARE_APP_COUNT "1"
                                      else if shareAppCount /= "-1" then do
                                        setValueToLocalStore SHARE_APP_COUNT (show ((INT.round $ (fromMaybe 0.0 (fromString (shareAppCount))))+1))
                                      else pure unit

                                      let newState = state{data{route = Nothing},props{currentStage = RideStarted, forFirst = true , showShareAppPopUp = (INT.round $ (fromMaybe 0.0 (fromString (getValueToLocalStore SHARE_APP_COUNT)))) `mod` 4 == 0 }}
                                      _ <- updateLocalStage RideStarted
                                      modifyScreenState $ HomeScreenStateType (\homeScreen -> newState)
                                      _ <- pure $ clearWaitingTimer state.props.waitingTimeTimerId
                                      homeScreenFlow
            "TRIP_FINISHED"       -> do -- TRIP FINISHED
                                      if (getValueToLocalStore HAS_TAKEN_FIRST_RIDE == "false") then do
                                        pure $ firebaseLogEvent "ny_user_first_ride_completed"
                                        (GetProfileRes response) <- Remote.getProfileBT ""
                                        setValueToLocalStore HAS_TAKEN_FIRST_RIDE ( show response.hasTakenRide)
                                        else pure unit
                                      _ <- pure $ firebaseLogEvent "ny_user_ride_completed"
                                      _ <- Remote.drawMapRoute srcLat srcLon dstLat dstLon (Remote.normalRoute "") "NORMAL" "" "" Nothing "pickup"
                                      _ <- pure $ enableMyLocation true
                                      _ <- updateLocalStage HomeScreen
                                      if (state.props.bookingId /= "") then do
                                        (RideBookingRes resp) <- Remote.rideBookingBT (state.props.bookingId)
                                        let (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
                                        let (RideBookingDetails contents) = bookingDetails.contents
                                        let (RideAPIEntity ride) = fromMaybe dummyRideAPIEntity (resp.rideList !! 0)
                                        let finalAmount =  INT.round $ fromMaybe 0.0 (fromString (getFinalAmount (RideBookingRes resp)))
                                        let differenceOfDistance = fromMaybe 0 contents.estimatedDistance - INT.round (fromMaybe 0.0 ride.chargeableRideDistance)
                                        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{startedAt = (convertUTCtoISC (fromMaybe "" resp.rideStartTime ) "h:mm A"), startedAtUTC = ((fromMaybe "" resp.rideStartTime)),endedAt = (convertUTCtoISC (fromMaybe "" resp.rideEndTime ) "h:mm A"), finalAmount = finalAmount, previousRideRatingState {distanceDifference = differenceOfDistance}},props{currentStage = RideCompleted, estimatedDistance = contents.estimatedDistance}})
                                        homeScreenFlow
                                        else homeScreenFlow
            "CANCELLED_PRODUCT"   -> do -- REMOVE POLYLINES
                                      _ <- pure $ firebaseLogEvent "ny_user_ride_cancelled"
                                      _ <- pure $ removeAllPolylines ""
                                      _ <- pure $ enableMyLocation true
                                      _ <- updateLocalStage HomeScreen
                                      removeChatService ""
                                      modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData)
                                      _ <- pure $ clearWaitingTimer state.props.waitingTimeTimerId
                                      homeScreenFlow
            "DRIVER_ASSIGNMENT"   -> if (not (isLocalStageOn RideAccepted || isLocalStageOn RideStarted )) then do
                                        _ <- pure $ firebaseLogEvent "ny_fs_driver_assignment"
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
      _ <- pure $ factoryResetApp ""
      _ <- pure $ firebaseLogEvent "ny_user_logout"
      modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData)
      OBP.enterMobileNumberScreenFlow -- Removed choose langauge screen
    SUBMIT_RATING state -> do
      _ <- Remote.rideFeedbackBT (Remote.makeFeedBackReq (state.data.previousRideRatingState.rating) (state.data.previousRideRatingState.rideId) (state.data.previousRideRatingState.feedback))
      _ <- updateLocalStage HomeScreen
      modifyScreenState $ HomeScreenStateType (\homeScreen -> HomeScreenData.initData)
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
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{ isSrcServiceable = true, showlocUnserviceablePopUp = false}})
              if (saveToCurrLocs && state.homeScreen.props.storeCurrentLocs) then addLocToCurrLoc state.homeScreen.props.sourceLat state.homeScreen.props.sourceLong state.homeScreen.data.source else pure unit
              pure unit
              else do
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{ isSrcServiceable = false, showlocUnserviceablePopUp = true}})
                pure unit
            else do
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{ isSrcServiceable = true, showlocUnserviceablePopUp = false}})
              pure unit

      homeScreenFlow
    RETRY  -> homeScreenFlow
    CHECK_SERVICEABILITY updatedState lat long-> do
      (ServiceabilityRes sourceServiceabilityResp) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq lat long)
      let sourceLat = if sourceServiceabilityResp.serviceable then lat else updatedState.props.sourceLat
          sourceLong = if sourceServiceabilityResp.serviceable then long else updatedState.props.sourceLong
      modifyScreenState $ HomeScreenStateType (\homeScreen -> updatedState{props{sourceLat = sourceLat, sourceLong = sourceLong, isSrcServiceable =sourceServiceabilityResp.serviceable , showlocUnserviceablePopUp = (not sourceServiceabilityResp.serviceable)}})
      homeScreenFlow
    HOME_SCREEN -> do
        when (isLocalStageOn FindingQuotes) $ do
          (GlobalState state) <- getState
          cancelEstimate state.homeScreen.props.estimateId
        _ <- pure $ removeAllPolylines ""
        _ <- lift $ lift $ liftFlow $ addMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) 9.9 9.9 160 0.5 0.9
        _ <- pure $ currentPosition ""
        _ <- updateLocalStage HomeScreen
        modifyScreenState $ HomeScreenStateType (\homeScreen ->  HomeScreenData.initData)
        homeScreenFlow
    CHECK_CURRENT_STATUS -> do
      when (isLocalStageOn FindingQuotes) $ do
        (GlobalState state) <- getState
        cancelEstimate state.homeScreen.props.estimateId
      _ <- pure $ removeAllPolylines ""
      _ <- lift $ lift $ liftFlow $ addMarker (getCurrentLocationMarker (getValueToLocalStore VERSION_NAME)) 9.9 9.9 160 0.5 0.9
      _ <- pure $ currentPosition ""
      _ <- updateLocalStage HomeScreen
      modifyScreenState $ HomeScreenStateType (\homeScreen ->  HomeScreenData.initData)
      currentFlowStatus
    UPDATE_LOCATION_NAME state lat lon -> do
      (GetPlaceNameResp locationName) <- Remote.placeNameBT (Remote.makePlaceNameReq lat lon (case (getValueToLocalStore LANGUAGE_KEY) of
                                                                                                                          "HI_IN" -> "HINDI"
                                                                                                                          "KN_IN" -> "KANNADA"
                                                                                                                          _      -> "ENGLISH"))
      _ <- pure $ firebaseLogEvent "ny_user_placename_api_lom_onDrag"
      let (PlaceName placeDetails) = fromMaybe HomeScreenData.dummyLocationName (locationName !! 0)
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
      (GetPlaceNameResp locationName) <- Remote.placeNameBT (Remote.makePlaceNameReq lat lon (case (getValueToLocalStore LANGUAGE_KEY) of
                                                                                                                          "HI_IN" -> "HINDI"
                                                                                                                          "KN_IN" -> "KANNADA"
                                                                                                                          _      -> "ENGLISH"))
      _ <- pure $ firebaseLogEvent "ny_user_placename_api_cpu_onDrag"
      let (PlaceName address) = (fromMaybe HomeScreenData.dummyLocationName (locationName !! 0))
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
        _ <- pure $ firebaseLogEvent "ny_user_addresses"
        savedLocationFlow
    OPEN_GOOGLE_MAPS state -> do
      _ <- pure $ firebaseLogEvent "ny_user_ride_track_gmaps"
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
                          _ <- pure $ firebaseLogEvent "ny_user_pickup_track_inapp"
                          pure unit
          RideStarted  -> do
                          _ <- pure $ firebaseLogEvent "ny_user_ride_track_inapp"
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
          let savedLocationWithHomeOrWorkTag = (filter (\listItem ->  (listItem.prefixImageUrl == "ny_ic_home_blue,https://assets.juspay.in/nammayatri/images/user/ny_ic_home_blue.png"|| (listItem.prefixImageUrl == "ny_ic_work_blue,https://assets.juspay.in/nammayatri/images/user/ny_ic_work_blue.png"))) (AddNewAddress.getSavedLocations listResp.list))
              recents = (differenceOfLocationLists recentPredictionsObject.predictionArray savedLocationWithHomeOrWorkTag)
              savedLocationsWithOtherTag = (filter (\listItem -> not(listItem.prefixImageUrl == "ny_ic_home_blue,https://assets.juspay.in/nammayatri/images/user/ny_ic_home_blue.png" || listItem.prefixImageUrl == "ny_ic_work_blue,https://assets.juspay.in/nammayatri/images/user/ny_ic_work_blue.png")) (AddNewAddress.getSavedLocations listResp.list))
              updatedList = (map (\item ->  item { postfixImageUrl = if not (checkPrediction item savedLocationsWithOtherTag) then "ny_ic_fav_red,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav_red.png"
                                                                        else "ny_ic_fav,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav.png" }) (recents))
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
      modifyScreenState $ InvoiceScreenStateType (\invoiceScreen -> invoiceScreen {props{fromHomeScreen= true},data{totalAmount = ("₹ " <> finalAmount), date = prevRideState.dateDDMMYY, tripCharges = ("₹ " <> finalAmount), selectedItem {date = prevRideState.dateDDMMYY, bookingId = prevRideState.bookingId,rideStartTime = prevRideState.rideStartTime, rideEndTime = prevRideState.rideEndTime, rideId = prevRideState.rideId, shortRideId = prevRideState.shortRideId,vehicleNumber = prevRideState.vehicleNumber,time = prevRideState.rideStartTime,source = prevRideState.source,destination = prevRideState.destination,driverName = prevRideState.driverName,totalAmount = ("₹ " <> finalAmount)}}})
      CUP.invoiceScreenFlow

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
          CUP.addNewAddressScreenFlow ""
        Just selectedLocationListItem -> do
          case selectedLocationListItem.locationItemType of
            Just RECENTS ->  getDistanceDiff state (fromMaybe 0.0 selectedLocationListItem.lat) (fromMaybe 0.0 selectedLocationListItem.lon)
            Nothing ->  getDistanceDiff state (fromMaybe 0.0 selectedLocationListItem.lat) (fromMaybe 0.0 selectedLocationListItem.lon)
            _ -> do
              (GetPlaceNameResp placeNameResp) <- getPlaceNameResp (selectedLocationListItem.placeId) (fromMaybe 0.0 selectedLocationListItem.lat) (fromMaybe 0.0 selectedLocationListItem.lon) selectedLocationListItem

              let (PlaceName placeName) = (fromMaybe HomeScreenData.dummyLocationName (placeNameResp!!0))
              let (LatLong placeLatLong) = (placeName.location)

              (ServiceabilityRes serviceabilityRes) <- Remote.originServiceabilityBT (Remote.makeServiceabilityReq (placeLatLong.lat) (placeLatLong.lon))
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
  (GlobalState homeScreenModifiedState) <- getState
  let finalState = homeScreenModifiedState.homeScreen -- bothLocationChangedState{props{isSrcServiceable =homeScreenModifiedState.homeScreen.props.isSrcServiceable, isDestServiceable = homeScreenModifiedState.homeScreen.props.isDestServiceable, isRideServiceable = homeScreenModifiedState.homeScreen.props.isRideServiceable }}
  if (finalState.props.sourceLat /= 0.0 && finalState.props.sourceLong /= 0.0) && (finalState.props.destinationLat /= 0.0 && finalState.props.destinationLong /= 0.0) && (finalState.data.source /= "") && (finalState.data.destination /= "")
    then do
      case finalState.props.sourceSelectedOnMap of
        false -> do
          _ <- pure $ locateOnMap false finalState.props.sourceLat finalState.props.sourceLong
          _ <- pure $ removeAllPolylines ""
          modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = ConfirmingLocation,rideRequestFlow = true}})
          _ <- pure $ updateLocalStage ConfirmingLocation
          void $ lift $ lift $ toggleLoader false
        true -> do
          (SearchRes rideSearchRes) <- Remote.rideSearchBT (Remote.makeRideSearchReq finalState.props.sourceLat finalState.props.sourceLong finalState.props.destinationLat finalState.props.destinationLong finalState.data.sourceAddress finalState.data.destinationAddress)
          void $ pure $ setFlowStatusData (FlowStatusData { source : {lat : finalState.props.sourceLat, lng : finalState.props.sourceLong, place : finalState.data.source}
                                                          , destination : {lat : finalState.props.destinationLat, lng : finalState.props.destinationLong, place : finalState.data.destination}
                                                          , sourceAddress : finalState.data.sourceAddress
                                                          , destinationAddress : finalState.data.destinationAddress })
          case finalState.props.currentStage of
            TryAgain -> do
              when (finalState.props.customerTip.enableTips) $ do
                cancelEstimate finalState.props.estimateId
              _ <- pure $ updateLocalStage TryAgain
              modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{searchId = rideSearchRes.searchId, currentStage = TryAgain, rideRequestFlow = true}})
            _        -> do
              routeResponse <- Remote.drawMapRoute finalState.props.sourceLat finalState.props.sourceLong finalState.props.destinationLat finalState.props.destinationLong (Remote.normalRoute "") "NORMAL" finalState.data.source finalState.data.destination rideSearchRes.routeInfo "pickup"
              case rideSearchRes.routeInfo of
                Just (Route response) -> do
                  let distance = if response.distance < 1000 then toString(response.distance)  <> " m" else parseFloat(INT.toNumber(response.distance) / 1000.0) 2 <> " km"
                      duration = (show (response.duration / 60)) <> " min"
                  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{rideDistance = distance, rideDuration = duration}, props{customerTip{enableTips = if (response.distance < 5000) then true else false}}})
                  _ <- setValueToLocalStore ENABLE_TIPS $ show (response.distance < 5000)
                  if response.distance >= 50000 then do
                    _ <- pure $ updateLocalStage DistanceOutsideLimits
                    modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = DistanceOutsideLimits ,rideRequestFlow = true, isSearchLocation = SearchLocation}})
                    void $ lift $ lift $ toggleLoader false

                    else if response.distance < 500 && (getValueToLocalStore LOCAL_STAGE) /= "ShortDistance" then do
                      _ <- pure $ updateLocalStage ShortDistance
                      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage = ShortDistance ,rideRequestFlow = true, isSearchLocation = SearchLocation, distance = response.distance}})
                      void $ lift $ lift $ toggleLoader false
                      else do
                        if flowType == "REPEAT_RIDE_FLOW" then lift $ lift $ liftFlow $ firebaseLogEventWithParams "ny_user_repeat_ride_flow" "searchId" rideSearchRes.searchId else pure unit
                        modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{searchId = rideSearchRes.searchId,currentStage = FindingEstimate, rideRequestFlow = true, isSearchLocation = SearchLocation, sourcePlaceId = Nothing, destinationPlaceId = Nothing}})
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
  fromLocation :  dummyBookingDetails
  }

dummyRideBookingAPIDetails ::RideBookingAPIDetails
dummyRideBookingAPIDetails= RideBookingAPIDetails{
  contents : dummyRideBookingDetails,
  fareProductType : ""
}

dummyRideBookingDetails :: RideBookingDetails
dummyRideBookingDetails = RideBookingDetails {
  toLocation : dummyBookingDetails,
  estimatedDistance : Nothing
}

savedLocationFlow :: FlowBT String Unit
savedLocationFlow = do
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
      CUP.addNewAddressScreenFlow "dummy"
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

      CUP.addNewAddressScreenFlow "edit Location"

    GO_BACK_FROM_SAVED_LOCATION -> do
      _ <- lift $ lift $ liftFlow $ reallocateMapFragment (getNewIDWithTag "CustomerHomeScreenMap")
      homeScreenFlow
  pure unit

referralScreenFlow :: FlowBT String Unit
referralScreenFlow = do
  flow <- UI.referralScreen
  case flow of
    UPDATE_REFERRAL referralCode -> do
      res <- lift $ lift $ Remote.updateProfile (Remote.makeUpdateProfileRequest Nothing Nothing (Just referralCode))
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
  lift $ lift $ liftFlow $ drawRoute (Remote.walkCoordinate srcLat srcLng destLat destLng) "DOT" "#323643" false srcMarker destMarker 8 "DRIVER_LOCATION_UPDATE" "" ""

checkAndUpdateSavedLocations :: HomeScreenState -> FlowBT String Unit
checkAndUpdateSavedLocations state = do
  if (getValueToLocalStore RELOAD_SAVED_LOCATION == "true") || (state.props.currentStage == HomeScreen)
    then do
      recentPredictionsObject <- lift $ lift $ getObjFromLocal state
      (savedLocationResp )<- lift $ lift $ Remote.getSavedLocationList ""
      case savedLocationResp of
        Right (SavedLocationsListRes listResp) -> do
          let savedLocationWithHomeOrWorkTag = (filter (\listItem ->  (listItem.prefixImageUrl == "ny_ic_home_blue,https://assets.juspay.in/nammayatri/images/user/ny_ic_home_blue.png" || (listItem.prefixImageUrl == "ny_ic_work_blue,https://assets.juspay.in/nammayatri/images/user/ny_ic_work_blue.png"))) (AddNewAddress.getSavedLocations listResp.list))
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
                          (\item -> item { postfixImageUrl =  if not (checkPrediction item (AddNewAddress.getSavedLocations listResp.list)) then "ny_ic_fav_red,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav_red.png" else "ny_ic_fav,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav.png" } ) twoElements},
                      savedLocations = (AddNewAddress.getSavedLocations listResp.list),
                      locationList =  map
                          (\item -> item { postfixImageUrl =  if not (checkPrediction item (AddNewAddress.getSavedLocations listResp.list)) then "ny_ic_fav_red,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav_red.png" else "ny_ic_fav,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav.png" } ) recent
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
  res <- lift $ lift $ Remote.cancelEstimate bookingId
  case res of
    Right res -> do
      -- TODO : to be removed after new bundle is 100% available (replace with pure unit)
      let (CancelEstimateRes resp) = res
      case resp.result of
        "Success" -> pure unit
        "BookingAlreadyCreated" -> do
          void $ pure $ toast "ACTIVE BOOKING EXISTS"
          _ <- pure $ firebaseLogEvent "ny_fs_cancel_estimate_booking_exists_right"
          currentRideFlow true
          homeScreenFlow
        _ -> do
          void $ pure $ toast "CANCEL FAILED"
          _ <- pure $ firebaseLogEvent "ny_fs_cancel_estimate_failed_right"
          homeScreenFlow
    Left err -> do
      let errResp = err.response
          codeMessage = decodeErrorCode errResp.errorMessage
      if ( err.code == 400 && codeMessage == "ACTIVE_BOOKING_EXISTS") then do
        void $ pure $ toast "ACTIVE BOOKING EXISTS"
        _ <- pure $ firebaseLogEvent "ny_fs_cancel_estimate_booking_exists_left"
        currentRideFlow true
        homeScreenFlow
      else do
        void $ pure $ toast "CANCEL FAILED"
        _ <- pure $ firebaseLogEvent "ny_fs_cancel_estimate_failed_left"
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