module Screens.OnBoardingFlow.Flow where

import Accessor (_computedPrice, _contents, _formattedAddress, _id, _lat, _lon, _status, _toLocation)
import Common.Types.App (GlobalPayload)
import Common.Types.App (LazyCheck(..))
-- import Components.LocationListItem.Controller (dummyLocationListState)
-- import Components.SavedLocationCard.Controller (getCardType)
-- import Components.SettingSideBar.Controller as SettingSideBarController
import Control.Monad.Except.Trans (lift)
import Data.Array (catMaybes, filter, length, null, snoc, (!!), any, sortBy, head, uncons)
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
import Helpers.Utils (hideSplash, getDistanceBwCordinates, adjustViewWithKeyboard, decodeErrorCode, getObjFromLocal, convertUTCtoISC, differenceOfLocationLists, filterRecentSearches, setText', seperateByWhiteSpaces, getNewTrackingId, checkPrediction, getRecentSearches, addToRecentSearches, saveRecents, clearWaitingTimer, toString, parseFloat, getCurrentLocationsObjFromLocal, addToPrevCurrLoc, saveCurrentLocations, getCurrentDate, getPrediction, getCurrentLocationMarker, parseNewContacts, getCurrentUTC)
import JBridge (currentPosition, drawRoute, enableMyLocation, factoryResetApp, firebaseLogEvent, firebaseLogEventWithParams, firebaseLogEventWithTwoParams, getVersionCode, getVersionName, hideKeyboardOnNavigation, isCoordOnPath, isInternetAvailable, isLocationEnabled, isLocationPermissionEnabled, loaderText, locateOnMap, openNavigation, reallocateMapFragment, removeAllPolylines, toast, toggleBtnLoader, toggleLoader, updateRoute, launchInAppRatingPopup, firebaseUserID, addMarker, generateSessionId, stopChatListenerService)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog)
import ModifyScreenState (modifyScreenState, updateRideDetails)
import Prelude (Unit, bind, discard, map, mod, negate, not, pure, show, unit, void, when, ($), (&&), (+), (-), (/), (/=), (<), (<=), (<>), (==), (>), (>=), (||))
import Presto.Core.Types.Language.Flow (doAff, fork, setLogField, delay)
import Resources.Constants (DecodeAddress(..), decodeAddress, encodeAddress, getKeyByLanguage, getValueByComponent, getWard)

import Services.API (AddressGeometry(..), BookingLocationAPIEntity(..), ConfirmRes(..), DeleteSavedLocationReq(..), Geometry(..), GetDriverLocationResp(..), GetPlaceNameResp(..), GetProfileRes(..), LatLong(..), LocationS(..), LogOutReq(..), LogOutRes(..), PlaceName(..), ResendOTPResp(..), RideAPIEntity(..), RideBookingAPIDetails(..), RideBookingDetails(..), RideBookingListRes(..), RideBookingRes(..), Route(..), SavedLocationReq(..), SavedLocationsListRes(..), SearchLocationResp(..), SearchRes(..), ServiceabilityRes(..), TriggerOTPResp(..), VerifyTokenResp(..), UserSosRes(..),  GetEmergContactsReq(..), GetEmergContactsResp(..), ContactDetails(..), FlowStatusRes(..), FlowStatus(..), CancelEstimateRes(..))
import Services.Backend as Remote
import Storage (KeyStore(..), deleteValueFromLocalStore, getValueToLocalNativeStore, getValueToLocalStore, isLocalStageOn, setValueToLocalNativeStore, setValueToLocalStore, updateLocalStage)
import Types.App (ABOUT_US_SCREEN_OUTPUT(..), ACCOUNT_SET_UP_SCREEN_OUTPUT(..), ADD_NEW_ADDRESS_SCREEN_OUTPUT(..), GlobalState(..), CONTACT_US_SCREEN_OUTPUT(..), FlowBT, HELP_AND_SUPPORT_SCREEN_OUTPUT(..), HOME_SCREEN_OUTPUT(..), MY_PROFILE_SCREEN_OUTPUT(..), MY_RIDES_SCREEN_OUTPUT(..), PERMISSION_SCREEN_OUTPUT(..), REFERRAL_SCREEN_OUPUT(..), SAVED_LOCATION_SCREEN_OUTPUT(..), SELECT_LANGUAGE_SCREEN_OUTPUT(..), ScreenType(..), TRIP_DETAILS_SCREEN_OUTPUT(..), EMERGECY_CONTACTS_SCREEN_OUTPUT(..))
import Effect (Effect)
import Control.Monad.Except (runExcept)
import Foreign.Class (class Encode)
import Foreign.Generic (decodeJSON, encodeJSON)

import Screens.EnterMobileNumberScreen.Controller (ScreenOutput(..))
import Screens.EnterMobileNumberScreen.View
import Screens.EnterMobileNumberScreen.Handler (enterMobileNumberScreen)

import Screens.ChooseLanguageScreen.Handler (chooseLanguageScreen)
import Screens.ChooseLanguageScreen.Controller (ScreenOutput(..))

import Screens.AccountSetUpScreen.Handler (accountSetUpScreen)
import Screens.AccountSetUpScreen.ScreenData as AccountSetUpScreenData
import Screens.Types (Gender(..)) as Gender
-- import Screens.EnterMobileNumberScreen.Screen

import Screens.PermissionScreen.Handler (permissionScreen)

enterMobileNumberScreenFlow :: FlowBT String Unit
enterMobileNumberScreenFlow = do
  lift $ lift $ doAff do liftEffect hideSplash -- Removed initial choose langauge screen
  setValueToLocalStore LANGUAGE_KEY "EN_US"
  void $ lift $ lift $ toggleLoader false
  _ <- pure $ firebaseLogEvent "ny_user_enter_mob_num_scn_view"
  flow <- enterMobileNumberScreen
  case flow of
    GoToAccountSetUp state -> do
            void $ lift $ lift $ loaderText (getString VERIFYING_OTP) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)  -- TODO : Handlde Loader in IOS Side
            void $ lift $ lift $ toggleLoader true
            (resp) <- lift $ lift $  Remote.verifyToken (Remote.makeVerifyOTPReq state.data.otp) state.data.tokenId
            case resp of
              Right resp -> do
                    _ <- pure $ firebaseLogEvent "ny_user_verify_otp"
                    modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen → enterMobileNumberScreen {props {enterOTP = false}})
                    let (VerifyTokenResp response) = resp
                        customerId = ((response.person)^. _id)
                    _ <- lift $ lift $ setLogField "customer_id" $ encode (customerId)
                    setValueToLocalStore CUSTOMER_ID customerId
                    setValueToLocalStore REGISTERATION_TOKEN response.token
                    -- currentFlowStatus
                    enterMobileNumberScreenFlow
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
    GoToChooseLanguage state ->
        chooseLanguageScreenFlow

chooseLanguageScreenFlow :: FlowBT String Unit
chooseLanguageScreenFlow = do
  lift $ lift $ doAff do liftEffect hideSplash
  setValueToLocalStore LANGUAGE_KEY "EN_US"
  _ <- pure $ firebaseLogEvent "ny_user_choose_lang_scn_view"
  flow <- chooseLanguageScreen
  case flow of
    NextScreen language -> do
                            setValueToLocalStore LANGUAGE_KEY language
                            _ <- lift $ lift $ liftFlow $(firebaseLogEventWithParams "ny_user_lang_choose" "language" (language))
                            enterMobileNumberScreenFlow
    Refresh state -> chooseLanguageScreenFlow

accountSetUpScreenFlow :: FlowBT String Unit
accountSetUpScreenFlow = do
  flow <- accountSetUpScreen
  case flow of
    GO_HOME state -> do
      void $ lift $ lift $ toggleLoader false
      let gender = getGenderValue state.data.gender
      resp <- lift $ lift $ Remote.updateProfile (Remote.makeUpdateProfileRequest (Just state.data.name) gender Nothing)
      case resp of
        Right response -> do
          setValueToLocalStore USER_NAME state.data.name
          case gender of
            Just value -> modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data{settingSideBar{gender = Just value}}})
            Nothing    -> pure unit
          _ <- pure $ firebaseLogEvent "ny_user_onboarded"
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

permissionScreenFlow :: String -> FlowBT String Unit
permissionScreenFlow triggertype = do
  _ <- pure $ hideKeyboardOnNavigation true
  flow <- permissionScreen triggertype
  permissionConditionA <- lift $ lift $ liftFlow $ isLocationPermissionEnabled unit
  permissionConditionB <- lift $ lift $ liftFlow $ isLocationEnabled unit
  internetCondition <- lift $ lift $ liftFlow $ isInternetAvailable unit
  case flow of
    REFRESH_INTERNET -> do
        if (os == "IOS") then pure unit
          else if not internetCondition then permissionScreenFlow "INTERNET_ACTION"
        --   else currentFlowStatus
          else enterMobileNumberScreenFlow
    TURN_ON_GPS -> if not internetCondition then permissionScreenFlow "INTERNET_ACTION" else pure unit
    TURN_ON_INTERNET ->
        case (getValueToLocalStore USER_NAME == "__failed") of
            true -> pure unit
            _ -> if (os == "IOS") then pure unit
                 else if (not (permissionConditionA && permissionConditionB) )then permissionScreenFlow "LOCATION_DISABLED"
                --  else currentFlowStatus
                 else enterMobileNumberScreenFlow

getGenderValue :: Maybe Gender.Gender -> Maybe String
getGenderValue gender =
  case gender of
    Just value -> case value of
      Gender.MALE -> Just "MALE"
      Gender.FEMALE -> Just "FEMALE"
      Gender.OTHER -> Just "OTHER"
      _ -> Just "PREFER_NOT_TO_SAY"
    Nothing -> Nothing