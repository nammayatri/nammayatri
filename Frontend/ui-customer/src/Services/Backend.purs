{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Services.Backend where

import Services.API

import Accessor (_deviceToken)
import Common.Types.App (Version(..), SignatureAuthData(..), LazyCheck(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..))
import Data.Array ((!!), take, any)
import Common.Types.App (Version(..), SignatureAuthData(..), LazyCheck (..), FeedbackAnswer)
import Data.Either (Either(..), either)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Engineering.Helpers.Commons (liftFlow, os, bundleVersion)
import Engineering.Helpers.Commons (liftFlow, os, bundleVersion, isPreviousVersion)
import Engineering.Helpers.Utils as EHU
import Foreign.Generic (encode)
import Helpers.Utils (decodeError, getTime, getPreviousVersion)
import JBridge (Locations, factoryResetApp, setKeyInSharedPrefKeys, toast, drawRoute, toggleBtnLoader)
import JBridge (factoryResetApp, setKeyInSharedPrefKeys, toast, removeAllPolylines, stopChatListenerService, MapRouteConfig)
import Juspay.OTP.Reader as Readers
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog)
import ModifyScreenState (modifyScreenState)
import Prelude (Unit, bind, discard, map, pure, unit, void, ($), ($>), (&&), (*>), (<<<), (=<<), (==), (<=), (||), show, (<>), (/=), when)
import Presto.Core.Types.API (Header(..), Headers(..), ErrorResponse)
import Presto.Core.Types.Language.Flow (Flow, APIResult, callAPI, doAff, loadS)
import Screens.Types (AccountSetUpScreenState(..), HomeScreenState(..), NewContacts, DisabilityT(..), Address, Stage(..))
import Services.Config as SC
import Storage (getValueToLocalStore, deleteValueFromLocalStore, getValueToLocalNativeStore, KeyStore(..), setValueToLocalStore)
import Tracker (trackApiCallFlow, trackExceptionFlow)
import Tracker.Labels (Label(..))
import Tracker.Types as Tracker
import Types.App (GlobalState(..), FlowBT, ScreenType(..))
import Types.App (GlobalState, FlowBT, ScreenType(..))
import Types.EndPoint as EP
import Constants as Constants

getHeaders :: String -> Boolean -> Flow GlobalState Headers
getHeaders val isGzipCompressionEnabled = do
    regToken <- loadS $ show REGISTERATION_TOKEN
    pure $ Headers $ [   Header "Content-Type" "application/json",
                        Header "x-client-version" (getValueToLocalStore VERSION_NAME),
                        Header "x-bundle-version" (getValueToLocalStore BUNDLE_VERSION),
                        Header "session_id" (getValueToLocalStore SESSION_ID),
                        Header "x-device" (getValueToLocalNativeStore DEVICE_DETAILS)
                    ] <> case regToken of
                        Nothing -> []
                        Just token -> [Header "token" token]
                      <> if isGzipCompressionEnabled then [Header "Accept-Encoding" "gzip"] else []
                      <> if val /= "" then [Header "x-f-token" val] else []

getHeaders' :: String -> Boolean -> FlowBT String Headers
getHeaders' val isGzipCompressionEnabled = do
    regToken <- lift $ lift $ loadS $ show REGISTERATION_TOKEN
    lift $ lift $ pure $ Headers $ [   Header "Content-Type" "application/json",
                        Header "x-client-version" (getValueToLocalStore VERSION_NAME),
                        Header "x-bundle-version" (getValueToLocalStore BUNDLE_VERSION),
                        Header "session_id" (getValueToLocalStore SESSION_ID),
                        Header "x-device" (getValueToLocalNativeStore DEVICE_DETAILS)
                    ] <> case regToken of
                        Nothing -> []
                        Just token -> [Header "token" token]
                      <> if isGzipCompressionEnabled then [Header "Accept-Encoding" "gzip"] else []
                      <> if val /= "" then [Header "x-f-token" val] else []



----------------------------------------------------------- API Results & BT Functions-------------------------------------------------------------------------------------------------

withAPIResult url f flow = do
  let start = getTime unit
  resp <- either (pure <<< Left) (pure <<< Right <<< f <<< _.response) =<< flow
  let end = getTime unit
  _ <- pure $ printLog "withAPIResult url" url
  case resp of
    Right res -> void $ pure $ printLog "success resp" res
    Left (err) -> do
      _ <- pure $ toggleBtnLoader "" false
      let errResp = err.response
      _ <- pure $ printLog "error resp" errResp
      let userMessage = decodeError errResp.errorMessage "errorMessage"
      let codeMessage = decodeError errResp.errorMessage "errorCode"
      if (err.code == 401 && (codeMessage == "INVALID_TOKEN" || codeMessage == "TOKEN_EXPIRED")) || (err.code == 400 && codeMessage == "TOKEN_EXPIRED") then do
        _ <- pure $ deleteValueFromLocalStore REGISTERATION_TOKEN
        _ <- pure $ deleteValueFromLocalStore REGISTRATION_APPROVED
        _ <- liftFlow $ stopChatListenerService
        _ <- pure $ factoryResetApp ""
        pure unit -- default if it fails
        else pure unit -- default if it fails
  pure resp

withAPIResultBT url f errorHandler flow = do
  let start = getTime unit
  resp <- either (pure <<< Left) (pure <<< Right <<< f <<< _.response) =<< flow
  let end = getTime unit
  _ <- pure $ printLog "withAPIResultBT url" url
  case resp of
    Right res -> do
      _ <- pure $ printLog "success resp" res
      pure res
    Left err -> do
      _ <- pure $ toggleBtnLoader "" false
      let errResp = err.response
      let userMessage = decodeError errResp.errorMessage "errorMessage"
      let codeMessage = decodeError errResp.errorMessage "errorCode"
      _ <- pure $ printLog "error resp" errResp
      if (err.code == 401 && (codeMessage == "INVALID_TOKEN" || codeMessage == "TOKEN_EXPIRED")) || (err.code == 400 && codeMessage == "TOKEN_EXPIRED") then do
          deleteValueFromLocalStore REGISTERATION_TOKEN
          deleteValueFromLocalStore REGISTRATION_APPROVED
          lift $ lift $ liftFlow $ stopChatListenerService
          pure $ factoryResetApp ""
        else pure unit
      errorHandler err

---------------------------------------------------------------TriggerOTPBT Function---------------------------------------------------------------------------------------------------
triggerOTPBT :: TriggerOTPReq → FlowBT String TriggerOTPResp
triggerOTPBT payload = do
    config <- EHU.getAppConfig Constants.appConfig
    when config.features.enableAutoReadOtp $ void $ lift $ lift $ doAff Readers.initiateSMSRetriever
    headers <- getHeaders' "" false
    withAPIResultBT (EP.triggerOTP "") (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
    where
    errorHandler errorPayload = do
        let errResp = errorPayload.response
        let codeMessage = decodeError errResp.errorMessage "errorCode"
        if (errorPayload.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then
            pure $ toast (getString OTP_RESENT_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER)
            else pure $ toast (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
        modifyScreenState $ ChooseLanguageScreenStateType (\chooseLanguage -> chooseLanguage { props {btnActive = false} })
        BackT $ pure GoBack


makeTriggerOTPReq :: String -> String -> String -> TriggerOTPReq
makeTriggerOTPReq mobileNumber countryCode otpChannel=
    let merchant = SC.getMerchantId ""
    in TriggerOTPReq
    {
      "mobileNumber"      : mobileNumber,
      "mobileCountryCode" : countryCode,
      "merchantId" : if merchant == "NA" then getValueToLocalNativeStore MERCHANT_ID else merchant,
      "otpChannel" : otpChannel
    }

---------------------------------------------------------------TriggerSignatureOTPBT Function---------------------------------------------------------------------------------------------------

triggerSignatureBasedOTP :: SignatureAuthData → Flow GlobalState (Either ErrorResponse TriggerSignatureOTPResp)
triggerSignatureBasedOTP (SignatureAuthData signatureAuthData) = do
    Headers headers <- getHeaders "" false
    withAPIResult (EP.triggerSignatureOTP "") unwrapResponse $ callAPI (Headers (headers <> [Header "x-sdk-authorization" signatureAuthData.signature])) (TriggerSignatureOTPReq signatureAuthData.authData)
    where
        unwrapResponse (x) = x

----------------------------------------------------------- ResendOTPBT Function ------------------------------------------------------------------------------------------------------

resendOTPBT :: String -> FlowBT String ResendOTPResp
resendOTPBT token = do
     headers <- getHeaders' "" false
     withAPIResultBT (EP.resendOTP token) (\x → x) errorHandler (lift $ lift $ callAPI headers (ResendOTPRequest token))
    where
    errorHandler  errorPayload  = do
        let errResp = errorPayload.response
        let codeMessage = decodeError errResp.errorMessage "errorCode"
        if ( errorPayload.code == 400 && codeMessage == "AUTH_BLOCKED") then
            pure $ toast (getString OTP_RESENT_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER)
            else pure $ toast (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
        BackT $ pure GoBack



-------------------------------------------------------------VerifyTokenBT Function----------------------------------------------------------------------------------------------------
verifyTokenBT :: VerifyTokenReq -> String -> FlowBT String VerifyTokenResp
verifyTokenBT payload token = do
    headers <- getHeaders' (payload ^. _deviceToken) false
    withAPIResultBT (EP.verifyToken token) (\x → x) errorHandler (lift $ lift $ callAPI headers (VerifyTokenRequest token payload))
    where
    errorHandler errorPayload = do
        let errResp = errorPayload.response
        let codeMessage = decodeError errResp.errorMessage "errorCode"
        if ( errorPayload.code == 400 && codeMessage == "TOKEN_EXPIRED") then
            pure $ toast (getString OTP_PAGE_HAS_BEEN_EXPIRED_PLEASE_REQUEST_OTP_AGAIN)
            else if ( errorPayload.code == 400 && codeMessage == "INVALID_AUTH_DATA") then do
                modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> enterMobileNumber{props{wrongOTP = true, btnActiveOTP = false}})
                void $ lift $ lift $ EHU.toggleLoader false
                pure $ toast "INVALID_AUTH_DATA"
            else if ( errorPayload.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then
                pure $ toast (getString OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER)
            else pure $ toast (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
        BackT $ pure GoBack

-- verifyTokenBT :: VerifyTokenReq -> String -> FlowBT String VerifyTokenResp
verifyToken payload token = do
    headers <- getHeaders (payload ^. _deviceToken) false
    withAPIResult (EP.verifyToken token) unwrapResponse $  callAPI headers (VerifyTokenRequest token payload)
    where
        unwrapResponse (x) = x

makeVerifyOTPReq :: String -> String -> VerifyTokenReq
makeVerifyOTPReq otp defaultId =
    let token = getValueToLocalNativeStore FCM_TOKEN
        deviceToken = if any (_ == token) ["__failed", "", " ", "null", "(null)"] then defaultId <> token else token
    in
        VerifyTokenReq {
            "otp": otp,
            "deviceToken": deviceToken,
            "whatsappNotificationEnroll": OPT_IN
        }

-------------------------------------------------------------Logout BT Funtion---------------------------------------------------------------------------------------------------------

logOutBT :: LogOutReq -> FlowBT String LogOutRes
logOutBT payload = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.logout "") (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack


------------------------------------------------------------------------ SearchLocationBT Function ------------------------------------------------------------------------------------

searchLocationBT :: SearchLocationReq -> FlowBT String SearchLocationResp
searchLocationBT payload = do
  headers <- getHeaders' "" true
  withAPIResultBT (EP.autoComplete "") (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
  where
  errorHandler errorPayload  = do
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage  = SearchLocationModel}})
                BackT $ pure GoBack


makeSearchLocationReq :: String -> Number -> Number -> Int -> String -> String-> SearchLocationReq
makeSearchLocationReq input lat lng radius language components = SearchLocationReq {
    "input" : input,
    "location" : (show lat <> "," <> show lng),
    "radius" : radius,
    "components" : components,
    "language" : language,
    "sessionToken" : Nothing,
    "strictbounds": Nothing,
    "origin" : LatLong {
            "lat" : lat,
            "lon" : lng
            }
    }

------------------------------------------------------------------------ OnCallBT Function ------------------------------------------------------------------------------------

onCallBT :: OnCallReq -> FlowBT String OnCallRes
onCallBT payload = do
  headers <- getHeaders' "" false
  withAPIResultBT (EP.onCall "") (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
  where
    errorHandler errorPayload = BackT $ pure GoBack

makeOnCallReq :: String -> String -> OnCallReq
makeOnCallReq rideID callType = OnCallReq {
    "rideId" : rideID,
    "callType" : callType
}

------------------------------------------------------------------------ PlaceDetailsBT Function --------------------------------------------------------------------------------------
placeDetailsBT :: PlaceDetailsReq -> FlowBT String PlaceDetailsResp
placeDetailsBT (PlaceDetailsReq id) = do
    headers <- lift $ lift $ getHeaders "" true
    withAPIResultBT (EP.placeDetails id) (\x → x) errorHandler (lift $ lift $ callAPI headers (PlaceDetailsReq id))
    where
    errorHandler errorPayload  = do
        pure $ toast (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
        _ <- lift $ lift $ EHU.toggleLoader false
        BackT $ pure GoBack

-- ------------------------------------------------------------------------ GetCoordinatesBT Function --------------------------------------------------------------------------------------
-- getCoordinatesBT :: GetCoordinatesReq -> FlowBT String GetCoordinatesResp
-- getCoordinatesBT (GetCoordinatesReq id language) = do
--     headers <- lift $ lift $ getHeaders ""
--     withAPIResultBT (EP.getCoordinates id language) (\x → x) errorHandler (lift $ lift $ callAPI headers (GetCoordinatesReq id language))
--     where
--     errorHandler errorPayload  = BackT $ pure GoBack


------------------------------------------------------------------------ RideSearchBT Function ----------------------------------------------------------------------------------------
rideSearchBT :: SearchReq -> FlowBT String SearchRes
rideSearchBT payload = do
        headers <- getHeaders' "" true
        withAPIResultBT (EP.searchReq "") (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
    where
      errorHandler errorPayload = do
            if errorPayload.code == 400 then
                pure $ toast (getString RIDE_NOT_SERVICEABLE)
              else pure $ toast (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props{currentStage = SearchLocationModel}})
            _ <- pure $ setValueToLocalStore LOCAL_STAGE "SearchLocationModel"
            BackT $ pure GoBack


makeRideSearchReq :: Number -> Number -> Number -> Number -> Address -> Address -> SearchReq
makeRideSearchReq slat slong dlat dlong srcAdd desAdd=
     SearchReq { "contents" : OneWaySearchReq{
                                               "destination" : SearchReqLocation {
                                                        "gps" : LatLong {
                                                            "lat" : dlat ,
                                                            "lon" : dlong
                                                            },
                                                        "address" : (LocationAddress desAdd)
                                               },
                                               "origin" : SearchReqLocation {
                                                "gps" : LatLong {
                                                            "lat" : slat ,
                                                            "lon" : slong
                                                },"address" : (LocationAddress srcAdd)
                                               }
                                              },
                 "fareProductType" : "ONE_WAY"
                }


------------------------------------------------------------------------ GetQuotes Function -------------------------------------------------------------------------------------------
getQuotes searchId = do
        headers <- getHeaders "" true
        withAPIResult (EP.getQuotes searchId) unwrapResponse $ callAPI headers (GetQuotesReq searchId)
    where
        unwrapResponse (x) = x

------------------------------------------------------------------------ RideConfirm Function ---------------------------------------------------------------------------------------
rideConfirm quoteId = do
        headers <- getHeaders "" false
        withAPIResult (EP.confirmRide quoteId) unwrapResponse $ callAPI headers (ConfirmRequest quoteId)
    where
        unwrapResponse (x) = x

------------------------------------------------------------------------ SelectEstimateBT Function ------------------------------------------------------------------------------------

selectEstimateBT :: DEstimateSelect -> String -> FlowBT String SelectEstimateRes
selectEstimateBT payload estimateId = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.selectEstimate estimateId) (\x → x) errorHandler (lift $ lift $ callAPI headers (SelectEstimateReq estimateId payload))
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack

selectEstimate payload estimateId = do
        headers <- getHeaders "" false
        withAPIResult (EP.selectEstimate estimateId) unwrapResponse $ callAPI headers (SelectEstimateReq estimateId payload)
    where
        unwrapResponse (x) = x

makeEstimateSelectReq :: Boolean -> Maybe Int -> DEstimateSelect
makeEstimateSelectReq isAutoAssigned tipForDriver= DEstimateSelect {
      "customerExtraFee": tipForDriver,
      "autoAssignEnabled": isAutoAssigned,
      "autoAssignEnabledV2": isAutoAssigned
    }

------------------------------------------------------------------------ SelectList Function ------------------------------------------------------------------------------------------
selectList estimateId = do
        headers <- getHeaders "" true
        withAPIResult (EP.selectList estimateId) unwrapResponse $ callAPI headers (SelectListReq estimateId)
    where
        unwrapResponse (x) = x

------------------------------------------------------------------------ RideBooking Function -----------------------------------------------------------------------------------------
rideBooking bookingId = do
        headers <- getHeaders "" true
        withAPIResult (EP.ridebooking bookingId) unwrapResponse $ callAPI headers (RideBookingReq bookingId)
    where
        unwrapResponse (x) = x

------------------------------------------------------------------------ CancelRideBT Function ----------------------------------------------------------------------------------------
cancelRideBT :: CancelReq -> String -> FlowBT String CancelRes
cancelRideBT payload bookingId = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.cancelRide bookingId) (\x → x) errorHandler (lift $ lift $ callAPI headers (CancelRequest payload bookingId))
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack

makeCancelRequest :: HomeScreenState -> CancelReq
makeCancelRequest state = CancelReq {
    "additionalInfo" : Just state.props.cancelDescription
  , "reasonCode" : state.props.cancelReasonCode
  , "reasonStage" : "OnAssign"
  }

------------------------------------------------------------------------ CallDriver Function ------------------------------------------------------------------------------------------

callDriverBT :: String -> FlowBT String CallDriverRes
callDriverBT rideId = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.callCustomerToDriver rideId) (\x → x) errorHandler (lift $ lift $ callAPI headers (CallDriverReq rideId))
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack


------------------------------------------------------------------------ Feedback Function --------------------------------------------------------------------------------------------

rideFeedbackBT :: FeedbackReq -> FlowBT String FeedbackRes
rideFeedbackBT payload = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.feedback "") (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack

makeFeedBackReq :: Int -> String -> String -> FeedbackReq
makeFeedBackReq rating rideId feedback = FeedbackReq
    {   "rating" : rating
    ,   "rideId" : rideId
    ,   "feedbackDetails" : feedback
    }


----------------------------------------------------------------------- RideBooking BT Function ---------------------------------------------------------------------------------------
rideBookingBT :: String -> FlowBT String RideBookingRes
rideBookingBT bookingId = do
        headers <- getHeaders' "" true
        withAPIResultBT (EP.ridebooking bookingId) (\x → x) errorHandler (lift $ lift $ callAPI headers  (RideBookingReq bookingId))
    where
        errorHandler errorPayload = do
            BackT $ pure GoBack

rideBookingList limit offset onlyActive = do
        headers <- getHeaders "" true
        withAPIResult (EP.rideBookingList limit offset onlyActive)  unwrapResponse $ callAPI headers (RideBookingListReq limit offset onlyActive)
    where
        unwrapResponse (x) = x


getProfileBT :: String -> FlowBT String GetProfileRes
getProfileBT _  = do
        headers <- getHeaders' "" true
        withAPIResultBT (EP.profile "") (\x → x) errorHandler (lift $ lift $ callAPI headers (GetProfileReq))
    where
    errorHandler (errorPayload) =  do
        BackT $ pure GoBack

-- updateProfileBT :: UpdateProfileReq -> FlowBT String UpdateProfileRes
updateProfile (UpdateProfileReq payload) = do
        headers <- getHeaders "" false
        withAPIResult (EP.profile "") unwrapResponse $ callAPI headers (UpdateProfileReq payload)
    where
        unwrapResponse (x) = x

mkUpdateProfileRequest :: LazyCheck -> UpdateProfileReq
mkUpdateProfileRequest _ =
    UpdateProfileReq{
          middleName : Nothing
        , lastName : Nothing
        , deviceToken : Nothing
        , firstName : Nothing
        , email : Nothing
        , referralCode : Nothing
        , gender : Nothing
        , language : Just case getValueToLocalNativeStore LANGUAGE_KEY of
            "EN_US" -> "ENGLISH"
            "KN_IN" -> "KANNADA"
            "HI_IN" -> "HINDI"
            "ML_IN" -> "MALAYALAM"
            "BN_IN" -> "BENGALI"
            "TA_IN" -> "TAMIL"
            _       -> "ENGLISH"
        , clientVersion : Nothing
        , bundleVersion : Nothing
        , disability : Nothing
        , hasDisability : Nothing
    }

editProfileRequest :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe Boolean -> Maybe Disability -> UpdateProfileReq
editProfileRequest firstName middleName lastName emailID gender hasDisability disabilityType =
    UpdateProfileReq{
          middleName : middleName
        , lastName : lastName
        , deviceToken : Nothing
        , firstName : firstName
        , email : emailID
        , referralCode : Nothing
        , gender : gender
        , language : Just case getValueToLocalNativeStore LANGUAGE_KEY of
            "EN_US" -> "ENGLISH"
            "KN_IN" -> "KANNADA"
            "HI_IN" -> "HINDI"
            "ML_IN" -> "MALAYALAM"
            "BN_IN" -> "BENGALI"
            "TA_IN" -> "TAMIL"
            _       -> "ENGLISH"
        , clientVersion : Nothing
        , bundleVersion : Nothing
        , disability : disabilityType
        , hasDisability : hasDisability
    }

mkDisabilityData :: DisabilityT -> String -> Disability
mkDisabilityData selectedDisability otherDisabilityDescription = 
    Disability{
      id : selectedDisability.id 
    , tag : selectedDisability.tag 
    , description : case selectedDisability.tag of
        "OTHER" -> otherDisabilityDescription
        _       -> selectedDisability.description
    }

placeNameBT :: GetPlaceNameReq -> FlowBT String GetPlaceNameResp
placeNameBT payload = do
     headers <- lift $ lift $ getHeaders "" true
     withAPIResultBT (EP.getPlaceName "") (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
    where
    errorHandler errorPayload = BackT $ pure GoBack

makePlaceNameReq :: Number -> Number -> String -> GetPlaceNameReq
makePlaceNameReq lat lng language = GetPlaceNameReq
    {"sessionToken" : Just "",
      "language" : Just language,
      "getBy" : GetPlaceNameBy {
          "tag" : "ByLatLong",
          "contents" :LatLongType ( LatLong {
              "lat" : lat,
              "lon" : lng
          })
      }
    }

makePlaceNameReqByPlaceId :: String -> String -> GetPlaceNameReq
makePlaceNameReqByPlaceId placeId language = GetPlaceNameReq
    {"sessionToken" : Just "",
      "language" : Just language,
      "getBy" : GetPlaceNameBy {
          "tag" : "ByPlaceId",
          "contents" : (PlaceId placeId)
      }
    }

getRouteBT :: String -> GetRouteReq -> FlowBT String GetRouteResp
getRouteBT routeState body = do
     headers <- lift $ lift $ getHeaders "" true
     withAPIResultBT (EP.getRoute routeState) (\x → x) errorHandler (lift $ lift $ callAPI headers (RouteReq routeState body))
    where
    errorHandler errorPayload = BackT $ pure GoBack

makeGetRouteReq :: Number -> Number -> Number -> Number -> GetRouteReq
makeGetRouteReq slat slng dlat dlng = GetRouteReq {
    "waypoints": [
      LatLong {
          "lon": slng,
          "lat": slat
      },
      LatLong{
          "lon": dlng,
          "lat": dlat
      }],
    "mode": Just "CAR",
    "calcPoints": true
}

walkCoordinate :: Number -> Number -> Number -> Number -> Locations
walkCoordinate srcLat srcLon destLat destLong = {
    "points": [
      {
        "lat": srcLat,
        "lng": srcLon
      },
      {
        "lat": destLat,
        "lng": destLong
      }
    ]
}

walkCoordinates :: Snapped -> Locations
walkCoordinates (Snapped points) =
  { "points": map (\(LatLong item) -> { "lat": item.lat, "lng": item.lon }) points
  }

postContactsReq :: (Array NewContacts) -> EmergContactsReq
postContactsReq contacts = EmergContactsReq {
  "defaultEmergencyNumbers" : map (\item -> ContactDetails {
      "mobileNumber": item.number,
      "name": item.name,
      "mobileCountryCode": "+91"
  }) contacts
}

emergencyContactsBT :: EmergContactsReq -> FlowBT String EmergContactsResp
emergencyContactsBT req = do
    headers <- lift $ lift $ getHeaders "" false
    withAPIResultBT (EP.emergencyContacts "") (\x → x) errorHandler (lift $ lift $ callAPI headers req)
    where
    errorHandler errorPayload = BackT $ pure GoBack

getEmergencyContactsBT ::  GetEmergContactsReq -> FlowBT String GetEmergContactsResp
getEmergencyContactsBT req = do
    headers <- lift $ lift $ getHeaders "" true
    withAPIResultBT (EP.emergencyContacts "") (\x → x) errorHandler (lift $ lift $ callAPI headers req)
    where
    errorHandler errorPayload = BackT $ pure GoBack

getDriverLocationBT :: String -> FlowBT String GetDriverLocationResp
getDriverLocationBT rideId = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.getCurrentLocation rideId) (\x → x) errorHandler (lift $ lift $ callAPI headers (GetDriverLocationReq rideId))
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack

getDriverLocation rideId = do
        headers <- getHeaders "" false
        withAPIResult (EP.getCurrentLocation rideId) unwrapResponse $ callAPI headers (GetDriverLocationReq rideId)
    where
        unwrapResponse (x) = x

getRoute routeState payload = do
    headers <- getHeaders "" true
    withAPIResult (EP.getRoute routeState) unwrapResponse $  callAPI headers (RouteReq routeState payload)
    where
        unwrapResponse (x) = x

addSavedLocationBT :: SavedReqLocationAPIEntity -> FlowBT String AddLocationResp
addSavedLocationBT payload = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.addLocation "") (\x -> x) errorHandler (lift $ lift $ callAPI headers payload)
    where
    errorHandler errorPayload = do
            BackT $ pure GoBack

getSavedLocationBT :: SavedLocationReq -> FlowBT String SavedLocationsListRes
getSavedLocationBT payload = do
    headers <- getHeaders' "" true
    withAPIResultBT (EP.savedLocation "") (\x -> x) errorHandler (lift $ lift $ callAPI headers payload)
    where
    errorHandler errorPayload = do
            BackT $ pure GoBack

getSavedLocationList payload = do
        headers <- getHeaders "" true
        withAPIResult (EP.savedLocation "") unwrapResponse $ callAPI headers (SavedLocationReq)
    where
        unwrapResponse (x) = x

deleteSavedLocationBT :: DeleteSavedLocationReq -> FlowBT String DeleteSavedLocationRes
deleteSavedLocationBT (DeleteSavedLocationReq tag) = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.deleteLocation tag) (\x -> x) errorHandler (lift $ lift $ callAPI headers (DeleteSavedLocationReq tag))
    where
    errorHandler errorPayload = do
            BackT $ pure GoBack

sendIssueBT :: SendIssueReq -> FlowBT String SendIssueRes
sendIssueBT req = do
    headers <- getHeaders' "" false
    withAPIResultBT ((EP.sendIssue "" )) (\x → x) errorHandler (lift $ lift $ callAPI headers req)
    where
    errorHandler (errorPayload) =  do
            BackT $ pure GoBack

----------------------------------------------------------------------------------------------
drawMapRoute :: Number -> Number -> Number -> Number -> Markers -> String -> String -> String -> Maybe Route -> String -> MapRouteConfig -> FlowBT String (Maybe Route)
drawMapRoute srcLat srcLng destLat destLng markers routeType srcAddress destAddress existingRoute routeAPIType specialLocation = do
    _ <- pure $ removeAllPolylines ""
    case existingRoute of
        Just (Route route) -> do
            let (Snapped points) = route.points
            case points of
                [] -> do
                    (GetRouteResp routeResponse) <- getRouteBT routeAPIType (makeGetRouteReq srcLat srcLng destLat destLng)
                    callDrawRoute ((routeResponse) !! 0)
                _  -> callDrawRoute existingRoute
        Nothing -> do
            (GetRouteResp routeResponse) <- getRouteBT routeAPIType (makeGetRouteReq srcLat srcLng destLat destLng)
            _ <- pure $ printLog "drawRouteResponse" routeResponse
            let ios = (os == "IOS")
            let route = ((routeResponse) !! 0)
            callDrawRoute route
    where
        callDrawRoute :: Maybe Route -> FlowBT String (Maybe Route)
        callDrawRoute route =
            case route of
                Just (Route routes) ->
                    if (routes.distance <= 50000) then do
                      lift $ lift $ liftFlow $ drawRoute (walkCoordinates routes.points) "LineString" "#323643" true markers.srcMarker markers.destMarker 8 routeType srcAddress destAddress specialLocation
                      pure route
                    else do
                      lift $ lift $ liftFlow $ drawRoute (walkCoordinate srcLat srcLng destLat destLng) "DOT" "#323643" false markers.srcMarker markers.destMarker 8 routeType srcAddress destAddress specialLocation
                      pure route 
                Nothing -> pure route

type Markers = {
    srcMarker :: String,
    destMarker :: String
}

driverTracking :: String -> Markers
driverTracking variant = {
    srcMarker : if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion "") then "ny_ic_auto_map" 
                else if variant == "AUTO_RICKSHAW" then "ic_auto_nav_on_map"
                else "ny_ic_vehicle_nav_on_map",
    destMarker : if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion "") then "src_marker" else "ny_ic_src_marker"
}

rideTracking :: String -> Markers
rideTracking variant = {
    srcMarker : if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion "") then "ny_ic_auto_map" 
                else if variant == "AUTO_RICKSHAW" then "ic_auto_nav_on_map"
                else "ny_ic_vehicle_nav_on_map",
    destMarker : if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion "") then "dest_marker" else "ny_ic_dest_marker"
}

normalRoute ::String -> Markers
normalRoute _ = {
    srcMarker : if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion "") then "src_marker" else "ny_ic_src_marker",
    destMarker : if isPreviousVersion (getValueToLocalStore VERSION_NAME) (getPreviousVersion "") then "dest_marker" else "ny_ic_dest_marker"
}


makeSendIssueReq :: Maybe String ->  Maybe String -> String -> String -> SendIssueReq
makeSendIssueReq email bookingId reason description= SendIssueReq {
    "contactEmail" : email ,
    "rideBookingId" : bookingId ,
    "issue" : Issue {
            "reason" : reason,
            "description" : description
        }
}

originServiceabilityBT :: ServiceabilityReq -> FlowBT String ServiceabilityRes
originServiceabilityBT req = do
    headers <- getHeaders' "" true
    withAPIResultBT ((EP.serviceabilityOrigin "" )) (\x → x) errorHandler (lift $ lift $ callAPI headers req)
    where
    errorHandler (errorPayload) =  do
            BackT $ pure GoBack

destServiceabilityBT :: DestinationServiceabilityReq -> FlowBT String ServiceabilityResDestination
destServiceabilityBT req = do
    headers <- getHeaders' "" true
    withAPIResultBT ((EP.serviceabilityDest "" )) (\x → x) errorHandler (lift $ lift $ callAPI headers req)
    where
    errorHandler (errorPayload) =  do
            BackT $ pure GoBack

makeServiceabilityReq :: Number -> Number -> ServiceabilityReq
makeServiceabilityReq latitude longitude = ServiceabilityReq {
    "location" : LatLong {
                "lat" : latitude,
                "lon" : longitude
            }
    }

makeServiceabilityReqForDest :: Number -> Number -> DestinationServiceabilityReq
makeServiceabilityReqForDest latitude longitude = DestinationServiceabilityReq {
    "location" : LatLong {
                "lat" : latitude,
                "lon" : longitude
            }
    }

---------------------------------------------------------------- flowStatus function -------------------------------------------------------------------
flowStatusBT :: String -> FlowBT String FlowStatusRes
flowStatusBT dummy = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.flowStatus "") (\x → x) errorHandler (lift $ lift $ callAPI headers FlowStatusReq)
    where
        errorHandler errorPayload = do
            BackT $ pure GoBack

---------------------------------------------------------------- notifyFlowEvent function -------------------------------------------------------------------

notifyFlowEvent requestBody = do
    headers <- getHeaders "" false
    withAPIResult (EP.notifyFlowEvent "") unwrapResponse $ callAPI headers requestBody
    where
        unwrapResponse (x) = x

notifyFlowEventBT :: NotifyFlowEventReq -> FlowBT String NotifyFlowEventRes
notifyFlowEventBT requestBody = do
     headers <- lift $ lift $ getHeaders "" false
     withAPIResultBT (EP.notifyFlowEvent "") (\x → x) errorHandler (lift $ lift $ callAPI headers requestBody)
    where
    errorHandler errorPayload = BackT $ pure GoBack

makeNotifyFlowEventReq :: String -> NotifyFlowEventReq
makeNotifyFlowEventReq event = NotifyFlowEventReq { "event" : event }

------------------------------------------------------------------------ CancelEstimate Function ------------------------------------------------------------------------------------

cancelEstimate estimateId = do
    headers <- getHeaders "" false
    withAPIResult (EP.cancelEstimate estimateId) unwrapResponse $ callAPI headers (CancelEstimateReq estimateId)
    where
        unwrapResponse (x) = x

cancelEstimateBT :: String -> FlowBT String CancelEstimateRes
cancelEstimateBT estimateId = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.cancelEstimate estimateId) (\x → x) errorHandler (lift $ lift $ callAPI headers (CancelEstimateReq estimateId))
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack

userSosBT :: UserSosReq -> FlowBT String UserSosRes
userSosBT requestBody = do
     headers <- lift $ lift $ getHeaders "" false
     withAPIResultBT (EP.userSos "") (\x → x) errorHandler (lift $ lift $ callAPI headers requestBody)
    where
    errorHandler errorPayload = BackT $ pure GoBack

userSosStatusBT :: String ->  SosStatus -> FlowBT String UserSosStatusRes
userSosStatusBT sosId requestBody = do
     headers <- lift $ lift $ getHeaders "" false
     withAPIResultBT (EP.userSosStatus sosId) (\x → x) errorHandler (lift $ lift $ callAPI headers (UserSosStatusReq sosId requestBody))
    where
    errorHandler errorPayload = BackT $ pure GoBack

callbackRequestBT :: LazyCheck -> FlowBT String RequestCallbackRes
callbackRequestBT lazyCheck = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.callbackRequest "") (\x → x) errorHandler (lift $ lift $ callAPI headers RequestCallbackReq)
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack

makeUserSosReq :: UserSosFlow -> String -> UserSosReq
makeUserSosReq flow rideId = UserSosReq {
     "flow" : flow,
     "rideId" : rideId
}

createUserSosFlow :: String -> String -> UserSosFlow
createUserSosFlow tag content = UserSosFlow {
    "tag" : tag,
    "contents" : content
}

makeSosStatus :: String -> SosStatus
makeSosStatus sosStatus = SosStatus {
     "status" : sosStatus
}


------------------------------------------------------------------------ Ride Feedback ------------------------------------------------------------------------------------

bookingFeedbackBT :: RideFeedbackReq -> FlowBT String RideFeedbackRes
bookingFeedbackBT payload = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.bookingFeedback "") (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack

makeRideFeedBackReq :: String -> Array FeedbackAnswer -> RideFeedbackReq
makeRideFeedBackReq id feedbackList = RideFeedbackReq
    {   "rideId" : id
    ,   "feedback" : feedbackList
    }

disabilityList :: FlowBT String (Either ErrorResponse GetDisabilityListResp)
disabilityList = do
  headers <- getHeaders' "" false
  lift $ lift $ withAPIResult (EP.disabilityList "") unwrapResponse $ callAPI headers GetDisabilityListReq
  where
    unwrapResponse x = x
------------------------------------------------------------------------ Person Stats ------------------------------------------------------------------------------------

getPersonStatsBT :: String -> FlowBT String PersonStatsRes
getPersonStatsBT _ = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.personStats "") (\x -> x) errorHandler (lift $ lift $ callAPI headers PersonStatsReq)
    where
    errorHandler errorPayload = do
            BackT $ pure GoBack 
 

----------------------------------- fetchIssueList ----------------------------------------

fetchIssueListBT :: String -> FlowBT String FetchIssueListResp
fetchIssueListBT language = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.fetchIssueList language) (\x → x) errorHandler (lift $ lift $ callAPI headers (FetchIssueListReq language))
    where
        errorHandler errorPayload =  do
            BackT $ pure GoBack
    -- pure $ FetchIssueListResp { issues: [IssueReportCustomerListItem { issueReportId: "12345", status: "AWAIT", category: "Lost Item", createdAt: "2023-07-29 12:34:56" },
    -- IssueReportCustomerListItem { issueReportId: "678910", status: "NEW", category: "App Related Issue", createdAt: "2023-07-29 12:34:56"},
    -- IssueReportCustomerListItem { issueReportId: "6732910", status: "NEW", category: "Lost Item", createdAt: "2023-07-28 12:34:56" },
    --     IssueReportCustomerListItem { issueReportId: "678910", status: "NEW", category: "App Related Issue", createdAt: "2023-07-29 12:34:56"},
    -- IssueReportCustomerListItem { issueReportId: "6732910", status: "NEW", category: "Lost Item", createdAt: "2023-07-28 12:34:56" },
    --     IssueReportCustomerListItem { issueReportId: "678910", status: "NEW", category: "App Related Issue", createdAt: "2023-07-29 12:34:56"},
    -- IssueReportCustomerListItem { issueReportId: "6732910", status: "NEW", category: "Lost Item", createdAt: "2023-07-28 12:34:56" },
    --     IssueReportCustomerListItem { issueReportId: "678910", status: "NEW", category: "App Related Issue", createdAt: "2023-07-29 12:34:56"},
    -- IssueReportCustomerListItem { issueReportId: "6732910", status: "NEW", category: "Lost Item", createdAt: "2023-07-28 12:34:56" },
    --     IssueReportCustomerListItem { issueReportId: "678910", status: "NEW", category: "App Related Issue", createdAt: "2023-07-29 12:34:56"},
    -- IssueReportCustomerListItem { issueReportId: "6732910", status: "NEW", category: "Lost Item", createdAt: "2023-07-28 12:34:56" },
    --     IssueReportCustomerListItem { issueReportId: "678910", status: "NEW", category: "App Related Issue", createdAt: "2023-07-29 12:34:56"},
    -- IssueReportCustomerListItem { issueReportId: "6732910", status: "NEW", category: "Lost Item", createdAt: "2023-07-28 12:34:56" },
    -- IssueReportCustomerListItem { issueReportId: "98765", status: "RESOLVED", category: "App Related Issue", createdAt: "2023-06-31 12:34:56" }]}


--------------------------------------------- Driver Report Issue ---------------------------------------------
getCategoriesBT :: String -> FlowBT String GetCategoriesRes
getCategoriesBT language = do
      headers <- getHeaders' "" false
      withAPIResultBT (EP.getCategories language) (\x → x) errorHandler (lift $ lift $ callAPI headers (GetCategoriesReq language))
      where
        errorHandler errorPayload = do
            BackT $ pure GoBack
    -- pure $ GetCategoriesRes { categories : [ Category {
    --     logoUrl: "ny_ic_lost_and_found,https://assets.juspay.in/nammayatri/images/common/ny_ic_lost_and_found.png",
    --     label: "LOST_AND_FOUND",
    --     issueCategoryId: "1",
    --     category: "lost and found"
    -- }, Category {
    --     logoUrl: "ny_ic_driver_related_issue,https://assets.juspay.in/nammayatri/images/common/ny_ic_driver_related_issue.png",
    --     label: "DRIVER_RELATED",
    --     issueCategoryId: "2",
    --     category: "driver related issues"
    -- }, Category {
    --     logoUrl: "ny_ic_app_related_issue,https://assets.juspay.in/nammayatri/images/common/ny_ic_app_related_issue.png",
    --     label: "APP_RELATED",
    --     issueCategoryId: "3",
    --     category: "app related issues"
    -- }, Category{
    --     logoUrl: "ny_ic_ride_related_issue,https://assets.juspay.in/nammayatri/images/common/ny_ic_ride_related_issue.png",
    --     label: "RIDE_RELATED",
    --     issueCategoryId: "4",
    --     category: "ride related issues"
    -- }]}

getOptionsBT :: String -> String -> String -> String -> FlowBT String GetOptionsRes
getOptionsBT language categoryId optionId issueReportId = do
      headers <- getHeaders' "" false
      withAPIResultBT (EP.getOptions categoryId optionId issueReportId language) (\x → x) errorHandler (lift $ lift $ callAPI headers (GetOptionsReq categoryId optionId issueReportId language))
        where
          errorHandler errorPayload = do
            BackT $ pure GoBack
    -- if categoryId /= "" && (optionId == "" && optionId /= "1"  && optionId /= "2")
    --     then (pure $ GetOptionsRes { options : [ Option {
    --             option: "Poor driver behaviour",
    --             label: "OPTION_1",
    --             issueOptionId: "1"
    --         }, Option {
    --             option: "Safety Concerns",
    --             label: "OPTION_2",
    --             issueOptionId: "2"
    --         }, Option {
    --             option: "Hygiene of the auto",
    --             label: "OPTION_#",
    --             issueOptionId: "3"
    --         }, Option {
    --             option: "Call Driver",
    --             label: "CALL_THE_DRIVER",
    --             issueOptionId: "4"
    --         }, Option {
    --             option: "Call Support",
    --             label: "CALL_THE_SUPPORT",
    --             issueOptionId: "5"
    --         }], messages : [Message {id : "1" , message : "Hey XYZ, We’re really sorry to hear you have been facing Driver related issues."}, 
    --             Message {id : "1" , message : "Please select the type of issue you are facing so we can help you out better"}]})
    --     else 
    --         if optionId == "1"  && optionId /= "2"
    --             then
    --                 pure $ GetOptionsRes { options : [ Option {
    --                 option: "Inappropriate Behaviour",
    --                 label: "OPTION_1",
    --                 issueOptionId: "11"
    --             }, Option {
    --                 option: "Abusive Language",
    --                 label: "OPTION_2",
    --                 issueOptionId: "12"
    --             }], messages : [Message {id : "1" , message : "Please select an option that best represents your issue."}]}
    --             else
    --                 pure $ GetOptionsRes { options : [], messages : [Message {id : "1" , message : "Please describe and submit your issue."}]}

postIssueBT :: String -> PostIssueReqBody -> FlowBT String PostIssueRes
postIssueBT language payload = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.postIssue language) (\x -> x) errorHandler (lift $ lift $ callAPI headers (PostIssueReq language payload))
    where
    errorHandler errorPayload = do
        BackT $ pure GoBack
    -- pure $ PostIssueRes {
    --     issueReportId: "12345",
    --     messages : [Message {id : "1" , message : "Will get back to you within 24 hours"}]
    -- }

issueInfoBT :: String -> String -> FlowBT String IssueInfoRes
issueInfoBT language issueId = do
      headers <- getHeaders' "" false
      withAPIResultBT (EP.issueInfo issueId language) (\x -> x) errorHandler (lift $ lift $ callAPI headers (IssueInfoReq issueId language))
        where
          errorHandler errorPayload = do
                BackT $ pure GoBack
    -- if issueId == "678910"
    -- then
    --     pure $ IssueInfoRes { 
    --         mediaFiles      : []
    --         , description   : "Old Chat is this"
    --         , issueReportId : "12345"
    --         , categoryId    : "56789"
    --         , chats : []
    --         , options : []
    --         , categoryLabel : "Lost and found"
    --     }
    -- else
    --     if issueId == "12345"
    --     then
    --         pure $ IssueInfoRes { 
    --             mediaFiles      : []
    --             , description   : "Old Chat is this"
    --             , issueReportId : "12345"
    --             , categoryId    : "56789"
    --             , categoryLabel : "Lost and found"
    --             , chats : [ChatDetail{
    --                 timestamp : "2023-08-14T12:34:04.398981Z",
    --                 content : Just "Bot Text",
    --                 id : "id",
    --                 label : Just "somelabel",
    --                 chatType : "text",
    --                 sender : "Bot"
    --             },
    --             ChatDetail{
    --                 timestamp : "123",
    --                 content : Just "Customer text",
    --                 id : "id",
    --                 label : Just "somelabel",
    --                 chatType : "text",
    --                 sender : "Customer"
    --             },
    --             ChatDetail{
    --                 timestamp : "123",
    --                 content : Just "Closed after 2 hours",
    --                 id : "id",
    --                 label : Just "STILL_HAVE_AN_ISSUE",
    --                 chatType : "text",
    --                 sender : "Customer"
    --             }]
    --             , options : [ ]
    --         }
    --     else
    --         pure $ IssueInfoRes { 
    --             mediaFiles      : []
    --             , description   : "The thing you typed coming from backend"
    --             , issueReportId : "12345"
    --             , categoryId    : "56789"
    --             , chats : []
    --             , options : []
    --             , categoryLabel : "Lost and found"
    --         }

updateIssue :: String -> String -> UpdateIssueReqBody -> FlowBT String UpdateIssueRes
updateIssue language issueId req = do
        -- (pure $ UpdateIssueRes { messages : [Message {id : "1" , message : "Hey XYZ, We’re really sorry to hear you have been facing Driver related issues."}, 
        --         Message {id : "1" , message : "Please select the type of issue you are facing so we can help you out better"}]})
        headers <- getHeaders' "" false
        withAPIResultBT (EP.updateIssue issueId language) (\x → x) errorHandler (lift $ lift $ callAPI headers (UpdateIssueReq issueId language req))
        where
          errorHandler errorPayload = do
                BackT $ pure GoBack

