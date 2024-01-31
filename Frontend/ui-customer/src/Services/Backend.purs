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
import Data.Array ((!!), catMaybes, concat, take, any, singleton, find, filter, length, null, mapMaybe)
import Common.Types.App (Version(..), SignatureAuthData(..), LazyCheck (..), FeedbackAnswer)
import Data.Either (Either(..), either)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Engineering.Helpers.Commons (liftFlow, os, convertUTCtoISC)
import Engineering.Helpers.Commons (liftFlow, os, isPreviousVersion, isInvalidUrl)
import Engineering.Helpers.Utils as EHU
import Foreign.Generic (encode)
import Helpers.Utils (decodeError, getTime)
import JBridge (Locations, factoryResetApp, setKeyInSharedPrefKeys, toast, drawRoute, toggleBtnLoader)
import JBridge (factoryResetApp, setKeyInSharedPrefKeys, toast, removeAllPolylines, stopChatListenerService, MapRouteConfig)
import Juspay.OTP.Reader as Readers
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog)
import ModifyScreenState (modifyScreenState)
import Prelude (Unit, bind, discard, map, pure, unit, void, identity, ($), ($>), (>), (&&), (*>), (<<<), (=<<), (==), (<=), (||), show, (<>), (/=), when)
import Presto.Core.Types.API (Header(..), Headers(..), ErrorResponse)
import Presto.Core.Types.Language.Flow (Flow, APIResult, callAPI, doAff, loadS) 
import Screens.Types (AccountSetUpScreenState(..), HomeScreenState(..), NewContacts, DisabilityT(..), Address, Stage(..), TicketBookingScreenData(..), TicketServiceData, City(..), PeopleCategoriesRespData)
import Services.Config as SC
import Storage (getValueToLocalStore, deleteValueFromLocalStore, getValueToLocalNativeStore, KeyStore(..), setValueToLocalStore)
import Tracker (trackApiCallFlow, trackExceptionFlow)
import Tracker.Labels (Label(..))
import Tracker.Types as Tracker
import Types.App (GlobalState(..), FlowBT, ScreenType(..))
import Types.EndPoint as EP
import Foreign.Object (empty)
import Data.String as DS
import ConfigProvider as CP
import Locale.Utils
import Debug

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
  if (isInvalidUrl url) then pure $ Left customError
  else do
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
  if (isInvalidUrl url) then errorHandler customError
  else do
    let start = getTime unit
    resp <- either (pure <<< Left) (pure <<< Right <<< f <<< _.response) =<< flow
    let end = getTime unit
    _ <- pure $ printLog "withAPIResultBT url" url
    case resp of
        Right res -> do
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


customError :: ErrorResponse
customError =  { code : 400
  , status : "success"
  , response : {
       error : true
     , errorMessage : "{\"errorCode\" : \"ERROR_OCCURED_TRY_AGAIN\", \"errorMessage\" : \"Error Occured ! Please try again later\"}"
     , userMessage : getString ERROR_OCCURED_TRY_AGAIN
    }
  , responseHeaders : empty
  }

---------------------------------------------------------------TriggerOTPBT Function---------------------------------------------------------------------------------------------------
triggerOTPBT :: TriggerOTPReq → FlowBT String TriggerOTPResp
triggerOTPBT payload = do
    config <- CP.getAppConfigFlowBT CP.appConfig
    when config.feature.enableAutoReadOtp $ void $ lift $ lift $ doAff Readers.initiateSMSRetriever
    headers <- getHeaders' "" false
    withAPIResultBT (EP.triggerOTP "") identity errorHandler (lift $ lift $ callAPI headers payload)
    where
    errorHandler errorPayload = do
        let errResp = errorPayload.response
        let codeMessage = decodeError errResp.errorMessage "errorCode"
        if (errorPayload.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then
            pure $ toast (getString OTP_RESENT_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER)
            else pure $ toast (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
        modifyScreenState $ ChooseLanguageScreenStateType (\chooseLanguage -> chooseLanguage { props {btnActive = false} })
        void $ lift $ lift $ EHU.toggleLoader false
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
     withAPIResultBT (EP.resendOTP token) identity errorHandler (lift $ lift $ callAPI headers (ResendOTPRequest token))
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
    withAPIResultBT (EP.verifyToken token) identity errorHandler (lift $ lift $ callAPI headers (VerifyTokenRequest token payload))
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
        withAPIResultBT (EP.logout "") identity errorHandler (lift $ lift $ callAPI headers payload)
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack


------------------------------------------------------------------------ SearchLocationBT Function ------------------------------------------------------------------------------------

searchLocationBT :: SearchLocationReq -> FlowBT String SearchLocationResp
searchLocationBT payload = do
  headers <- getHeaders' "" true
  withAPIResultBT (EP.autoComplete "") identity errorHandler (lift $ lift $ callAPI headers payload)
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
  withAPIResultBT (EP.onCall "") identity errorHandler (lift $ lift $ callAPI headers payload)
  where
    errorHandler errorPayload = BackT $ pure GoBack

makeOnCallReq :: String -> String -> String -> OnCallReq
makeOnCallReq rideID callType exophoneNumber = OnCallReq {
    "rideId" : rideID,
    "callType" : callType,
    "exophoneNumber" : exophoneNumber
}

------------------------------------------------------------------------ PlaceDetailsBT Function --------------------------------------------------------------------------------------
placeDetailsBT :: PlaceDetailsReq -> FlowBT String PlaceDetailsResp
placeDetailsBT (PlaceDetailsReq id) = do
    headers <- lift $ lift $ getHeaders "" true
    withAPIResultBT (EP.placeDetails id) identity errorHandler (lift $ lift $ callAPI headers (PlaceDetailsReq id))
    where
    errorHandler errorPayload  = do
        pure $ toast (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
        _ <- lift $ lift $ EHU.toggleLoader false
        BackT $ pure GoBack

-- ------------------------------------------------------------------------ GetCoordinatesBT Function --------------------------------------------------------------------------------------
-- getCoordinatesBT :: GetCoordinatesReq -> FlowBT String GetCoordinatesResp
-- getCoordinatesBT (GetCoordinatesReq id language) = do
--     headers <- lift $ lift $ getHeaders ""
--     withAPIResultBT (EP.getCoordinates id language) identity errorHandler (lift $ lift $ callAPI headers (GetCoordinatesReq id language))
--     where
--     errorHandler errorPayload  = BackT $ pure GoBack


------------------------------------------------------------------------ RideSearchBT Function ----------------------------------------------------------------------------------------
rideSearchBT :: SearchReq -> FlowBT String SearchRes
rideSearchBT payload = do
        headers <- getHeaders' "" true
        withAPIResultBT (EP.searchReq "") identity errorHandler (lift $ lift $ callAPI headers payload)
    where
      errorHandler errorPayload = do
            if errorPayload.code == 400 then
                pure $ toast (getString RIDE_NOT_SERVICEABLE)
              else pure $ toast (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props{currentStage = SearchLocationModel}})
            _ <- pure $ setValueToLocalStore LOCAL_STAGE "SearchLocationModel"
            BackT $ pure GoBack


makeRideSearchReq :: Number -> Number -> Number -> Number -> Address -> Address -> SearchReq
makeRideSearchReq slat slong dlat dlong srcAdd desAdd =
    let appConfig = CP.getAppConfig CP.appConfig
    in  SearchReq { "contents" : OneWaySearchReq{
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
                                                  },
                                                  "isReallocationEnabled" : Just appConfig.feature.enableReAllocation
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
        withAPIResultBT (EP.selectEstimate estimateId) identity errorHandler (lift $ lift $ callAPI headers (SelectEstimateReq estimateId payload))
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
        withAPIResultBT (EP.cancelRide bookingId) identity errorHandler (lift $ lift $ callAPI headers (CancelRequest payload bookingId))
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
    withAPIResultBT (EP.callCustomerToDriver rideId) identity errorHandler (lift $ lift $ callAPI headers (CallDriverReq rideId))
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack


------------------------------------------------------------------------ Feedback Function --------------------------------------------------------------------------------------------

rideFeedbackBT :: FeedbackReq -> FlowBT String FeedbackRes
rideFeedbackBT payload = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.feedback "") identity errorHandler (lift $ lift $ callAPI headers payload)
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack

makeFeedBackReq :: Int -> String -> String -> Maybe Boolean -> FeedbackReq
makeFeedBackReq rating rideId feedback wasOfferedAssistance = FeedbackReq
    {   "rating" : rating
    ,   "rideId" : rideId
    ,   "feedbackDetails" : feedback
    ,   "wasOfferedAssistance" : wasOfferedAssistance
    }


----------------------------------------------------------------------- RideBooking BT Function ---------------------------------------------------------------------------------------
rideBookingBT :: String -> FlowBT String RideBookingRes
rideBookingBT bookingId = do
        headers <- getHeaders' "" true
        withAPIResultBT (EP.ridebooking bookingId) identity errorHandler (lift $ lift $ callAPI headers  (RideBookingReq bookingId))
    where
        errorHandler errorPayload = do
            BackT $ pure GoBack

rideBookingList limit offset onlyActive = do
        headers <- getHeaders "" true
        withAPIResult (EP.rideBookingList limit offset onlyActive Nothing)  unwrapResponse $ callAPI headers (RideBookingListReq limit offset onlyActive Nothing)
    where
        unwrapResponse (x) = x


rideBookingListWithStatus limit offset status = do
        headers <- getHeaders "" true
        withAPIResult (EP.rideBookingList limit offset "false" (Just status))  unwrapResponse $ callAPI headers (RideBookingListReq limit offset "false" (Just status))
    where
        unwrapResponse (x) = x

getProfileBT :: String -> FlowBT String GetProfileRes
getProfileBT _  = do
        headers <- getHeaders' "" true
        withAPIResultBT (EP.profile "") identity errorHandler (lift $ lift $ callAPI headers (GetProfileReq))
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
        , language : Just case getLanguageLocale languageKey of
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
        , language : Just case getLanguageLocale languageKey of
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
     withAPIResultBT (EP.getPlaceName "") identity errorHandler (lift $ lift $ callAPI headers payload)
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
     withAPIResultBT (EP.getRoute routeState) identity errorHandler (lift $ lift $ callAPI headers (RouteReq routeState body))
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
    withAPIResultBT (EP.emergencyContacts "") identity errorHandler (lift $ lift $ callAPI headers req)
    where
    errorHandler errorPayload = BackT $ pure GoBack

getEmergencyContactsBT ::  GetEmergContactsReq -> FlowBT String GetEmergContactsResp
getEmergencyContactsBT req = do
    headers <- lift $ lift $ getHeaders "" true
    withAPIResultBT (EP.emergencyContacts "") identity errorHandler (lift $ lift $ callAPI headers req)
    where
    errorHandler errorPayload = BackT $ pure GoBack

getDriverLocationBT :: String -> FlowBT String GetDriverLocationResp
getDriverLocationBT rideId = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.getCurrentLocation rideId) identity errorHandler (lift $ lift $ callAPI headers (GetDriverLocationReq rideId))
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
    withAPIResultBT ((EP.sendIssue "" )) identity errorHandler (lift $ lift $ callAPI headers req)
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

data TrackingType = RIDE_TRACKING | DRIVER_TRACKING


getRouteMarkers :: String -> City -> TrackingType -> Markers
getRouteMarkers variant city trackingType = 
  { srcMarker : mkSrcMarker ,
    destMarker : mkDestMarker 
  }
  where 
    mkSrcMarker :: String 
    mkSrcMarker = getCitySpecificMarker city
    
    getCitySpecificMarker :: City -> String
    getCitySpecificMarker city = case city of
        Hyderabad -> "ny_ic_black_yellow_auto"
        _         -> getMarkerVariant variant

    getMarkerVariant :: String -> String
    getMarkerVariant variant =
        case variant of
            "AUTO_RICKSHAW" -> "ic_auto_nav_on_map"
            _               -> "ny_ic_vehicle_nav_on_map"
    
    mkDestMarker :: String
    mkDestMarker = 
        case trackingType of 
            RIDE_TRACKING -> "ny_ic_dest_marker"
            DRIVER_TRACKING -> "ny_ic_src_marker"
      



normalRoute ::String -> Markers
normalRoute _ = {
    srcMarker : "ny_ic_src_marker",
    destMarker : "ny_ic_dest_marker"
}


makeSendIssueReq :: Maybe String ->  Maybe String -> String -> String -> Maybe Boolean -> SendIssueReq
makeSendIssueReq email bookingId reason description nightSafety = SendIssueReq {
    "contactEmail" : email ,
    "rideBookingId" : bookingId ,
    "issue" : Issue {
            "reason" : reason,
            "description" : description
        },
    "nightSafety" : nightSafety
}

originServiceabilityBT :: ServiceabilityReq -> FlowBT String ServiceabilityRes
originServiceabilityBT req = do
    headers <- getHeaders' "" true
    withAPIResultBT ((EP.serviceabilityOrigin "" )) identity errorHandler (lift $ lift $ callAPI headers req)
    where
    errorHandler (errorPayload) =  do
            BackT $ pure GoBack

destServiceabilityBT :: DestinationServiceabilityReq -> FlowBT String ServiceabilityResDestination
destServiceabilityBT req = do
    headers <- getHeaders' "" true
    withAPIResultBT ((EP.serviceabilityDest "" )) identity errorHandler (lift $ lift $ callAPI headers req)
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
        withAPIResultBT (EP.flowStatus "") identity errorHandler (lift $ lift $ callAPI headers FlowStatusReq)
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
     withAPIResultBT (EP.notifyFlowEvent "") identity errorHandler (lift $ lift $ callAPI headers requestBody)
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
        withAPIResultBT (EP.cancelEstimate estimateId) identity errorHandler (lift $ lift $ callAPI headers (CancelEstimateReq estimateId))
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack

userSosBT :: UserSosReq -> FlowBT String UserSosRes
userSosBT requestBody = do
     headers <- lift $ lift $ getHeaders "" false
     withAPIResultBT (EP.userSos "") identity errorHandler (lift $ lift $ callAPI headers requestBody)
    where
    errorHandler errorPayload = BackT $ pure GoBack

userSosStatusBT :: String ->  SosStatus -> FlowBT String UserSosStatusRes
userSosStatusBT sosId requestBody = do
     headers <- lift $ lift $ getHeaders "" false
     withAPIResultBT (EP.userSosStatus sosId) identity errorHandler (lift $ lift $ callAPI headers (UserSosStatusReq sosId requestBody))
    where
    errorHandler errorPayload = BackT $ pure GoBack

callbackRequestBT :: LazyCheck -> FlowBT String RequestCallbackRes
callbackRequestBT lazyCheck = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.callbackRequest "") identity errorHandler (lift $ lift $ callAPI headers RequestCallbackReq)
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
    withAPIResultBT (EP.bookingFeedback "") identity errorHandler (lift $ lift $ callAPI headers payload)
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

getTicketPlaceServicesBT :: String -> FlowBT String TicketServicesResponse
getTicketPlaceServicesBT placeId = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.ticketPlaceServices placeId) (\x -> x) errorHandler (lift $ lift $ callAPI headers (TicketServiceReq placeId))
    where
    errorHandler errorPayload = do
            BackT $ pure GoBack 

getTicketPlacesBT :: String -> FlowBT String TicketPlaceResponse
getTicketPlacesBT _ = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.ticketPlaces "") (\x -> x) errorHandler (lift $ lift $ callAPI headers TicketPlaceReq)
    where
    errorHandler errorPayload = do
      BackT $ pure GoBack 

bookTicketsBT :: TicketBookingReq -> String -> FlowBT String CreateOrderRes
bookTicketsBT payload placeId = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.ticketPlaceBook placeId) (\x -> x) errorHandler (lift $ lift $ callAPI headers (TicketBookingRequest placeId payload))
    where
    errorHandler errorPayload = do
            BackT $ pure GoBack

mkBookingTicketReq :: TicketBookingScreenData -> TicketBookingReq -- TODO:: Refactor and make it generic without having state for serviceType
mkBookingTicketReq ticketBookingScreenData = 
  TicketBookingReq 
    { services : createTicketServiceRequest ticketBookingScreenData.servicesInfo,
      visitDate : convertUTCtoISC ticketBookingScreenData.dateOfVisit "YYYY-MM-DD"
    }
  where
    createTicketServiceRequest :: Array TicketServiceData -> Array TicketService
    createTicketServiceRequest services = mapMaybe createPeopleCategoriesRespRequest services

    createPeopleCategoriesRespRequest :: TicketServiceData -> Maybe TicketService
    createPeopleCategoriesRespRequest service = 
        let maybeBusinessHour = maybe Nothing (findSelectedBusinessHour service) service.selectedBHid
        in maybe Nothing (getTicketServiceReqElement service) maybeBusinessHour

    findSelectedBusinessHour service bhId = find (\businessHour -> businessHour.bhourId == bhId) service.businessHours
    
    getValidCategories category = do
        let filteredPC = filter (\pc -> pc.currentValue > 0) category.peopleCategories
        if null filteredPC then Nothing
        else Just $ TicketBookingCategory {
        categoryId : category.categoryId,
        peopleCategories : map mkPeopleCatReq filteredPC
        }

    mkPeopleCatReq peopleCat = TicketBookingPeopleCategory {peopleCategoryId : peopleCat.peopleCategoryId, numberOfUnits : peopleCat.currentValue}

    getTicketServiceReqElement service businessHour = do
        let categories = filter (\cat -> cat.isSelected) businessHour.categories
            filteredValidCategories = mapMaybe getValidCategories categories
        if (null categories) || (null filteredValidCategories) then Nothing
        else Just $ 
                TicketService {
                serviceId : service.id,
                businessHourId : businessHour.bhourId,
                categories : filteredValidCategories
            }


------------------------------------------------------------------------ ZoneTicketBookingFlow --------------------------------------------------------------------------------
getAllBookingsBT :: BookingStatus ->  FlowBT String GetAllBookingsRes
getAllBookingsBT status = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.getAllBookings (show status)) (\x -> x) errorHandler (lift $ lift $ callAPI headers (GetAllBookingsReq (show status)))
    where 
    errorHandler errorPayload = do
            BackT $ pure GoBack


getTicketBookingDetailsBT :: String -> FlowBT String TicketBookingDetails
getTicketBookingDetailsBT shortId = do
    headers <- getHeaders' "" false 
    withAPIResultBT (EP.ticketBookingDetails shortId) (\x -> x) errorHandler (lift $ lift $ callAPI headers (GetBookingInfoReq shortId))
    where
    errorHandler errorPayload = do 
            BackT $ pure GoBack

getTicketStatusBT :: String -> FlowBT String GetTicketStatusResp
getTicketStatusBT shortId = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.ticketStatus shortId) (\x -> x) errorHandler (lift $ lift $ callAPI headers (GetTicketStatusReq shortId))
    where
    errorHandler errorPayload = do
            BackT $ pure GoBack

getTicketStatus shortId = do
  headers <- getHeaders "" false
  withAPIResult (EP.ticketStatus shortId) unwrapResponse $ callAPI headers (GetTicketStatusReq shortId)
  where
  unwrapResponse x = x
 

----------------------------------- fetchIssueList ----------------------------------------

fetchIssueListBT :: String -> FlowBT String FetchIssueListResp
fetchIssueListBT language = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.fetchIssueList language) (\x → x) errorHandler (lift $ lift $ callAPI headers (FetchIssueListReq language))
    where
        errorHandler _ =  do
            BackT $ pure GoBack

--------------------------------------------- Driver Report Issue ---------------------------------------------
getCategoriesBT :: String -> FlowBT String GetCategoriesRes
getCategoriesBT language = do
      headers <- getHeaders' "" false
      withAPIResultBT (EP.getCategories language) (\x → x) errorHandler (lift $ lift $ callAPI headers (GetCategoriesReq language))
      where
        errorHandler _ = do
            BackT $ pure GoBack

getOptionsBT :: String -> String -> String -> String -> FlowBT String GetOptionsRes
getOptionsBT language categoryId optionId issueReportId = do
      headers <- getHeaders' "" false
      withAPIResultBT (EP.getOptions categoryId optionId issueReportId language) (\x → x) errorHandler (lift $ lift $ callAPI headers (GetOptionsReq categoryId optionId issueReportId language))
        where
          errorHandler _ = do
            BackT $ pure GoBack

postIssueBT :: String -> PostIssueReqBody -> FlowBT String PostIssueRes
postIssueBT language payload = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.postIssue language) (\x -> x) errorHandler (lift $ lift $ callAPI headers (PostIssueReq language payload))
    where
        errorHandler _ = do
            BackT $ pure GoBack

issueInfoBT :: String -> String -> FlowBT String IssueInfoRes
issueInfoBT language issueId = do
      headers <- getHeaders' "" false
      withAPIResultBT (EP.issueInfo issueId language) (\x -> x) errorHandler (lift $ lift $ callAPI headers (IssueInfoReq issueId language))
        where
          errorHandler _ = do
                BackT $ pure GoBack

updateIssue :: String -> String -> UpdateIssueReqBody -> FlowBT String UpdateIssueRes
updateIssue language issueId req = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.updateIssue issueId language) (\x → x) errorHandler (lift $ lift $ callAPI headers (UpdateIssueReq issueId language req))
        where
          errorHandler _ = do
                BackT $ pure GoBack


-------------------------------------------------------- Metro Booking --------------------------------------------------------

-- getMetroBookingStatus :: String -> FlowBT String GetMetroBookingStatusResp
getMetroBookingStatus shortOrderID = do --pure $ Right dummyResp 
  headers <- getHeaders "" false
  withAPIResult (EP.getMetroBookingStatus shortOrderID) unwrapResponse $ callAPI headers (GetMetroBookingStatusReq shortOrderID)
  where
    unwrapResponse x = x


dummyResp :: GetMetroBookingStatusResp
dummyResp = (GetMetroBookingStatusResp  dummyMetroBookingStatus)


dummyMetroBookingStatus :: MetroTicketBookingStatus
dummyMetroBookingStatus = 
  MetroTicketBookingStatus {
    _type: "SingleJourney",
    createdAt : "",
    bookingId : "d663387c-b2b1-4e1b-9dd5-269c777fe5c1",
    payment : Nothing
    ,
    price: 1,
    quantity: 2,
    stations: [],
    status: "CONFIRMED",
    tickets: [
        FRFSTicketAPI {
          status : "ACTIVE"
        , qrData : "Namma Yatri"
        , validTill : "30 Jan 2024, 22:00"
        , ticketNumber : "JASF98234324"
        }
      , FRFSTicketAPI {
          status : "ACTIVE"
        , qrData : "Namma Yatri PARTNER"
        , validTill : "30 Jan 2024, 22:00"
        , ticketNumber : "324F98223984923"
        }
      , FRFSTicketAPI {
          status : "ACTIVE"
        , qrData : "Namma Yatri CHENNAI"
        , validTill : "30 Jan 2024, 22:00"
        , ticketNumber : "KJQEDF98234324"
        }
    ],
    vehicleType: "Metro_",
    validTill: "2024-01-26T19:37:12.516Z"
    }


getMetroBookingStatusListBT :: FlowBT String GetMetroBookingListResp
getMetroBookingStatusListBT = do
      headers <- getHeaders' "" false
      withAPIResultBT (EP.getMetroBookingList "") (\x → x) errorHandler (lift $ lift $ callAPI headers (GetMetroBookingListReq))
      where
        errorHandler _ = do
            BackT $ pure GoBack


retryMetroTicketPaymentBT :: String -> FlowBT String RetryMetrTicketPaymentResp
retryMetroTicketPaymentBT quoteId = do
      headers <- getHeaders' "" false
      withAPIResultBT (EP.retryMetrTicketPayment quoteId) (\x → x) errorHandler (lift $ lift $ callAPI headers (RetryMetrTicketPaymentReq quoteId))
      where
        errorHandler _ = do
            BackT $ pure GoBack

retryMetroTicketPayment quoteId = do--pure $ Right dummyRetryResp 
  headers <- getHeaders "" false
  withAPIResult (EP.retryMetrTicketPayment quoteId) unwrapResponse $ callAPI headers (RetryMetrTicketPaymentReq quoteId)
  where
    unwrapResponse x = x


-- data FRFSTicketBookingStatus = NEW | APPROVED | PAYMENT_PENDING | CONFIRMING | FAILED | CONFIRMED





dummyRetryResp :: RetryMetrTicketPaymentResp 
dummyRetryResp = (RetryMetrTicketPaymentResp dummyMetroBookingStatus)

dummyMetroBookingStatusList :: GetMetroBookingListResp 
dummyMetroBookingStatusList = (GetMetroBookingListResp [dummyMetroBookingStatus])
getMetroStationBT :: String -> FlowBT String GetMetroStationResponse
getMetroStationBT _ = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.getMetroStations "") (\x -> x) errorHandler (lift $ lift $ callAPI headers GetMetroStationReq)
    where
    errorHandler errorPayload = do
      BackT $ pure GoBack 

searchMetroBT :: SearchMetroReq -> FlowBT String SearchMetroResp
searchMetroBT  requestBody = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.searchMetro "") (\x -> x) errorHandler (lift $ lift $ callAPI headers requestBody)
    where
    errorHandler errorPayload = do
      BackT $ pure GoBack 

makeSearchMetroReq :: String -> String -> Int -> SearchMetroReq
makeSearchMetroReq srcCode destCode count = SearchMetroReq {
    "fromStationCode" : srcCode,
    "toStationCode" : destCode,
    "quantity" : count
    }

getMetroQuotesBT :: String -> FlowBT String GetMetroQuotesRes
getMetroQuotesBT searchId = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.getMetroQuotes searchId) (\x → x) errorHandler (lift $ lift $ callAPI headers (GetMetroQuotesReq searchId))
        where
          errorHandler _ = do
                BackT $ pure GoBack

getMetroQuotes searchId = do
  headers <- getHeaders "" false
  withAPIResult (EP.getMetroQuotes searchId) unwrapResponse $ callAPI headers (GetMetroQuotesReq searchId)
  where
  unwrapResponse x = x
 
confirmMetroQuoteBT :: String -> FlowBT String MetroTicketBookingStatus
confirmMetroQuoteBT quoteId = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.confirmMetroQuote quoteId) (\x → x) errorHandler (lift $ lift $ callAPI headers (ConfirmMetroQuoteReq quoteId))
        where
          errorHandler _ = do
                BackT $ pure GoBack

getMetroStatusBT :: String -> FlowBT String GetMetroBookingStatusResp
getMetroStatusBT bookingId = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.getMetroBookingStatus bookingId) (\x → x) errorHandler (lift $ lift $ callAPI headers (GetMetroBookingStatusReq bookingId))
        where
          errorHandler _ = do
                BackT $ pure GoBack


