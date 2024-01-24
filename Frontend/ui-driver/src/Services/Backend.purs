{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Services.Backend where

import Data.Maybe
import Services.API

import Common.Types.App (Version(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..))
import Data.Either (Either(..), either)
import Data.Int as INT
import Data.Number as Number
import Data.String as DS
import Data.Array as DA
import Debug (spy)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (liftFlow, isInvalidUrl)
import Engineering.Helpers.Utils (toggleLoader)
import Foreign.Generic (encode)
import Foreign.NullOrUndefined (undefined)
import Helpers.Utils (decodeErrorCode, getTime, toStringJSON, decodeErrorMessage, LatLon(..))
import JBridge (setKeyInSharedPrefKeys, toast, factoryResetApp, stopLocationPollingAPI, Locations, getVersionName, stopChatListenerService)
import Juspay.OTP.Reader as Readers
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog)
import Prelude (bind, discard, pure, unit, identity, ($), ($>), (&&), (*>), (<<<), (=<<), (==), void, map, show, class Show, (<>), (||), not, (/=))
import Presto.Core.Types.API (ErrorResponse(..), Header(..), Headers(..))
import Presto.Core.Types.Language.Flow (Flow, callAPI, doAff, loadS)
import Screens.Types (DriverStatus)
import Services.Config as SC
import Services.EndPoints as EP
import Storage (KeyStore(..), deleteValueFromLocalStore, getValueToLocalStore, getValueToLocalNativeStore)
import Storage (getValueToLocalStore, KeyStore(..))
import Tracker (trackApiCallFlow, trackExceptionFlow)
import Tracker.Labels (Label(..))
import Tracker.Types as Tracker
import Types.App (FlowBT, GlobalState(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import Types.ModifyScreenState (modifyScreenState)
import Foreign.Object (empty)
import Locale.Utils

getHeaders :: String -> Boolean -> Flow GlobalState Headers
getHeaders dummy isGzipCompressionEnabled = do
    _ <- pure $ printLog "dummy" dummy
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


getHeaders' :: String -> Boolean -> FlowBT String Headers
getHeaders' dummy isGzipCompressionEnabled = do
    regToken <- lift $ lift $ loadS $ show REGISTERATION_TOKEN
    _ <- pure $ spy "import headers" regToken
    lift $ lift $ pure $ Headers $ [   Header "Content-Type" "application/json",
                        Header "x-client-version" (getValueToLocalStore VERSION_NAME),
                        Header "x-bundle-version" (getValueToLocalStore BUNDLE_VERSION),
                        Header "session_id" (getValueToLocalStore SESSION_ID),
                        Header "x-device" (getValueToLocalNativeStore DEVICE_DETAILS)
                    ] <> case regToken of
                        Nothing -> []
                        Just token -> [Header "token" token]
                    <> if isGzipCompressionEnabled then [Header "Accept-Encoding" "gzip"] else []

withAPIResult url f flow = do
    if (isInvalidUrl url) then pure $ Left customError
    else do
        let start = getTime unit
        resp <- either (pure <<< Left) (pure <<< Right <<< f <<< _.response) =<< flow
        let end = getTime unit
        _ <- pure $ printLog "withAPIResult url" url
        case resp of
            Right res -> void $ pure $ printLog "success resp" res
            Left err -> do
                let errResp = err.response
                _ <- pure $ printLog "error resp" errResp
                let codeMessage = decodeErrorCode errResp.errorMessage
                let userMessage = decodeErrorMessage errResp.errorMessage
                if (err.code == 401 && (codeMessage == "INVALID_TOKEN" || codeMessage == "TOKEN_EXPIRED")) || (err.code == 400 && codeMessage == "TOKEN_EXPIRED") then do
                    _ <- pure $ deleteValueFromLocalStore REGISTERATION_TOKEN
                    _ <- pure $ deleteValueFromLocalStore VERSION_NAME
                    _ <- pure $ deleteValueFromLocalStore BASE_URL
                    _ <- pure $ deleteValueFromLocalStore TEST_FLOW_FOR_REGISTRATOION
                    _ <- pure $ deleteValueFromLocalStore IS_RIDE_ACTIVE
                    _ <- pure $ deleteValueFromLocalStore IS_DRIVER_ENABLED
                    -- _ <- stopLocationPollingAPI
                    _ <- liftFlow $ stopChatListenerService
                    _ <- pure $ factoryResetApp ""
                    pure unit -- default if it fails
                    else pure unit -- default if it fails
        pure resp


withAPIResultBT url f errorHandler flow = do
    if (isInvalidUrl url) then errorHandler customErrorBT
    else do
        let start = getTime unit
        resp <- either (pure <<< Left) (pure <<< Right <<< f <<< _.response) =<< flow
        let end = getTime unit
        _ <- pure $ printLog "withAPIResultBT url" url
        case resp of
            Right res -> do
                _ <- pure $ printLog "success resp" res
                pure res
            Left (err) -> do
                let errResp = err.response
                _ <- pure $ printLog "error resp" errResp
                let codeMessage = decodeErrorCode errResp.errorMessage
                let userMessage = decodeErrorMessage errResp.errorMessage
                if (err.code == 401 && (codeMessage == "INVALID_TOKEN" || codeMessage == "TOKEN_EXPIRED")) || (err.code == 400 && codeMessage == "TOKEN_EXPIRED") then do
                    deleteValueFromLocalStore REGISTERATION_TOKEN
                    deleteValueFromLocalStore VERSION_NAME
                    deleteValueFromLocalStore BASE_URL
                    deleteValueFromLocalStore TEST_FLOW_FOR_REGISTRATOION
                    deleteValueFromLocalStore IS_RIDE_ACTIVE
                    deleteValueFromLocalStore IS_DRIVER_ENABLED
                    lift $ lift $ liftFlow $ stopChatListenerService
                    _ <- pure $ factoryResetApp ""
                    pure unit
                        else pure unit
                errorHandler (ErrorPayload err)

customErrorBT = ErrorPayload { code : 400
  , status : "success"
  , response : {
       error : true
     , errorMessage : "{\"errorCode\" : \"ERROR_OCCURED_TRY_AGAIN\", \"errorMessage\" : \"Error Occured ! Please try again later\"}"
     , userMessage : getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
    }
  , responseHeaders : empty
  }

customError :: ErrorResponse
customError =  { code : 400
  , status : "success"
  , response : {
       error : true
     , errorMessage : "{\"errorCode\" : \"ERROR_OCCURED_TRY_AGAIN\", \"errorMessage\" : \"Error Occured ! Please try again later\"}"
     , userMessage : getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
    }
  , responseHeaders : empty
  }

--------------------------------- triggerOTPBT---------------------------------------------------------------------------------------------------------------------------------
triggerOTPBT :: TriggerOTPReq → FlowBT String TriggerOTPResp
triggerOTPBT payload = do
    _ <- lift $ lift $ doAff Readers.initiateSMSRetriever
    headers <- getHeaders' "" false
    withAPIResultBT (EP.triggerOTP "") identity errorHandler (lift $ lift $ callAPI headers payload)
    where
    errorHandler (ErrorPayload errorPayload) = do
        let errResp = errorPayload.response
        let codeMessage = decodeErrorCode errResp.errorMessage
        if (errorPayload.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then
            pure $ toast $ getString OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_RESENDING_OTP
            else pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
        modifyScreenState $ ChooseLanguageScreenStateType (\chooseLanguageScreen -> chooseLanguageScreen { props {btnActive = false} })
        BackT $ pure GoBack


makeTriggerOTPReq :: String → LatLon -> TriggerOTPReq
makeTriggerOTPReq mobileNumber (LatLon lat lng _) = TriggerOTPReq
    let operatingCity = getValueToLocalStore DRIVER_LOCATION
        latitude = mkLatLon lat
        longitude = mkLatLon lng
    in
    {
      "mobileNumber"      : mobileNumber,
      "mobileCountryCode" : "+91",
      "merchantId" : if (SC.getMerchantId "") == "NA" then getValueToLocalNativeStore MERCHANT_ID else (SC.getMerchantId "" ),
      "merchantOperatingCity" : mkOperatingCity operatingCity,
      "registrationLat" : latitude,
      "registrationLon" : longitude
    }
    where 
        mkOperatingCity :: String -> Maybe String
        mkOperatingCity operatingCity = 
            if DA.any (_ == operatingCity) [ "__failed", "--"] then Nothing
            else if operatingCity == "Puducherry" then Just "Pondicherry"
            else Just operatingCity
        mkLatLon :: String -> Maybe Number
        mkLatLon latlon = 
            if latlon == "0.0" 
                then Nothing
                else Number.fromString latlon




--------------------------------- resendOTPBT ---------------------------------------------------------------------------------------------------------------------------------
resendOTPBT :: String -> FlowBT String ResendOTPResp
resendOTPBT token = do
     headers <- getHeaders' "" false
     withAPIResultBT (EP.resendOTP token) identity errorHandler (lift $ lift $ callAPI headers (ResendOTPRequest token))
    where
    errorHandler (ErrorPayload errorPayload)  = do
        let errResp = errorPayload.response
        let codeMessage = decodeErrorCode errResp.errorMessage
        if ( errorPayload.code == 400 && codeMessage == "AUTH_BLOCKED") then
            pure $ toast $ getString OTP_RESENT_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER
            else pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
        BackT $ pure GoBack




--------------------------------- verifyTokenBT ---------------------------------------------------------------------------------------------------------------------------------
verifyTokenBT :: VerifyTokenReq -> String -> FlowBT String VerifyTokenResp
verifyTokenBT payload token = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.verifyToken token) identity errorHandler (lift $ lift $ callAPI headers (VerifyTokenRequest token payload))
    where
    errorHandler (ErrorPayload errorPayload) = do
        let errResp = errorPayload.response
        let codeMessage = decodeErrorCode errResp.errorMessage
        if errorPayload.code == 400 && codeMessage == "TOKEN_EXPIRED" then do
            pure $ toast $ getString OTP_PAGE_HAS_BEEN_EXPIRED_PLEASE_REQUEST_OTP_AGAIN
            void $ lift $ lift $ toggleLoader false
            else if errorPayload.code == 400 && codeMessage == "INVALID_AUTH_DATA" then do
                modifyScreenState $ EnterOTPScreenType (\enterOTPScreen -> enterOTPScreen { props {isValid = true} })
                void $ lift $ lift $ toggleLoader false
            else if errorPayload.code == 429 && codeMessage == "HITS_LIMIT_EXCEED" then do
                void $ lift $ lift $ toggleLoader false
                pure $ toast $ getString OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER
            else pure $ toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
        BackT $ pure GoBack

makeVerifyOTPReq :: String -> VerifyTokenReq
makeVerifyOTPReq otp = VerifyTokenReq {
      "otp": otp,
      "deviceToken": if getValueToLocalNativeStore FCM_TOKEN == "__failed" then "" else (getValueToLocalNativeStore FCM_TOKEN),
      "whatsappNotificationEnroll": OPT_IN
    }

------------------------------------------ driverActiveInactiveBT -------------------------------------------------------------
driverActiveInactiveBT :: String -> String -> FlowBT String DriverActiveInactiveResp
driverActiveInactiveBT status status_n = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.driverActiveInactiveSilent status status_n) identity errorHandler (lift $ lift $ callAPI headers (DriverActiveInactiveReq status status_n))
    where
        errorHandler (ErrorPayload errorPayload) =  do
            let codeMessage = decodeErrorCode errorPayload.response.errorMessage
                accountBlocked = errorPayload.code == 403 && codeMessage == "DRIVER_ACCOUNT_BLOCKED"
            if accountBlocked then modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen { props { accountBlockedPopup = true }})
            else modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen { props { goOfflineModal = false }})
            pure if not accountBlocked then toast $ getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN else unit
            void $ lift $ lift $ toggleLoader false
            BackT $ pure GoBack
--------------------------------- startRide ---------------------------------------------------------------------------------------------------------------------------------

startRide :: String -> StartRideReq -> Flow GlobalState (Either ErrorResponse StartRideResponse)
startRide productId payload = do
        headers <- getHeaders "" false
        withAPIResult (EP.startRide productId) unwrapResponse $ callAPI headers ((StartRideRequest productId payload))
    where
        unwrapResponse (x) = x

makeStartRideReq :: String -> Number -> Number -> String -> StartRideReq
makeStartRideReq otp lat lon ts = StartRideReq {
    "rideOtp": otp,
    "point": Point {
        lat,
        lon,
        ts
        }
}
--------------------------------- endRide ---------------------------------------------------------------------------------------------------------------------------------

endRide :: String -> EndRideReq -> FlowBT String EndRideResponse
endRide productId payload = do
        headers <-getHeaders' "" false
        withAPIResultBT (EP.endRide productId) identity errorHandler (lift $ lift $ callAPI headers (EndRideRequest productId payload))
    where
      errorHandler (ErrorPayload errorPayload) =  do
            void $ lift $ lift $ toggleLoader false
            BackT $ pure GoBack

makeEndRideReq :: Number -> Number -> Maybe Boolean -> Int -> Int -> String -> EndRideReq
makeEndRideReq lat lon numDeviation tripDistance tripDistanceWithAcc ts = EndRideReq {
    "point" :  Point {
        lat,
        lon,
        ts
    },
    "numberOfDeviation" : numDeviation,
    "uiDistanceCalculationWithAccuracy" : tripDistanceWithAcc,
    "uiDistanceCalculationWithoutAccuracy" : tripDistance
}

--------------------------------- driverCancelRide ---------------------------------------------------------------------------------------------------------------------------------

cancelRide :: String -> DriverCancelRideReq -> FlowBT String DriverCancelRideResponse
cancelRide productId payload = do
        headers <-getHeaders' "" false
        withAPIResultBT (EP.cancelRide productId) identity errorHandler (lift $ lift $ callAPI headers (DriverCancelRideRequest productId payload))
    where
      errorHandler (ErrorPayload errorPayload) =  do
            void $ lift $ lift $ toggleLoader false
            BackT $ pure GoBack

makeCancelRideReq :: String -> String -> DriverCancelRideReq
makeCancelRideReq info reason = DriverCancelRideReq {
    "additionalInfo": info,
    "reasonCode": reason
}

--------------------------------- logOutBT ---------------------------------------------------------------------------------------------------------------------------------

logOutBT :: LogOutReq -> FlowBT String LogOutRes
logOutBT payload = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.logout "") identity errorHandler (lift $ lift $ callAPI headers payload)
    where
        errorHandler (ErrorPayload errorPayload) = do
            BackT $ pure GoBack

--------------------------------- getDriverInfoBT ---------------------------------------------------------------------------------------------------------------------------------

getDriverInfoBT :: GetDriverInfoReq -> FlowBT String GetDriverInfoResp
getDriverInfoBT payload = do
     headers <- getHeaders' "" true
     withAPIResultBT ((EP.getDriverInfo "" )) identity errorHandler (lift $ lift $ callAPI headers payload)
    where
        errorHandler (ErrorPayload errorPayload) =  do
            BackT $ pure GoBack

getDriverInfoApi payload = do
     _ <-pure $ spy "(getValueToLocalStore REGISTERATION_TOKEN) after" (getValueToLocalStore REGISTERATION_TOKEN)
     _ <- pure $ spy "(getValueToLocalStore REGISTERATION_TOKEN) before" (getValueToLocalStore REGISTERATION_TOKEN)
    --  _ <- pure $ spy "(getValueToLocalStore REGISTERATION_TOKEN) before effect" (liftEffect $ (getValueToLocalStoreNew REGISTERATION_TOKEN))
     headers <- getHeaders "" true
    --  pure $ printLog "headers" headers
     _ <- pure $ spy "(getValueToLocalStore REGISTERATION_TOKEN) after" (getValueToLocalStore REGISTERATION_TOKEN)
     _ <- pure $ spy "(getValueToLocalStore REGISTERATION_TOKEN) after" (getValueToLocalStore REGISTERATION_TOKEN)
    --  _ <- pure $ spy "(getValueToLocalStore REGISTERATION_TOKEN) after effetct" (liftEffect $ (getValueToLocalStoreNew REGISTERATION_TOKEN))
     withAPIResult (EP.getDriverInfo "") unwrapResponse $ callAPI headers (GetDriverInfoReq { })
    where
        unwrapResponse (x) = x

--------------------------------- getAllRcDataBT ---------------------------------------------------------------------------------------------------------------------------------

getAllRcDataBT :: GetAllRcDataReq -> FlowBT String GetAllRcDataResp
getAllRcDataBT payload = do
    headers <- getHeaders' "" true
    withAPIResultBT ((EP.getAllRcData "")) identity errorHandler (lift $ lift $ callAPI headers payload)
    where
        errorHandler (ErrorPayload errorPayload) =  do
            BackT $ pure GoBack

dummyVehicleObject :: Vehicle
dummyVehicleObject = Vehicle
   {
      "variant":"",
      "createdAt":"",
      "color":"",
      "category": Nothing,
      "driverId":"",
      "capacity": Nothing,
      "model":"",
      "registrationNo":""
   }
--------------------------------- offerRideBT ---------------------------------------------------------------------------------------------------------------------------------
offerRideBT :: OfferRideReq -> FlowBT String OfferRideResp
offerRideBT payload = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.offerRide "") identity errorHandler (lift $ lift $ callAPI headers payload)
    where
    errorHandler (ErrorPayload errorPayload) = do
        let errResp = errorPayload.response
        let codeMessage = decodeErrorCode errResp.errorMessage
        if (errorPayload.code == 400) then
            pure $ toast (getString INVALID_PARAMETERS)
            else if (errorPayload.code == 401) then
                pure $ toast (getString UNAUTHORIZED)
            else pure $ toast (getString SOME_ERROR_OCCURED_IN_OFFERRIDE)
        BackT $ pure GoBack


makeOfferRideReq :: String -> Maybe Number -> OfferRideReq
makeOfferRideReq requestId offeredFare = OfferRideReq
    {
      "searchRequestId" : requestId,
      "offeredFare" : offeredFare
    }

--------------------------------- getRideHistoryResp -------------------------------------------------------------------------

getRideHistoryReq limit offset onlyActive status day = do
        headers <- getHeaders "" true
        withAPIResult (EP.getRideHistory limit offset onlyActive status day) unwrapResponse $ callAPI headers (GetRidesHistoryReq limit offset onlyActive status day)
    where
        unwrapResponse (x) = x


getRideHistoryReqBT :: String -> String -> String -> String -> String -> FlowBT String GetRidesHistoryResp
getRideHistoryReqBT limit offset onlyActive status day= do
        headers <- lift $ lift $ getHeaders "" true
        withAPIResultBT (EP.getRideHistory limit offset onlyActive status day) identity errorHandler (lift $ lift $ callAPI headers (GetRidesHistoryReq limit offset onlyActive status day))
    where
    errorHandler (ErrorPayload errorPayload) =  do
        BackT $ pure GoBack

--------------------------------- GetRidesSummaryListResp --------------------------------------------------------------------------------------------------
getRideSummaryListReq dateList = do
        headers <- getHeaders "" true
        withAPIResult (EP.getRidesSummaryList dateList) unwrapResponse $ callAPI headers (GetRidesSummaryListReq dateList)
    where
        unwrapResponse (x) = x


getRideSummaryListReqBT :: Array String -> FlowBT String GetRidesSummaryListResp
getRideSummaryListReqBT dateList = do
        headers <- lift $ lift $ getHeaders "" true
        withAPIResultBT (EP.getRidesSummaryList dateList) (\x → x) errorHandler (lift $ lift $ callAPI headers (GetRidesSummaryListReq dateList))
    where
    errorHandler (ErrorPayload errorPayload) =  do
        BackT $ pure GoBack

--------------------------------- updateDriverInfoBT ---------------------------------------------------------------------------------------------------------------------------------
updateDriverInfoBT :: UpdateDriverInfoReq -> FlowBT String UpdateDriverInfoResp
updateDriverInfoBT payload = do
        headers <-getHeaders' "" true
        withAPIResultBT (EP.updateDriverInfo "") identity errorHandler (lift $ lift $ callAPI headers (UpdateDriverInfoRequest payload))
    where
        errorHandler (ErrorPayload errorPayload) =  do
            pure $ toast $ decodeErrorMessage errorPayload.response.errorMessage
            BackT $ pure GoBack

mkUpdateDriverInfoReq :: String -> UpdateDriverInfoReq
mkUpdateDriverInfoReq dummy
  = UpdateDriverInfoReq
    { middleName: Nothing
    , firstName: Nothing
    , lastName: Nothing
    , deviceToken: Nothing
    , canDowngradeToSedan: Nothing
    , canDowngradeToHatchback: Nothing
    , canDowngradeToTaxi: Nothing
    , language:
        Just case getLanguageLocale languageKey of
          "EN_US" -> "ENGLISH"
          "KN_IN" -> "KANNADA"
          "HI_IN" -> "HINDI"
          "ML_IN" -> "MALAYALAM"
          "BN_IN" -> "BENGALI"
          "TA_IN" -> "TAMIL"
          "TE_IN" -> "TELUGU"
          _ -> "ENGLISH"
    , bundleVersion: Nothing
    , clientVersion: Nothing
    , gender: Nothing
    , languagesSpoken: Nothing
    , hometown: Nothing
    , vehicleName: Nothing
    , availableUpiApps: Nothing
    }


--------------------------------- getDriverInfoBT ---------------------------------------------------------------------------------------------------------------------------------

listCancelReasonBT :: ListCancelReasonReq -> FlowBT String ListCancelReasonResp
listCancelReasonBT payload = do
    headers <- getHeaders' "" true
    withAPIResultBT (EP.listCancelReason "" ) identity errorHandler (lift $ lift $ callAPI headers payload)
    where
        errorHandler (ErrorPayload errorPayload) =  do
            let errResp = errorPayload.response
            if (errorPayload.code == 400) then
                pure $ toast (getString INVALID_TOKEN)
                else if (errorPayload.code == 401) then
                    pure $ toast (getString UNAUTHORIZED)
                else pure $ toast "Some error occured in listCancelReasonBT"
            BackT $ pure GoBack

--------------------------------- getRouteBT ---------------------------------------------------------------------------------------------------------------------------------

getRouteBT :: GetRouteReq -> String -> FlowBT String GetRouteResp
getRouteBT body routeType = do
     headers <- lift $ lift $ getHeaders "" true
     withAPIResultBT (EP.getRoute routeType) identity errorHandler (lift $ lift $ callAPI headers (RouteReq routeType body))
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
walkCoordinate fromLong fromLat driverLong driverLat = {
    "points": [
      {
        "lat": fromLat,
        "lng": fromLong
      },
      {
        "lat": driverLat,
        "lng": driverLong
      }
    ]
}

walkCoordinates :: Snapped -> Locations
walkCoordinates (Snapped points) =
  { "points": map (\(LatLong item) -> { "lat": item.lat, "lng": item.lon }) points
  }

--------------------------------- onBoardingFlow  ---------------------------------------------------------------------------------------------------------------------------------
getCorrespondingErrorMessage :: ErrorResponse -> String
getCorrespondingErrorMessage errorPayload = do
    let errorCode = decodeErrorCode errorPayload.response.errorMessage
    case errorCode of
        "IMAGE_VALIDATION_FAILED" -> getString IMAGE_VALIDATION_FAILED
        "IMAGE_NOT_READABLE" -> getString IMAGE_NOT_READABLE
        "IMAGE_LOW_QUALITY" -> getString IMAGE_LOW_QUALITY
        "IMAGE_INVALID_TYPE" -> getString IMAGE_INVALID_TYPE
        "IMAGE_DOCUMENT_NUMBER_MISMATCH" -> getString IMAGE_DOCUMENT_NUMBER_MISMATCH
        "IMAGE_EXTRACTION_FAILED" -> getString IMAGE_EXTRACTION_FAILED
        "IMAGE_NOT_FOUND" -> getString IMAGE_NOT_FOUND
        "IMAGE_NOT_VALID" -> getString IMAGE_NOT_VALID
        "DRIVER_ALREADY_LINKED" -> getString DRIVER_ALREADY_LINKED
        "DL_ALREADY_UPDATED" -> getString DL_ALREADY_UPDATED
        "DL_ALREADY_LINKED"  -> getString DL_ALREADY_LINKED
        "RC_ALREADY_LINKED" -> getString RC_ALREADY_LINKED
        "RC_ALREADY_UPDATED" -> getString RC_ALREADY_UPDATED
        "UNPROCESSABLE_ENTITY" -> getString PLEASE_CHECK_FOR_IMAGE_IF_VALID_DOCUMENT_IMAGE_OR_NOT
        "NO_MOBILE_NUMBER_REGISTERED" -> getString NO_MOBILE_NUMBER_REGISTERED
        "EXCEED_OTP_GENERATION_LIMIT" -> getString EXCEED_OTP_GENERATION_LIMIT
        "AADHAAR_NUMBER_NOT_EXIST" -> getString AADHAAR_NUMBER_NOT_EXIST
        "INVALID_OTP" -> getString INVALID_OTP
        "NO_SHARE_CODE" -> getString NO_SHARE_CODE
        "WRONG_SHARE_CODE" -> getString WRONG_SHARE_CODE
        "INVALID_SHARE_CODE" -> getString INVALID_SHARE_CODE
        "SESSION_EXPIRED" -> getString SESSION_EXPIRED
        "OTP_ATTEMPT_EXCEEDED" -> getString OTP_ATTEMPT_EXCEEDED
        "UPSTREAM_INTERNAL_SERVER_ERROR" -> getString UPSTREAM_INTERNAL_SERVER_ERROR
        "TRANSACTION_ALREADY_COMPLETED" -> getString TRANSACTION_ALREADY_COMPLETED
        "PLAN_NOT_FOUND" -> getString PLAN_NOT_FOUND
        "MANDATE_NOT_FOUND" -> getString MANDATE_NOT_FOUND
        "ACTIVE_MANDATE_EXISTS" -> getString ACTIVE_MANDATE_EXISTS
        "NO_ACTIVE_MANDATE_EXIST" -> getString NO_ACTIVE_MANDATE_EXIST
        "NO_PLAN_FOR_DRIVER" -> getString NO_PLAN_FOR_DRIVER
        "INVALID_PAYMENT_MODE" -> getString INVALID_PAYMENT_MODE
        "INVALID_AUTO_PAY_STATUS" -> getString INVALID_AUTO_PAY_STATUS
        "DRIVER_HOME_LOCATION_NOT_FOUND" -> getString DRIVER_HOME_LOCATION_NOT_FOUND
        "DRIVER_HOME_LOCATION_DOES_NOT_EXIST" -> getString DRIVER_HOME_LOCATION_DOES_NOT_EXIST
        "DRIVER_HOME_LOCATION_LIMIT_REACHED" -> getString DRIVER_HOME_LOCATION_LIMIT_REACHED
        "DRIVER_GO_HOME_REQUEST_NOT_FOUND" -> getString DRIVER_GO_HOME_REQUEST_NOT_FOUND
        "DRIVER_GO_HOME_REQUEST_DOES_NOT_EXIST" -> getString DRIVER_GO_HOME_REQUEST_DOES_NOT_EXIST
        "DRIVER_GO_HOME_REQUEST_DAILY_USAGE_LIMIT_REACHED" -> getString DRIVER_GO_HOME_REQUEST_DAILY_USAGE_LIMIT_REACHED
        "DRIVER_GO_HOME_REQUEST_ALREADY_ACTIVE" -> getString DRIVER_GO_HOME_REQUEST_ALREADY_ACTIVE
        "DRIVER_GO_HOME_REQUEST_NOT_PRESENT" -> getString DRIVER_GO_HOME_REQUEST_NOT_PRESENT
        "DRIVER_HOME_LOCATION_OUTSIDE_SERVICE_AREA" -> getString DRIVER_HOME_LOCATION_OUTSIDE_SERVICE_AREA
        "NEW_LOCATION_TOO_CLOSE_TO_PREVIOUS_HOME_LOCATION" -> getString NEW_LOCATION_TOO_CLOSE_TO_PREVIOUS_HOME_LOCATION
        "DRIVER_HOME_LOCATION_DOES_NOT_BELONG_TO_DRIVER" -> getString DRIVER_HOME_LOCATION_DOES_NOT_BELONG_TO_DRIVER
        "DRIVER_HOME_LOCATION_DELETE_WHILE_ACTIVE_ERROR" -> getString DRIVER_HOME_LOCATION_DELETE_WHILE_ACTIVE_ERROR
        "null" -> getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
        "" -> getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
        undefined -> getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER

registerDriverRC payload = do
     headers <- getHeaders "" false
     withAPIResult (EP.registerDriverRC "") unwrapResponse $ callAPI headers payload
    where
        unwrapResponse (x) = x

makeRcActiveOrInactive payload = do
     headers <- getHeaders "" false
     withAPIResult (EP.makeRcActiveOrInactive "") unwrapResponse $ callAPI headers payload
    where
        unwrapResponse (x) = x

deleteRc :: DeleteRcReq -> Flow GlobalState (Either ErrorResponse DeleteRcResp)
deleteRc payload = do
        headers <- getHeaders "" false
        withAPIResult (EP.deleteRc "" ) unwrapResponse $ callAPI headers $ payload
    where unwrapResponse x = x



deleteRcReq :: String -> DeleteRcReq
deleteRcReq rcNo = DeleteRcReq
    {
        "rcNo" : rcNo
    }

makeRcActiveOrInactiveReq :: Boolean -> String -> MakeRcActiveOrInactiveReq
makeRcActiveOrInactiveReq isActivate rcNo =  MakeRcActiveOrInactiveReq
    {
        "rcNo" : rcNo,
        "isActivate" : isActivate
    }

callDriverToDriverBT :: String -> FlowBT String CallDriverToDriverResp
callDriverToDriverBT rcNo = do
  headers <- getHeaders' "" false
  withAPIResultBT (EP.callDriverToDriver rcNo) identity errorHandler (lift $ lift $ callAPI headers (CallDriverToDriverReq rcNo))
  where
    errorHandler (ErrorPayload errorPayload) = BackT $ pure GoBack

makeDriverRCReq :: String -> String -> Maybe String -> Boolean -> DriverRCReq
makeDriverRCReq regNo imageId dateOfRegistration multipleRc= DriverRCReq
    {
      "vehicleRegistrationCertNumber" : regNo,
      "operatingCity" : "BANGALORE",
      "imageId" : imageId,
      "dateOfRegistration" : dateOfRegistration,
      "multipleRC" : multipleRc
    }

registerDriverDLBT :: DriverDLReq -> FlowBT String  DriverDLResp
registerDriverDLBT payload = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.registerDriverDL "" ) (\x -> x) errorHandler (lift $ lift $ callAPI headers payload)
    where
    errorHandler (ErrorPayload errorPayload) = do
        BackT $ pure GoBack

registerDriverDL payload = do
     headers <- getHeaders "" false
     withAPIResult (EP.registerDriverDL "") unwrapResponse $ callAPI headers payload
    where
        unwrapResponse (x) = x

makeDriverDLReq :: String -> String -> Maybe String -> String -> String -> DriverDLReq
makeDriverDLReq dlNumber dob dateOfIssue imageIdFront imageIdBack = DriverDLReq
    {
        "driverLicenseNumber": dlNumber,
        "driverDateOfBirth": dob,
        "operatingCity": "BANGALORE",
        "imageId1": imageIdFront,
        "imageId2" : Nothing,
        "dateOfIssue" : dateOfIssue
    }

validateImageBT :: ValidateImageReq -> FlowBT String ValidateImageRes
validateImageBT payload = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.validateImage "") (\x -> x) errorHandler (lift $ lift $ callAPI headers payload)
    where
    errorHandler (ErrorPayload errorPayload) = do
        BackT $ pure GoBack

validateImage payload = do
     headers <- getHeaders "" false
     withAPIResult (EP.validateImage "") unwrapResponse $ callAPI headers payload
    where
        unwrapResponse (x) = x

makeValidateImageReq :: String -> String -> ValidateImageReq
makeValidateImageReq image imageType= ValidateImageReq
    {
      "image" : image,
      "imageType" : imageType
    }

driverRegistrationStatusBT :: DriverRegistrationStatusReq -> FlowBT String DriverRegistrationStatusResp
driverRegistrationStatusBT payload = do
     headers <- getHeaders' "" false
     withAPIResultBT ((EP.driverRegistrationStatus "" )) identity errorHandler (lift $ lift $ callAPI headers payload)
    where
        errorHandler (ErrorPayload errorPayload) =  do
            BackT $ pure GoBack

referDriver payload = do
     headers <- getHeaders "" false
     withAPIResult (EP.referDriver "") unwrapResponse $ callAPI headers payload
    where
        unwrapResponse (x) = x

makeReferDriverReq :: String -> ReferDriverReq
makeReferDriverReq referralNumber = ReferDriverReq
    {
      "value" : referralNumber
    }

getDriverProfileStatsBT :: DriverProfileStatsReq -> FlowBT String DriverProfileStatsResp
getDriverProfileStatsBT payload = do
     headers <- getHeaders' "" false
     withAPIResultBT ((EP.getstatsInfo "" )) identity errorHandler (lift $ lift $ callAPI headers payload)
    where
        errorHandler (ErrorPayload errorPayload) =  do
            BackT $ pure GoBack

driverArrived :: String -> DriverArrivedReq -> Flow GlobalState (Either ErrorResponse DriverArrivedRes)
driverArrived rideId payload = do
     headers <- getHeaders "" false
     withAPIResult (EP.driverArrived rideId) unwrapResponse $ callAPI headers $ DriverArrivedRequest rideId payload
    where
        unwrapResponse x = x

flowStatusBT :: String -> FlowBT String FlowStatusRes
flowStatusBT _ = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.flowStatus "") identity errorHandler (lift $ lift $ callAPI headers FlowStatusReq)
    where
        errorHandler errorPayload = do
            BackT $ pure GoBack
--------------------------------- messageList  --------------------------------------------------------------------------------------------------------
messageListBT :: String -> String -> FlowBT String MessageListRes
messageListBT limit offset = do
        headers <- lift $ lift $ getHeaders "" true
        withAPIResultBT (EP.messageList limit offset) identity errorHandler (lift $ lift $ callAPI headers (MessageListReq limit offset))
    where
    errorHandler (ErrorPayload errorPayload) =  do
        BackT $ pure GoBack

--------------------------------- messageSeen  --------------------------------------------------------------------------------------------------------
messageSeenBT :: String -> FlowBT String MessageSeenRes
messageSeenBT messageId = do
        headers <- lift $ lift $ getHeaders "" false
        withAPIResultBT (EP.messageSeen messageId) identity errorHandler (lift $ lift $ callAPI headers (MessageSeenReq messageId))
    where
    errorHandler (ErrorPayload errorPayload) =  do
        BackT $ pure GoBack

--------------------------------- likeMessage  --------------------------------------------------------------------------------------------------------
likeMessageBT :: String -> FlowBT String LikeMessageRes
likeMessageBT messageId = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.likeMessage messageId) identity errorHandler (lift $ lift $ callAPI headers (LikeMessageReq messageId))
    where
    errorHandler (ErrorPayload errorPayload) =  do
        BackT $ pure GoBack

--------------------------------- messageResponse --------------------------------------------------------------------------------------------------------
messageResponseBT :: String -> MessageReplyReq -> FlowBT String MessageResponseRes
messageResponseBT messageId reply = do
        headers <- lift $ lift $ getHeaders "" false
        withAPIResultBT (EP.messageResponse messageId) identity errorHandler (lift $ lift $ callAPI headers (MessageResponseReq messageId reply))
    where
    errorHandler (ErrorPayload errorPayload) =  do
        BackT $ pure GoBack

makeMessageReplyReq :: String -> MessageReplyReq
makeMessageReplyReq reply = MessageReplyReq {
        reply : reply
    }

------------------------------------- makeLinkReferralCode ---------------------------------------------
makeLinkReferralCodeReq :: String -> String  → LinkReferralCodeReq
makeLinkReferralCodeReq  referralCode  referralLinkPassword = LinkReferralCodeReq
    {
      "referralLinkPassword" : referralLinkPassword
    , "referralCode" : referralCode
    }

linkReferralCode payload = do
     headers <- getHeaders "" false
     withAPIResult (EP.linkReferralCode "") unwrapResponse $ callAPI headers payload
    where
        unwrapResponse (x) = x

---------------------------------------- getPerformance ---------------------------------------------
getPerformanceBT :: GetPerformanceReq -> FlowBT String GetPerformanceRes
getPerformanceBT payload = do
     headers <- getHeaders' "" false
     withAPIResultBT (EP.getPerformance "") identity errorHandler (lift $ lift $ callAPI headers payload)
    where
        errorHandler (ErrorPayload errorPayload) =  do
            BackT $ pure GoBack

---------------------------------------- generateReferralCode ---------------------------------------------
generateReferralCode :: GenerateReferralCodeReq -> Flow GlobalState (Either ErrorResponse GenerateReferralCodeRes)
generateReferralCode payload = do
    headers <- getHeaders "" false
    withAPIResult (EP.generateReferralCode "") unwrapResponse (callAPI headers payload)
    where
        unwrapResponse x = x

----------------------------------- validateAlternateNumber --------------------------

validateAlternateNumber payload = do
    headers <- getHeaders "" false
    withAPIResult (EP.driverAlternateNumber "") unwrapResponse $ callAPI headers payload
    where
         unwrapResponse (x) = x

makeValidateAlternateNumberRequest :: String -> DriverAlternateNumberReq
makeValidateAlternateNumberRequest number = DriverAlternateNumberReq {
     "alternateNumber" : number,
    "mobileCountryCode" : "+91"

 }

---------------------------------- ResendAlternateNumberOtp ------------------------------------------
resendAlternateNumberOTP payload = do
    headers <- getHeaders "" false
    withAPIResult (EP.alternateNumberResendOTP "") unwrapResponse $ (callAPI headers payload)
   where
         unwrapResponse (x) = x

makeResendAlternateNumberOtpRequest :: String -> AlternateNumberResendOTPRequest
makeResendAlternateNumberOtpRequest number = AlternateNumberResendOTPRequest {
    "alternateNumber" : number,
    "mobileCountryCode" : "+91"
 }

---------------------------verifyAlternateNumber------------------------------------
verifyAlternateNumberOTP payload = do
    headers <- getHeaders "" false
    withAPIResult (EP.verifyAlternateNumberOTP "") unwrapResponse (callAPI headers payload)
   where
        unwrapResponse (x) = x

makeVerifyAlternateNumberOtpRequest :: String -> DriverAlternateNumberOtpReq
makeVerifyAlternateNumberOtpRequest otp = DriverAlternateNumberOtpReq {

      "otp" : otp
 }

-----------------------------------removeAlternateNumber-----------------------------------

removeAlternateNumber payload = do
      headers <- getHeaders "" false
      withAPIResult (EP.removeAlternateNumber "") unwrapResponse (callAPI headers payload)
   where
        unwrapResponse (x) = x


--------------------------------------------- Driver Report Issue ---------------------------------------------
getCategoriesBT :: String -> FlowBT String GetCategoriesRes
getCategoriesBT language = do
  headers <- getHeaders' "" true
  withAPIResultBT (EP.getCategories language) identity errorHandler (lift $ lift $ callAPI headers (GetCategoriesReq language))
  where
    errorHandler (ErrorPayload errorPayload) = BackT $ pure GoBack

getOptionsBT :: String -> String -> FlowBT String GetOptionsRes
getOptionsBT categoryId language = do
  headers <- getHeaders' "" true
  withAPIResultBT (EP.getOptions categoryId language) identity errorHandler (lift $ lift $ callAPI headers (GetOptionsReq categoryId language))
    where
      errorHandler (ErrorPayload errorPayload) = BackT $ pure GoBack

postIssueBT :: PostIssueReq -> FlowBT String PostIssueRes
postIssueBT payload = do
  headers <- getHeaders' "" false
  withAPIResultBT (EP.postIssue "") (\x -> x) errorHandler (lift $ lift $ callAPI headers payload)
    where
      errorHandler (ErrorPayload errorPayload) = BackT $ pure GoBack

issueInfoBT :: String -> FlowBT String IssueInfoRes
issueInfoBT issueId = do
  headers <- getHeaders' "" true
  withAPIResultBT (EP.issueInfo issueId) (\x -> x) errorHandler (lift $ lift $ callAPI headers (IssueInfoReq issueId))
    where
      errorHandler (ErrorPayload errorPayload) = BackT $ pure GoBack

callCustomerBT :: String -> FlowBT String CallCustomerRes
callCustomerBT rideId = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.callDriverToCustomer rideId) identity errorHandler (lift $ lift $ callAPI headers (CallCustomerReq rideId))
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack

----------------------------------- fetchIssueList ----------------------------------------

fetchIssueListBT :: FetchIssueListReq -> FlowBT String FetchIssueListResp
fetchIssueListBT payload = do
     headers <- getHeaders' "" true
     withAPIResultBT (EP.fetchIssueList "") identity errorHandler (lift $ lift $ callAPI headers payload)
    where
        errorHandler (ErrorPayload errorPayload) =  do
            BackT $ pure GoBack


----------------------------------- deleteIssue -------------------------------------
deleteIssueBT :: String -> FlowBT String DeleteIssueResp
deleteIssueBT issueId = do
     headers <- getHeaders' "" false
     withAPIResultBT (EP.deleteIssue issueId) identity errorHandler (lift $ lift $ callAPI headers (DeleteIssueReq issueId))
    where
        errorHandler (ErrorPayload errorPayload) =  do
            BackT $ pure GoBack

----------------------------------- currentDateAndTime -------------------------------------
currentDateAndTimeBT :: String -> FlowBT String CurrentDateAndTimeRes
currentDateAndTimeBT _ = do
     headers <- getHeaders' "" false
     withAPIResultBT (EP.currentDateAndTime "") identity errorHandler (lift $ lift $ callAPI headers (CurrentDateAndTimeReq ""))
    where
        errorHandler (ErrorPayload errorPayload) =  do
            BackT $ pure GoBack

---------------------------------------- otpRide ---------------------------------------------

otpRide dummyRideOtp payload = do
        headers <- getHeaders "" false
        withAPIResult (EP.otpRide dummyRideOtp) unwrapResponse $ callAPI headers ((OTPRideRequest payload))
    where
        unwrapResponse (x) = x

makeOTPRideReq :: String -> Number -> Number -> String -> OTPRideReq
makeOTPRideReq otp lat lon ts = OTPRideReq {
    specialZoneOtpCode: otp,
    point: Point {
        lat,
        lon,
        ts
    }
}

------------------------------------------------------------------------ OnCallBT Function ------------------------------------------------------------------------------------

onCallBT :: OnCallReq -> FlowBT String OnCallRes
onCallBT payload = do
  headers <- getHeaders' "" false
  withAPIResultBT (EP.onCall "") identity errorHandler (lift $ lift $ callAPI headers payload)
  where
    errorHandler errorPayload = BackT $ pure GoBack

makeOnCallReq :: String -> String -> OnCallReq
makeOnCallReq rideID exophoneNumber = OnCallReq {
    "rideId" : rideID,
    "exophoneNumber" : exophoneNumber
}

--------------------------------- leaderBoard  --------------------------------------------------------------------------------------------------------
leaderBoardBT :: LeaderBoardReq -> FlowBT String LeaderBoardRes
leaderBoardBT request = do
    headers <- getHeaders' "" true
    case request of
        (DailyRequest date) ->
            withAPIResultBT (EP.leaderBoardDaily date) identity errorHandler (lift $ lift $ callAPI headers request)
        (WeeklyRequest fromDate toDate) ->
            withAPIResultBT (EP.leaderBoardWeekly fromDate toDate) identity errorHandler (lift $ lift $ callAPI headers request)
    where
    errorHandler (ErrorPayload errorPayload) =  do
        BackT $ pure GoBack

leaderBoard request = do
    headers <- getHeaders "" true
    case request of
        (DailyRequest date) ->
            withAPIResult (EP.leaderBoardDaily date) unwrapResponse (callAPI headers request)
        (WeeklyRequest fromDate toDate) ->
            withAPIResult (EP.leaderBoardWeekly fromDate toDate) unwrapResponse (callAPI headers request)
    where
        unwrapResponse (x) = x

driverProfileSummary :: String -> Flow GlobalState (Either ErrorResponse DriverProfileSummaryRes)
driverProfileSummary lazy = do
  headers <- getHeaders "" true
  withAPIResult (EP.profileSummary lazy) (\x -> x) (callAPI headers DriverProfileSummaryReq)

createPaymentOrder :: String -> Flow GlobalState (Either ErrorResponse CreateOrderRes)
createPaymentOrder dummy = do
    headers <- getHeaders "" true
    withAPIResult (EP.createOrder dummy) unwrapResponse $ callAPI headers (CreateOrderReq dummy)
    where
        unwrapResponse (x) = x

paymentOrderStatus :: String -> Flow GlobalState (Either ErrorResponse OrderStatusRes)
paymentOrderStatus orderId = do
    headers <- getHeaders "" false
    withAPIResult (EP.orderStatus orderId) unwrapResponse $ callAPI headers (OrderStatusReq orderId)
    where
        unwrapResponse (x) = x


getPaymentHistory :: String -> String -> Maybe String -> Flow GlobalState (Either ErrorResponse GetPaymentHistoryResp)
getPaymentHistory from to status = do
      headers <- getHeaders "" true
      withAPIResult (EP.paymentHistory from to status) unwrapResponse (callAPI headers (GetPaymentHistoryReq from to status))
   where
        unwrapResponse (x) = x


---------------------------------------- triggerAadhaarOtp ---------------------------------------------
triggerAadhaarOtp :: String -> Flow GlobalState (Either ErrorResponse GenerateAadhaarOTPResp)
triggerAadhaarOtp aadhaarNumber = do
  headers <- getHeaders "" false
  withAPIResult (EP.triggerAadhaarOTP "") unwrapResponse $ callAPI headers $ makeReq aadhaarNumber
  where
    makeReq :: String -> GenerateAadhaarOTPReq
    makeReq number = GenerateAadhaarOTPReq {
      aadhaarNumber : number,
      consent : "Y"
    }
    unwrapResponse x = x

---------------------------------------- verifyAadhaarOtp ---------------------------------------------
verifyAadhaarOtp :: String -> Flow GlobalState (Either ErrorResponse VerifyAadhaarOTPResp)
verifyAadhaarOtp aadhaarNumber = do
  headers <- getHeaders "" false
  withAPIResult (EP.verifyAadhaarOTP "") unwrapResponse $ callAPI headers $ makeReq aadhaarNumber
  where
    makeReq :: String -> VerifyAadhaarOTPReq
    makeReq otp = VerifyAadhaarOTPReq {
      otp : fromMaybe 0 $ INT.fromString otp
    , shareCode : DS.take 4 otp
    }
    unwrapResponse x = x

unVerifiedAadhaarData :: String -> String -> String -> Flow GlobalState (Either ErrorResponse ApiSuccessResult)
unVerifiedAadhaarData driverName driverGender driverDob = do
  headers <- getHeaders "" false
  withAPIResult (EP.unVerifiedAadhaarData "") unwrapResponse $ callAPI headers $ makeReq driverName driverGender driverDob
  where
    makeReq :: String -> String -> String -> UnVerifiedDataReq
    makeReq driverName driverGender driverDob = UnVerifiedDataReq {
        driverName : driverName ,
        driverGender : driverGender,
        driverDob : driverDob
    }
    unwrapResponse x = x


getKioskLocations :: String -> Flow GlobalState (Either ErrorResponse KioskLocationResp)
getKioskLocations dummy = do
    headers <- getHeaders "" false
    withAPIResult (EP.getKioskLocations "") unwrapResponse $ callAPI headers (KioskLocationReq "")
    where
        unwrapResponse (x) = x

getUiPlans :: String -> Flow GlobalState (Either ErrorResponse UiPlansResp)
getUiPlans dummy = do
    headers <- getHeaders "" false
    withAPIResult (EP.getUiPlans "") unwrapResponse $ callAPI headers (UiPlansReq "")
    where
        unwrapResponse (x) = x

getUiPlansBT :: String -> FlowBT String UiPlansResp
getUiPlansBT dummy = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.getUiPlans "") identity errorHandler (lift $ lift $ callAPI headers (UiPlansReq ""))
    where
        errorHandler (ErrorPayload errorPayload) =  do
            pure $ toast $ decodeErrorMessage errorPayload.response.errorMessage
            BackT $ pure GoBack

getCurrentPlan :: String -> Flow GlobalState (Either ErrorResponse GetCurrentPlanResp)
getCurrentPlan driverId = do
    headers <- getHeaders "" false
    withAPIResult (EP.getCurrentPlan driverId) unwrapResponse $ callAPI headers (GetCurrentPlanReq driverId)
    where
        unwrapResponse (x) = x


subscribePlan :: String  -> Flow GlobalState (Either ErrorResponse SubscribePlanResp)
subscribePlan planId = do
  headers <- getHeaders "" false
  withAPIResult (EP.subscribePlan planId) unwrapResponse $ callAPI headers $ (SubscribePlanReq planId)
  where
    unwrapResponse x = x

paymentDues :: String -> Flow GlobalState (Either ErrorResponse PaymentDuesResp)
paymentDues dummy = do
    headers <- getHeaders "" false
    withAPIResult (EP.paymentDues "") unwrapResponse $ callAPI headers (PaymentDuesReq "")
    where
        unwrapResponse (x) = x

selectPlan :: String  -> Flow GlobalState (Either ErrorResponse SelectPlanResp)
selectPlan planId = do
  headers <- getHeaders "" false
  withAPIResult (EP.selectPlan planId) unwrapResponse $ callAPI headers (SelectPlanReq planId)
  where
    unwrapResponse x = x

resumeMandate :: String -> Flow GlobalState (Either ErrorResponse ResumeMandateResp)
resumeMandate driverId = do
    headers <- getHeaders "" false
    withAPIResult (EP.resumeMandate driverId) unwrapResponse $ callAPI headers (ResumeMandateReq driverId)
    where
        unwrapResponse (x) = x

suspendMandate :: String -> Flow GlobalState (Either ErrorResponse SuspendMandateResp)
suspendMandate _ = do
    headers <- getHeaders "" false
    withAPIResult (EP.suspendMandate "") unwrapResponse $ callAPI headers (SuspendMandateReq "")
    where
        unwrapResponse (x) = x

postRideFeedback :: String ->  Int -> String -> Flow GlobalState (Either ErrorResponse PostRideFeedbackResp)
postRideFeedback rideId rating feedback = do 
    headers <- getHeaders "" false 
    withAPIResult (EP.postRideFeedback "") unwrapResponse $ callAPI headers $ makeReq rideId rating feedback
    where
        makeReq :: String -> Int -> String -> PostRideFeedbackReq
        makeReq rideId rating feedback = PostRideFeedbackReq {
            rideId : rideId,
            feedbackDetails : feedback,
            ratingValue : rating
        }
        unwrapResponse (x) = x 

paymentHistoryListV2 :: String -> String -> String -> Flow GlobalState (Either ErrorResponse HistoryEntityV2Resp)
paymentHistoryListV2 limit offset historyType = do 
    headers <- getHeaders "" false 
    withAPIResult (EP.paymentHistoryListV2 limit offset historyType) unwrapResponse $ callAPI headers (HistoryEntityV2Req limit offset historyType)
    where
        unwrapResponse (x) = x 

paymentEntityDetails :: String -> Flow GlobalState (Either ErrorResponse HistoryEntryDetailsEntityV2Resp)
paymentEntityDetails id = do 
    headers <- getHeaders "" false 
    withAPIResult (EP.paymentEntityDetails id) unwrapResponse $ callAPI headers (HistoryEntryDetailsEntityV2Req id)
    where
        unwrapResponse (x) = x 

cleardues :: String -> Flow GlobalState (Either ErrorResponse ClearDuesResp)
cleardues _ = do
    headers <- getHeaders "" false
    withAPIResult (EP.cleardues "") unwrapResponse $ callAPI headers (ClearDuesReq "")
    where
        unwrapResponse (x) = x

----------------------------------------------autoComplete-------------------------------------------------
autoComplete :: String -> String -> String -> String -> Flow GlobalState (Either ErrorResponse AutoCompleteResp)
autoComplete searchVal lat lon language = do
  headers <- getHeaders  "" true
  withAPIResult (EP.autoComplete "") unwrapResponse $ callAPI headers $ makeReq searchVal lat lon language
  where
    makeReq :: String -> String -> String -> String -> AutoCompleteReq
    makeReq searchVal lat lon language= AutoCompleteReq {
        components : "",
        sessionToken : Nothing,
        location : (lat <> "," <> lon),
        radius : 50000,
        input : searchVal,
        language : language,
        strictbounds : Just true,
        origin : LatLong {
            lat : fromMaybe 0.0 (Number.fromString lat),
            lon : fromMaybe 0.0 (Number.fromString lon)
        }
    }
    unwrapResponse (x) = x 

-----------------------------------------getPlaceName--------------------------------------------------
placeName :: GetPlaceNameReq -> Flow GlobalState (Either ErrorResponse GetPlaceNameResp)
placeName payload = do
     headers <- getHeaders  "" false
     withAPIResult (EP.getPlaceName "") unwrapResponse $ callAPI headers payload
    where
        unwrapResponse (x) = x 

makePlaceNameReq :: Number -> Number -> String -> GetPlaceNameReq
makePlaceNameReq lat lng language = GetPlaceNameReq
    {"sessionToken" : Just "",
      "language" : Just language,
      "getBy" : GetPlaceNameBy {
          "tag" : "ByLatLong",
          "contents" :LatLongType ( LatLonBody {
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

-------------------------------------------Driver Go To--------------------------------------------------

addDriverHomeLocation :: Number -> Number  -> String -> String -> Flow GlobalState (Either ErrorResponse AddHomeLocationResp)
addDriverHomeLocation lat lon address tag = do
  headers <- getHeaders "" false
  withAPIResult (EP.addDriverHomeLocation "") unwrapResponse $ callAPI headers $ makeAddReq lat lon address tag
  where
    makeAddReq :: Number -> Number -> String -> String -> AddHomeLocationReq
    makeAddReq lat lon address tag = AddHomeLocationReq {
        position : LatLong $ {
            lat : lat,
            lon : lon
        } ,
        address : address,
        tag : tag
    }
    unwrapResponse x = x


getDriverHomeLocation :: String -> Flow GlobalState (Either ErrorResponse GetHomeLocationsRes)
getDriverHomeLocation _ = do
  headers <- getHeaders "" false
  withAPIResult (EP.getDriverHomeLocation "") unwrapResponse $ callAPI headers (GetHomeLocationReq)
  where
    unwrapResponse x = x

activateDriverGoTo :: String -> String -> Flow GlobalState (Either ErrorResponse ActivateDriverGoToResp)
activateDriverGoTo id currentLocation = do
  headers <- getHeaders "" false
  withAPIResult (EP.activateDriverGoTo id currentLocation) unwrapResponse $ callAPI headers (ActivateDriverGoToReq id currentLocation)
  where
    unwrapResponse x = x

deactivateDriverGoTo :: String -> Flow GlobalState (Either ErrorResponse DeactivateDriverGoToResp)
deactivateDriverGoTo _ = do
  headers <- getHeaders "" false
  withAPIResult (EP.deactivateDriverGoTo "") unwrapResponse $ callAPI headers (DeactivateDriverGoToReq)
  where
    unwrapResponse x = x

deleteDriverHomeLocation :: String -> Flow GlobalState (Either ErrorResponse DeleteDriverHomeLocationResp)
deleteDriverHomeLocation id = do
  headers <- getHeaders "" false
  withAPIResult (EP.deleteDriverHomeLocation id) unwrapResponse $ callAPI headers (DeleteDriverHomeLocationReq id)
  where
    unwrapResponse x = x

updateDriverHomeLocation :: String -> Number ->Number -> String -> String  -> Flow GlobalState (Either ErrorResponse UpdateHomeLocationResp)
updateDriverHomeLocation qParam lat lon address tag = do
  headers <- getHeaders "" false
  withAPIResult (EP.updateDriverHomeLocation qParam) unwrapResponse $ callAPI headers $ makeUpdateReq qParam lat lon address tag
  where
    makeUpdateReq :: String -> Number -> Number -> String -> String -> UpdateHomeLocationReq
    makeUpdateReq param lat lon address tag = UpdateHomeLocationReq {
        qParam : param,
        body : AddHomeLocationReq $ {
            position : LatLong $ {
                lat : lat,
                lon : lon
            } ,
            address : address,
            tag : tag
        }
    }
    unwrapResponse x = x

rideRoute :: String -> Flow GlobalState (Either ErrorResponse RideRouteResp)
rideRoute rideId = do
  headers <- getHeaders "" false
  withAPIResult (EP.rideRoute rideId) unwrapResponse $ callAPI headers $ RideRouteReq rideId
  where
    unwrapResponse x = x

------------------------------------------------------------------------- MerchantOperatingCity List -----------------------------------------------------------------------------

getMerchantOperatingCityListBT :: String -> FlowBT String GetCityRes
getMerchantOperatingCityListBT _ = do 
    let id = if (SC.getMerchantId "") == "NA" then getValueToLocalNativeStore MERCHANT_ID else (SC.getMerchantId "" )
    headers <- getHeaders' "" false 
    withAPIResultBT (EP.getMerchantIdList id) (\x -> x) errorHandler (lift $ lift $ callAPI headers (GetCityReq id))
    where
    errorHandler errorPayload = do 
            BackT $ pure GoBack        

getCoinTransactionsReqBT :: String -> FlowBT String CoinTransactionRes
getCoinTransactionsReqBT date = do
        headers <- lift $ lift $ getHeaders "" true
        withAPIResultBT (EP.getCoinTransactions date) (\x → x) errorHandler (lift $ lift $ callAPI headers (CoinTransactionReq date))
    where
    errorHandler (ErrorPayload errorPayload) =  do
        BackT $ pure GoBack

getCoinUsageHistoryReqBT :: String -> String -> FlowBT String CoinsUsageRes
getCoinUsageHistoryReqBT limit offset = do
        headers <- lift $ lift $ getHeaders "" true
        withAPIResultBT (EP.getCoinUsageHistory limit offset) (\x → x) errorHandler (lift $ lift $ callAPI headers (CoinsUsageReq limit offset))
    where
    errorHandler (ErrorPayload errorPayload) =  do
        BackT $ pure GoBack

convertCoinToCash :: Int -> Flow GlobalState (Either ErrorResponse ConvertCoinToCashRes)
convertCoinToCash coins = do 
    headers <- getHeaders "" false 
    withAPIResult (EP.convertCoinToCash "") unwrapResponse $ callAPI headers $ makeReq coins
    where
        makeReq :: Int -> ConvertCoinToCashReq
        makeReq coins = ConvertCoinToCashReq {
            coins : coins
        }
        unwrapResponse (x) = x 

------------------------------------------------------------------------- Referred Drivers -----------------------------------------------------------------------------

referredDriversBT :: ReferredDriversReq -> FlowBT String ReferredDriversResp
referredDriversBT payload = do
    headers <- getHeaders' "" true
    withAPIResultBT (EP.referredDrivers "") identity errorHandler (lift $ lift $ callAPI headers payload)
    where 
        errorHandler (ErrorPayload errorPayload) =  do
            BackT $ pure GoBack


detectCity :: Number -> Number -> Flow GlobalState (Either ErrorResponse DetectCityResp)
detectCity lat lon = do
  headers <- getHeaders "" false
  withAPIResult (EP.detectCity "") unwrapResponse $ callAPI headers $ makeDetectCityReq
  where
    unwrapResponse x = x
    makeDetectCityReq = DetectCityReq $ {
        lat : lat,
        lon : lon
    }
