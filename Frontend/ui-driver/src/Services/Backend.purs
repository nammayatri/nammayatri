module Services.Backend where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..))
import Data.Either (Either(..), either)
import Presto.Core.Types.API (Header(..), Headers(..))
import Presto.Core.Types.Language.Flow (Flow, callAPI, doAff)
import Helpers.Utils (decodeErrorCode, decodeErrorMessage, toString,getTime)
import Foreign.Generic (encode)
import JBridge (setKeyInSharedPrefKeys,toast,factoryResetApp, toggleLoader, stopLocationPollingAPI, Locations, getVersionName)
import Juspay.OTP.Reader as Readers
import Types.ModifyScreenState(modifyScreenState)
import Types.App (GlobalState, FlowBT, ScreenType(..))
import Services.APITypes
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (bind, discard, pure, unit, ($), ($>), (&&), (*>), (<<<), (=<<), (==), void, map, show, class Show)
import Storage (KeyStore(..), deleteValueFromLocalStore, getValueToLocalStore, getValueToLocalNativeStore)
import Tracker (trackApiCallFlow, trackExceptionFlow)
import Tracker.Labels (Label(..))
import Tracker.Types as Tracker
import Services.EndPoints as EP
import Engineering.Helpers.Commons (liftFlow, bundleVersion)
import Data.Maybe
import Log (printLog)
import Effect.Class (liftEffect)
import Storage (getValueToLocalStore, KeyStore(..))
import Debug.Trace

getHeaders :: String -> Flow GlobalState Headers
getHeaders dummy = do 
    _ <- pure $ printLog "dummy" dummy
    if ((getValueToLocalStore REGISTERATION_TOKEN) == "__failed") then pure $ (Headers [Header "Content-Type" "application/json", Header "x-client-version" (getValueToLocalStore VERSION_NAME), Header "x-bundle-version" (getValueToLocalStore BUNDLE_VERSION)]) 
        else pure $ (Headers [Header "Content-Type" "application/json", Header "token" (getValueToLocalStore REGISTERATION_TOKEN), Header "x-client-version" (getValueToLocalStore VERSION_NAME), Header "x-bundle-version" (getValueToLocalStore BUNDLE_VERSION) ])

getHeaders' :: String -> FlowBT String Headers
getHeaders' dummy = do 
        _ <- pure $ printLog "dummy" dummy
        if ((getValueToLocalStore REGISTERATION_TOKEN) == "__failed") then 
          lift $ lift $ pure $ (Headers [Header "Content-Type" "application/json", Header "x-client-version" (getValueToLocalStore VERSION_NAME), Header "x-bundle-version" (getValueToLocalStore BUNDLE_VERSION)]) 
          else lift $ lift $ pure $ (Headers [Header "Content-Type" "application/json",Header "token" (getValueToLocalStore REGISTERATION_TOKEN), Header "x-client-version" (getValueToLocalStore VERSION_NAME), Header "x-bundle-version" (getValueToLocalStore BUNDLE_VERSION)])

withAPIResult url f flow = do
    let start = getTime unit
    resp <- either (pure <<< Left) (pure <<< Right <<< f <<< _.response) =<< flow
    let end = getTime unit
    _ <- pure $ printLog "withAPIResult url" url
    case resp of
        Right res -> do
            _ <- (trackApiCallFlow Tracker.Network Tracker.Info NETWORK_CALL start end 200  "SUCCESS" url "" "") $> res
            pure unit
        Left (err) -> do
            _ <- pure $ printLog "Err Code" (err.code)
            _ <- pure $ printLog "Err" err
            let errResp = err.response
            let codeMessage = decodeErrorCode errResp.errorMessage
            _ <- pure $ printLog "code" codeMessage
            let userMessage = decodeErrorMessage errResp.errorMessage

            _ <- (trackApiCallFlow Tracker.Network Tracker.Exception DETAILS start end (err.code) (codeMessage) url "" "") $> errResp
            _ <- trackExceptionFlow Tracker.API_CALL Tracker.Sdk DETAILS url (codeMessage)
            if (err.code == 401 &&  codeMessage == "INVALID_TOKEN") then do
                _ <- pure $ deleteValueFromLocalStore REGISTERATION_TOKEN
                _ <- pure $ deleteValueFromLocalStore LANGUAGE_KEY
                _ <- pure $ deleteValueFromLocalStore VERSION_NAME
                _ <- pure $ deleteValueFromLocalStore BASE_URL
                _ <- pure $ deleteValueFromLocalStore TEST_FLOW_FOR_REGISTRATOION
                _ <- pure $ deleteValueFromLocalStore IS_RIDE_ACTIVE
                _ <- pure $ deleteValueFromLocalStore IS_DRIVER_ENABLED
                -- _ <- pure $ stopLocationPollingAPI
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
            (lift $ lift $ (trackApiCallFlow Tracker.Network Tracker.Info NETWORK_CALL start end 200  "SUCCESS" url "" "")) $> res
        Left (err) -> do
            _ <- pure $ printLog "Err Code" (err.code)
            _ <- pure $ printLog "Err" err

            let errResp = err.response
            let codeMessage = decodeErrorCode errResp.errorMessage
            _ <- pure $ printLog "code" codeMessage
            let userMessage = decodeErrorMessage errResp.errorMessage
            _ <- pure $ printLog "message" userMessage
            if (err.code == 401 &&  codeMessage == "INVALID_TOKEN") then do
                deleteValueFromLocalStore REGISTERATION_TOKEN
                deleteValueFromLocalStore LANGUAGE_KEY
                deleteValueFromLocalStore VERSION_NAME
                deleteValueFromLocalStore BASE_URL
                deleteValueFromLocalStore TEST_FLOW_FOR_REGISTRATOION
                deleteValueFromLocalStore IS_RIDE_ACTIVE
                deleteValueFromLocalStore IS_DRIVER_ENABLED
                -- _ <- lift $ lift $ liftFlow $ stopLocationPollingAPI
                _ <- pure $ printLog "before" userMessage
                _ <- pure $ factoryResetApp ""
                pure unit
                -- _ <- pure $ printLog "after" userMessage
                    else if (err.code == 400 && userMessage == "Invalid start time.") then
                        -- pure $ toast (getString INVALID_START_TIME)
                        pure unit
                    else pure unit
            (lift $ lift $ (trackApiCallFlow Tracker.Network Tracker.Exception DETAILS start end (err.code) (codeMessage) url "" "")) *> (errorHandler (ErrorPayload err))

withAPIResultBT' url enableCache key f errorHandler flow = do
    let start = getTime unit
    resp <- either (pure <<< Left) (pure <<< Right <<< f <<< _.response) =<< flow
    let end = getTime unit
    _ <- pure $ printLog "withAPIResultBT' url" url
    case resp of
        Right res -> if enableCache then do
                        _ <- pure $ setKeyInSharedPrefKeys key (toString (encode res))
                        (lift $ lift $ (trackApiCallFlow Tracker.Network Tracker.Info NETWORK_CALL start end 200  "SUCCESS" url "" "")) $> res
                        else (lift $ lift $ (trackApiCallFlow Tracker.Network Tracker.Info NETWORK_CALL start end 200  "SUCCESS" url "" "")) $> res
        Left  (err) -> do
            _ <- pure $ printLog "Err Code" (err.code)
            _ <- pure $ printLog "Err" err
            let errResp = err.response
            let codeMessage = decodeErrorCode errResp.errorMessage
            _ <- pure $ printLog "code" codeMessage
            let userMessage = decodeErrorMessage errResp.errorMessage

            if (err.code == 401 &&  codeMessage == "INVALID_TOKEN") then do
                deleteValueFromLocalStore REGISTERATION_TOKEN
                deleteValueFromLocalStore LANGUAGE_KEY
                deleteValueFromLocalStore VERSION_NAME
                deleteValueFromLocalStore BASE_URL
                deleteValueFromLocalStore TEST_FLOW_FOR_REGISTRATOION
                deleteValueFromLocalStore IS_RIDE_ACTIVE
                deleteValueFromLocalStore IS_DRIVER_ENABLED
                -- _ <- lift $ lift $ liftFlow $ stopLocationPollingAPI
                pure $ factoryResetApp ""
                  else pure unit
            (lift $ lift $ (trackApiCallFlow Tracker.Network Tracker.Info NETWORK_CALL start end (err.code) (codeMessage) url "" "")) *> (errorHandler (ErrorPayload err))


--------------------------------- triggerOTPBT---------------------------------------------------------------------------------------------------------------------------------
triggerOTPBT :: TriggerOTPReq → FlowBT String TriggerOTPResp
triggerOTPBT payload = do
    _ <- lift $ lift $ doAff Readers.initiateSMSRetriever
    headers <- getHeaders' ""
    withAPIResultBT (EP.triggerOTP "") (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
    where
    errorHandler (ErrorPayload errorPayload) = do
        let errResp = errorPayload.response
        let codeMessage = decodeErrorCode errResp.errorMessage
        if (errorPayload.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then 
            pure $ toast (getString LIMIT_EXCEEDED_PLEASE_TRY_AGAIN_AFTER_10MIN)
            else pure $ toast (getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER)
        modifyScreenState $ ChooseLanguageScreenStateType (\chooseLanguageScreen -> chooseLanguageScreen { props {btnActive = false} })
        BackT $ pure GoBack


makeTriggerOTPReq :: String       → TriggerOTPReq
makeTriggerOTPReq    mobileNumber = TriggerOTPReq
    {
      "mobileNumber"      : mobileNumber,
      "mobileCountryCode" : "+91",
      "merchantId" : "7f7896dd-787e-4a0b-8675-e9e6fe93bb8f"
    }




--------------------------------- resendOTPBT ---------------------------------------------------------------------------------------------------------------------------------
resendOTPBT :: String -> FlowBT String ResendOTPResp
resendOTPBT token = do
     headers <- getHeaders' ""
     withAPIResultBT (EP.resendOTP token) (\x → x) errorHandler (lift $ lift $ callAPI headers (ResendOTPRequest token))
    where
    errorHandler (ErrorPayload errorPayload)  = do
        let errResp = errorPayload.response
        let codeMessage = decodeErrorCode errResp.errorMessage
        if ( errorPayload.code == 400 && codeMessage == "AUTH_BLOCKED") then
            pure $ toast (getString LIMIT_EXCEEDED)
            else pure $ toast (getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER)
        BackT $ pure GoBack




--------------------------------- verifyTokenBT ---------------------------------------------------------------------------------------------------------------------------------
verifyTokenBT :: VerifyTokenReq -> String -> FlowBT String VerifyTokenResp
verifyTokenBT payload token = do
    headers <- getHeaders' ""
    withAPIResultBT (EP.verifyToken token) (\x → x) errorHandler (lift $ lift $ callAPI headers (VerifyTokenRequest token payload))
    where
    errorHandler (ErrorPayload errorPayload) = do
        let errResp = errorPayload.response
        let codeMessage = decodeErrorCode errResp.errorMessage
        if ( errorPayload.code == 400 && codeMessage == "TOKEN_EXPIRED") then
            pure $ toast (getString YOUR_REQUEST_HAS_TIMEOUT_TRY_AGAIN)
            else if ( errorPayload.code == 400 && codeMessage == "INVALID_AUTH_DATA") then do
                modifyScreenState $ EnterOTPScreenType (\enterOTPScreen -> enterOTPScreen { props {isValid = true} })
                void $ lift $ lift $ toggleLoader false
            else if ( errorPayload.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then
                pure $ toast (getString LIMIT_EXCEEDED_PLEASE_TRY_AGAIN_AFTER_10MIN)
            else pure $ toast (getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER)
        BackT $ pure GoBack

makeVerifyOTPReq :: String -> VerifyTokenReq
makeVerifyOTPReq otp = VerifyTokenReq {
      "otp": otp,
      "deviceToken": if getValueToLocalNativeStore FCM_TOKEN == "__failed" then "" else (getValueToLocalNativeStore FCM_TOKEN),
      "whatsappNotificationEnroll": OPT_IN
    }

------------------------------------------ driverActiveInactiveBT -------------------------------------------------------------
driverActiveInactiveBT :: String -> FlowBT String DriverActiveInactiveResp
driverActiveInactiveBT status = do
        headers <- getHeaders' ""
        withAPIResultBT (EP.driverActiveInactive status) (\x → x) errorHandler (lift $ lift $ callAPI headers (DriverActiveInactiveReq status ))
    where
        errorHandler (ErrorPayload errorPayload) =  do 
            modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen { props { goOfflineModal = true }})
            pure $ toast (getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER)
            void $ lift $ lift $ toggleLoader false
            BackT $ pure GoBack
--------------------------------- startRide ---------------------------------------------------------------------------------------------------------------------------------

startRide productId payload = do
        headers <- getHeaders ""
        withAPIResult (EP.startRide productId) unwrapResponse $ callAPI headers ((StartRideRequest productId payload))
    where
        unwrapResponse (x) = x

makeStartRideReq :: String -> Number -> Number -> StartRideReq 
makeStartRideReq otp lat lon = StartRideReq {
    "rideOtp": otp,
    "point": Point {
        "lat" : lat,
        "lon" : lon
        }
}
--------------------------------- endRide ---------------------------------------------------------------------------------------------------------------------------------

endRide :: String -> EndRideReq -> FlowBT String EndRideResponse
endRide productId payload = do
        headers <-getHeaders' ""
        withAPIResultBT (EP.endRide productId) (\x → x) errorHandler (lift $ lift $ callAPI headers (EndRideRequest productId payload))
    where
      errorHandler (ErrorPayload errorPayload) =  do
            void $ lift $ lift $ toggleLoader false
            BackT $ pure GoBack

makeEndRideReq :: Number -> Number -> EndRideReq 
makeEndRideReq lat lon = EndRideReq { 
    "point" :  Point 
    {
        "lat" : lat,
        "lon" : lon
    }
}

--------------------------------- driverCancelRide ---------------------------------------------------------------------------------------------------------------------------------

cancelRide :: String -> DriverCancelRideReq -> FlowBT String DriverCancelRideResponse
cancelRide productId payload = do
        headers <-getHeaders' ""
        withAPIResultBT (EP.cancelRide productId) (\x → x) errorHandler (lift $ lift $ callAPI headers (DriverCancelRideRequest productId payload))
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
        headers <- getHeaders' ""
        withAPIResultBT (EP.logout "") (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
    where
        errorHandler (ErrorPayload errorPayload) = do
            BackT $ pure GoBack

--------------------------------- getDriverInfoBT ---------------------------------------------------------------------------------------------------------------------------------

getDriverInfoBT :: GetDriverInfoReq -> FlowBT String GetDriverInfoResp
getDriverInfoBT payload = do
     headers <- getHeaders' ""
     withAPIResultBT ((EP.getDriverInfo "" )) (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
    where
        errorHandler (ErrorPayload errorPayload) =  do
            BackT $ pure GoBack

getDriverInfoApi payload = do
     _ <-pure $ spy "(getValueToLocalStore REGISTERATION_TOKEN) after" (getValueToLocalStore REGISTERATION_TOKEN)
     _ <- pure $ spy "(getValueToLocalStore REGISTERATION_TOKEN) before" (getValueToLocalStore REGISTERATION_TOKEN)
    --  _ <- pure $ spy "(getValueToLocalStore REGISTERATION_TOKEN) before effect" (liftEffect $ (getValueToLocalStoreNew REGISTERATION_TOKEN))
     headers <- getHeaders ""
    --  pure $ printLog "headers" headers
     _ <- pure $ spy "(getValueToLocalStore REGISTERATION_TOKEN) after" (getValueToLocalStore REGISTERATION_TOKEN)
     _ <- pure $ spy "(getValueToLocalStore REGISTERATION_TOKEN) after" (getValueToLocalStore REGISTERATION_TOKEN)
    --  _ <- pure $ spy "(getValueToLocalStore REGISTERATION_TOKEN) after effetct" (liftEffect $ (getValueToLocalStoreNew REGISTERATION_TOKEN))
     withAPIResult (EP.getDriverInfo "") unwrapResponse $ callAPI headers (GetDriverInfoReq { })
    where
        unwrapResponse (x) = x

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
    headers <- getHeaders' ""
    withAPIResultBT (EP.offerRide "") (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
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

getRideHistoryReq limit offset onlyActive = do
        headers <- getHeaders ""
        withAPIResult (EP.getRideHistory limit offset onlyActive) unwrapResponse $ callAPI headers (GetRidesHistoryReq limit offset onlyActive)
    where
        unwrapResponse (x) = x


getRideHistoryReqBT :: String -> String -> String -> FlowBT String GetRidesHistoryResp
getRideHistoryReqBT limit offset onlyActive = do
        headers <- lift $ lift $ getHeaders ""
        withAPIResultBT (EP.getRideHistory limit offset onlyActive) (\x → x) errorHandler (lift $ lift $ callAPI headers (GetRidesHistoryReq limit offset onlyActive))
    where
    errorHandler (ErrorPayload errorPayload) =  do 
        BackT $ pure GoBack
--------------------------------- updateDriverInfoBT ---------------------------------------------------------------------------------------------------------------------------------
updateDriverInfoBT :: UpdateDriverInfoReq -> FlowBT String UpdateDriverInfoResp
updateDriverInfoBT payload = do
        headers <-getHeaders' ""
        withAPIResultBT (EP.updateDriverInfo "") (\x → x) errorHandler (lift $ lift $ callAPI headers (UpdateDriverInfoRequest payload))
    where
        errorHandler (ErrorPayload errorPayload) =  do
            BackT $ pure GoBack

makeUpdateDriverInfoReq :: String -> UpdateDriverInfoReq
makeUpdateDriverInfoReq deviceToken = UpdateDriverInfoReq {
       "middleName": Nothing,
       "firstName" : Nothing,
       "lastName"  : Nothing,
       "deviceToken" : Nothing,
       "canDowngradeToSedan" : Nothing,
       "canDowngradeToHatchback" : Nothing,
       "language" : Nothing
    }

makeUpdateDriverLangChange :: String -> UpdateDriverInfoReq
makeUpdateDriverLangChange deviceToken = UpdateDriverInfoReq {
        "middleName": Nothing,
        "firstName" : Nothing,
        "lastName"  : Nothing,
        "deviceToken" : Nothing,
        "canDowngradeToSedan" : Nothing,
        "canDowngradeToHatchback" : Nothing,
        "language" : Just case getValueToLocalNativeStore LANGUAGE_KEY of
            "EN_US" -> "ENGLISH"
            "KN_IN" -> "KANNADA"
            "HI_IN" -> "HINDI"
            "ML_IN" -> "MALAYALAM"
            "TA_IN" -> "TAMIL"
            _       -> "ENGLISH"
    }

--------------------------------- getDriverInfoBT ---------------------------------------------------------------------------------------------------------------------------------

listCancelReasonBT :: ListCancelReasonReq -> FlowBT String ListCancelReasonResp
listCancelReasonBT payload = do
    headers <- getHeaders' ""
    withAPIResultBT (EP.listCancelReason "" ) (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
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

getRouteBT :: GetRouteReq -> FlowBT String GetRouteResp
getRouteBT body = do
     headers <- lift $ lift $ getHeaders ""
     withAPIResultBT (EP.getRoute "") (\x → x) errorHandler (lift $ lift $ callAPI headers body)
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
    "journeyCoordinates": [
      {
        "lat": fromLat,
        "lng": fromLong
      },
      {
        "lat": driverLat,
        "lng": driverLong
      }
    ],
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

walkCoordinates :: Snapped -> Snapped -> Locations
walkCoordinates (Snapped points) (Snapped snappedWaypoints) = {
    "journeyCoordinates": map (\(LatLong item) -> {"lat": item.lat, "lng": item.lon})snappedWaypoints,
    "points": map (\(LatLong item) -> {"lat": item.lat, "lng": item.lon})points
}

dummySnapped :: Snapped
dummySnapped = Snapped[LatLong{"lat": 0.0, "lon": 0.0}]

--------------------------------- onBoardingFlow  ---------------------------------------------------------------------------------------------------------------------------------

registerDriverRCBT :: DriverRCReq -> FlowBT String  DriverRCResp 
registerDriverRCBT payload = do 
        headers <- getHeaders' ""
        withAPIResultBT (EP.registerDriverRC "" ) (\x -> x) errorHandler (lift $ lift $ callAPI headers payload)
    where
    errorHandler (ErrorPayload errorPayload) = do 
        BackT $ pure GoBack

registerDriverRC payload = do
     headers <- getHeaders ""
     withAPIResult (EP.registerDriverRC "") unwrapResponse $ callAPI headers payload
    where
        unwrapResponse (x) = x

makeDriverRCReq :: String -> String -> Maybe String -> DriverRCReq
makeDriverRCReq regNo imageId dateOfRegistration = DriverRCReq
    {
      "vehicleRegistrationCertNumber" : regNo,
      "operatingCity" : "BANGALORE",
      "imageId" : imageId,
      "dateOfRegistration" : dateOfRegistration
    }

registerDriverDLBT :: DriverDLReq -> FlowBT String  DriverDLResp 
registerDriverDLBT payload = do 
        headers <- getHeaders' ""
        withAPIResultBT (EP.registerDriverDL "" ) (\x -> x) errorHandler (lift $ lift $ callAPI headers payload)
    where
    errorHandler (ErrorPayload errorPayload) = do 
        BackT $ pure GoBack

registerDriverDL payload = do
     headers <- getHeaders ""
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
        headers <- getHeaders' ""
        withAPIResultBT (EP.validateImage "") (\x -> x) errorHandler (lift $ lift $ callAPI headers payload)
    where
    errorHandler (ErrorPayload errorPayload) = do 
        BackT $ pure GoBack

validateImage payload = do
     headers <- getHeaders ""
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
     headers <- getHeaders' ""
     withAPIResultBT ((EP.driverRegistrationStatus "" )) (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
    where
        errorHandler (ErrorPayload errorPayload) =  do
            BackT $ pure GoBack

referDriver payload = do
     headers <- getHeaders ""
     withAPIResult (EP.referDriver "") unwrapResponse $ callAPI headers payload
    where
        unwrapResponse (x) = x

makeReferDriverReq :: String -> ReferDriverReq
makeReferDriverReq referralNumber = ReferDriverReq
    {
      "value" : referralNumber
    }

--------------------------------- getDriverInfoBT  ---------------------------------------------------------------------------------------------------------------------------------

getDriverProfileStatsBT :: DriverProfileStatsReq -> FlowBT String DriverProfileStatsResp
getDriverProfileStatsBT payload = do
     headers <- getHeaders' ""
     withAPIResultBT ((EP.getstatsInfo "" )) (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
    where
        errorHandler (ErrorPayload errorPayload) =  do
            BackT $ pure GoBack

--------------------------------- getDriverInfoBT  ---------------------------------------------------------------------------------------------------------------------------------getDriverProfileStatsBT :: DriverProfileStatsReq -> FlowBT String DriverProfileStatsResp
driverArrivedBT :: String -> DriverArrivedReq -> FlowBT String DriverArrivedRes
driverArrivedBT rideId payload = do
     headers <- getHeaders' ""
     withAPIResultBT ((EP.driverArrived rideId)) (\x → x) errorHandler (lift $ lift $ callAPI headers (DriverArrivedRequest rideId payload))
    where
        errorHandler (ErrorPayload errorPayload) =  do
            BackT $ pure GoBack

flowStatusBT :: String -> FlowBT String FlowStatusRes
flowStatusBT _ = do 
        headers <- getHeaders' ""
        withAPIResultBT (EP.flowStatus "") (\x → x) errorHandler (lift $ lift $ callAPI headers FlowStatusReq)
    where
        errorHandler errorPayload = do
            BackT $ pure GoBack
--------------------------------- messageList  --------------------------------------------------------------------------------------------------------
messageListBT :: String -> String -> FlowBT String MessageListRes
messageListBT limit offset = do
        headers <- lift $ lift $ getHeaders ""
        withAPIResultBT (EP.messageList limit offset) (\x → x) errorHandler (lift $ lift $ callAPI headers (MessageListReq limit offset))
    where
    errorHandler (ErrorPayload errorPayload) =  do 
        BackT $ pure GoBack

--------------------------------- messageSeen  --------------------------------------------------------------------------------------------------------
messageSeenBT :: String -> FlowBT String MessageSeenRes
messageSeenBT messageId = do
        headers <- lift $ lift $ getHeaders ""
        withAPIResultBT (EP.messageSeen messageId) (\x → x) errorHandler (lift $ lift $ callAPI headers (MessageSeenReq messageId))
    where
    errorHandler (ErrorPayload errorPayload) =  do 
        BackT $ pure GoBack

--------------------------------- messageResponse --------------------------------------------------------------------------------------------------------
messageResponseBT :: String -> MessageReplyReq -> FlowBT String MessageResponseRes
messageResponseBT messageId reply = do
        headers <- lift $ lift $ getHeaders ""
        withAPIResultBT (EP.messageResponse messageId) (\x → x) errorHandler (lift $ lift $ callAPI headers (MessageResponseReq messageId reply))
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
     headers <- getHeaders ""
     withAPIResult (EP.linkReferralCode "") unwrapResponse $ callAPI headers payload
    where
        unwrapResponse (x) = x

---------------------------------------- getPerformance ---------------------------------------------
getPerformanceBT :: GetPerformanceReq -> FlowBT String GetPerformanceRes
getPerformanceBT payload = do
     headers <- getHeaders' ""
     withAPIResultBT (EP.getPerformance "") (\x → x) errorHandler (lift $ lift $ callAPI headers payload)
    where
        errorHandler (ErrorPayload errorPayload) =  do
            BackT $ pure GoBack
