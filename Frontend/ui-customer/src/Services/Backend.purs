{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Services.Backend where

import Locale.Utils
import Services.API
import Common.RemoteConfig.Utils (stuckRideFilterConfig)
import Accessor (_deviceToken)
import Common.Types.App (Version(..), SignatureAuthData(..), LazyCheck(..), FeedbackAnswer, City(..))
import ConfigProvider as CP
import Control.Monad.Except.Trans (lift, runExceptT)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..), runBackT)
import Data.Array ((!!), catMaybes, concat, take, any, singleton, find, filter, length, null, mapMaybe, head)
import Data.Either (Either(..), either)
import Data.Int as INT
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String as DS
import Data.Foldable (or)
import Resources.Constants as Constants
import Engineering.Helpers.Events as Events
import Engineering.Helpers.Commons (liftFlow, os, convertUTCtoISC, isPreviousVersion, isInvalidUrl, getNewIDWithTag, flowRunner)
import Engineering.Helpers.Utils as EHU
import Engineering.Helpers.Commons as EHC
import Foreign.Generic (encode)
import Foreign.Object (empty)
import Helpers.Utils (decodeError, getTime, decodeErrorCode)
import JBridge (factoryResetApp, setKeyInSharedPrefKeys, removeAllPolylines, stopChatListenerService, MapRouteConfig, Locations, factoryResetApp, setKeyInSharedPrefKeys, drawRoute, toggleBtnLoader)
import JBridge as JB
import Juspay.OTP.Reader as Readers
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog)
import ModifyScreenState (modifyScreenState)
import Prelude
import Presto.Core.Types.API (Header(..), Headers(..), ErrorResponse)
import Presto.Core.Types.Language.Flow (Flow, APIResult, doAff, loadS, fork)
import Screens.Types (TicketServiceData, AccountSetUpScreenState(..), HomeScreenState(..), NewContacts, DisabilityT(..), Address, Stage(..), TicketBookingScreenData(..), AutoCompleteReqType(..))
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
import MerchantConfig.Types (GeoCodeConfig)
import Debug
import Effect.Uncurried (runEffectFn10)
import Data.Function.Uncurried (runFn2)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Mobility.Prelude as MP
import SessionCache
import LocalStorage.Cache (removeValueFromCache)
import Helpers.API (callApiBT, getDeviceDetails)
import Services.API (ServiceabilityType(..)) as ServiceabilityType
import Screens.PermissionScreen.Handler as PermissionScreen
import Screens.Types as ST
import Engineering.Helpers.BackTrack
import Effect.Aff
import Helpers.API (noInternetScreenHandler)
import DecodeUtil
import Data.Int (fromString, toNumber)
import Services.CallAPI (callAPI)

getHeaders :: String -> Boolean -> Flow GlobalState Headers
getHeaders val isGzipCompressionEnabled = do
    regToken <- loadS $ show REGISTERATION_TOKEN
    pure $ Headers $ [   Header "Content-Type" "application/json",
                        Header "x-client-version" (getValueToLocalStore VERSION_NAME),
                        Header "x-config-version" (getValueFromWindow "CONFIG_VERSION"),
                        Header "x-bundle-version" (getValueToLocalStore BUNDLE_VERSION),
                        Header "session_id" (getValueToLocalStore SESSION_ID),
                        Header "x-device" getDeviceDetails,
                        Header "client-id" (getValueToLocalStore CUSTOMER_CLIENT_ID)
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
                        Header "x-config-version" (getValueToLocalStore CONFIG_VERSION),
                        Header "x-bundle-version" (getValueToLocalStore BUNDLE_VERSION),
                        Header "session_id" (getValueToLocalStore SESSION_ID),
                        Header "x-device" getDeviceDetails,
                        Header "client-id" (getValueToLocalStore CUSTOMER_CLIENT_ID)
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
    resp <- Events.measureDurationFlow ("CallAPI." <> DS.replace (DS.Pattern $ SC.getBaseUrl "") (DS.Replacement "") url) $ either (pure <<< Left) (pure <<< Right <<< f <<< _.response) =<< flow    
    let end = getTime unit
    void $ pure $ printLog "withAPIResult url" url
    case resp of
        Right res -> do
            let _ = setKeyInWindow "noInternetCount" 0 
            void $ pure $ printLog "success resp" res
        Left (err) -> do
            _ <- pure $ toggleBtnLoader "" false
            let errResp = err.response
            _ <- pure $ printLog "error resp" errResp
            let userMessage = decodeError errResp.errorMessage "errorMessage"
            let codeMessage = decodeError errResp.errorMessage "errorCode"
            if err.code == -1 then do noInternetScreenHandler "lazy"
            else if (err.code == 401 && (codeMessage == "INVALID_TOKEN" || codeMessage == "TOKEN_EXPIRED")) || (err.code == 400 && codeMessage == "TOKEN_EXPIRED") then do
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
    resp <- Events.measureDurationFlowBT ("CallAPI." <> DS.replace (DS.Pattern $ SC.getBaseUrl "") (DS.Replacement "") url) $ either (pure <<< Left) (pure <<< Right <<< f <<< _.response) =<< flow    
    let end = getTime unit
    _ <- pure $ printLog "withAPIResultBT url" url
    case resp of
        Right res -> do
            let _ = setKeyInWindow "noInternetCount" 0
            pure res
        Left err -> do
            _ <- pure $ toggleBtnLoader "" false
            let errResp = err.response
            let userMessage = decodeError errResp.errorMessage "errorMessage"
            let codeMessage = decodeError errResp.errorMessage "errorCode"
            _ <- pure $ printLog "error resp" errResp
            if err.code == -1 then do lift $ lift $ noInternetScreenHandler "lazy"
            else if (err.code == 401 && (codeMessage == "INVALID_TOKEN" || codeMessage == "TOKEN_EXPIRED")) || (err.code == 400 && codeMessage == "TOKEN_EXPIRED") then do
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
            void $ lift $ lift $ EHU.showToast $ (getString OTP_RESENT_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER)
            else void $ lift $ lift $ EHU.showToast $ (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
        modifyScreenState $ ChooseLanguageScreenStateType (\chooseLanguage -> chooseLanguage { props {btnActive = false} })
        void $ lift $ lift $ EHU.toggleLoader false
        BackT $ pure GoBack


makeTriggerOTPReq :: String -> String -> String -> Boolean -> TriggerOTPReq
makeTriggerOTPReq mobileNumber countryCode otpChannel allowBlockedUserLogin =
    let merchant = SC.getMerchantId ""
    in TriggerOTPReq
    {
      "mobileNumber"      : mobileNumber,
      "mobileCountryCode" : countryCode,
      "merchantId" : if merchant == "NA" then getValueToLocalNativeStore MERCHANT_ID else merchant,
      "otpChannel" : otpChannel,
      "allowBlockedUserLogin" : allowBlockedUserLogin
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
            void $ lift $ lift $ EHU.showToast  (getString OTP_RESENT_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER)
            else void $ lift $ lift $ EHU.showToast  (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
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
            void $ lift $ lift $ EHU.showToast  (getString OTP_PAGE_HAS_BEEN_EXPIRED_PLEASE_REQUEST_OTP_AGAIN)
            else if ( errorPayload.code == 400 && codeMessage == "INVALID_AUTH_DATA") then do
                modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumber -> enterMobileNumber{props{wrongOTP = true, btnActiveOTP = false}})
                void $ lift $ lift $ EHU.toggleLoader false
                void $ lift $ lift $ EHU.showToast  "INVALID_AUTH_DATA"
            else if ( errorPayload.code == 429 && codeMessage == "HITS_LIMIT_EXCEED") then
                void $ lift $ lift $ EHU.showToast  (getString OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER)
            else void $ lift $ lift $ EHU.showToast  (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
        BackT $ pure GoBack

-- verifyTokenBT :: VerifyTokenReq -> String -> FlowBT String VerifyTokenResp
verifyToken :: VerifyTokenReq -> String -> Flow GlobalState (Either ErrorResponse VerifyTokenResp)
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

logOutBT :: LogOutReq -> FlowBT String APISuccessResp
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
                modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{currentStage  = HomeScreen}})
                BackT $ pure GoBack


makeSearchLocationReq :: String -> Number -> Number -> String -> String -> GeoCodeConfig -> Maybe AutoCompleteReqType -> String -> Maybe String -> SearchLocationReq
makeSearchLocationReq input lat lng language components geoCodeConfig autoCompleteType sessionToken searchType = SearchLocationReq {
    "input" : input,
    "location" : (show lat <> "," <> show lng),
    "radius" : geoCodeConfig.radius,
    "components" : components,
    "language" : language,
    "searchType" : searchType,
    "strictbounds": if geoCodeConfig.strictBounds then Just true else Nothing,
    "origin" : LatLong {
            "lat" : lat,
            "lon" : lng
            },
    "sessionToken" : Just sessionToken,
    "autoCompleteType" : (show <$> autoCompleteType)
    }

------------------------------------------------------------------------ OnCallBT Function ------------------------------------------------------------------------------------

onCallBT :: OnCallReq -> FlowBT String APISuccessResp
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
        void $ lift $ lift $ EHU.showToast  (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
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
        let _ = JB.removeKeysInSharedPrefs $ show RATE_CARD_INFO
        void $ pure $ removeValueFromCache (show RATE_CARD_INFO)
        withAPIResultBT (EP.searchReq "") identity handleError (lift $ lift $ callAPI headers payload)
    where
        handleError :: ErrorResponse -> FlowBT String SearchRes
        handleError errorPayload = do
            let errResp = errorPayload.response
                userMessage = decodeError errResp.errorMessage "errorMessage"
                codeMessage = decodeError errResp.errorMessage "errorCode"
                message = case errorPayload.code, codeMessage, userMessage of
                            400, "RIDE_NOT_SERVICEABLE", _ -> getString RIDE_NOT_SERVICEABLE
                            400, _, "ACTIVE_BOOKING_PRESENT_FOR_OTHER_INVOLVED_PARTIES" -> getString BOOKING_CANNOT_PROCEED_ONE_PARTY_HAS_ACTIVE_BOOKING
                            400, _, _ -> codeMessage
                            _,_,_ -> getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN
            void $ lift $ lift $ EHU.showToast  message
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props{currentStage = HomeScreen}})
            void $ pure $ setValueToLocalStore LOCAL_STAGE "HomeScreen"
            void $ lift $ lift $ EHU.toggleLoader false
            BackT $ pure GoBack


makeRideSearchReq :: Number -> Number -> Number -> Number -> Address -> Address -> String -> Boolean -> Boolean -> String -> Boolean -> ST.FareProductType -> SearchReq
makeRideSearchReq slat slong dlat dlong srcAdd desAdd startTime sourceManuallyMoved destManuallyMoved sessionToken isSpecialLocation searchActionType = -- check this for rentals
    let appConfig = CP.getAppConfig CP.appConfig
        searchRequest = ( OneWaySearchReq
                { "startTime" : Just startTime
                , "destination" : SearchReqLocation 
                    { "gps" : LatLong 
                        { "lat" : dlat 
                        , "lon" : dlong
                        }
                    , "address" : validateLocationAddress dlat dlong (LocationAddress desAdd)
                    }
                , "origin" : SearchReqLocation 
                    { "gps" : LatLong 
                        { "lat" : slat 
                        , "lon" : slong
                        }
                    , "address" : validateLocationAddress slat slong (LocationAddress srcAdd)
                    }
                , "isReallocationEnabled" : Just appConfig.feature.enableReAllocation
                , "isSourceManuallyMoved" : Just sourceManuallyMoved
                , "isDestinationManuallyMoved" : Just destManuallyMoved
                , "sessionToken" : Just sessionToken
                , "isSpecialLocation" : Just isSpecialLocation
                , "quotesUnifiedFlow" : Just true
                , "rideRequestAndRideOtpUnifiedFlow" : if searchActionType == ST.AMBULANCE then Just false else Just true 
                }
            )
    in case searchActionType of
        ST.DELIVERY -> SearchReq 
            { "contents" : DeliverySearchRequest searchRequest
            , "fareProductType" : "DELIVERY"
            }
        _ -> SearchReq 
            { "contents" : OneWaySearchRequest searchRequest
            , "fareProductType" : show searchActionType
            }
            
    where 
        validateLocationAddress :: Number -> Number -> LocationAddress -> LocationAddress
        validateLocationAddress lat long address = 
            let addressValidated = validateAddressHelper address
            in 
                if addressValidated
                    then address 
                    else fallbackAndFetchAgain lat long

        validateAddressHelper :: LocationAddress -> Boolean
        validateAddressHelper (LocationAddress address) = 
            or
                [ isNonEmpty address.area
                , isNonEmpty address.state
                , isNonEmpty address.country
                , isNonEmpty address.building
                , isNonEmpty address.door
                , isNonEmpty address.street
                , isNonEmpty address.city
                , isNonEmpty address.areaCode
                , isNonEmpty address.ward
                , isNonEmpty address.placeId
                ]
        
        fallbackAndFetchAgain :: Number -> Number -> LocationAddress
        fallbackAndFetchAgain lat long = 
            let addressFetched = runFn2 JB.getLocationNameV2 lat long
            in 
                if addressFetched /= "NO_LOCATION_FOUND" then
                    LocationAddress $ Constants.encodeAddress addressFetched [] Nothing lat long
                else do
                    let calNearbyLocation = MP.calculateNearbyLocation lat long
                        nearbyAddressFetched = runFn2 JB.getLocationNameV2 lat long
                    if nearbyAddressFetched /= "NO_LOCATION_FOUND" then
                        LocationAddress $ Constants.encodeAddress nearbyAddressFetched [] Nothing lat long
                    else
                        LocationAddress $ Constants.encodeAddress (getValueToLocalStore CUSTOMER_LOCATION) [] Nothing lat long

        isNonEmpty :: Maybe String -> Boolean
        isNonEmpty = maybe false (\s -> not $ DS.null s)

------------------------------------------------------------------------ GetQuotes Function -------------------------------------------------------------------------------------------
getQuotes :: String -> Flow GlobalState (Either ErrorResponse GetQuotesRes)
getQuotes searchId = do
        headers <- getHeaders "" true
        withAPIResult (EP.getQuotes searchId) unwrapResponse $ callAPI headers (GetQuotesReq searchId)
    where
        unwrapResponse (x) = x

------------------------------------------------------------------------ RideConfirm Function ---------------------------------------------------------------------------------------
rideConfirm :: String -> Flow GlobalState (Either ErrorResponse ConfirmRes)
rideConfirm quoteId = do
        headers <- getHeaders "" false
        withAPIResult (EP.confirmRide quoteId) unwrapResponse $ callAPI headers (ConfirmRequest quoteId)
    where
        unwrapResponse (x) = x

addOrEditStop :: String -> StopReq -> Boolean -> Flow GlobalState (Either ErrorResponse APISuccessResp)
addOrEditStop bookingId req isEdit = do 
    headers <- getHeaders "" false
    withAPIResult (EP.addOrEditStop isEdit bookingId) unwrapResponse $ callAPI headers (StopRequest bookingId isEdit req)
    where
        unwrapResponse (x) = x

------------------------------------------------------------------------ SelectEstimateBT Function ------------------------------------------------------------------------------------

selectEstimateBT :: DEstimateSelect -> String -> FlowBT String APISuccessResp
selectEstimateBT payload estimateId = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.selectEstimate estimateId) identity errorHandler (lift $ lift $ callAPI headers (SelectEstimateReq estimateId payload))
    where
      errorHandler errorPayload = do
            let errResp = errorPayload.response
                codeMessage = decodeError errResp.errorMessage "errorCode"
                userMessage = decodeError errResp.errorMessage "errorMessage"
            case errorPayload.code, codeMessage, userMessage of
                400, "SEARCH_REQUEST_EXPIRED", _ -> void $ lift $ lift $ EHU.showToast  (getString ESTIMATES_EXPIRY_ERROR)
                400, _, "ACTIVE_BOOKING_PRESENT_FOR_OTHER_INVOLVED_PARTIES" -> void $ lift $ lift $ EHU.showToast  (getString BOOKING_CANNOT_PROCEED_ONE_PARTY_HAS_ACTIVE_BOOKING)
                400, _, _ -> void $ lift $ lift $ EHU.showToast  codeMessage
                _, _, _ -> void $ lift $ lift $ EHU.showToast  (getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN)
            modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen {props{currentStage = SearchLocationModel}})
            _ <- pure $ setValueToLocalStore LOCAL_STAGE "SearchLocationModel"
            BackT $ pure GoBack

selectEstimate :: DEstimateSelect -> String -> Flow GlobalState (Either ErrorResponse APISuccessResp)
selectEstimate payload estimateId = do
        headers <- getHeaders "" false
        withAPIResult (EP.selectEstimate estimateId) unwrapResponse $ callAPI headers (SelectEstimateReq estimateId payload)
    where
        unwrapResponse (x) = x

makeEstimateSelectReq :: Boolean -> Maybe Int -> Array String -> Boolean -> Maybe DeliveryDetails -> DEstimateSelect
makeEstimateSelectReq isAutoAssigned tipForDriver otherSelectedEstimates isAdvancedBookingEnabled deliveryDetails = DEstimateSelect {
      "customerExtraFee": tipForDriver,
      "autoAssignEnabled": isAutoAssigned,
      "autoAssignEnabledV2": isAutoAssigned,
      "otherSelectedEstimates": otherSelectedEstimates,
      "isAdvancedBookingEnabled": isAdvancedBookingEnabled,
      "deliveryDetails": deliveryDetails
    }

------------------------------------------------------------------------ SelectList Function ------------------------------------------------------------------------------------------
selectList :: String -> Flow GlobalState (Either ErrorResponse SelectListRes)
selectList estimateId = do
        headers <- getHeaders "" true
        withAPIResult (EP.selectList estimateId) unwrapResponse $ callAPI headers (SelectListReq estimateId)
    where
        unwrapResponse (x) = x

------------------------------------------------------------------------ RideBooking Function -----------------------------------------------------------------------------------------
rideBooking :: String -> Flow GlobalState (Either ErrorResponse RideBookingRes)
rideBooking bookingId = do
        headers <- getHeaders "" true
        withAPIResult (EP.ridebooking bookingId) unwrapResponse $ callAPI headers (RideBookingReq bookingId)
    where
        unwrapResponse (x) = x

ridebookingStatus bookingId = do
        headers <- getHeaders "" true
        withAPIResult (EP.ridebookingStatus bookingId) unwrapResponse $ callAPI headers (RideBookingStatusReq bookingId)
    where
        unwrapResponse (x) = x
------------------------------------------------------------------------ CancelRideBT Function ----------------------------------------------------------------------------------------
cancelRideBT :: CancelReq -> String -> FlowBT String APISuccessResp
cancelRideBT payload bookingId = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.cancelRide bookingId) identity errorHandler (lift $ lift $ callAPI headers (CancelRequest payload bookingId))
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack

cancelRide :: CancelReq -> String -> Flow GlobalState (Either ErrorResponse APISuccessResp)
cancelRide payload bookingId = do
        headers <- getHeaders "" false
        withAPIResult (EP.cancelRide bookingId) unwrapResponse $ callAPI headers (CancelRequest payload bookingId)
    where
        unwrapResponse (x) = x

makeCancelRequest :: String -> String -> CancelReq
makeCancelRequest cancelDescription cancelReasonCode = CancelReq {
    "additionalInfo" : Just cancelDescription
  , "reasonCode" : cancelReasonCode
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

makeFeedBackReqs :: Int -> String -> String -> Maybe Boolean -> String -> Maybe Boolean -> Maybe Boolean -> FeedbackReq
makeFeedBackReqs rating rideId feedback favDriver audio wasRideSafe wasOfferedAssistance = FeedbackReq
    {   "rating" : rating
    ,   "rideId" : rideId
    ,   "feedbackDetails" : feedback
    ,   "shouldFavDriver" : favDriver
    ,   "wasRideSafe" : wasRideSafe
    ,   "wasOfferedAssistance" : wasOfferedAssistance
    ,   "mbAudio" : if audio == "" then Nothing else Just audio
    }

removeFavouriteDriver :: String -> Flow GlobalState (Either ErrorResponse RemoveFavouriteDriverResp)
removeFavouriteDriver id = do
        headers <- getHeaders "" false
        withAPIResult (EP.removeFavouriteDriver id) unwrapResponse $ callAPI headers (RemoveFavouriteDriverReq id)
    where
        unwrapResponse (x) = x

getFavouriteDriverList = do
        headers <- getHeaders "" false
        withAPIResult (EP.getFavouriteDriverList)  unwrapResponse $ callAPI headers (GetFavouriteDriverListReq)
    where
        unwrapResponse (x) = x

getFavouriteDriverTrips :: String -> String -> String -> String -> Flow GlobalState (Either ErrorResponse FavouriteDriverTripsResp)
getFavouriteDriverTrips limit offset onlyActive driverNo = do
        headers <- getHeaders "" false
        withAPIResult (EP.getFavouriteDriverTrips limit offset onlyActive (Just "COMPLETED") Nothing)  unwrapResponse $ callAPI headers (makeUpdateReq limit offset onlyActive (Just "COMPLETED") Nothing driverNo)
    where
        makeUpdateReq :: String -> String -> String -> Maybe String -> Maybe String -> String -> FavouriteDriverTripsReq
        makeUpdateReq limit offset onlyActive status clientId driverNo = 
            FavouriteDriverTripsReq limit offset onlyActive status clientId $ FavouriteDriverTripsBody $ { driverNumber : driverNo }

        unwrapResponse (x) = x

----------------------------------------------------------------------- RideBooking BT Function ---------------------------------------------------------------------------------------
rideBookingBT :: String -> FlowBT String RideBookingRes
rideBookingBT bookingId = do
        headers <- getHeaders' "" true
        withAPIResultBT (EP.ridebooking bookingId) identity errorHandler (lift $ lift $ callAPI headers  (RideBookingReq bookingId))
    where
        errorHandler errorPayload = do
            BackT $ pure GoBack

rideBookingList :: String -> String -> String -> Flow GlobalState (Either ErrorResponse RideBookingListRes)
rideBookingList limit offset onlyActive = do
    headers <- getHeaders "" true
    let config = stuckRideFilterConfig ""
    if (config.enable && onlyActive == show true) 
      then do 
        let lim = fromMaybe 1 $ fromString limit
        resp <- withAPIResult (EP.rideBookingList (if lim < 5 then "5" else "10") offset onlyActive Nothing Nothing)  unwrapResponse $ callAPI headers (RideBookingListReq limit offset onlyActive Nothing Nothing)
        case resp of
          Right resp -> do 
            let filteredResp = spy "getFilteredRides" $ getFilteredRides resp lim
            pure $ Right $ filteredResp
          Left err -> pure $ Left err
      else
        withAPIResult (EP.rideBookingList limit offset onlyActive Nothing Nothing)  unwrapResponse $ callAPI headers (RideBookingListReq limit offset onlyActive Nothing Nothing)
    where
        unwrapResponse (x) = x

getFilteredRides :: RideBookingListRes -> Int -> RideBookingListRes
getFilteredRides (RideBookingListRes resp) lim = 
  let config = stuckRideFilterConfig ""
  in 
    RideBookingListRes {
    list : take lim $ filter (\(RideBookingRes resp) ->
      let duration = case resp.returnTime of
                      Nothing -> fromMaybe config.estimatedDurationFallback resp.estimatedDuration
                      Just date -> runFn2 JB.differenceBetweenTwoUTC (fromMaybe resp.createdAt resp.rideScheduledTime) date
      in ((JB.getEpochTime (fromMaybe resp.createdAt resp.rideScheduledTime)) + ((toNumber duration) * 1000.0) + config.buffer) >= JB.getEpochTime (EHC.getCurrentUTC "")) resp.list
    } 

rideBookingListWithStatus :: String -> String -> String -> Maybe String -> Flow GlobalState (Either ErrorResponse RideBookingListRes)
rideBookingListWithStatus limit offset status maybeClientId = do
    headers <- getHeaders "" true
    withAPIResult (EP.rideBookingList limit offset "false" (Just status) maybeClientId) unwrapResponse $ callAPI headers (RideBookingListReq limit offset "false" (Just status) maybeClientId)
  where
    unwrapResponse (x) = x

makeRideBookingListWithStatus :: String -> String -> String -> Maybe String -> RideBookingListReq
makeRideBookingListWithStatus limit offset status maybeClientId = RideBookingListReq limit offset "false" (Just status) maybeClientId

getProfileBT :: String -> FlowBT String GetProfileRes
getProfileBT _  = do
        headers <- getHeaders' "" true
        withAPIResultBT (EP.profile "") identity errorHandler (lift $ lift $ callAPI headers (GetProfileReq))
    where
    errorHandler (errorPayload) =  do
        BackT $ pure GoBack

-- updateProfileBT :: UpdateProfileReq -> FlowBT String UpdateProfileRes
updateProfile :: UpdateProfileReq -> Flow GlobalState (Either ErrorResponse APISuccessResp)
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
        , deviceId : Nothing
        , androidId : Nothing
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
        , deviceId : Nothing
        , androidId : Nothing
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

makeGetRouteReq :: Number -> Number -> Number -> Number -> Maybe String -> GetRouteReq
makeGetRouteReq slat slng dlat dlng mbRideId = GetRouteReq {
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
    "calcPoints": true,
    "rideId": mbRideId
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
  defaultEmergencyNumbers : map (\item -> ContactDetails {
      mobileNumber: item.number,
      name: item.name,
      mobileCountryCode: "+91",
      priority: Just item.priority,
      enableForFollowing: Just item.enableForFollowing,
      enableForShareRide: Just item.enableForShareRide,
      shareTripWithEmergencyContactOption: Just item.shareTripWithEmergencyContactOption.key,
      contactPersonId : Nothing,
      onRide : Nothing,
      notifiedViaFCM : Nothing
  }) contacts
}

emergencyContactsBT :: EmergContactsReq -> FlowBT String APISuccessResp
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

getDriverLocation :: String -> Flow GlobalState (Either ErrorResponse GetDriverLocationResp)
getDriverLocation rideId = do
        headers <- getHeaders "" false
        withAPIResult (EP.getCurrentLocation rideId) unwrapResponse $ callAPI headers (GetDriverLocationReq rideId)
    where
        unwrapResponse (x) = x

getRoute :: String -> GetRouteReq -> Flow GlobalState (Either ErrorResponse GetRouteResp)
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

getSavedLocationList :: String -> Flow GlobalState (Either ErrorResponse SavedLocationsListRes)
getSavedLocationList _ = do
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

sendIssueBT :: SendIssueReq -> FlowBT String APISuccessResp
sendIssueBT req = do
    headers <- getHeaders' "" false
    withAPIResultBT ((EP.sendIssue "" )) identity errorHandler (lift $ lift $ callAPI headers req)
    where
    errorHandler (errorPayload) =  do
            BackT $ pure GoBack

----------------------------------------------------------------------------------------------
drawMapRoute :: Number -> Number -> Number -> Number -> JB.MarkerConfig -> JB.MarkerConfig -> String -> Maybe Route -> String -> MapRouteConfig -> FlowBT String (Maybe Route)
drawMapRoute srcLat srcLng destLat destLng sourceMarkerConfig destMarkerConfig routeType existingRoute routeAPIType specialLocation = do
    void $ pure $ removeAllPolylines ""
    case existingRoute of
        Just (Route route) -> do
            let (Snapped points) = route.points
            case points of
                [] -> do
                    (GetRouteResp routeResponse) <- getRouteBT routeAPIType (makeGetRouteReq srcLat srcLng destLat destLng Nothing)
                    callDrawRoute ((routeResponse) !! 0)
                _  -> callDrawRoute existingRoute
        Nothing -> do
            (GetRouteResp routeResponse) <- getRouteBT routeAPIType (makeGetRouteReq srcLat srcLng destLat destLng Nothing)
            _ <- pure $ printLog "drawRouteResponse" routeResponse
            let ios = (os == "IOS")
            let route = ((routeResponse) !! 0)
            callDrawRoute route
    where
        callDrawRoute :: Maybe Route -> FlowBT String (Maybe Route)
        callDrawRoute route = do 
            case route of
                Just (Route routes) -> do
                    let routeConfig = JB.mkRouteConfig (walkCoordinates routes.points) sourceMarkerConfig destMarkerConfig{anchorV = 1.0} Nothing routeType "LineString" true JB.DEFAULT specialLocation
                    lift $ lift $ liftFlow $ drawRoute [routeConfig] (getNewIDWithTag "CustomerHomeScreen")
                    pure route
                    
                Nothing -> pure route

type Markers = {
    srcMarker :: String,
    destMarker :: String
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

locServiceabilityBT :: ServiceabilityReq -> ServiceabilityType.ServiceabilityType -> FlowBT String ServiceabilityRes
locServiceabilityBT req serviceabilityType = do
    let serviceabilityType' = DS.toLower $ show serviceabilityType
    headers <- getHeaders' "" true
    withAPIResultBT (EP.locServiceability serviceabilityType') identity errorHandler (lift $ lift $ callAPI headers (ServiceabilityRequest serviceabilityType' req))
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

notifyFlowEvent :: NotifyFlowEventReq -> Flow GlobalState (Either ErrorResponse APISuccessResp)
notifyFlowEvent requestBody = do
    headers <- getHeaders "" false
    withAPIResult (EP.notifyFlowEvent "") unwrapResponse $ callAPI headers requestBody
    where
        unwrapResponse (x) = x

notifyFlowEventBT :: NotifyFlowEventReq -> FlowBT String APISuccessResp
notifyFlowEventBT requestBody = do
     headers <- lift $ lift $ getHeaders "" false
     withAPIResultBT (EP.notifyFlowEvent "") identity errorHandler (lift $ lift $ callAPI headers requestBody)
    where
    errorHandler errorPayload = BackT $ pure GoBack

makeNotifyFlowEventReq :: String -> NotifyFlowEventReq
makeNotifyFlowEventReq event = NotifyFlowEventReq { "event" : event }

------------------------------------------------------------------------ CancelEstimate Function ------------------------------------------------------------------------------------

cancelEstimate :: String -> Flow GlobalState (Either ErrorResponse APISuccessResp)
cancelEstimate estimateId = do
    headers <- getHeaders "" false
    withAPIResult (EP.cancelEstimate estimateId) unwrapResponse $ callAPI headers (CancelEstimateReq estimateId)
    where
        unwrapResponse (x) = x

cancelEstimateBT :: String -> FlowBT String APISuccessResp
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

userSosStatusBT :: String ->  SosStatus -> FlowBT String APISuccessResp
userSosStatusBT sosId requestBody = do
     headers <- lift $ lift $ getHeaders "" false
     withAPIResultBT (EP.userSosStatus sosId) identity errorHandler (lift $ lift $ callAPI headers (UserSosStatusReq sosId requestBody))
    where
    errorHandler errorPayload = BackT $ pure GoBack

callbackRequestBT :: LazyCheck -> FlowBT String APISuccessResp
callbackRequestBT lazyCheck = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.callbackRequest "") identity errorHandler (lift $ lift $ callAPI headers RequestCallbackReq)
    where
      errorHandler errorPayload = do
            BackT $ pure GoBack

makeUserSosReq :: UserSosFlow -> String -> Boolean -> Boolean -> Maybe LatLong -> Maybe Boolean -> UserSosReq
makeUserSosReq flow rideId isRideEnded notifyAllContacts currentLocation sendPNOnPostRideSOS = UserSosReq {
     "flow" : flow,
     "rideId" : rideId,
     "isRideEnded" : isRideEnded,
     "notifyAllContacts" : notifyAllContacts,
     "customerLocation" : currentLocation,
     "sendPNOnPostRideSOS" : sendPNOnPostRideSOS
}

createUserSosFlow :: String -> String -> UserSosFlow
createUserSosFlow tag content = UserSosFlow {
    "tag" : tag,
    "contents" : content
}

makeSosStatus :: String -> String -> SosStatus
makeSosStatus sosStatus comment= SosStatus {
     "status" : sosStatus,
     "comment" : comment
}


------------------------------------------------------------------------ Ride Feedback ------------------------------------------------------------------------------------


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

getTicketPlaceServicesBT :: String -> String-> FlowBT String TicketServicesResponse
getTicketPlaceServicesBT placeId date= do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.ticketPlaceServices placeId date) (\x -> x) errorHandler (lift $ lift $ callAPI headers (TicketServiceReq placeId date))
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
    createTicketServiceRequest services = catMaybes $ map createPeopleCategoriesRespRequest services
      where
        createPeopleCategoriesRespRequest :: TicketServiceData -> Maybe TicketService
        createPeopleCategoriesRespRequest service =
            let filteredSelCategories = filter (\category -> category.isSelected ) service.serviceCategories 
                updateFilteredSCOntheBasisOfPC = updateServiceCategories filteredSelCategories
                finalCategoires = filter (\sc -> not null sc.peopleCategories) updateFilteredSCOntheBasisOfPC
                mbbusinessHourId = case service.selectedBHId of
                                    Nothing -> getBHIdForSelectedTimeIntervals finalCategoires
                                    Just val -> Just val
            in
            case mbbusinessHourId of
                Nothing -> Nothing
                Just bhourId -> 
                    let generatedCatsData = map generateCatData finalCategoires in 
                    if null generatedCatsData then Nothing 
                    else Just $
                            TicketService 
                            {   serviceId : service.id,
                                businessHourId : bhourId,
                                categories : map generateCatData finalCategoires
                            }

        getBHIdForSelectedTimeIntervals categories = 
            case (categories !! 0) of 
                Nothing -> Nothing
                Just cat -> maybe Nothing (\selTimeInterval -> Just selTimeInterval.bhourId) (getMbTimeInterval cat)
                
        getMbTimeInterval cat = maybe Nothing (\opDay -> opDay.timeIntervals !! 0) cat.validOpDay
                     
        updateServiceCategories serviceCategories = map (\cat -> cat {peopleCategories = filter (\pc -> pc.currentValue > 0) cat.peopleCategories}) serviceCategories

        generateCatData category = 
          TicketBookingCategory
          {  categoryId : category.categoryId,
             peopleCategories : map (\pc -> TicketBookingPeopleCategory {peopleCategoryId : pc.peopleCategoryId, numberOfUnits : pc.currentValue}) category.peopleCategories
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

getTicketStatus :: String -> Flow GlobalState (Either ErrorResponse GetTicketStatusResp)
getTicketStatus shortId = do
  headers <- getHeaders "" false
  withAPIResult (EP.ticketStatus shortId) identity $ callAPI headers (GetTicketStatusReq shortId)

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

getOptionsBT :: String -> String -> String -> String -> String -> FlowBT String GetOptionsRes
getOptionsBT language categoryId optionId rideId issueReportId = do
      headers <- getHeaders' "" false
      withAPIResultBT (EP.getOptions categoryId optionId rideId issueReportId language) (\x → x) errorHandler (lift $ lift $ callAPI headers (GetOptionsReq categoryId optionId rideId issueReportId language))
        where
          errorHandler _ = do
            BackT $ pure GoBack

postIssueBT :: String -> PostIssueReqBody -> FlowBT String (Either ErrorResponse PostIssueRes)
postIssueBT language payload = do
    headers <- getHeaders' "" false
    lift $ lift $ withAPIResult (EP.postIssue language) unwrapResponse $ callAPI headers (PostIssueReq language payload)
    where
        unwrapResponse x = x

getCorrespondingErrorMessage :: ErrorResponse -> String
getCorrespondingErrorMessage errorPayload = do
    let errorCode = decodeErrorCode errorPayload.response.errorMessage
    case errorCode of
        "ISSUE_REPORT_ALREADY_EXISTS" -> getString ISSUE_REPORT_ALREADY_EXISTS
        _ -> getString SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN

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

------------------------------------------------------------------------ SafetyFlow --------------------------------------------------------------------------------

getEmergencySettings :: String -> Flow GlobalState (Either ErrorResponse GetEmergencySettingsRes)
getEmergencySettings _  = do
    headers <- getHeaders "" true
    withAPIResult (EP.getEmergencySettings "") identity $ callAPI headers (GetEmergencySettingsReq)

updateEmergencySettings :: UpdateEmergencySettingsReq -> Flow GlobalState (Either ErrorResponse APISuccessResp)
updateEmergencySettings (UpdateEmergencySettingsReq payload) = do
        headers <- getHeaders "" false
        withAPIResult (EP.updateEmergencySettings "") unwrapResponse $ callAPI headers (UpdateEmergencySettingsReq payload)
    where
        unwrapResponse (x) = x

markRideAsSafe :: String -> Boolean -> Boolean -> Flow GlobalState (Either ErrorResponse APISuccessResp)
markRideAsSafe sosId isMock isRideEnded = do
        headers <- getHeaders "" false
        let reqBody = UpdateAsSafeReqBody {isMock : isMock, isRideEnded : isRideEnded}
        withAPIResult (EP.updateSafeRide sosId) unwrapResponse $ callAPI headers (UpdateAsSafeReq sosId reqBody)
    where
        unwrapResponse (x) = x

getSosDetails :: String -> Flow GlobalState (Either ErrorResponse GetSosDetailsRes)
getSosDetails rideId = do
        headers <- getHeaders "" true
        withAPIResult (EP.getSosDetails rideId) identity $ callAPI headers (GetSosDetailsReq rideId)

sendSafetySupport :: AskSupportReq -> Flow GlobalState (Either ErrorResponse APISuccessResp)
sendSafetySupport req = do
        headers <- getHeaders "" true
        withAPIResult (EP.safetySupport "") unwrapResponse $ callAPI headers req
    where
        unwrapResponse (x) = x

makeAskSupportRequest :: String -> Boolean -> String -> AskSupportReq
makeAskSupportRequest bId isSafe description = AskSupportReq{
    "bookingId" : bId,
    "isSafe" : isSafe,
    "description" : description
}

createMockSos :: Boolean -> Boolean -> Flow GlobalState (Either ErrorResponse APISuccessResp)
createMockSos onRide startDrill = do
        headers <- getHeaders "" false
        withAPIResult (EP.createMockSos "") unwrapResponse $ callAPI headers $ CreateMockSosReq {onRide : onRide, startDrill : startDrill}
    where
        unwrapResponse (x) = x

shareRide :: ShareRideReq -> Flow GlobalState (Either ErrorResponse APISuccessResp)
shareRide (ShareRideReq req) = do
        headers <- getHeaders "" false
        withAPIResult (EP.shareRide "") unwrapResponse $ callAPI headers (ShareRideReq req)
    where
        unwrapResponse (x) = x

getFollowRide :: String -> Flow GlobalState (Either ErrorResponse FollowRideRes)
getFollowRide _ = do
  headers <- getHeaders "" false
  withAPIResult (EP.followRide "") identity $ callAPI headers FollowRideReq

-------------------------------------------------------- Metro Booking --------------------------------------------------------

-- getMetroBookingStatus :: String -> FlowBT String GetMetroBookingStatusResp
getMetroBookingStatus :: String -> Flow GlobalState (Either ErrorResponse GetMetroBookingStatusResp)
getMetroBookingStatus shortOrderID = do 
  headers <- getHeaders "" false
  withAPIResult (EP.getMetroBookingStatus shortOrderID) unwrapResponse $ callAPI headers (GetMetroBookingStatusReq shortOrderID)
  where
    unwrapResponse x = x

getMetroBookingStatusListBT :: String -> Maybe String -> Maybe String -> FlowBT String GetMetroBookingListResp
getMetroBookingStatusListBT vehicleType limit offset = do
      headers <- getHeaders' "" false
      withAPIResultBT (EP.getMetroBookingList vehicleType limit offset) (\x → x) errorHandler (lift $ lift $ callAPI headers (GetMetroBookingListReq vehicleType limit offset))
      where
        errorHandler _ = do
            BackT $ pure GoBack

getMetroBookingStatusList :: String -> Maybe String -> Maybe String -> Flow GlobalState (Either ErrorResponse GetMetroBookingListResp)
getMetroBookingStatusList vehicleType limit offset = do 
  headers <- getHeaders "" false
  withAPIResult (EP.getMetroBookingList vehicleType limit offset) unwrapResponse $ callAPI headers (GetMetroBookingListReq vehicleType limit offset)
  where
    unwrapResponse x = x


retryMetroTicketPaymentBT :: String -> FlowBT String RetryMetrTicketPaymentResp
retryMetroTicketPaymentBT quoteId = do
      headers <- getHeaders' "" false
      withAPIResultBT (EP.retryMetrTicketPayment quoteId) (\x → x) errorHandler (lift $ lift $ callAPI headers (RetryMetrTicketPaymentReq quoteId))
      where
        errorHandler _ = do
            BackT $ pure GoBack

retryMetroTicketPayment :: String -> Flow GlobalState (Either ErrorResponse RetryMetrTicketPaymentResp)
retryMetroTicketPayment quoteId = do
  headers <- getHeaders "" false
  withAPIResult (EP.retryMetrTicketPayment quoteId) unwrapResponse $ callAPI headers (RetryMetrTicketPaymentReq quoteId)
  where
    unwrapResponse x = x

getMetroStationBT :: String -> String -> String -> String-> String -> FlowBT String GetMetroStationResponse
getMetroStationBT vehicleType city routeCode endStationCode location = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.getMetroStations vehicleType city routeCode endStationCode location) (\x -> x) errorHandler (lift $ lift $ callAPI headers $ GetMetroStationReq vehicleType city routeCode endStationCode location)
    where
    errorHandler errorPayload = do
      BackT $ pure GoBack 

getBusRoutesBT :: String -> String -> String -> FlowBT String GetBusRoutesResponse
getBusRoutesBT city startStationCode endStationCode= do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.getBusRoutes city startStationCode endStationCode ) (\x -> x) errorHandler (lift $ lift $ callAPI headers $ GetBusRoutesReq city startStationCode endStationCode)
    where
    errorHandler errorPayload = do
      BackT $ pure GoBack

frfsSearchBT :: String -> FRFSSearchAPIReq -> FlowBT String FrfsSearchResp
frfsSearchBT vehicleType requestBody = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.frfsSearch vehicleType) (\x -> x) errorHandler (lift $ lift $ callAPI headers (FrfsSearchRequest requestBody vehicleType))
    where
    errorHandler errorPayload = do
      BackT $ pure GoBack

frfsSearch :: String -> FRFSSearchAPIReq -> Flow GlobalState (Either ErrorResponse FrfsSearchResp)
frfsSearch vehicleType requestBody = do
    headers <- getHeaders "" false
    withAPIResult (EP.frfsSearch vehicleType) identity $ callAPI headers (FrfsSearchRequest requestBody vehicleType)

busAutoCompleteBT :: String -> String -> String -> Maybe String -> String -> Maybe String -> FlowBT String AutoCompleteResp
busAutoCompleteBT vehicleType city location input limit offset = do 
    headers <- getHeaders' "" false
    withAPIResultBT (EP.busAutoComplete vehicleType city location input limit offset) (\x -> x) errorHandler (lift $ lift $ callAPI headers $ BusAutoCompleteReq vehicleType city location input limit offset)
    where
    errorHandler errorPayload = do
      BackT $ pure GoBack 


makeSearchMetroReq :: String -> String -> Int -> Maybe String-> FRFSSearchAPIReq
makeSearchMetroReq srcCode destCode count routeCode = FRFSSearchAPIReq {
    "fromStationCode" : srcCode,
    "toStationCode" : destCode,
    "quantity" : count,
    "routeCode" : routeCode
    }

frfsQuotesBT :: String -> FlowBT String FrfsQuotesRes
frfsQuotesBT searchId = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.frfsQuotes searchId) (\x → x) errorHandler (lift $ lift $ callAPI headers (FrfsQuotesReq searchId))
        where
          errorHandler _ = do
                BackT $ pure GoBack

frfsQuotes :: String -> Flow GlobalState (Either ErrorResponse FrfsQuotesRes)
frfsQuotes searchId = do
  headers <- getHeaders "" false
  withAPIResult (EP.frfsQuotes searchId) unwrapResponse $ callAPI headers (FrfsQuotesReq searchId)
  where
  unwrapResponse x = x
 
confirmMetroQuoteBT :: String -> FlowBT String FRFSTicketBookingStatusAPIRes
confirmMetroQuoteBT quoteId = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.confirmMetroQuote quoteId) (\x → x) errorHandler (lift $ lift $ callAPI headers (ConfirmMetroQuoteReq quoteId))
        where
          errorHandler _ = do
                BackT $ pure GoBack

confirmMetroQuote :: String -> Flow GlobalState (Either ErrorResponse FRFSTicketBookingStatusAPIRes)
confirmMetroQuote quoteId = do
  headers <- getHeaders "" false
  withAPIResult (EP.confirmMetroQuote quoteId) unwrapResponse $ callAPI headers (ConfirmMetroQuoteReq quoteId)
  where
  unwrapResponse x = x

getMetroStatusBT :: String -> FlowBT String GetMetroBookingStatusResp
getMetroStatusBT bookingId = do
        headers <- getHeaders' "" false
        withAPIResultBT (EP.getMetroBookingStatus bookingId) (\x → x) errorHandler (lift $ lift $ callAPI headers (GetMetroBookingStatusReq bookingId))
        where
          errorHandler _ = do
                BackT $ pure GoBack

getMetroStatus :: String -> Flow GlobalState (Either ErrorResponse GetMetroBookingStatusResp)
getMetroStatus bookingId = do
  headers <- getHeaders "" false
  withAPIResult (EP.getMetroBookingStatus bookingId) unwrapResponse $ callAPI headers (GetMetroBookingStatusReq bookingId)
  where
  unwrapResponse x = x

metroBookingSoftCancelBT :: String -> FlowBT String APISuccessResp
metroBookingSoftCancelBT bookingId = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.metroBookingSoftCancel bookingId) (\x -> x) errorHandler (lift $ lift $ callAPI headers (MetroBookingSoftCancelReq bookingId))
    where
    errorHandler errorPayload = do
      BackT $ pure GoBack

metroBookingSoftCancelStatusBT :: String -> FlowBT String MetroBookingSoftCancelStatusResp
metroBookingSoftCancelStatusBT bookingId = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.getMetroBookingSoftCancelStatus bookingId) (\x -> x) errorHandler (lift $ lift $ callAPI headers (MetroBookingSoftCancelStatusReq bookingId))
    where
    errorHandler errorPayload = do
      BackT $ pure GoBack

metroBookingSoftCancelStatus :: String -> Flow GlobalState (Either ErrorResponse MetroBookingSoftCancelStatusResp)
metroBookingSoftCancelStatus bookingId = do
    headers <- getHeaders "" false
    withAPIResult (EP.getMetroBookingSoftCancelStatus bookingId) unwrapResponse $ callAPI headers (MetroBookingSoftCancelStatusReq bookingId)
    where
    unwrapResponse x = x

metroBookingHardCancelBT :: String -> FlowBT String APISuccessResp
metroBookingHardCancelBT bookingId = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.metroBookingHardCancel bookingId) (\x -> x) errorHandler (lift $ lift $ callAPI headers (MetroBookingHardCancelReq bookingId))
    where
    errorHandler errorPayload = do
      BackT $ pure GoBack

metroBookingHardCancelStatusBT :: String -> FlowBT String MetroBookingHardCancelStatusResp
metroBookingHardCancelStatusBT bookingId = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.getMetroBookingHardCancelStatus bookingId) (\x -> x) errorHandler (lift $ lift $ callAPI headers (MetroBookingHardCancelStatusReq bookingId))
    where
    errorHandler errorPayload = do
      BackT $ pure GoBack

metroBookingHardCancelStatus :: String -> Flow GlobalState (Either ErrorResponse MetroBookingHardCancelStatusResp)
metroBookingHardCancelStatus bookingId = do
    headers <- getHeaders "" false
    withAPIResult (EP.getMetroBookingHardCancelStatus bookingId) unwrapResponse $ callAPI headers (MetroBookingHardCancelStatusReq bookingId)
    where
    unwrapResponse x = x

getFRFSBookingConfigBT :: String -> FlowBT String FRFSConfigAPIRes
getFRFSBookingConfigBT city = do
    headers <- getHeaders' "" true
    withAPIResultBT (EP.getFRFSBookingConfig city) identity errorHandler (lift $ lift $ callAPI headers (MetroBookingConfigReq city))
    where
    errorHandler errorPayload = do
      BackT $ pure GoBack

------------------------------------------------------------------------- Push SDK Events -----------------------------------------------------------------------------
pushSDKEvents :: Flow GlobalState (Either ErrorResponse APISuccessResp)
pushSDKEvents = do
    headers <- getHeaders "" false
    events <- liftFlow $ Events.getEvents
    withAPIResult (EP.pushSDKEvents "") unwrapResponse $ callAPI headers (SDKEventsReq { event : events, events : [] })
    where
        unwrapResponse x = x

  
addStop :: String -> AddStopReq -> FlowBT String APISuccessResp
addStop bookingId req = (callApiBT (AddStopRequest bookingId req))

makeAddStopReq :: Number -> Number -> Address -> AddStopReq
makeAddStopReq lat lon stop  = AddStopReq{
    "address" :  (LocationAddress stop),
    "gps" : LatLong {
        "lat" : lat ,
        "lon" : lon
        }
    }

makeEditStopReq :: Number -> Number -> Address -> EditStopReq
makeEditStopReq lat lon stop  = EditStopReq{
    "address" :  (LocationAddress stop),
    "gps" : LatLong {
        "lat" : lat ,
        "lon" : lon
        }
    }

makeStopReq :: Number -> Number -> Address -> StopReq
makeStopReq lat lon stop  = StopReq{
    "address" :  (LocationAddress stop),
    "gps" : LatLong {
        "lat" : lat ,
        "lon" : lon
        }
    }

mkRentalSearchReq :: Number -> Number -> Number -> Number -> Address -> Address -> String -> Int -> Int -> SearchReq
mkRentalSearchReq slat slong dlat dlong srcAdd desAdd startTime estimatedRentalDistance estimatedRentalDuration =
    let appConfig = CP.getAppConfig CP.appConfig
    in  SearchReq { "contents" : RentalSearchRequest (
                                        RentalSearchReq {
                                                "stops" : if dlat == 0.0 then Nothing else 
                                                    (Just [SearchReqLocation {
                                                           "gps" : LatLong {
                                                               "lat" : dlat ,
                                                               "lon" : dlong
                                                               },
                                                           "address" : (LocationAddress desAdd)
                                                  }]), 
                                                  "origin" : SearchReqLocation {
                                                   "gps" : LatLong {
                                                               "lat" : slat ,
                                                               "lon" : slong
                                                   },"address" : (LocationAddress srcAdd)
                                                  },
                                                  "isReallocationEnabled" : Just appConfig.feature.enableRentalReallocation,
                                                  "startTime" : startTime,
                                                  "estimatedRentalDistance" : estimatedRentalDistance,
                                                  "estimatedRentalDuration" : estimatedRentalDuration,
                                                  "quotesUnifiedFlow" : Just true,
                                                  "rideRequestAndRideOtpUnifiedFlow": Just true
                                                 }),
                    "fareProductType" : "RENTAL"
                   }

------------------------------------------------------------------------ Edit Location API -----------------------------------------------------------------------------
makeEditLocationRequest :: String -> Maybe SearchReqLocation -> Maybe SearchReqLocation -> EditLocationRequest
makeEditLocationRequest rideId srcAddress destAddress =
    EditLocationRequest rideId $ makeEditLocationReq srcAddress destAddress

makeEditLocationReq :: Maybe SearchReqLocation -> Maybe SearchReqLocation -> EditLocationReq
makeEditLocationReq srcAddress destAddress = 
    EditLocationReq {
        "origin" : srcAddress,
        "destination" : destAddress
    }

makeEditLocationResultRequest :: String -> GetEditLocResultReq
makeEditLocationResultRequest bookingUpdateRequestId = GetEditLocResultReq bookingUpdateRequestId


makeEditLocResultConfirmReq :: String -> EditLocResultConfirmReq
makeEditLocResultConfirmReq bookingUpdateRequestId = EditLocResultConfirmReq bookingUpdateRequestId


------------------------------------------------------------------------------- Intercity ---------------------------------------------------------------------------------
makeRoundTripReq :: Number -> Number -> Number -> Number -> Address -> Address -> String -> Maybe String ->  Boolean -> SearchReq
makeRoundTripReq slat slong dlat dlong srcAdd desAdd startTime returnTime roundTrip =
    let appConfig = CP.getAppConfig CP.appConfig
    in  SearchReq { "contents" : RoundTripSearchRequest (
                                RoundTripSearchReq {
                                        "stops" : if dlat == 0.0 then Nothing else 
                                            (Just [SearchReqLocation {
                                                    "gps" : LatLong {
                                                        "lat" : dlat ,
                                                        "lon" : dlong
                                                        },
                                                    "address" : (LocationAddress desAdd)
                                            }]), 
                                            "origin" : SearchReqLocation {
                                            "gps" : LatLong {
                                                        "lat" : slat ,
                                                        "lon" : slong
                                            },"address" : (LocationAddress srcAdd)
                                            },
                                            "startTime" : startTime,
                                            "returnTime" : returnTime,
                                            "roundTrip" : roundTrip,
                                            "isReallocationEnabled" : Just appConfig.feature.enableReAllocation
                                            }),
                    "fareProductType" : "INTER_CITY"
                   }----------------------------------------------------------------------------- MultiChat ----------------------------------------------------------------------------------------


----------------------------------------- DELiVERY FLOW ----------------------------------------------

getDeliveryImage :: String -> Flow GlobalState (Either ErrorResponse GetDeliveryImageResponse)
getDeliveryImage rideId = do
    headers <- getHeaders "" false
    withAPIResult (EP.getDeliveryImage rideId) unwrapResponse $ callAPI headers (GetDeliveryImageReq rideId)
    where
        unwrapResponse (x) = x

getDeliveryImageBT :: String -> FlowBT String GetDeliveryImageResponse
getDeliveryImageBT rideId = do
    headers <- getHeaders' "" false
    withAPIResultBT (EP.getDeliveryImage rideId) identity errorHandler (lift $ lift $ callAPI headers (GetDeliveryImageReq rideId))
    where
    errorHandler errorPayload = do
        BackT $ pure GoBack

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

---------------------------------------- confirmMetroQuoteV2 ---------------------------------------------
confirmMetroQuoteV2 :: String -> FRFSQuoteConfirmReq -> Flow GlobalState (Either ErrorResponse FRFSTicketBookingStatusAPIRes)
confirmMetroQuoteV2 quoteId confirmQuoteReqV2Body = do
  headers <- getHeaders "" false
  withAPIResult (EP.confirmMetroQuoteV2 quoteId) unwrapResponse $ callAPI headers (ConfirmFRFSQuoteReqV2 quoteId confirmQuoteReqV2Body)
  where
    unwrapResponse x = x
