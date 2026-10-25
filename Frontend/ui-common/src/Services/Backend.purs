module Services.Common.Backend where

import Locale.Utils

import Common.Types.App (Version(..), SignatureAuthData(..), LazyCheck(..), FeedbackAnswer)
import ConfigProvider as CP
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..))
import Data.Array ((!!), catMaybes, concat, take, any, singleton, find, filter, length, null, mapMaybe)
import Data.Either (Either(..), either)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String as DS
import Data.Foldable (or)
import Engineering.Helpers.Events as Events
import Engineering.Helpers.Commons (liftFlow, os, convertUTCtoISC, isPreviousVersion, isInvalidUrl, getNewIDWithTag)
import Engineering.Helpers.Utils as EHU
import Engineering.Helpers.Commons as EHC
import Foreign.Object (empty)
import Helpers.Utils (decodeError, getTime)
import JBridge (factoryResetApp, setKeyInSharedPrefKeys, removeAllPolylines, stopChatListenerService, MapRouteConfig, Locations, factoryResetApp, setKeyInSharedPrefKeys, drawRoute, toggleBtnLoader)
import JBridge as JB
import Juspay.OTP.Reader as Readers
import Language.Strings (getString)
import Language.Types(STR(..))
import Log (printLog)
import Prelude (class Eq, class Show, not, Unit, bind, discard, map, pure, unit, void, identity, ($), ($>), (>), (&&), (*>), (<<<), (=<<), (==), (<=), (||), show, (<>), (/=), when, (<$>))
import Presto.Core.Types.API (Header(..), Headers(..), ErrorResponse)
import Presto.Core.Types.Language.Flow (Flow, APIResult, callAPI, doAff, loadS)
import Screens.Types (HomeScreenState(..))
import Services.Config as SC
import Storage (getValueToLocalStore, deleteValueFromLocalStore, getValueToLocalNativeStore, KeyStore(..), setValueToLocalStore)
import Tracker (trackApiCallFlow, trackExceptionFlow)
import Tracker.Labels (Label(..))
import Tracker.Types as Tracker
import Types.App (GlobalState(..), FlowBT, ScreenType(..))
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
import Helpers.API (callApiBT)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode, defaultEnumDecode, defaultEnumEncode)
import Services.Config (getBaseUrl)
import Data.Generic.Rep (class Generic)
import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, ErrorPayload, Method(..), defaultDecodeResponse, defaultMakeRequestWithoutLogs, standardEncode, defaultMakeRequestString)
import Foreign.Generic (decode, encode, class Decode, class Encode)
import Data.Show.Generic (genericShow)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import Data.Newtype (class Newtype)
import Data.Eq.Generic (genericEq)
import Foreign.Generic (decodeJSON)
import Services.Common.API

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

data App = User | Driver

derive instance genericApp :: Generic App _
instance decodeApp :: Decode App where decode = defaultEnumDecode
instance encodeApp :: Encode App where encode = defaultEnumEncode
instance eqApp :: Eq App where eq = genericEq

getHeaders :: String -> App -> Boolean -> Flow GlobalState Headers
getHeaders val app isGzipCompressionEnabled = do
    regToken <- loadS $ show REGISTERATION_TOKEN
    pure $ Headers $ [   Header "Content-Type" "application/json",
                        Header "x-client-version" (getValueToLocalStore VERSION_NAME),
                        Header "x-config-version" (getValueFromWindow "CONFIG_VERSION"),
                        Header "x-bundle-version" (getValueToLocalStore BUNDLE_VERSION),
                        Header "session_id" (getValueToLocalStore SESSION_ID),
                        Header "x-device" (getValueToLocalNativeStore DEVICE_DETAILS)
                    ] <> case regToken of
                        Nothing -> []
                        Just token -> [Header "token" token]
                      <> if isGzipCompressionEnabled then [Header "Accept-Encoding" "gzip"] else []
                      <> if val /= "" then [Header "x-f-token" val] else []
                      <> if app == User then [Header "client-id" (getValueToLocalStore CUSTOMER_CLIENT_ID)] else []

withAPIResult url f flow = do
  if (isInvalidUrl url) then pure $ Left customError
  else do
    let start = getTime unit
    resp <- Events.measureDurationFlow ("CallAPI." <> DS.replace (DS.Pattern $ SC.getBaseUrl "") (DS.Replacement "") url) $ either (pure <<< Left) (pure <<< Right <<< f <<< _.response) =<< flow    
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

---------------------------------------------------Get Driver Profile Api---------------------------------------------------------------
data DriverProfileReq = DriverProfileReq String Boolean

data DriverProfileReqById = DriverProfileReqById String Boolean

newtype DriverProfile = DriverProfile
  { certificates :: Array String,
    homeTown :: Maybe String,
    driverName :: String,
    aboutMe :: Maybe String,
    drivingSince :: Maybe Int,
    onboardedAt :: String,
    pledges :: Array String,
    driverStats :: DriverStatSummary,
    languages :: Array String,
    aspirations :: Array String,
    vehicleNum :: Maybe String,
    vechicleVariant :: Variant, ---------
    vehicleTags :: Array String,
    profileImage :: Maybe String,
    images :: Array String,
    topReviews :: Array DriverReview
  }

newtype DriverProfileRes = DriverProfileRes
  {
    response :: DriverProfile
  }

derive instance genericDriverProfileReq :: Generic DriverProfileReq _
instance standardEncodeDriverProfileReq :: StandardEncode DriverProfileReq where standardEncode (DriverProfileReq body body2) = standardEncode body
instance showDriverProfileReq :: Show DriverProfileReq where show = genericShow
instance decodeDriverProfileReq :: Decode DriverProfileReq where decode = defaultDecode
instance encodeDriverProfileReq  :: Encode DriverProfileReq where encode = defaultEncode

derive instance genericDriverProfileRes :: Generic DriverProfileRes _
derive instance newtypeDriverProfileRes :: Newtype DriverProfileRes _
instance standardEncodeDriverProfileRes :: StandardEncode DriverProfileRes where standardEncode (DriverProfileRes body) = standardEncode body
instance showDriverProfileRes :: Show DriverProfileRes where show = genericShow
instance decodeDriverProfileRes :: Decode DriverProfileRes where decode = defaultDecode
instance encodeDriverProfileRes  :: Encode DriverProfileRes where encode = defaultEncode

derive instance genericDriverProfile :: Generic DriverProfile _
derive instance newtypeDriverProfile :: Newtype DriverProfile _
instance standardEncodeDriverProfile :: StandardEncode DriverProfile where standardEncode (DriverProfile body) = standardEncode body
instance showDriverProfile :: Show DriverProfile where show = genericShow
instance decodeDriverProfile :: Decode DriverProfile where decode = defaultDecode
instance encodeDriverProfile  :: Encode DriverProfile where encode = defaultEncode


instance makeDriverProfileReq :: RestEndpoint DriverProfileReq where
  makeRequest reqBody@(DriverProfileReq id withImages) headers = defaultMakeRequestWithoutLogs GET (profile id withImages) headers reqBody Nothing
  encodeRequest req = standardEncode req

derive instance genericDriverProfileReqById :: Generic DriverProfileReqById _
instance standardEncodeDriverProfileReqById :: StandardEncode DriverProfileReqById where standardEncode (DriverProfileReqById body body2) = standardEncode body
instance showDriverProfileReqById :: Show DriverProfileReqById where show = genericShow
instance decodeDriverProfileReqById :: Decode DriverProfileReqById where decode = defaultDecode
instance encodeDriverProfileReqById :: Encode DriverProfileReqById where encode = defaultEncode

instance makeDriverProfileReqById :: RestEndpoint DriverProfileReqById where
  makeRequest reqBody@(DriverProfileReqById id withImages) headers = defaultMakeRequestWithoutLogs GET (profileById id withImages) headers reqBody Nothing
  encodeRequest req = standardEncode req

profile :: String -> Boolean-> String
profile rideId withImages = (getBaseUrl "1") <>"/knowYourDriver" <> "/" <> rideId <> "?isImages=" <> show withImages

getDriverProfile rideId withImages= do
        headers <- getHeaders "" User true
        withAPIResult (profile rideId withImages)  unwrapResponse $ callAPI headers (DriverProfileReq rideId withImages)
    where
        unwrapResponse (x) = x

profileById :: String -> Boolean -> String
profileById driverId withImages = (getBaseUrl "2") <>"/knowYourFavDriver" <> "/" <> driverId <> "?isImages=" <> show withImages

getDriverProfileById driverId withImages= do
        headers <- getHeaders "" Driver true
        withAPIResult (profileById driverId withImages)  unwrapResponse $ callAPI headers (DriverProfileReqById driverId withImages)
    where
        unwrapResponse (x) = x