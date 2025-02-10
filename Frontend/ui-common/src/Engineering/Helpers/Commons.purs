{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Engineering.Helpers.Commons where

import Prelude

import Common.Types.Sdk (SDKRequest(..), SDKResponse(..))
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (lift)
import Control.Monad.State as S
import Common.Types.App (Version(..),GlobalPayload(..), DateObj, CalendarDate, CalendarWeek, YoutubeData, CarouselHolderData, CalendarMonth)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn3, runFn3)
import Data.Int as INT
import Data.Maybe (fromMaybe, Maybe(..))
import Data.String (Pattern(..),split)
import Data.Int (fromString, toNumber)
import Data.Number.Format (toStringWith, fixed) as Number
import Data.String as DS
import DecodeUtil
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler, attempt, launchAff)
import Effect.Aff.AVar (new)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Ref (Ref, read, write)
import Effect.Uncurried (EffectFn2, EffectFn8, EffectFn7, mkEffectFn2, mkEffectFn6, mkEffectFn7, runEffectFn2, runEffectFn6, runEffectFn7, runEffectFn8, EffectFn1, EffectFn6)
import Foreign.Class (class Decode, class Encode)
import Foreign.Object (empty, insert, lookup, Object, foldM, delete)
import JSURI (decodeURIComponent)
import Presto.Core.Types.API (standardEncodeJSON)
import Foreign.Generic (class Decode, ForeignError, decode, decodeJSON, encode)
import Foreign.Generic.Class (class DecodeWithOptions, class EncodeWithOptions)
import Presto.Core.Language.Runtime.API (APIRunner)
import Presto.Core.Language.Runtime.Interpreter (PermissionCheckRunner, PermissionRunner(..), PermissionTakeRunner, Runtime(..), run,UIRunner(..))
import Presto.Core.Types.API (Header(..), Headers(..), Request(..), RestAPIOptions(..)) as API
import Presto.Core.Types.Language.Flow (Flow, doAff, defaultState, getState, modifyState)
import Presto.Core.Types.Permission (PermissionStatus(..))
import Presto.Core.Utils.Encoding (defaultDecodeJSON, defaultEncodeJSON)
import Common.Types.App (FlowBT, ClevertapEventParams)
import Effect.Aff.AVar (new)
import Data.String as DS
import Data.Array ((!!))
import Data.Number.Format as Number
import Engineering.OS.Permission (checkIfPermissionsGranted, requestPermissions)
import Data.Function.Uncurried (Fn1(..), runFn2)
import Data.Either (Either(..), either, hush)
import Data.Foldable (foldl)
import Log (printLog)
import Data.Maybe (maybe)
import Data.Number (pow, round)

foreign import callAPI :: EffectFn7 String String String String Boolean Boolean String Unit
foreign import callAPIWithOptions :: EffectFn8 String String String String Boolean Boolean String String Unit
foreign import callbackMapper :: forall a. a -> String
foreign import atobImpl :: String -> String
foreign import getWindowVariable :: forall a. String -> (a -> (Maybe a)) -> (Maybe a) -> Effect a
foreign import setWindowVariableImpl :: forall a. String -> a -> Effect Unit
foreign import screenWidth :: Unit -> Int
foreign import getDeviceHeight :: Unit -> Int
foreign import getScreenPpi :: Unit -> Int
foreign import screenHeight :: Unit -> Int
foreign import getVersionByKey :: String -> String
foreign import callSahay ::  String  -> EffectFnAff String
foreign import safeMarginTopImpl :: Unit -> Int
foreign import safeMarginBottomImpl :: Unit -> Int
foreign import getNewIDWithTag :: String -> String
foreign import getOs :: Unit -> String
foreign import setText :: String -> String -> Unit
foreign import getExpiryTime :: String -> Boolean -> Int
foreign import getCurrentUTC :: String -> String
foreign import convertUTCtoISC :: String -> String -> String
foreign import convertUTCTimeToISTTimeinHHMMSS :: String -> String
foreign import getCurrentTimeStamp :: Unit -> Number
foreign import markPerformance :: String -> Effect Unit
foreign import getDateFromObj :: Fn1 DateObj String
foreign import getFormattedDate :: Fn1 String String
foreign import formatCurrencyWithCommas :: String -> String
foreign import camelCaseToSentenceCase :: String -> String
foreign import getVideoID :: String -> String
foreign import getImageUrl :: String -> String -> String
foreign import getPastDays :: Int -> Array CalendarDate
foreign import getPastYears :: Int -> Array CalendarDate
foreign import getPastWeeks :: Int -> Array CalendarWeek
foreign import getPastMonths :: Int -> Array CalendarMonth
foreign import getDayName :: String -> String
foreign import getFutureDate :: String -> Int -> String
foreign import setEventTimestamp :: String -> Effect Unit
foreign import getTimeStampObject :: Unit -> Effect (Array ClevertapEventParams)
foreign import updateIdMap :: EffectFn1 String CarouselHolderData
foreign import resetIdMap :: EffectFn1 String Unit
foreign import updatePushInIdMap :: Fn2 String Boolean Unit
foreign import getValueFromIdMap :: EffectFn1 String CarouselHolderData
foreign import getRandomID :: Int -> String
foreign import toStringJSON :: forall a. a -> String
foreign import getMarkerCallback :: forall action. Fn2 (action -> Effect Unit) (String -> action) String
foreign import splitString :: Fn3 String String Int String

foreign import isTrue :: forall a. a -> Boolean 
foreign import convertTo2DArray :: forall w. Array String -> Array(Array String)
foreign import parseSecondsOfDayToUTC :: Int -> String
foreign import getMidnightUTC :: Unit -> String
foreign import convertDateTimeConfigToUTCImpl :: EffectFn6 Int Int Int Int Int Int String
foreign import getUTCAfterNSecondsImpl :: Fn2 String Int String
foreign import getUTCAfterNHoursImpl :: Fn2 String Int String
foreign import compareUTCDateImpl :: Fn2 String String Int
foreign import jBridgeMethodExists :: String -> Boolean
foreign import getDateMinusNDays :: Fn2 String Int String

foreign import getUTCBeforeNSecondsImpl :: Fn2 String Int String

foreign import loadWebViewWithURL :: EffectFn2 String String Unit

foreign import addBenchMark :: Fn2 String Boolean Unit

os :: String
os = getOs unit

sendSdkRequest :: forall a b st. EncodeWithOptions a => DecodeWithOptions (Maybe b) => SDKRequest a  -> Flow st (SDKResponse b)
sendSdkRequest request@(SDKRequest req) = do
  result <- doAff do (fromEffectFnAff <<< callSahay $ request')
  case runExcept $ defaultDecodeJSON result of
    Right response -> pure response
    Left err -> do
     pure $ SDKResponse
          {
            success : false,
            event : req.event,
            payload: Nothing
          }
  where
      request' = defaultEncodeJSON request

type NativeHeader = { field :: String , value :: String}
type NativeHeaders = Array NativeHeader
type AffError = (Error -> Effect Unit)
type AffSuccess s = (s -> Effect Unit)

newtype NativeRequest = NativeRequest
  { method :: String
  , url :: String
  , payload :: String
  , headers :: NativeHeaders
  }


mkNativeRequest :: API.Request -> NativeRequest
mkNativeRequest (API.Request request@{headers: API.Headers hs}) = NativeRequest
                                          { method : show request.method
                                            , url: request.url
                                            , payload: request.payload
                                            , headers: mkNativeHeader <$> hs
                                            }

mkNativeHeader :: API.Header -> NativeHeader
mkNativeHeader (API.Header field val) = { field: field, value: val}

liftFlow :: forall val st. (Effect val)  -> Flow st val
liftFlow effVal = doAff do liftEffect (effVal)

fromNull :: forall a. a -> Maybe a -> a
fromNull a b = fromMaybe a b

window :: forall a st. String -> Flow st (Maybe a)
window key = liftFlow (getWindowVariable key Just Nothing)

setWindowVariable :: forall a st. String -> a -> Flow st Unit
setWindowVariable key value = liftFlow (setWindowVariableImpl key value)

flowRunner :: ∀ return st. st -> Flow st return -> Aff (Either Error return)
flowRunner state flow = do
  let
    freeFlow = S.evalStateT $ run standardRunTime flow
  attempt $ new (defaultState state) >>= freeFlow

permissionCheckRunner :: PermissionCheckRunner
permissionCheckRunner = checkIfPermissionsGranted

permissionTakeRunner :: PermissionTakeRunner
permissionTakeRunner = requestPermissions

permissionRunner :: PermissionRunner
permissionRunner = PermissionRunner permissionCheckRunner permissionTakeRunner

standardRunTime :: Runtime
standardRunTime =
  Runtime
    pure
    permissionRunner
    apiRunner

readFromRef :: forall st. Ref st → FlowBT String st st
readFromRef ref = lift $ lift $ doAff $ liftEffect $ read ref

getGlobalPayload :: String -> Maybe GlobalPayload
getGlobalPayload key = do
  let mBPayload = runFn3 getFromWindow key Nothing Just
  maybe (Nothing) (\payload -> decodeForeignAnyImpl payload) mBPayload

writeToRef :: forall st. st → Ref st → FlowBT String st Unit
writeToRef d ref = lift $ lift $ doAff $ liftEffect $ write d ref

modifyEpassRef :: forall st. (st → st) → Ref st → FlowBT String st st
modifyEpassRef f ref = do
  d ← readFromRef ref
  writeToRef (f d) ref
  pure d

safeMarginTop :: Int
safeMarginTop = safeMarginTopImpl unit

safeMarginTopWithDefault :: Int -> Int
safeMarginTopWithDefault def = 
  let safeMargin = safeMarginTop
  in if safeMargin == 0 then def else safeMargin

safeMarginBottomWithDefault :: Int -> Int
safeMarginBottomWithDefault def = 
  let safeMargin = safeMarginBottom
  in if safeMargin == 0 then def else safeMargin

safeMarginBottom :: Int
safeMarginBottom = safeMarginBottomImpl unit

strToBool :: String -> Maybe Boolean
strToBool value = case (DS.toLower value) of
                    "true"  -> Just true
                    "false" -> Just false
                    _       -> Nothing

truncate :: Int -> Number -> Number
truncate precision num = (round (num * (10.0 `pow` (toNumber  precision)))) / (10.0 `pow` (toNumber precision))

isPreviousVersion :: String -> String -> Boolean
isPreviousVersion currentVersion previousVersion = numericVersion currentVersion <= numericVersion previousVersion

numericVersion :: String -> Int
numericVersion versionName = do
  let versionArray = (DS.split (DS.Pattern ".") versionName)
      majorUpdateIndex = fromMaybe (0) $ INT.fromString $ fromMaybe "NA" $ versionArray !! 0
      minorUpdateIndex = fromMaybe (0) $ INT.fromString $ fromMaybe "NA" $ versionArray !! 1
      patchUpdateIndex = fromMaybe (0) $ INT.fromString $ fromMaybe "NA" $ versionArray !! 2
  (majorUpdateIndex * 100 + minorUpdateIndex * 10 + patchUpdateIndex)

parseFloat :: Number -> Int -> String
parseFloat num prec = Number.toStringWith (Number.fixed prec) num

stringToVersion :: String -> Version
stringToVersion reqVersion =
  let versionArray = split (Pattern ".") reqVersion
      madeVersion = Version {
    major : fromMaybe (-1) (fromString $ fromMaybe "NA" $ versionArray !! 0),
    minor : fromMaybe (-1) (fromString $ fromMaybe "NA" $ versionArray !! 1),
    maintenance : fromMaybe (-1) (fromString $ fromMaybe "NA" $ versionArray !! 2)
  }
  in
    madeVersion

apiRunner :: APIRunner
apiRunner (API.Request request@{headers: API.Headers hs}) =
  makeAff
    ( \cb -> do
        let _ = runFn2 addBenchMark ("apiRunner_" <> request.url) false
        void $ pure $ printLog "callAPI request" request   
        case request.options of
          Nothing -> do
            _ <- runEffectFn7 callAPI (show request.method) request.url request.payload (standardEncodeJSON headers) shouldFormEncode isSSLPinnedURL $ callback cb
            pure $ nonCanceler
          Just (API.RestAPIOptions ops) -> do
            _ <- runEffectFn8 callAPIWithOptions (show request.method) request.url request.payload (standardEncodeJSON headers) shouldFormEncode isSSLPinnedURL (standardEncodeJSON ops) $ callback cb
            pure $ nonCanceler
    )
  where
    callback cb = callbackMapper $
        mkEffectFn6 $
          \status response statusCode url responseHeaders urlEncodedResponse -> do
            let _ = runFn2 addBenchMark ("apiRunner_" <> atobImpl url) true
            let formattedResponse = { status : status,
              response : extractResponse response urlEncodedResponse,
              code : fromMaybe (-3) (fromString statusCode),
              responseHeaders : fromMaybe empty $ hush $ runExcept $ decodeJSON $ atobImpl responseHeaders
            }
            void $ pure $ printLog "callAPI response" formattedResponse
            void $ pure $ printLog "response" formattedResponse.response
            cb $ Right formattedResponse
    headers = foldl (\acc (API.Header key value) -> insert key value acc) empty hs
    isSSLPinnedURL = lookup "x-pinned" headers == Just "true"
    shouldFormEncode = lookup "Content-Type" headers == Just "application/x-www-form-urlencoded"

    extractResponse response urlEncodedResponse = do
      case hush $ runExcept $ decode urlEncodedResponse of
        Just resp -> fromMaybe (atobImpl response) $ decodeURIComponent resp
        _ -> atobImpl response

getMapsLanguageFormat :: String -> String
getMapsLanguageFormat key = 
  case key of 
    "HI_IN" -> "HINDI"
    "KN_IN" -> "KANNADA"
    "BN_IN" -> "BENGALI"
    "ML_IN" -> "MALAYALAM"
    "TE_IN" -> "TELUGU"
    _       -> "ENGLISH"


getYoutubeData :: YoutubeData
getYoutubeData  = {
  videoTitle : "title",
  setVideoTitle : false,
  showMenuButton : false,
  showDuration : true,
  showSeekBar : true,
  videoId : "videoId",
  videoType : "PORTRAIT_VIDEO",
  videoHeight : 0,
  showFullScreen : false,
  hideFullScreenButton : false
}

isInvalidUrl :: String -> Boolean
isInvalidUrl url = do
  let strippedUrl = DS.stripPrefix (DS.Pattern "https://") url
  maybe false (\val ->  DS.contains (DS.Pattern "(null)") val || DS.contains (DS.Pattern "__failed") val || DS.contains (DS.Pattern "//") val) strippedUrl

convertDateTimeConfigToUTC :: Int -> Int -> Int -> Int -> Int -> Int -> Effect String
convertDateTimeConfigToUTC = runEffectFn6 convertDateTimeConfigToUTCImpl

getUTCAfterNSeconds :: String -> Int -> String
getUTCAfterNSeconds = runFn2 getUTCAfterNSecondsImpl

getUTCAfterNHours :: String -> Int -> String
getUTCAfterNHours = runFn2 getUTCAfterNHoursImpl

compareUTCDate :: String -> String -> Int
compareUTCDate = runFn2 compareUTCDateImpl

getUTCBeforeNSeconds :: String -> Int -> String 
getUTCBeforeNSeconds = runFn2 getUTCBeforeNSecondsImpl