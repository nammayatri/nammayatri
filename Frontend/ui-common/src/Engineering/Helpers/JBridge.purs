{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module JBridge where


import Prelude

import Effect (Effect)
import Effect.Aff (Fiber)
import Presto.Core.Flow (Flow)
import Engineering.Helpers.Commons (liftFlow)
import Data.Maybe (Maybe(..))
import Common.Types.App (EventPayload(..),ChatComponent(..), DateObj, LayoutBound, ClevertapEventParams)
-- import Types.APIv2 (Address)
import Foreign (Foreign)
import Control.Monad.Except (runExcept)
import Effect.Uncurried (EffectFn3)
-- import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (decodeJSON)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import Data.Either (Either(..))
import Engineering.Helpers.Commons (screenHeight, screenWidth, parseFloat)
import Effect.Uncurried (EffectFn3, EffectFn2, EffectFn1, runEffectFn1, EffectFn5)
import Data.Maybe (Maybe(..))
-- import LoaderOverlay.Handler as UI
-- import Effect.Aff (launchAff)
-- import Effect.Class (liftEffect)
-- import PrestoDOM.Core(terminateUI)
import Presto.Core.Types.Language.Flow
import Engineering.Helpers.Commons (screenHeight, screenWidth)
import Data.Int (toNumber)
import Data.Function.Uncurried (Fn2(..))
import Presto.Core.Flow (doAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign.Generic (encodeJSON)
import Data.Either (Either(..), hush)
import Effect.Uncurried (EffectFn3, EffectFn2)
import Data.Function.Uncurried (Fn3, runFn3, Fn1)
import Foreign.Class (encode)
-- -- import Control.Monad.Except.Trans (lift)
-- -- foreign import _keyStoreEntryPresent :: String -> Effect Boolean
-- -- foreign import _createKeyStoreEntry :: String -> String -> (Effect Unit) -> (String -> Effect Unit) -> Effect Unit
-- -- foreign import _deleteKeyStoreEntry :: String -> Effect Unit
-- -- foreign import _keyStoreEncrypt :: String -> String -> (String -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit
-- -- foreign import _keyStoreDecrypt :: String -> String -> (String -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit
-- -- foreign import _keyStoreSign :: String -> String -> (String -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit
-- -- foreign import _getKeyStorePublicKey :: String -> (String -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit
-- -- foreign import _sendSafetyNetRequest :: String -> String -> (String -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit
-- -- foreign import _safefyNetShutdownSafeBrowsing :: (String -> Effect Unit) -> Effect Unit
-- -- foreign import _safefyNetInitSafeBrowsing :: (String -> Effect Unit) -> Effect Unit
-- -- foreign import _jweEncrypt :: String -> String -> String -> (String -> Either String String) -> (String -> Either String String) -> Effect (Either String String)
-- -- foreign import _jweDecrypt :: String -> String -> (String -> Either String String) -> (String -> Either String String) -> Effect (Either String String)
-- -- foreign import _jwsSign :: String -> String -> String -> (String -> Either String String) -> (String -> Either String String) -> Effect (Either String String)
-- -- foreign import _jwsVerify :: String -> String -> Effect Boolean
-- -- foreign import _rsaEncryption :: String -> String -> String -> (String -> Either String String) -> (String -> Either String String) -> Effect (Either String String)
-- foreign import _addCertificates :: String -> Effect Unit
-- foreign import _isPlayStoreInstalled :: Effect Boolean
-- foreign import _isNetworkAvailable :: Effect Boolean
-- foreign import _renewFile :: String -> Effect Unit
-- foreign import readFileImpl      :: String -> Effect String
foreign import showLoaderImpl      :: String -> Effect Unit
-- foreign import readFile'      :: String -> Effect String
-- foreign import showLoader'      :: String -> Effect Unit
foreign import locateOnMap :: EffectFn5 Boolean Number Number String (Array Location) Unit

foreign import exitLocateOnMap :: String -> Unit
foreign import shareTextMessage :: String -> String -> Unit
foreign import shareImageMessage :: String -> String -> Unit
foreign import showInAppNotification :: String -> String -> String -> String -> String -> String -> String -> String -> Int -> Effect Unit
foreign import enableMyLocation :: Boolean -> Unit
foreign import isLocationPermissionEnabled :: Unit -> Effect Boolean
foreign import isMicrophonePermissionEnabled :: Unit -> Effect Boolean
-- foreign import getPackageName   :: Effect String
foreign import getVersionCode   :: Effect Int
foreign import getVersionName   :: Effect String
-- foreign import getManufacturerName :: Unit -> String
foreign import getAndroidVersion :: Effect Int
-- foreign import showQrCodeImpl      :: String -> String -> Effect Unit
-- foreign import scanQrCode       :: forall action. String MerchantConfig.Utils-> (action -> Effect Unit) ->  (String -> action) -> Effect Unit
-- foreign import timePicker       :: forall action. (action -> Effect Unit) -> (Int -> Int -> action) -> Effect Unit
foreign import datePicker       :: forall action. String -> (action -> Effect Unit)  -> (String -> Int -> Int -> Int -> action) -> Effect Unit
foreign import setFCMToken :: forall action. (action -> Effect Unit) -> (String  -> action) -> Effect Unit
foreign import setFCMTokenWithTimeOut :: forall action. Int -> (action -> Effect Unit) -> (String  -> action) -> Effect Unit
-- foreign import dateTimePicker :: forall action. (action -> Effect Unit) -> (Int -> action) -> Effect Unit
-- foreign import getNearbyPlaces :: forall action. (action -> Effect Unit) -> (Place -> action) -> Effect Unit
-- foreign import isNetworkAvailable :: Unit -> Boolean
foreign import openUrlInApp  :: String -> Effect Unit
foreign import openUrlInMailApp  :: String -> Effect Unit
foreign import addMarkerImpl :: String -> Number -> Number -> Int -> Number -> Number -> Effect Boolean
foreign import removeMarker :: String -> Unit
-- foreign import parseAddress      :: String -> Address
foreign import disableActionEditText :: String -> Unit
foreign import uploadFile :: Unit -> Effect Unit
foreign import previewImage :: String -> Effect Unit
foreign import storeCallBackImageUpload :: forall action. (action -> Effect Unit) -> (String -> String -> String -> action) -> Effect Unit
foreign import renderBase64Image :: String -> String -> Boolean -> String -> Effect Unit
foreign import setScaleType :: String -> String -> String -> Effect Unit
foreign import copyToClipboard :: String -> Unit
foreign import drawRoute :: Locations -> String -> String -> Boolean -> String -> String -> Int -> String -> String -> String -> MapRouteConfig -> Effect Unit
foreign import updateRouteMarker :: UpdateRouteMarker -> Effect Unit
foreign import isCoordOnPath :: Locations -> Number -> Number -> Int -> Effect IsLocationOnPath
foreign import updateRoute :: Locations -> String -> String -> MapRouteConfig -> Effect Unit
-- -- foreign import drawActualRoute :: String -> String -> Locations -> Effect Int
-- -- foreign import showAndDrawRoute :: String -> String -> String -> Locations -> Effect Int
-- foreign import addMarkers :: Markers -> Effect Unit
-- foreign import removePolyLine   :: String -> Effect Unit
foreign import isOverlayPermissionEnabled :: Unit -> Effect Boolean
foreign import requestLocation  :: Unit -> Effect Unit

foreign import initiateLocationServiceClient :: Effect Unit
foreign import waitingCountdownTimer :: forall action. Int -> (action -> Effect Unit) -> (String -> String -> Int -> action) -> Effect Unit
foreign import checkOverlayPermission  :: Unit -> Effect Unit
foreign import requestAutoStartPermission  :: Unit -> Effect Unit
foreign import requestBatteryPermission :: Unit -> Effect Unit
foreign import isBatteryPermissionEnabled :: Unit -> Effect Boolean
foreign import reallocateMapFragment :: String -> Effect Unit
foreign import showMapImpl :: forall action. String -> Boolean -> String -> Number -> (action -> Effect Unit) -> (String -> String -> String -> action) -> Effect Boolean
foreign import mapSnapShot :: forall action. String -> Locations -> String -> Boolean -> (action -> Effect Unit) -> (String -> action) -> Effect Boolean
foreign import getCurrentLatLong  :: Effect Paths
foreign import isLocationEnabled :: Unit -> Effect Boolean
foreign import getCurrentPosition  :: forall action. (action -> Effect Unit) -> (String -> String -> action) -> Effect Unit
foreign import getCurrentPositionWithTimeout  :: forall action. (action -> Effect Unit) -> (String -> String -> action) -> Int -> Effect Unit

foreign import isMockLocation :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit
foreign import animateCamera :: Number -> Number -> Int -> String -> Effect Unit
-- foreign import moveCamera :: Number -> Number -> Number -> Number -> Effect Unit
foreign import minimizeApp    :: String -> Unit
foreign import toast          :: String -> Unit
foreign import factoryResetApp :: String -> Unit
foreign import startTimerWithTime :: forall action. String -> String -> String -> (action -> Effect Unit) -> (Int -> String -> String -> String-> action)  -> Effect Unit
foreign import hideKeyboardOnNavigation :: Boolean -> Unit
foreign import askNotificationPermission :: Unit -> Effect Unit
-- foreign import onEvent        :: Foreign -> Effect Unit
-- foreign import _onEventWithCB :: Foreign -> (String -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit
-- -- foreign import getSessionInfo :: { android_id_raw :: String, android_id :: String, os_version :: String, package_name :: String, android_api_level :: String }
foreign import getKeyInSharedPrefKeys :: String -> String
foreign import getKeyInNativeSharedPrefKeys :: String -> String
foreign import setKeyInSharedPrefKeysImpl :: String -> String -> Effect Unit
foreign import setEnvInNativeSharedPrefKeysImpl :: String -> String -> Effect Unit
foreign import removeKeysInSharedPrefs :: String -> Unit
foreign import removeKeysInNativeSharedPrefs :: String -> Unit
foreign import toggleLoaderImpl :: Boolean -> Effect Unit
foreign import loaderTextImpl :: String -> String -> Effect Unit
foreign import generatePDF :: forall invoiceScreenState. invoiceScreenState -> String -> Unit
foreign import requestKeyboardShow :: String -> Effect Unit
foreign import showDialer :: String -> Boolean -> Unit
foreign import getAAID :: String -> String
-- -- foreign import removePolyLineById :: Int -> Effect Unit
foreign import removeAllPolylines :: String -> Unit
foreign import currentPosition  :: String -> Unit
foreign import openNavigation  :: Number -> Number -> Number -> Number -> Effect Unit
foreign import stopLocationPollingAPI :: Effect Unit
foreign import startLocationPollingAPI :: Effect Unit
foreign import startChatListenerService :: Effect Unit
foreign import scrollOnResume :: forall action. (action -> Effect Unit) -> (action) -> Effect Unit
foreign import stopChatListenerService :: Effect Unit
foreign import storeCallBackMessageUpdated :: forall action. (action -> Effect Unit) -> String -> String  -> (String -> String -> String -> String -> action) -> Effect Unit
foreign import dateCallback :: forall action. Fn2 (action -> Effect Unit) action Unit
foreign import unregisterDateAndTime :: Effect Unit
foreign import storeCallBackOpenChatScreen :: forall action. (action -> Effect Unit) -> (action) -> Effect Unit
foreign import sendMessage :: String -> Unit
foreign import getSuggestionsfromLocal :: String -> Array String
foreign import getSuggestionfromKey :: String -> String -> String
foreign import scrollToEnd :: String -> Boolean -> Effect Unit
foreign import metaLogEvent :: String -> Unit
foreign import firebaseLogEvent :: String -> Effect Unit
foreign import firebaseLogEventWithParams :: String -> String -> String -> Effect Unit
foreign import firebaseLogEventWithTwoParams :: String -> String -> String -> String -> String -> Effect Unit
foreign import firebaseScreenNameLog :: String  -> Effect Unit
foreign import firebaseUserID :: String  -> Effect Unit
-- foreign import closeApp       :: String -> Effect Unit
foreign import storeCallBackDriverLocationPermission :: forall action. (action -> Effect Unit) -> (Boolean -> action) -> Effect Unit
foreign import setStoreCallBackPopUp :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit
foreign import deletePopUpCallBack :: String -> Unit
-- foreign import requestLocationPermissionDriver :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit
foreign import storeCallBackOverlayPermission :: forall action. (action -> Effect Unit) -> (Boolean -> action) -> Effect Unit
foreign import storeCallBackBatteryUsagePermission :: forall action. (action -> Effect Unit) -> (Boolean -> action) -> Effect Unit
foreign import isInternetAvailable :: Unit -> Effect Boolean
foreign import storeCallBackInternetAction :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit

foreign import openWhatsAppSupport :: String -> Effect Unit
foreign import generateSessionToken :: String -> String
foreign import addMediaFile :: String -> String -> String -> String -> String -> String -> Effect Unit

foreign import toggleBtnLoader :: String -> Boolean -> Unit
foreign import getBtnLoader :: String -> Boolean
foreign import launchInAppRatingPopup :: Unit -> Effect Unit
foreign import getExtendedPath :: Locations -> Locations
foreign import generateSessionId :: Unit -> String

foreign import initialWebViewSetUp :: forall action. (action -> Effect Unit) -> String -> (String -> action) -> Effect Unit
foreign import goBackPrevWebPage ::  String -> Effect Unit
foreign import storeMainFiberOb :: forall a. Fiber a -> Effect Unit
foreign import getMainFiber :: forall f a. Fn2 (f -> Maybe f) (Maybe f) (Maybe (Fiber a))
foreign import detectPhoneNumbers :: forall action. (action -> Effect Unit) -> (String  -> action) -> Effect Unit
foreign import setCleverTapUserData :: String -> String -> Unit
foreign import setCleverTapUserProp :: String -> String -> Unit
foreign import cleverTapCustomEvent :: String -> Effect Unit
foreign import cleverTapCustomEventWithParams :: String -> String -> String -> Effect Unit
foreign import cleverTapSetLocation :: Unit -> Effect Unit
foreign import cleverTapEvent :: String -> Array ClevertapEventParams -> Unit
foreign import saveSuggestions :: String -> Suggestions -> Unit
foreign import saveSuggestionDefs :: String -> SuggestionDefinitions -> Unit
foreign import launchDateSettings :: String -> Unit
foreign import startLottieProcess :: LottieAnimationConfig -> Unit
foreign import methodArgumentCount :: String -> Int
foreign import hideLoader :: Effect Unit
foreign import emitJOSEvent :: Fn3 String String Foreign Unit
foreign import getLayoutBounds :: Fn1 String LayoutBound
foreign import getAllDates :: Fn1 Int (Array DateObj)
foreign import horizontalScrollToPos :: EffectFn3 String String Int Unit
foreign import withinTimeRange :: String -> String -> String -> Boolean
foreign import getChatMessages :: String -> Array ChatComponent
foreign import scrollViewFocus :: String -> Int -> Boolean

type LottieAnimationConfig = {
    rawJson :: String
  , lottieId :: String
  , repeat :: Boolean
  , speed :: Number
  , scaleType :: String
  , minProgress :: Number
  , maxProgress :: Number
}

lottieAnimationConfig :: LottieAnimationConfig
lottieAnimationConfig = {
    rawJson : ""
  , lottieId : ""
  , repeat : true
  , speed : 0.6
  , scaleType : "DEFAULT"
  , minProgress : 0.0
  , maxProgress : 1.0
}

-- -- keyStoreEntryPresent :: String -> Flow Boolean
-- -- keyStoreEntryPresent = liftFlow <<< _keyStoreEntryPresent

-- -- createKeyStoreEntry :: String -> (Either String DateTime) -> Flow (Either String String)
-- -- createKeyStoreEntry alias (Left  s) = pure $ Left s
-- -- createKeyStoreEntry alias (Right expiry) = doAff do
-- --   isoTime <- liftEffect $ toISOString $ fromDateTime expiry
-- --   makeAff \cb -> do
-- --     _createKeyStoreEntry alias isoTime (cb $ Right $ Right "key created") (cb <<< Right <<< Left)
-- --     pure nonCanceler

-- -- deleteKeyStoreEntry :: String -> Flow Unit
-- -- deleteKeyStoreEntry keyAlias = liftFlow $ _deleteKeyStoreEntry keyAlias

-- -- keyStoreEncrypt :: String -> String -> Flow (Either String String)
-- -- keyStoreEncrypt alias payload = doAff do
-- --   makeAff \cb -> do
-- --     _keyStoreEncrypt alias payload (Right >>> Right >>> cb) (Left >>> Right >>> cb)
-- --     pure nonCanceler

-- -- keyStoreDecrypt :: String -> String -> Flow (Either String String)
-- -- keyStoreDecrypt keyAlias cipher = doAff do
-- --   makeAff \cb -> do
-- --     _keyStoreDecrypt keyAlias cipher (Right >>> Right >>> cb) (Left >>> Right >>> cb)
-- --     pure nonCanceler

-- -- keyStoreSign :: String -> String -> Flow (Either String String)
-- -- keyStoreSign alias payload = doAff do
-- --   makeAff (\cb -> do
-- --     _keyStoreSign alias payload (Right >>> Right >>> cb) (Left >>> Right >>> cb)
-- --     pure nonCanceler)

-- -- getKeyStorePublicKey :: String -> Flow (Either String String)
-- -- getKeyStorePublicKey keyAlias = doAff do
-- --   makeAff \cb -> do
-- --     _getKeyStorePublicKey keyAlias (Right >>> Right >>> cb) (Left >>> Right >>> cb)
-- --     pure nonCanceler

-- -- sendSafetyNetRequest :: String -> String -> Flow (Either String String)
-- -- sendSafetyNetRequest key nonce = do
-- --   res <- oneOf [sendSafetyNetRequestImpl key nonce, doAff do (makeAff \cb -> do (registerEvent' "onPause" (\_ -> _safefyNetShutdownSafeBrowsing (Left >>> Right >>> cb)) *> pure nonCanceler)) ]
-- --   case res of
-- --     Left "onPause" -> do
-- --       _ <- doAff do (makeAff \cb -> do (registerEventImpl "onResume" (\_ -> _safefyNetInitSafeBrowsing (Left >>> Right >>> cb)) *> pure nonCanceler))
-- --       sendSafetyNetRequest key nonce
-- --     _ -> pure res

-- -- sendSafetyNetRequestImpl :: String -> String -> Flow (Either String String)
-- -- sendSafetyNetRequestImpl key nonce = doAff do
-- --   makeAff \cb -> do
-- --     _sendSafetyNetRequest nonce key (Right >>> Right >>> cb) (Left >>> Right >>> cb)
-- --     pure nonCanceler

-- -- jweEncrypt :: String -> String -> String -> Flow String
-- -- jweEncrypt plainTxt headers keyPath = (liftFlow $ _jweEncrypt plainTxt headers keyPath Left Right) >>= either throwErr pure

-- -- jweDecrypt :: String -> String -> Flow String
-- -- jweDecrypt cipher alias = (liftFlow $ _jweDecrypt cipher alias Left Right) >>= either throwErr pure

-- -- jwsSign :: String -> String -> String -> Flow String
-- -- jwsSign payload headers alias = (liftFlow $ _jwsSign payload headers alias Left Right) >>= either throwErr pure

-- -- jwsVerify :: String -> String -> Flow Boolean
-- -- jwsVerify payload key = liftFlow $ _jwsVerify payload key

-- -- rsaEncryption :: String -> String -> String -> Flow String
-- -- rsaEncryption plainTxt algo key = (liftFlow $ _rsaEncryption plainTxt algo key Left Right) >>= either throwErr pure

showLoader :: forall st. String -> Flow st Unit
showLoader str = liftFlow (showLoaderImpl str)

-- readFile :: String -> Flow st String
-- readFile = liftFlow <<< readFileImpl

-- showQrCode :: String -> String -> Effect Unit
-- showQrCode id str = showQrCodeImpl id str

addMarker :: String -> Number -> Number -> Int -> Number -> Number -> Effect Boolean
addMarker title lat lng markerSize anchorV anchorV1 = (addMarkerImpl title lat lng markerSize anchorV anchorV1)

showMap :: forall action. String -> Boolean -> String -> Number -> (action -> Effect Unit) -> (String -> String -> String -> action) -> Effect Boolean
showMap = showMapImpl --liftFlow (showMapImpl id mapType)

-- loader :: Boolean -> Maybe LoaderMessage -> Flow GlobalState Unit
-- loader flag message = do
--     _ <- pure $ hideKeyboardOnNavigation true
--     case message of
--         Just (LoaderMessage (Title title) (SubTitle subTitle)) -> do
--             _ <- liftFlow (loaderTextImpl (title)  (subTitle))
--             liftFlow (toggleLoaderImpl flag)
--         Nothing -> liftFlow (toggleLoaderImpl flag)

showMarker :: String -> Number -> Number -> Int -> Number -> Number -> Effect Boolean
showMarker title lat lng markerSize anchorV anchorV1 = addMarker title lat lng markerSize anchorV anchorV1


setKeyInSharedPrefKeys :: forall st. String -> String -> Flow st Unit
setKeyInSharedPrefKeys key val = liftFlow (setKeyInSharedPrefKeysImpl key val)

setEnvInNativeSharedPrefKeys :: forall st. String -> String -> Flow st Unit
setEnvInNativeSharedPrefKeys key val = liftFlow (setEnvInNativeSharedPrefKeysImpl key val)

-- onEventWithCB :: Foreign -> Flow GlobalState (Either String String)
-- onEventWithCB obj = doAff do
--   makeAff (\cb -> do
--     _onEventWithCB obj (Right >>> Right >>> cb) (Left >>> Right >>> cb)
--     pure nonCanceler)

type Locations = {
    points :: Coordinates
}

type MapRouteConfig = {
    sourceSpecialTagIcon :: String
  , destSpecialTagIcon :: String
  , vehicleSizeTagIcon :: Int
}

type Coordinates = Array Paths

type Paths = {
    lat :: Number
  , lng :: Number
}

type IsLocationOnPath = {
    points :: Coordinates
  , isInPath :: Boolean
  , eta :: Int
  , distance :: Int
}

type Location = {
  lat :: Number,
  lng :: Number,
  place :: String,
  address :: Maybe String
}

type UpdateRouteMarker = {
    locations :: Locations
  , sourceName :: String
  , destName :: String
  , sourceIcon :: String
  , destIcon :: String
  , mapRouteConfig :: MapRouteConfig
}

type Suggestions = Array
  {
    key :: String,
    value :: Array String
  }

type SuggestionDefinitions = Array
  {
    key :: String,
    value :: {en_us :: String, ta_in :: String, kn_in :: String, hi_in :: String, ml_in :: String, bn_in :: String}
  }

-- type Point = Array Number

-- type Markers = {
--     markerObject :: Array MarkerObject
-- }

-- type MarkerObject = {
--     type :: String,
--     title :: String,
--     coordinates :: Array Number
-- }
-- data Title = Title String
-- data SubTitle = SubTitle String

-- data LoaderMessage = LoaderMessage Title SubTitle

getHeightFromPercent :: Int -> Int
getHeightFromPercent percent =
  let scrHeight = (screenHeight unit)
    in ((scrHeight / 100) * percent)

getWidthFromPercent :: Int -> Int
getWidthFromPercent percent =
  let scrWidth = (screenWidth unit)
    in ((scrWidth / 100) * percent)

fromMetersToKm :: Int -> String
fromMetersToKm distanceInMeters
  | distanceInMeters >= 1000 = parseFloat (toNumber distanceInMeters / 1000.0) 1 <> " km"
  | otherwise = show distanceInMeters <> " m"

getArray :: Int ->Array Int
getArray count = if count == 0 then [count] else [count] <> (getArray (count - 1))
