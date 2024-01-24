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
import Common.Types.App (EventPayload(..),ChatComponent(..), LazyCheck(..), DateObj, LayoutBound, ClevertapEventParams, ShareImageConfig, YoutubeData, CarouselModal, PolylineAnimationConfig, DisplayBase64ImageConig)
-- import Types.APIv2 (Address)
import Foreign (Foreign)
import Control.Monad.Except (runExcept)
-- import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (decodeJSON)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import Data.Either (Either(..))
import Engineering.Helpers.Commons (screenHeight, screenWidth, parseFloat)
import Effect.Uncurried
import Data.Maybe (Maybe(..))
-- import LoaderOverlay.Handler as UI
-- import Effect.Aff (launchAff)
-- import Effect.Class (liftEffect)
-- import PrestoDOM.Core(terminateUI)
import Presto.Core.Types.Language.Flow
import Engineering.Helpers.Commons (screenHeight, screenWidth, os)
import Data.Int (toNumber)
import Data.Function.Uncurried (Fn2(..))
import Presto.Core.Flow (doAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign.Generic (encodeJSON)
import Data.Either (Either(..), hush)
import Data.Function.Uncurried (Fn3, runFn3, Fn1,Fn4, runFn2)
import Foreign.Class (encode)
import Constants.Configs (getPolylineAnimationConfig)
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
foreign import locateOnMap :: EffectFn1 LocateOnMapConfig Unit

foreign import exitLocateOnMap :: String -> Unit
foreign import shareTextMessage :: String -> String -> Unit
foreign import shareImageMessage :: String -> ShareImageConfig -> Unit
foreign import showInAppNotification :: String -> String -> String -> String -> String -> String -> String -> String -> Int -> Effect Unit
foreign import enableMyLocation :: Boolean -> Unit
foreign import isLocationPermissionEnabled :: Unit -> Effect Boolean
foreign import checkAndAskNotificationPermission :: Boolean -> Effect Unit
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
foreign import setFCMTokenWithTimeOut :: EffectFn2 Int (String -> Effect Unit) Unit
-- foreign import dateTimePicker :: forall action. (action -> Effect Unit) -> (Int -> action) -> Effect Unit
-- foreign import getNearbyPlaces :: forall action. (action -> Effect Unit) -> (Place -> action) -> Effect Unit
-- foreign import isNetworkAvailable :: Unit -> Boolean
foreign import openUrlInApp  :: String -> Effect Unit
foreign import renderCameraProfilePicture :: String -> Effect Unit
foreign import openUrlInMailApp  :: String -> Effect Unit
foreign import addMarkerImpl :: String -> Number -> Number -> Int -> Number -> Number -> Effect Boolean
foreign import removeMarker :: String -> Unit
-- foreign import parseAddress      :: String -> Address
foreign import disableActionEditText :: String -> Unit
foreign import uploadFile :: Boolean -> Effect Unit
foreign import previewImage :: String -> Effect Unit
foreign import storeCallBackImageUpload :: forall action. (action -> Effect Unit) -> (String -> String -> String -> action) -> Effect Unit
foreign import renderBase64Image :: String -> String -> Boolean -> String -> Effect Unit
foreign import storeCallBackUploadMultiPartData :: forall action. EffectFn2 (action -> Effect Unit)  (String -> String -> action) Unit
foreign import setScaleType :: String -> String -> String -> Effect Unit
foreign import copyToClipboard :: String -> Unit
foreign import updateRouteMarker :: UpdateRouteMarker -> Effect Unit
foreign import isCoordOnPath :: Locations -> Number -> Number -> Int -> Effect IsLocationOnPath
foreign import updateRoute :: EffectFn1 UpdateRouteConfig Unit
-- -- foreign import drawActualRoute :: String -> String -> Locations -> Effect Int
-- -- foreign import showAndDrawRoute :: String -> String -> String -> Locations -> Effect Int
-- foreign import addMarkers :: Markers -> Effect Unit
-- foreign import removePolyLine   :: String -> Effect Unit
foreign import isOverlayPermissionEnabled :: Unit -> Effect Boolean
foreign import requestLocation  :: Unit -> Effect Unit

foreign import initiateLocationServiceClient :: Effect Unit
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
foreign import getCurrentPositionWithTimeoutImpl  :: forall action. EffectFn4 (action -> Effect Unit) (String -> String -> String -> action) Int Boolean Unit

foreign import translateStringWithTimeout :: forall action. (action -> Effect Unit) -> (String -> action) -> Int -> String -> Effect Unit

foreign import isMockLocation :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit
foreign import animateCamera :: Number -> Number -> Number -> String -> Effect Unit
-- foreign import moveCamera :: Number -> Number -> Number -> Number -> Effect Unit
foreign import minimizeApp    :: String -> Unit
foreign import toast          :: String -> Unit
foreign import restartApp :: Unit -> Effect Unit
-- Deprecated
foreign import factoryResetApp :: String -> Unit
foreign import listDownloadedTranslationModels :: forall action. (action -> Effect Unit) -> Int -> Effect Unit
foreign import hideKeyboardOnNavigation :: Boolean -> Unit
foreign import askNotificationPermission :: Unit -> Effect Unit
-- foreign import onEvent        :: Foreign -> Effect Unit
-- foreign import _onEventWithCB :: Foreign -> (String -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit
-- -- foreign import getSessionInfo :: { android_id_raw :: String, android_id :: String, os_version :: String, package_name :: String, android_api_level :: String }
foreign import getKeyInSharedPrefKeys :: String -> String
foreign import getKeyInNativeSharedPrefKeys :: String -> String
foreign import setKeyInSharedPrefKeysImpl :: String -> String -> Effect Unit
foreign import setKeyInSharedPref :: Fn2 String String Unit
foreign import setEnvInNativeSharedPrefKeysImpl :: String -> String -> Effect Unit
foreign import removeKeysInSharedPrefs :: String -> Unit
foreign import removeKeysInNativeSharedPrefs :: String -> Unit
foreign import toggleLoaderImpl :: Boolean -> Effect Unit
foreign import loaderTextImpl :: String -> String -> Effect Unit
foreign import generatePDF :: forall invoiceScreenState. invoiceScreenState -> String -> Unit
foreign import requestKeyboardShow :: String -> Effect Unit
foreign import showKeyboard :: String -> Effect Unit
foreign import showDialer :: String -> Boolean -> Unit
foreign import getAAID :: String -> String
-- -- foreign import removePolyLineById :: Int -> Effect Unit
foreign import removeAllPolylinesAndMarkers :: Fn2 (Array String) Unit Unit
-- foreign import removeAllPolylines :: String -> Unit
foreign import currentPosition  :: String -> Unit
foreign import openNavigation  :: Number -> Number -> Number -> Number -> String -> Effect Unit
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
foreign import setYoutubePlayer :: Fn3 YoutubeData String String Unit
foreign import addCarouselImpl :: EffectFn2 CarouselModal String Unit
foreign import scrollToEnd :: String -> Boolean -> Effect Unit
foreign import metaLogEvent :: String -> Unit
foreign import metaLogEventWithParams :: String -> String -> String -> Effect Unit
foreign import metaLogEventWithTwoParams :: String -> String -> String -> String -> String -> Effect Unit
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
foreign import storeCallBackNotificationPermission :: forall action. (action -> Effect Unit) -> (Boolean -> action) -> Effect Unit
foreign import isInternetAvailable :: Unit -> Effect Boolean
foreign import storeCallBackInternetAction :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit

foreign import openWhatsAppSupport :: String -> Effect Unit
foreign import generateSessionToken :: String -> String
foreign import addMediaFile :: String -> String -> String -> String -> String -> String -> Effect Unit
foreign import clearFocus :: EffectFn1 String Unit
foreign import removeMediaPlayer :: EffectFn1 String Unit
foreign import renderBase64ImageFile :: EffectFn4 String String Boolean String Unit
foreign import saveAudioFile :: EffectFn1 String String
foreign import uploadMultiPartData :: EffectFn3 String String String Unit
foreign import startAudioRecording :: EffectFn1 String Boolean
foreign import stopAudioRecording :: EffectFn1 String String
foreign import differenceBetweenTwoUTC :: Fn2 String String Int

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
foreign import setCleverTapUserData :: String -> String -> Effect Unit
foreign import setCleverTapUserProp :: Array ClevertapEventParams -> Unit
foreign import cleverTapCustomEvent :: String -> Effect Unit
foreign import cleverTapCustomEventWithParams :: String -> String -> String -> Effect Unit
foreign import cleverTapSetLocation :: Unit -> Effect Unit
foreign import cleverTapEvent :: String -> Array ClevertapEventParams -> Unit
foreign import saveSuggestions :: String -> Suggestions -> Unit
foreign import saveSuggestionDefs :: String -> SuggestionDefinitions -> Unit
foreign import extractReferrerUrl :: Unit -> Effect Unit
foreign import launchDateSettings :: String -> Unit
foreign import startLottieProcess :: LottieAnimationConfig -> Unit
foreign import methodArgumentCount :: String -> Int
foreign import hideLoader :: Effect Unit
foreign import emitJOSEvent :: Fn3 String String Foreign Unit
foreign import getLayoutBounds :: Fn1 String LayoutBound
foreign import horizontalScrollToPos :: EffectFn3 String String Int Unit
foreign import withinTimeRange :: String -> String -> String -> Boolean
foreign import getChatMessages :: LazyCheck -> Array ChatComponent
foreign import storeKeyBoardCallback :: forall action. EffectFn2 (action -> Effect Unit) (String -> action) Unit
foreign import scrollViewFocus :: String -> Int -> Boolean
foreign import clearChatMessages :: Effect Unit
foreign import getLocationPermissionStatus :: Fn1 Unit String 
foreign import pauseYoutubeVideo :: Unit -> Unit 
foreign import storeCallBackLocateOnMap :: forall action. (action -> Effect Unit) -> (String -> String -> String -> action) -> Effect Unit
foreign import debounceFunction :: forall action. Int -> (action -> Effect Unit) -> (String -> Boolean -> action) -> Boolean -> Effect Unit
foreign import updateInputString :: String -> Unit
foreign import downloadMLTranslationModel :: EffectFn1 String Unit
foreign import supportsInbuildYoutubePlayer :: Unit -> Boolean 
foreign import addCarouselWithVideoExists :: Unit -> Boolean
foreign import isNetworkTimeEnabled :: EffectFn1 Unit Boolean
foreign import storeOnResumeCallback :: forall action. Fn2 (action -> Effect Unit) action Unit
foreign import getLocationNameV2 :: Fn2 Number Number String
foreign import getLatLonFromAddress :: Fn1 String { latitude :: Number, longitude :: Number }
foreign import isNotificationPermissionEnabled :: Unit -> Effect Boolean

foreign import setMapPaddingImpl :: EffectFn4 Int Int Int Int Unit

foreign import displayBase64Image :: EffectFn1 DisplayBase64ImageConig Unit

foreign import drawRouteV2 :: DrawRouteConfig -> Effect Unit 

setMapPadding :: Int -> Int -> Int -> Int -> Effect Unit
setMapPadding = runEffectFn4 setMapPaddingImpl


drawRoute :: Locations -> String -> String -> Boolean -> String -> String -> Int -> String -> String -> String -> MapRouteConfig -> String -> Effect Unit
drawRoute locations style routeColor isActual startMarker endMarker routeWidth routeType startMarkerLabel endMarkerLabel mapRouteConfig pureScriptID = do
  let routeConfig = mkRouteConfig locations startMarker endMarker routeType startMarkerLabel endMarkerLabel style isActual mapRouteConfig
      drawRouteConfig = mkDrawRouteConfig routeConfig pureScriptID
  drawRouteV2 drawRouteConfig

getCurrentPositionWithTimeout :: forall action. (action -> Effect Unit) -> (String -> String -> String -> action) -> Int -> Boolean -> Effect Unit
getCurrentPositionWithTimeout = runEffectFn4 getCurrentPositionWithTimeoutImpl

type LottieAnimationConfig = {
    rawJson :: String
  , lottieId :: String
  , repeat :: Boolean
  , speed :: Number
  , scaleType :: String
  , minProgress :: Number
  , maxProgress :: Number
  , forceToUseRemote :: Boolean
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
  , forceToUseRemote : false
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

removeAllPolylines :: String -> Unit 
removeAllPolylines str = removeAllPolylinesImpl markersToRemove
  where 
    removeAllPolylinesImpl :: Array String -> Unit
    removeAllPolylinesImpl mrkrToRemove = runFn2 removeAllPolylinesAndMarkers mrkrToRemove unit
    
    markersToRemove :: Array String   
    markersToRemove = ["ic_auto_nav_on_map" , "ny_ic_vehicle_nav_on_map" , "ny_ic_black_yellow_auto" , "ny_ic_src_marker", "ny_ic_dest_marker"]


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

type LocateOnMapConfig = {
    goToCurrentLocation :: Boolean
  , lat :: Number
  , lon :: Number
  , geoJson :: String
  , points :: (Array Location)
  , zoomLevel :: Number
  , labelId :: String
}

locateOnMapConfig :: LocateOnMapConfig
locateOnMapConfig = {
    goToCurrentLocation : false
  , lat : 0.0
  , lon : 0.0
  , geoJson : ""
  , points : []
  , zoomLevel : if (os == "IOS") then 19.0 else 17.0
  , labelId : ""
}


type MapRouteConfig = {
    sourceSpecialTagIcon :: String
  , destSpecialTagIcon :: String
  , vehicleSizeTagIcon :: Int
  , isAnimation :: Boolean
  , polylineAnimationConfig :: PolylineAnimationConfig
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
  address :: Maybe String,
  city :: Maybe String
}

type UpdateRouteMarker = {
    locations :: Locations
  , sourceName :: String
  , destName :: String
  , sourceIcon :: String
  , destIcon :: String
  , mapRouteConfig :: MapRouteConfig
  , pureScriptID :: String
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

type UpdateRouteConfig = {
    json :: Locations
  , destMarker :: String
  , eta :: String
  , srcMarker :: String
  , specialLocation :: MapRouteConfig
  , zoomLevel :: Number
  , pureScriptID :: String
}

updateRouteConfig :: UpdateRouteConfig
updateRouteConfig = {
    json : {points: []}
  , destMarker : ""
  , eta : ""
  , srcMarker : ""
  , specialLocation : {
      sourceSpecialTagIcon: "", 
      destSpecialTagIcon: "", 
      vehicleSizeTagIcon: 0, 
      isAnimation: false, 
      polylineAnimationConfig: {
        color: "", 
        draw: 0, 
        fade: 0, 
        delay: 0
      } 
  }
  , zoomLevel : if (os == "IOS") then 19.0 else 17.0
  , pureScriptID : ""
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
getArray count = if count <= 0 then [] else [count] <> (getArray (count - 1))

addCarousel :: CarouselModal ->  String -> Effect Unit
addCarousel = runEffectFn2 addCarouselImpl

displayBase64ImageConfig :: DisplayBase64ImageConig
displayBase64ImageConfig = {
    source : ""
  , id : ""
  , scaleType : "CENTER_CROP"
  , inSampleSize : 1
}

---------- ################################### DRAW ROUTE CONFIG ################################### ----------

type DrawRouteConfig = {
  routes :: {
    normalRoute :: RouteConfig
  },
  pureScriptID :: String
}

type RouteConfig = {
  locations :: Locations,
  style :: String,
  routeColor :: String,
  isActual :: Boolean,
  startMarker :: String,
  endMarker :: String,
  routeWidth :: Int,
  routeType :: String,
  startMarkerLabel :: String,
  endMarkerLabel :: String,
  mapRouteConfig :: MapRouteConfig
}

mkRouteConfig :: Locations -> String -> String -> String -> String -> String -> String -> Boolean -> MapRouteConfig -> RouteConfig
mkRouteConfig normalRoute startMarker endMarker routeType startMarkerLabel endMarkerLabel style isActual mapRouteConfig = 
  routeConfig{
    locations = normalRoute,
    startMarker = startMarker,
    endMarker = endMarker,
    routeType = routeType,
    startMarkerLabel = startMarkerLabel,
    endMarkerLabel = endMarkerLabel,
    style = style,
    isActual = isActual,
    mapRouteConfig = mapRouteConfig
  }

mkDrawRouteConfig :: RouteConfig -> String -> DrawRouteConfig
mkDrawRouteConfig normalRouteConfig pureScriptID = 
  { routes : {
      normalRoute : normalRouteConfig
    },
    pureScriptID : pureScriptID
  }


routeConfig :: RouteConfig 
routeConfig = {
  locations : {points: []},
  style : "LineString",
  routeColor : "#323643",
  isActual : true,
  startMarker : "",
  endMarker : "",
  routeWidth : 8,
  routeType : "",
  startMarkerLabel : "",
  endMarkerLabel : "",
  mapRouteConfig : defaultMapRouteConfig
}

defaultMapRouteConfig :: MapRouteConfig
defaultMapRouteConfig = {
  sourceSpecialTagIcon : "",
  destSpecialTagIcon : "",
  vehicleSizeTagIcon : 90,
  isAnimation : false,
  polylineAnimationConfig : getPolylineAnimationConfig
}
