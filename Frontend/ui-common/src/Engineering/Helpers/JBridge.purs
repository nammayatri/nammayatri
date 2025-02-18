{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module JBridge where

import Common.Resources.Constants as Constant
import Common.Styles.Colors as Color
import Common.Types.App
import Constants.Configs (getPolylineAnimationConfig)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), hush)
import Data.Function.Uncurried (Fn2(..), Fn3, runFn3, Fn1, Fn4, runFn2, Fn5)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler, Fiber, Aff, launchAff)
import PrestoDOM.Core (isScreenActive)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Uncurried
import Engineering.Helpers.Commons (screenHeight, screenWidth, os, callbackMapper, parseFloat, liftFlow)
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode, encode)
import Foreign.Generic (decodeJSON)
import Language.Types (STR(..))
import Prelude
import Presto.Core.Flow (Flow, doAff)
import Presto.Core.Types.Language.Flow
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import Data.Eq.Generic (genericEq)
import Data.Array (head, (!!))

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
foreign import fetchPackageName :: Unit -> Effect String
foreign import exitLocateOnMap :: String -> Unit
foreign import shareTextMessage :: String -> String -> Unit
foreign import shareImageMessage :: String -> ShareImageConfig -> Unit
foreign import showInAppNotification :: InAppNotificationPayload -> Effect Unit
foreign import emitJOSEventWithCb :: forall action. Fn4 String JOSEventInnerPayload (action -> Effect Unit) (String -> action) Unit
foreign import enableMyLocation :: Boolean -> Unit
foreign import isLocationPermissionEnabled :: Unit -> Effect Boolean
foreign import isBackgroundLocationEnabled :: Unit -> Effect Boolean
foreign import checkAndAskNotificationPermission :: Boolean -> Effect Unit
foreign import isMicrophonePermissionEnabled :: Unit -> Effect Boolean
-- foreign import getPackageName   :: Effect String
foreign import getVersionCode   :: Effect Int
foreign import getVersionName   :: Effect String
foreign import getManufacturerName :: Unit -> String
foreign import getAndroidVersion :: Effect Int
-- foreign import showQrCodeImpl      :: String -> String -> Effect Unit
-- foreign import scanQrCode       :: forall action. String MerchantConfig.Utils-> (action -> Effect Unit) ->  (String -> action) -> Effect Unit
foreign import datePicker       :: forall action. String -> (action -> Effect Unit)  -> (String -> Int -> Int -> Int -> action) -> Effect Unit
foreign import setFCMToken :: forall action. (action -> Effect Unit) -> (String  -> action) -> Effect Unit
foreign import setFCMTokenWithTimeOut :: EffectFn2 Int (String -> Effect Unit) Unit
-- foreign import dateTimePicker :: forall action. (action -> Effect Unit) -> (Int -> action) -> Effect Unit
-- foreign import getNearbyPlaces :: forall action. (action -> Effect Unit) -> (Place -> action) -> Effect Unit
-- foreign import isNetworkAvailable :: Unit -> Boolean
foreign import openUrlInApp  :: String -> Effect Unit
foreign import renderCameraProfilePicture :: String -> Effect Unit
foreign import openUrlInMailApp  :: String -> Effect Unit
foreign import showMarkerImpl :: EffectFn1 ShowMarkerConfig Boolean
foreign import fadeInFadeOutMarker :: Fn3 String String String Unit
foreign import removeMarker :: String -> Unit
foreign import removeAllMarkers :: String -> Unit
-- foreign import parseAddress      :: String -> Address
foreign import disableActionEditText :: String -> Unit
foreign import uploadFile :: UploadFileConfig -> Boolean -> Effect Unit
foreign import previewImage :: String -> Effect Unit
foreign import storeCallBackImageUpload :: forall action. (action -> Effect Unit) -> (String -> String -> String -> action) -> Effect Unit
foreign import renderBase64Image :: String -> String -> Boolean -> String -> Effect Unit
foreign import storeCallBackUploadMultiPartData :: forall action. EffectFn2 (action -> Effect Unit)  (String -> String -> action) Unit
foreign import setScaleType :: String -> String -> String -> Effect Unit
foreign import copyToClipboard :: String -> Unit
foreign import updateMarker :: MarkerConfig -> Effect Unit
foreign import isCoordOnPath :: Locations -> Number -> Number -> Int -> Effect IsLocationOnPath
foreign import updateRoute :: EffectFn1 UpdateRouteConfig Unit
-- -- foreign import drawActualRoute :: String -> String -> Locations -> Effect Int
-- -- foreign import showAndDrawRoute :: String -> String -> String -> Locations -> Effect Int
-- foreign import addMarkers :: Markers -> Effect Unit
-- foreign import removePolyLine   :: String -> Effect Unit
foreign import isOverlayPermissionEnabled :: Unit -> Effect Boolean
foreign import requestLocation  :: Unit -> Effect Unit
foreign import requestBackgroundLocation  :: Unit -> Effect Unit

foreign import initiateLocationServiceClient :: Effect Unit
foreign import checkOverlayPermission  :: Unit -> Effect Unit
foreign import requestAutoStartPermission  :: Unit -> Effect Unit
foreign import requestBatteryPermission :: Unit -> Effect Unit
foreign import isBatteryPermissionEnabled :: Unit -> Effect Boolean
foreign import reallocateMapFragment :: String -> Effect Unit
foreign import showMapImpl :: forall action. String -> Boolean -> String -> Number -> Number -> Number -> (action -> Effect Unit) -> (String -> String -> String -> action) -> Effect Boolean
foreign import mapSnapShot :: forall action. String -> Locations -> String -> Boolean -> (action -> Effect Unit) -> (String -> action) -> Effect Boolean
foreign import getCurrentLatLong  :: Effect Paths
foreign import isLocationEnabled :: Unit -> Effect Boolean
foreign import getCurrentPosition  :: forall action. (action -> Effect Unit) -> (String -> String -> action) -> Effect Unit
foreign import getCurrentPositionWithTimeoutImpl  :: forall action. EffectFn4 (action -> Effect Unit) (String -> String -> String -> action) Int Boolean Unit

foreign import translateStringWithTimeout :: forall action. (action -> Effect Unit) -> (String -> action) -> Int -> String -> Effect Unit

foreign import isMockLocation :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit
foreign import animateCamera :: Number -> Number -> Number -> String -> Effect Unit
foreign import moveCameraWithoutAnimation :: Number -> Number -> Number -> String -> Effect Unit
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
foreign import removeAllPolygons :: String -> Unit
foreign import currentPosition  :: String -> Unit
foreign import openNavigation  :: Number -> Number -> String -> Unit
foreign import stopLocationPollingAPI :: Effect Unit
foreign import startLocationPollingAPI :: Effect Unit
foreign import startChatListenerService :: Effect Unit
foreign import startService :: String -> Unit
foreign import stopService :: String -> Unit
foreign import scrollOnResume :: forall action. (action -> Effect Unit) -> (action) -> Effect Unit
foreign import stopChatListenerService :: Effect Unit
foreign import storeCallBackMessageUpdated :: forall action. (action -> Effect Unit) -> String -> String  -> (String -> String -> String -> String -> action) -> action -> Effect Unit
foreign import dateCallback :: forall action. Fn2 (action -> Effect Unit) action Unit
foreign import unregisterDateAndTime :: Effect Unit
foreign import storeCallBackOpenChatScreen :: forall action. (action -> Effect Unit) -> (action) -> Effect Unit
foreign import sendMessage :: String -> Unit
foreign import getSuggestionsfromLocal :: String -> String -> Array String
foreign import getSuggestionfromKey :: String -> String -> String -> String
foreign import setYoutubePlayer :: forall action. Fn5 YoutubeData String String (action -> Effect Unit) (String -> action) Unit
foreign import addMediaPlayer :: String -> String -> Boolean -> Effect Unit
foreign import switchYoutubeVideo :: String -> Unit
foreign import addCarouselImpl :: EffectFn2 CarouselModal String Unit
foreign import addReels :: forall action. EffectFn5 String Int String (action -> Effect Unit) (String -> String -> Foreign -> Foreign -> action) Unit
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
foreign import storeCallBackInternetAction :: forall action. EffectFn3 (action -> Effect Unit) (String -> action) String Unit
foreign import storeNoInternetAction :: forall action. EffectFn2 (action -> Effect Unit) action Unit
foreign import clearNoInternetAction :: Unit -> Effect Unit

foreign import openWhatsAppSupport :: String -> Effect Unit
foreign import generateSessionToken :: String -> String
foreign import addMediaFile :: EffectFn7 String String String String String String Boolean Unit
foreign import clearFocusFunction :: String -> Unit
foreign import removeMediaPlayer :: EffectFn1 String Unit
foreign import renderBase64ImageFile :: EffectFn4 String String Boolean String Unit
foreign import saveAudioFile :: EffectFn1 String String
foreign import uploadMultiPartData :: EffectFn5 String String String String String Unit
foreign import startAudioRecording :: EffectFn1 String Boolean
foreign import stopAudioRecording :: EffectFn1 String String
foreign import differenceBetweenTwoUTC :: Fn2 String String Int

foreign import differenceBetweenTwoUTCInMinutes :: Fn2 String String Int
foreign import getSecondsFromUTCTime :: Fn1 String Int

foreign import toggleBtnLoader :: String -> Boolean -> Unit
foreign import getBtnLoader :: String -> Boolean
foreign import launchInAppRatingPopup :: Unit -> Effect Unit
foreign import getExtendedPath :: Locations -> Locations
foreign import generateSessionId :: Unit -> String
foreign import cleanOnResumeCallback :: Effect Unit
foreign import cleanOnPauseCallback :: Effect Unit
foreign import initialWebViewSetUp :: forall action. (action -> Effect Unit) -> String -> (String -> action) -> Effect Unit
foreign import goBackPrevWebPage ::  String -> Effect Unit
foreign import detectPhoneNumbers :: forall action. (action -> Effect Unit) -> (String  -> action) -> Effect Unit
foreign import setCleverTapUserData :: String -> String -> Effect Unit
foreign import setCleverTapProfileData :: String -> String -> Effect Unit
foreign import loginCleverTapUser :: Unit -> Effect Unit
foreign import setCleverTapUserProp :: Array ClevertapEventParams -> Unit
foreign import cleverTapCustomEvent :: String -> Effect Unit
foreign import cleverTapCustomEventWithParams :: String -> String -> String -> Effect Unit
foreign import cleverTapSetLocation :: Unit -> Effect Unit
foreign import voipDialer :: forall action. EffectFn6 String Boolean String Boolean (action -> Effect Unit) (String -> String -> String -> Int -> Int -> String -> String -> String -> action) Unit
foreign import initSignedCall :: String -> Boolean -> Unit
foreign import isSignedCallInitialized :: Effect Boolean
foreign import destroySignedCall :: Unit -> Effect Unit
foreign import cleverTapEvent :: String -> Array ClevertapEventParams -> Unit
foreign import saveSuggestions :: String -> Suggestions -> Unit
foreign import saveSuggestionDefs :: String -> SuggestionDefinitions -> Unit
foreign import extractReferrerUrl :: Unit -> Effect Unit
foreign import launchDateSettings :: String -> Unit
foreign import startLottieProcess :: LottieAnimationConfig -> Unit
foreign import methodArgumentCount :: String -> Int
foreign import hideLoader ::String -> Effect Unit
foreign import emitJOSEvent :: Fn3 String String Foreign Unit
foreign import getLayoutBounds :: Fn1 String LayoutBound
foreign import horizontalScrollToPos :: EffectFn3 String String Int Unit
foreign import withinTimeRange :: String -> String -> String -> Boolean
foreign import timeValidity :: String -> String -> Boolean
foreign import isServiceRunning :: Fn1 String Boolean
foreign import getChatMessages :: LazyCheck -> Array ChatComponent
foreign import storeKeyBoardCallback :: forall action. EffectFn2 (action -> Effect Unit) (String -> action) Unit
foreign import scrollViewFocus :: String -> Int -> Boolean
foreign import clearChatMessages :: Effect Unit
foreign import getLocationPermissionStatus :: Fn1 Unit String
foreign import pauseYoutubeVideo :: Unit -> Unit
foreign import releaseYoutubeView :: Unit -> Unit
foreign import storeCallBackLocateOnMap :: EffectFn2 (String -> String -> String -> Effect Unit) ((String -> String -> String -> Effect Unit) -> String -> String -> String -> Effect Unit) Unit
foreign import storeCallbackHotspotMap :: forall action. EffectFn2 (action -> Effect Unit) (String -> String -> String -> String -> action) Unit
foreign import debounceFunction :: forall action. Int -> (action -> Effect Unit) -> (String -> Boolean -> action) -> Boolean -> Effect Unit
foreign import updateInputString :: String -> Unit
foreign import downloadMLTranslationModel :: EffectFn1 String Unit
foreign import supportsInbuildYoutubePlayer :: Unit -> Boolean
foreign import addCarouselWithVideoExists :: Unit -> Boolean
foreign import isNetworkTimeEnabled :: EffectFn1 Unit Boolean
foreign import storeOnResumeCallback :: forall action. Fn2 (action -> Effect Unit) action Unit
foreign import refreshFlowCallback :: Fn2 String (Unit -> Effect Unit) Unit
foreign import getLocationNameV2 :: Fn2 Number Number String
foreign import getLatLonFromAddress :: Fn1 String { latitude :: Number, longitude :: Number }
foreign import isNotificationPermissionEnabled :: Unit -> Effect Boolean
foreign import askRequestedPermissions :: Array String -> Unit
foreign import askRequestedPermissionsWithCallback :: forall action. Array String -> (action -> Effect Unit) -> (Boolean -> action) -> Effect Unit
foreign import setupCamera :: String -> Boolean -> Unit
foreign import startRecord :: forall action. (action -> Effect Unit)  -> (String -> String -> action) -> Effect Unit
foreign import stopRecord :: Unit -> Effect Unit
foreign import drawCircleOnMap :: EffectFn1 CircleConfig Unit

foreign import clearAudioPlayer :: String -> Unit
foreign import pauseAudioPlayer :: String -> Unit
foreign import startAudioPlayer :: forall action. Fn4 String (action -> Effect Unit) (String -> action) String Unit
foreign import datePickerImpl :: forall action. EffectFn5 (action -> Effect Unit) (String -> Int -> Int -> Int -> action) Int Number Number Unit
foreign import timePickerImpl :: forall action. EffectFn3 (action -> Effect Unit) ( Int -> Int -> String -> action) Number Unit
foreign import setMapPaddingImpl :: EffectFn4 Int Int Int Int Unit

foreign import displayBase64Image :: EffectFn1 DisplayBase64ImageConig Unit
foreign import triggerCallBackQueue :: EffectFn1 String  Unit
foreign import updateQueue :: EffectFn4 String String String (String -> String -> String -> Effect Unit) Unit
foreign import drawRouteV2 :: DrawRouteConfig -> Effect Unit
foreign import renderSliderImpl :: forall action. EffectFn3 (action -> Effect Unit) (Int -> action) SliderConfig Unit
foreign import getCircleCallback :: forall action. EffectFn2 (action -> Effect Unit) (String -> String -> String -> action) Unit

foreign import isAccessibilityEnabled :: String -> Boolean
foreign import getFromUTC :: String -> String -> String
foreign import getDeviceID :: Unit -> String
foreign import getAndroidId :: Unit -> String
foreign import getAppName :: Unit -> String
foreign import initialiseShakeListener :: forall action. EffectFn3 (action -> Effect Unit) (Int -> action) ShakeListenerConfig Unit
foreign import unregisterShakeListener :: Unit -> Unit
foreign import registerShakeListener :: Unit -> Unit
foreign import fetchFilesFromFolderPath :: String -> Array String
foreign import isPackageInstalled :: String -> Boolean
foreign import requestUninstallPackage :: String -> Boolean
foreign import storeOnPauseCallback :: forall action. Fn2 (action -> Effect Unit) action Unit
foreign import launchCustomTab :: forall action. EffectFn2 String action  Unit
foreign import getMedianUTCTime :: Fn2 String String String
foreign import storeCallBackPickContact :: forall action. EffectFn2 (action -> Effect Unit)  (String -> String -> action) Unit
foreign import pickContact :: EffectFn1 String Boolean
foreign import getResourceIdentifier :: String -> String -> Int
foreign import executeJS :: Fn2 (Array String) String String

type SliderConfig = {
  id :: String,
  sliderConversionRate :: Number,
  sliderMinValue :: Int,
  sliderMaxValue :: Int,
  sliderDefaultValue :: Int,
  toolTipId :: String,
  enableToolTip :: Boolean,
  progressColor :: String,
  thumbColor :: String,
  bgColor :: String,
  bgAlpha :: Int ,
  stepFunctionForCoinConversion :: Int,
  getCallbackOnProgressChanged :: Boolean
}

sliderConfig :: SliderConfig
sliderConfig = {
  id : "",
  sliderConversionRate : 1.0,
  sliderMinValue : 0,
  sliderMaxValue : 100,
  sliderDefaultValue : 0,
  stepFunctionForCoinConversion : 1,
  toolTipId : "",
  enableToolTip : false,
  getCallbackOnProgressChanged : false,
  progressColor : Color.white900,
  thumbColor : Color.blue800,
  bgColor : Color.black,
  bgAlpha : 50
}

clearFocus :: EffectFn1 String Unit
clearFocus = mkEffectFn1 $ \id -> pure $ clearFocusFunction id

foreign import initHVSdk :: forall action. EffectFn8 String String String Boolean String String (String -> action) (action -> Effect Unit) Unit
foreign import decodeAndStoreImage :: Fn1 String String
foreign import convertAudioToBase64 :: Fn1 String String
foreign import encodeToBase64 :: forall action. EffectFn5 String Int (String -> Maybe String) (Maybe String) (action -> Effect Unit) (Effect String)
foreign import isSdkTokenExpired :: Fn1 String Boolean
foreign import makeSdkTokenExpiry :: Fn1 Int String
foreign import getEpochTime :: Fn1 String Number
setMapPadding :: Int -> Int -> Int -> Int -> Effect Unit
setMapPadding = runEffectFn4 setMapPaddingImpl

drawRoute :: Array RouteConfig -> String -> Effect Unit
drawRoute routeConfigs pureScriptID = do
  let drawRouteConfig = mkDrawRouteConfig routeConfigs pureScriptID
  drawRouteV2 drawRouteConfig

getCurrentPositionWithTimeout :: forall action. (action -> Effect Unit) -> (String -> String -> String -> action) -> Int -> Boolean -> Effect Unit
getCurrentPositionWithTimeout = runEffectFn4 getCurrentPositionWithTimeoutImpl


timePickerWithoutTimeout :: forall action. (action -> Effect Unit) -> ( Int -> Int -> String -> action) -> Number -> Effect Unit
timePickerWithoutTimeout = runEffectFn3 timePickerImpl

type LottieAnimationConfig = {
    rawJson :: String
  , lottieId :: String
  , repeat :: Boolean
  , speed :: Number
  , scaleType :: String
  , minProgress :: Number
  , maxProgress :: Number
  , progress :: Number
  , forceToUseRemote :: Boolean
  , cacheEnabled :: Boolean
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
  , progress : 0.0
  , forceToUseRemote : false
  , cacheEnabled : true
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

showMarker :: MarkerConfig -> Number -> Number -> Int -> Number -> Number -> String -> Effect Boolean
showMarker markerConfig lat lng markerSize anchorV anchorV1 purescriptId = runEffectFn1 showMarkerImpl defaultShowMarkerConfig {markerConfig = markerConfig, lat = lat, lng = lng, markerSize = markerSize, anchorV = anchorV, anchorV1 = anchorV1, isSpecialZone = false, purescriptId = purescriptId}

showMap :: forall action. String -> Boolean -> String -> Number -> Number -> Number -> (action -> Effect Unit) -> (String -> String -> String -> action) -> Effect Boolean
showMap = showMapImpl --liftFlow (showMapImpl id mapType)

-- loader :: Boolean -> Maybe LoaderMessage -> Flow GlobalState Unit
-- loader flag message = do
--     _ <- pure $ hideKeyboardOnNavigation true
--     case message of
--         Just (LoaderMessage (Title title) (SubTitle subTitle)) -> do
--             _ <- liftFlow (loaderTextImpl (title)  (subTitle))
--             liftFlow (toggleLoaderImpl flag)
--         Nothing -> liftFlow (toggleLoaderImpl flag)

removeAllPolylines :: String -> Unit
removeAllPolylines str = removeAllPolylinesImpl markersToRemove
  where
    removeAllPolylinesImpl :: Array String -> Unit
    removeAllPolylinesImpl mrkrToRemove = runFn2 removeAllPolylinesAndMarkers mrkrToRemove unit

    markersToRemove :: Array String
    markersToRemove = ["ic_auto_nav_on_map" , "ny_ic_vehicle_nav_on_map" , "ny_ic_black_yellow_auto" , "ny_ic_koc_auto_on_map", "ny_ic_src_marker", "ny_ic_dest_marker", "ny_ic_suv_nav_on_map", "ny_ic_hatchback_nav_on_map", "ny_ic_blue_marker","ny_ic_drop_loc_marker", "ny_ic_bike_nav_on_map","ny_ic_bike_pickup_nav_on_map", "", "ny_ic_suv_plus_nav_on_map", "ny_ic_bike_delivery_nav_on_map","ny_ic_ambulance_nav_on_map"]


setKeyInSharedPrefKeys :: forall st. String -> String -> Flow st Unit
setKeyInSharedPrefKeys key val = liftFlow (setKeyInSharedPrefKeysImpl key val)

setEnvInNativeSharedPrefKeys :: forall st. String -> String -> Flow st Unit
setEnvInNativeSharedPrefKeys key val = liftFlow (setEnvInNativeSharedPrefKeysImpl key val)

datePickerWithTimeout :: forall action. (action -> Effect Unit) -> (String -> Int -> Int -> Int -> action) -> Int -> Number -> Number -> Effect Unit
datePickerWithTimeout = runEffectFn5 datePickerImpl

timePickerWithTimeout :: forall action. (action -> Effect Unit) -> ( Int -> Int -> String -> action) -> Number -> Effect Unit
timePickerWithTimeout = runEffectFn3 timePickerImpl

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
  , markerCallbackForTags :: Array String
  , markerCallback :: String
  , specialZoneMarkerConfig :: SpecialZoneMarkerConfig
  , navigateToNearestGate :: Boolean
  , locationName :: String
  , locateOnMapPadding :: LocateOnMapPadding
  , enableMapClickListener :: Boolean
  , editPickUpThreshold :: Number
  , editPickupLocation :: Boolean
  , thresholdDistToSpot :: Int
  , circleConfig :: CircleConfig
}

locateOnMapConfig :: LocateOnMapConfig
locateOnMapConfig = {
    goToCurrentLocation : false
  , lat : 0.0
  , lon : 0.0
  , geoJson : ""
  , points : []
  , zoomLevel : Constant.zoomLevel
  , labelId : ""
  , editPickUpThreshold : 100.0
  , editPickupLocation : false
  , markerCallbackForTags : []
  , markerCallback : ""
  , specialZoneMarkerConfig : {
        showLabel : false
      , showLabelActionImage : false
      , labelImage : ""
      , labelMaxWidth : Constant.locateOnMapLabelMaxWidth
      , labelActionImage : ""
      , theme : "LIGHT"
      , spotIcon : "ny_ic_zone_pickup_marker_green"
      , selectedSpotIcon : "ny_ic_selected_zone_pickup_marker_green"
      , showZoneLabel : false
      }
  , navigateToNearestGate : true
  , locationName : ""
  , locateOnMapPadding : { left : 1.0, top : 1.0, right : 1.0, bottom : 1.0 }
  , enableMapClickListener : false
  , thresholdDistToSpot : 3
  , circleConfig : defaultCircleConfig
}

type ShowMarkerConfig = {
  markerConfig :: MarkerConfig,
  lat :: Number,
  lng :: Number,
  markerSize :: Int,
  anchorV :: Number,
  anchorV1 :: Number,
  isSpecialZone :: Boolean,
  purescriptId :: String
}

defaultShowMarkerConfig :: ShowMarkerConfig
defaultShowMarkerConfig = {
  markerConfig : defaultMarkerConfig,
  lat : 9.9,
  lng : 9.9,
  markerSize : 160,
  anchorV : 0.0,
  anchorV1 : 0.0,
  isSpecialZone : false,
  purescriptId : ""
}

type LocateOnMapPadding = {
    left :: Number
  , top :: Number
  , right :: Number
  , bottom :: Number
}

type SpecialZoneMarkerConfig = {
    showLabelActionImage :: Boolean
  , labelActionImage :: String
  , labelMaxWidth :: Int
  , theme :: String
  , spotIcon :: String
  , selectedSpotIcon :: String
  , showLabel :: Boolean
  , labelImage :: String
  , showZoneLabel :: Boolean
}

type MarkerImageConfig = {
  image :: String,
  height :: Int,
  width :: Int
}

defaultMarkerImageConfig :: MarkerImageConfig
defaultMarkerImageConfig = {
  image : "",
  height : 28,
  width : 28
}

type ActionImageConfig = {
  image :: String,
  height :: Int,
  width :: Int,
  orientation :: String,
  background :: String,
  padding :: EdgeInsets,
  layoutMargin :: EdgeInsets,
  layoutPadding :: EdgeInsets
}

type EdgeInsets = {
    left :: Int
  , top :: Int
  , right :: Int
  , bottom :: Int
}

defaultActionImageConfig :: ActionImageConfig
defaultActionImageConfig = {
    image : ""
  , height : 30
  , width : 30
  , orientation : "VERTICAL"
  , background : ""
  , padding : dummyMarkerEdgeInsets
  , layoutMargin : dummyMarkerEdgeInsets
  , layoutPadding : dummyMarkerEdgeInsets
}

type MarkerConfig = {
    showPointer :: Boolean
  , pointerIcon :: String
  , primaryText :: String
  , shortTitle :: String
  , secondaryText :: String
  , labelImage :: MarkerImageConfig
  , labelActionImage :: MarkerImageConfig
  , labelMaxWidth :: Int
  , labelMaxLines :: Int
  , labelTextSize :: Int
  , labelHeight :: Int
  , markerCallback :: String
  , markerCallbackForTags :: Array String
  , theme :: String
  , position :: Paths
  , rotation :: Number
  , markerId :: String
  , markerSize :: Number
  , useSourcePoint :: Boolean
  , useDestPoints :: Boolean
  , usePosition :: Boolean
  , useMarkerSize :: Boolean
  , animationConfig :: AnimationConfig
  , useAnchorConfig :: Boolean
  , markerSizeWidth :: Number
  , markerSizeHeight :: Number
  , anchorV :: Number
  , anchorU :: Number
  , actionImage :: ActionImageConfig
}

dummyMarkerEdgeInsets :: EdgeInsets
dummyMarkerEdgeInsets = {
  left : -1,
  top : -1,
  right : -1,
  bottom : -1
}

defaultMarkerConfig :: MarkerConfig
defaultMarkerConfig = {
    showPointer : true
  , pointerIcon : ""
  , primaryText : ""
  , shortTitle : ""
  , secondaryText : ""
  , labelImage : defaultMarkerImageConfig
  , labelActionImage : defaultMarkerImageConfig
  , markerCallback : ""
  , labelMaxWidth : if os == "IOS" then 100 else 300
  , labelMaxLines : 1
  , labelTextSize : 11
  , labelHeight : 16
  , markerCallbackForTags : []
  , theme : "LIGHT"
  , position : { lat : 0.0, lng : 0.0 }
  , rotation : 0.0
  , markerId : ""
  , animationConfig : defaultAnimationConfig
  , anchorV : 0.5
  , anchorU : 0.5
  , markerSize : 90.0
  , useMarkerSize : false
  , useSourcePoint : false
  , useAnchorConfig : false --- use in case of ios
  , markerSizeWidth : 30.0  --- use in case of ios
  , markerSizeHeight : 40.0 --- use in case of ios
  , useDestPoints : true
  , usePosition : false
  , actionImage : defaultActionImageConfig
}

type CircleConfig = {
  radius :: Number,
  primaryStrokeColor :: String,
  fillColor :: String,
  strokeWidth :: Int,
  secondaryStrokeColor :: String,
  circleId :: String,
  centerLat :: Number,
  centerLon :: Number,
  showPrimaryStrokeColor :: Boolean,
  strokePattern :: String,
  isCircleClickable :: Boolean
}

defaultCircleConfig :: CircleConfig
defaultCircleConfig = {
  radius : 0.0,
  primaryStrokeColor : "#00FFFFFF",
  fillColor : "#00FFFFFF",
  strokeWidth : 0,
  secondaryStrokeColor : "#00FFFFFF",
  circleId : "",
  centerLat : 0.0,
  centerLon : 0.0,
  showPrimaryStrokeColor : true,
  strokePattern : "NORMAL",
  isCircleClickable : false
}

data StrokePattern = NORMAL | DASHED | DOTTED
derive instance genericStrokePattern :: Generic StrokePattern _
instance showStrokePattern :: Show StrokePattern where show = genericShow

type AnimationConfig = {
  animationType :: String,
  animationDuration :: String
}

defaultAnimationConfig :: AnimationConfig
defaultAnimationConfig = {
    animationType : "NONE"
  , animationDuration : "0"
}

type MapRouteConfig = {
    sourceSpecialTagIcon :: String
  , destSpecialTagIcon :: String
  , vehicleSizeTagIcon :: Int
  , isAnimation :: Boolean
  , polylineAnimationConfig :: PolylineAnimationConfig
  , autoZoom :: Boolean
  , dashUnit :: Int
  , gapUnit :: Int
}

type Coordinates = Array Paths

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
  city :: Maybe String,
  isSpecialPickUp :: Maybe Boolean
}

type Suggestions = Array
  {
    key :: String,
    value :: Array String
  }

type SuggestionDefinitions = Array
  {
    key :: String,
    value :: {en_us :: String, ta_in :: String, kn_in :: String, hi_in :: String, ml_in :: String, bn_in :: String, te_in :: String}
  }

type UpdateRouteConfig = {
    json :: Locations
  , destMarkerConfig :: MarkerConfig
  , eta :: String
  , srcMarker :: String
  , specialLocation :: MapRouteConfig
  , zoomLevel :: Number
  , autoZoom :: Boolean
  , pureScriptID :: String
  , polylineKey :: String
  , locationName :: String
}

type InAppNotificationPayload = {
  event :: String,
  title :: String,
  message :: String,
  onTapAction :: String,
  action1Text :: String,
  action2Text :: String,
  action1Image :: String,
  action2Image :: String,
  channelId :: String,
  durationInMilliSeconds :: Int,
  showLoader :: Boolean
}

type JOSEventInnerPayload = { -- Added this to make the events payload generic
  param1 :: String,
  param2 :: String
}

josEventInnerPayload :: JOSEventInnerPayload
josEventInnerPayload = {
  param1 : "",
  param2 : ""
}


inAppNotificationPayload :: InAppNotificationPayload
inAppNotificationPayload = {
  event : "in_app_notification",
  title : "",
  message : "",
  onTapAction : "",
  action1Text : "",
  action2Text : "",
  action1Image : "",
  action2Image : "",
  channelId : "channel",
  durationInMilliSeconds : 5000,
  showLoader : false
}

updateRouteConfig :: UpdateRouteConfig
updateRouteConfig = {
    json : {points: []}
  , destMarkerConfig : defaultMarkerConfig
  , eta : ""
  , srcMarker : ""
  , specialLocation : mapRouteConfig
  , zoomLevel : if (os == "IOS") then 19.0 else 17.0
  , autoZoom : true
  , pureScriptID : ""
  , polylineKey : ""
  , locationName : ""
}

mapRouteConfig :: MapRouteConfig
mapRouteConfig = {
      sourceSpecialTagIcon: "",
      destSpecialTagIcon: "",
      vehicleSizeTagIcon: 0,
      isAnimation: false,
      autoZoom : true,
      dashUnit : 1,
      gapUnit : 0,
      polylineAnimationConfig: {
        color: "",
        draw: 0,
        fade: 0,
        delay: 0
      }
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
  , adjustViewBounds : true
}
---------- ################################### DRAW ROUTE CONFIG ################################### ----------

type DrawRouteConfig = {
  routeConfigs :: Array RouteConfig,
  pureScriptID :: String
}

type RouteConfig = {
  locations :: Locations,
  style :: String,
  routeColor :: String,
  isActual :: Boolean,
  routeWidth :: Int,
  routeType :: String,
  mapRouteConfig :: MapRouteConfig,
  startMarkerConfig :: MarkerConfig,
  endMarkerConfig :: MarkerConfig,
  stopMarkerConfigs :: Array MarkerConfig,
  routeKey :: String
}

mkRouteConfig :: Locations -> MarkerConfig -> MarkerConfig -> Maybe (Array MarkerConfig) -> String -> String -> Boolean -> RouteKeysType -> MapRouteConfig -> RouteConfig
mkRouteConfig normalRoute startMarkerConfig endMarkerConfig mbStopMarkerConfigs routeType style isActual key mapRouteConfig =
  routeConfig{
    locations = normalRoute,
    routeType = routeType,
    style = style,
    isActual = isActual,
    mapRouteConfig = mapRouteConfig,
    startMarkerConfig = startMarkerConfig,
    endMarkerConfig = endMarkerConfig,
    stopMarkerConfigs = fromMaybe [] mbStopMarkerConfigs,
    routeKey = show key
  }

mkDrawRouteConfig :: Array RouteConfig -> String -> DrawRouteConfig
mkDrawRouteConfig routeConfigs pureScriptID =
  { routeConfigs : routeConfigs,
    pureScriptID : pureScriptID
  }


routeConfig :: RouteConfig
routeConfig = {
  locations : {points: []},
  style : "LineString",
  routeColor : "#323643",
  isActual : true,
  routeWidth : 8,
  routeType : "",
  mapRouteConfig : defaultMapRouteConfig,
  startMarkerConfig : defaultMarkerConfig,
  endMarkerConfig : defaultMarkerConfig,
  stopMarkerConfigs : [],
  routeKey : show DEFAULT
}

defaultMapRouteConfig :: MapRouteConfig
defaultMapRouteConfig = {
  sourceSpecialTagIcon : "",
  destSpecialTagIcon : "",
  vehicleSizeTagIcon : 90,
  isAnimation : false,
  autoZoom : true,
  dashUnit : 1,
  gapUnit : 0,
  polylineAnimationConfig : getPolylineAnimationConfig
}

data DatePicker = DatePicker String Int Int Int

data TimePicker = TimePicker Int Int String

data CloseAction = SELECTED | DISMISSED | CANCELLED
derive instance genericCloseAction :: Generic CloseAction _
instance showCloseAction :: Show CloseAction where show = genericShow

showDateTimePicker ∷ forall action. (action → Effect Unit) -> (String → Int → Int → Int → String → Int → Int → action) -> Maybe String -> Maybe String -> Boolean -> Boolean -> Aff Unit
showDateTimePicker push action maybePrevDate maybeMaxDate openDatePicker openTimePicker = do
  let
    prevDate =  case maybePrevDate of
                  Nothing -> -1.0
                  Just date -> getEpochTime date
    maxDate =  case maybeMaxDate of
                  Nothing -> -1.0
                  Just date -> getEpochTime date
  if openDatePicker && openTimePicker then do
    datePicker <- makeAff \cb -> datePickerWithTimeout (cb <<< Right) DatePicker 30000 prevDate maxDate $> nonCanceler
    let (DatePicker dateResp year month day) = datePicker
    if dateResp == show SELECTED then do
      timePicker <- makeAff \cb -> timePickerWithTimeout (cb <<< Right) TimePicker prevDate $> nonCanceler
      let (TimePicker hour minute timeResp) = timePicker
      liftEffect $ push $ action dateResp year month day timeResp hour minute
    else
      liftEffect $ push $ action dateResp year month day "" 0 0
  else if openDatePicker then do
    datePicker <- makeAff \cb -> datePickerWithTimeout (cb <<< Right) DatePicker 30000 prevDate maxDate $> nonCanceler
    let (DatePicker dateResp year month day) = datePicker
    liftEffect $ push $ action dateResp year month day "" 0 0
  else if openTimePicker then do
    timePicker <- makeAff \cb -> timePickerWithTimeout (cb <<< Right) TimePicker prevDate $> nonCanceler
    let (TimePicker hour minute timeResp) = timePicker
    liftEffect $ push $ action "" 0 0 0 timeResp hour minute
  else
    liftEffect $ push $ action "" 0 0 0 "" 0 0


renderSlider :: forall action. (action -> Effect Unit) -> (Int -> action) -> SliderConfig -> Effect Unit
renderSlider = runEffectFn3 renderSliderImpl

foreign import updateSliderValue :: UpdateSliderConfig -> Effect Unit

type UpdateSliderConfig = {
  sliderValue :: Int,
  id :: String
}


data RouteKeysType  = DEFAULT | RENTAL | ADVANCED | DELIVERY_DESTINATION

derive instance genericRouteKeysType :: Generic RouteKeysType _
instance showRouteKeysType :: Show RouteKeysType where show = genericShow
instance eqRouteKeysType :: Eq RouteKeysType where eq = genericEq

encodeToBase64Type :: String -> Int ->  Aff (Maybe String)
encodeToBase64Type url delay =  makeAff (\cb -> runEffectFn5 encodeToBase64 url delay (Just) (Nothing) ((cb <<< Right)) $> nonCanceler)

type ShakeListenerConfig = {
  shakeAccelerationThreshold :: Number,
  consecutiveShakeInterval :: Int,
  shakeCountResetTime :: Int
}

defaultShakeListenerConfig :: ShakeListenerConfig
defaultShakeListenerConfig = {
  shakeAccelerationThreshold : 4.0,
  consecutiveShakeInterval : 500,
  shakeCountResetTime : 3000
}

handleLocateOnMapCallback :: String -> ((String -> String -> String -> Effect Unit) -> String -> String -> String -> Effect Unit)
handleLocateOnMapCallback screenName = (\push key lat lon -> do
                  isSuccess <- handleCallback screenName key lat lon push
                  when (not isSuccess) $ runEffectFn4 updateQueue key lat lon (handleLocateOnMapCallback screenName push))
  where
  handleCallback :: String -> String -> String -> String -> (String -> String -> String -> Effect Unit) -> Effect Boolean
  handleCallback screenName key lat lon push = do
    isActive <- isScreenActive "default" screenName
    when isActive $ do push key lat lon
    pure isActive

foreign import triggerReloadApp :: String ->Effect Unit

foreign import rsEncryption :: String -> String
