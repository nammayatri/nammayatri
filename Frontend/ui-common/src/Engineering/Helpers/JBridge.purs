module JBridge where


import Prelude

import Effect (Effect)
-- import Effect.Aff (makeAff, nonCanceler)
import Presto.Core.Flow (Flow)
import Types.App (GlobalState)
import Engineering.Helpers.Commons (liftFlow)
import Screens.Types (InvoiceScreenState)
-- import Common.Types.App (Place)
-- import Types.APIv2 (Address)
-- import Foreign (Foreign)
import Control.Monad.Except (runExcept)
-- import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (decodeJSON)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import Data.Either (Either(..))
import Engineering.Helpers.Commons (screenHeight, screenWidth)
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
-- foreign import readFile'      :: String -> Effect String
foreign import showLoader'      :: String -> Effect Unit
foreign import locateOnMap :: Boolean -> Number -> Number -> Unit

foreign import exitLocateOnMap :: String -> Unit
foreign import shareTextMessage :: String -> String -> Unit
foreign import shareImageMessage :: String -> String -> Unit

foreign import enableMyLocation :: Boolean -> Unit
foreign import isLocationPermissionEnabled :: Unit -> Effect Boolean
-- foreign import getPackageName   :: Effect String
foreign import getVersionCode   :: Effect Int
foreign import getVersionName   :: Effect String
-- foreign import getManufacturerName :: Unit -> String
-- foreign import getAndroidVersion :: Effect Int
-- foreign import showQrCode'      :: String -> String -> Effect Unit
-- foreign import scanQrCode       :: forall action. String -> (action -> Effect Unit) ->  (String -> action) -> Effect Unit
-- foreign import timePicker       :: forall action. (action -> Effect Unit) -> (Int -> Int -> action) -> Effect Unit
foreign import datePicker       :: forall action. String -> (action -> Effect Unit)  -> (Int -> Int -> Int -> action) -> Effect Unit
foreign import setFCMToken :: forall action. (action -> Effect Unit) -> (String  -> action) -> Effect Unit
-- foreign import dateTimePicker :: forall action. (action -> Effect Unit) -> (Int -> action) -> Effect Unit
-- foreign import getNearbyPlaces :: forall action. (action -> Effect Unit) -> (Place -> action) -> Effect Unit
-- foreign import isNetworkAvailable :: Unit -> Boolean
foreign import openUrlInApp  :: String -> Effect Unit
-- foreign import openUrlInMailApp  :: String -> Effect Unit
foreign import addMarker' :: String -> Number -> Number -> Int -> Number -> Number -> Effect Boolean
foreign import removeMarker :: String -> Effect Unit
-- foreign import parseAddress      :: String -> Address
foreign import disableActionEditText :: String -> Unit
foreign import uploadFile :: Unit -> Effect Unit 
foreign import previewImage :: String -> Effect Unit 
foreign import storeCallBackImageUpload :: forall action. (action -> Effect Unit) -> (String -> String -> action) -> Effect Unit
foreign import renderBase64Image :: String -> String -> Effect Unit 
foreign import copyToClipboard :: String -> Unit
foreign import drawRoute :: Locations -> String -> String -> Boolean -> String -> String -> Int -> String -> String -> String -> Effect Unit
foreign import isCoordOnPath :: Locations -> Number -> Number -> Int -> Effect IsLocationOnPath
foreign import updateRoute :: Locations -> String -> String -> Effect Unit
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
foreign import showMap' :: forall action. String -> Boolean -> String -> Number -> (action -> Effect Unit) -> (String -> String -> String -> action) -> Effect Boolean
foreign import mapSnapShot :: forall action. String -> Locations -> String -> Boolean -> (action -> Effect Unit) -> (String -> action) -> Effect Boolean
foreign import getCurrentLatLong  :: Effect Paths
foreign import isLocationEnabled :: Unit -> Effect Boolean
foreign import getCurrentPosition  :: forall action. (action -> Effect Unit) -> (String -> String -> action) -> Effect Unit
foreign import animateCamera :: Number -> Number -> Int -> Effect Unit
-- foreign import moveCamera :: Number -> Number -> Number -> Number -> Effect Unit
foreign import minimizeApp    :: String -> Unit
foreign import toast          :: String -> Unit
foreign import factoryResetApp :: String -> Unit
foreign import startTimerWithTime :: forall action. String -> String -> String -> (action -> Effect Unit) -> (Int -> String -> String -> String-> action)  -> Effect Unit
foreign import hideKeyboardOnNavigation :: Boolean -> Unit
-- foreign import onEvent        :: Foreign -> Effect Unit
-- foreign import _onEventWithCB :: Foreign -> (String -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit
-- -- foreign import getSessionInfo :: { android_id_raw :: String, android_id :: String, os_version :: String, package_name :: String, android_api_level :: String }
foreign import getKeyInSharedPrefKeys :: String -> String
foreign import getKeyInNativeSharedPrefKeys :: String -> String
foreign import setKeyInSharedPrefKeys' :: String -> String -> Effect Unit
foreign import setEnvInNativeSharedPrefKeys' :: String -> String -> Effect Unit
foreign import removeKeysInSharedPrefs :: String -> Unit
foreign import removeKeysInNativeSharedPrefs :: String -> Unit
foreign import toggleLoader' :: Boolean -> Effect Unit
foreign import loaderText' :: String -> String -> Effect Unit
foreign import generatePDF :: InvoiceScreenState -> String -> Unit
foreign import requestKeyboardShow :: String -> Effect Unit
foreign import showDialer          :: String -> Unit
foreign import getAAID :: String -> String
-- -- foreign import removePolyLineById :: Int -> Effect Unit
foreign import removeAllPolylines :: String -> Unit
foreign import currentPosition  :: String -> Unit
foreign import openNavigation  :: Number -> Number -> Number -> Number -> Effect Unit
foreign import stopLocationPollingAPI :: Effect Unit
foreign import startLocationPollingAPI :: Effect Unit
foreign import firebaseLogEvent :: String -> Unit
foreign import firebaseLogEventWithParams :: String -> String -> String -> Effect Unit
foreign import firebaseLogEventWithTwoParams :: String -> String -> String -> String -> String -> Effect Unit
foreign import firebaseScreenNameLog :: String  -> Effect Unit
foreign import firebaseUserID :: String  -> Effect Unit
-- foreign import closeApp       :: String -> Effect Unit
foreign import storeCallBackDriverLocationPermission :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit
foreign import setStoreCallBackPopUp :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit
foreign import deletePopUpCallBack :: String -> Unit 
-- foreign import requestLocationPermissionDriver :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit
foreign import storeCallBackOverlayPermission :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit
foreign import storeCallBackBatteryUsagePermission :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit
foreign import isInternetAvailable :: Unit -> Effect Boolean
foreign import storeCallBackInternetAction :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit

foreign import openWhatsAppSupport :: String -> Effect Unit

foreign import startLottieProcess :: String -> String -> Boolean -> Number -> String -> Unit
foreign import generateSessionToken :: String -> String

foreign import toggleBtnLoader :: String -> Boolean -> Unit
foreign import getBtnLoader :: String -> Boolean
foreign import launchInAppRatingPopup :: Unit -> Effect Unit

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
-- --   res <- oneOf [sendSafetyNetRequest' key nonce, doAff do (makeAff \cb -> do (registerEvent' "onPause" (\_ -> _safefyNetShutdownSafeBrowsing (Left >>> Right >>> cb)) *> pure nonCanceler)) ]
-- --   case res of
-- --     Left "onPause" -> do
-- --       _ <- doAff do (makeAff \cb -> do (registerEvent' "onResume" (\_ -> _safefyNetInitSafeBrowsing (Left >>> Right >>> cb)) *> pure nonCanceler))
-- --       sendSafetyNetRequest key nonce
-- --     _ -> pure res

-- -- sendSafetyNetRequest' :: String -> String -> Flow (Either String String)
-- -- sendSafetyNetRequest' key nonce = doAff do
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

showLoader :: String -> Flow GlobalState Unit
showLoader str = liftFlow (showLoader' str)

-- readFile :: String -> Flow GlobalState String
-- readFile = liftFlow <<< readFile'

-- showQrCode :: String -> String -> Effect Unit
-- showQrCode id str = showQrCode' id str

addMarker :: String -> Number -> Number -> Int -> Number -> Number -> Effect Boolean
addMarker title lat lng markerSize anchorV anchorV1 = (addMarker' title lat lng markerSize anchorV anchorV1)

showMap :: forall action. String -> Boolean -> String -> Number -> (action -> Effect Unit) -> (String -> String -> String -> action) -> Effect Boolean
showMap = showMap' --liftFlow (showMap' id mapType)

toggleLoader :: Boolean -> Flow GlobalState Unit
toggleLoader flag = liftFlow (toggleLoader' flag)

loaderText :: String -> String -> Flow GlobalState Unit
loaderText mainTxt subTxt = liftFlow (loaderText' mainTxt subTxt)

-- loader :: Boolean -> Maybe LoaderMessage -> Flow GlobalState Unit
-- loader flag message = do
--     _ <- pure $ hideKeyboardOnNavigation true
--     case message of
--         Just (LoaderMessage (Title title) (SubTitle subTitle)) -> do
--             _ <- liftFlow (loaderText' (title)  (subTitle))
--             liftFlow (toggleLoader' flag)
--         Nothing -> liftFlow (toggleLoader' flag)

showMarker :: String -> Number -> Number -> Int -> Number -> Number -> Effect Boolean
showMarker title lat lng markerSize anchorV anchorV1 = addMarker title lat lng markerSize anchorV anchorV1


setKeyInSharedPrefKeys :: String -> String -> Flow GlobalState Unit 
setKeyInSharedPrefKeys key val = liftFlow (setKeyInSharedPrefKeys' key val)

setEnvInNativeSharedPrefKeys :: String -> String -> Flow GlobalState Unit 
setEnvInNativeSharedPrefKeys key val = liftFlow (setEnvInNativeSharedPrefKeys' key val)

-- onEventWithCB :: Foreign -> Flow GlobalState (Either String String)
-- onEventWithCB obj = doAff do
--   makeAff (\cb -> do
--     _onEventWithCB obj (Right >>> Right >>> cb) (Left >>> Right >>> cb)
--     pure nonCanceler)

type Locations = {
    journeyCoordinates :: Coordinates
  , points :: Coordinates
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