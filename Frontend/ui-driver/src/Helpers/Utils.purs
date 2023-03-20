{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Helpers.Utils where

-- import Prelude (Unit, bind, discard, identity, pure, show, unit, void, ($), (<#>), (<$>), (<*>), (<<<), (<>), (>>=))
import Effect (Effect)
import Prelude (Unit, bind, pure, discard, unit, void, ($), identity, (<*>), (<#>), (+), (<>))
import Data.Traversable (traverse)
import Effect.Aff (error, killFiber, launchAff, launchAff_)
import Juspay.OTP.Reader (initiateSMSRetriever)
import Juspay.OTP.Reader.Flow as Reader
import Data.Maybe (Maybe(..), fromMaybe)
import Juspay.OTP.Reader as Readers
import Data.Array.NonEmpty (fromArray)
import Effect.Class (liftEffect)
import Screens.Types (AllocationData, YoutubeData)
import Language.Strings (getString)
import Language.Types(STR(..))
import Prelude ((/),(*),(-))
import Data.Array ((!!)) as DA
import Data.String (Pattern(..), split) as DS
import Math
-- import Control.Monad.Except (runExcept)
-- import Data.Array.NonEmpty (fromArray)
-- import Data.DateTime (Date, DateTime)
-- import Data.Either (Either(..))
-- import Data.JSDate (parse, toDateTime)
-- import Data.Maybe (Maybe(..))
-- import Data.Newtype (unwrap, class Newtype)
-- import Data.Traversable (traverse)
-- import Effect (Effect)
-- import Effect.Class (liftEffect)
-- import Foreign.Generic (class Decode, class Encode,decodeJSON, encodeJSON)
-- import Effect.Aff (error, killFiber, launchAff, launchAff_)
-- import Effect.Console (logShow)
-- import Effect.Timer (setTimeout, TimeoutId)
-- import Engineering.Helpers.Commons (flowRunner, liftFlow)
-- import Juspay.OTP.Reader (initiateSMSRetriever)
-- import Juspay.OTP.Reader.Flow as Reader
-- import Juspay.OTP.Reader as Readers
-- import Presto.Core.Flow (Flow)
-- import Types.App (GlobalState)
-- foreign import getTimer :: forall action. String -> String ->String -> (action -> Effect Unit)  -> (String -> action)  -> Effect Unit
-- foreign import get15sTimer :: forall action. (action -> Effect Unit) -> (String -> action)  -> Effect Unit
-- foreign import get5sTimer :: forall action. (action -> Effect Unit) -> (String -> action)  -> Effect Unit
-- foreign import get10sTimer :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit
-- -- foreign import getCurrentLatLong'  :: Effect String



-- -- foreign import readFile'      :: String -> Effect String
-- foreign import eval'          :: String -> Effect Unit
-- -- foreign import closeApp       :: String -> Unit
-- -- foreign import minimizeApp    :: String -> Unit
-- -- foreign import toast          :: String -> Unit
-- -- foreign import requestReadAndSendSmsPermission :: Boolean -> Unit
-- -- foreign import hideKeyboardOnNavigation :: Boolean -> Unit
-- -- foreign import showAuthScreen :: String -> String-> Unit
-- -- foreign import setF           :: forall a. Foreign -> String -> a -> Foreign
-- -- foreign import onEvent        :: Foreign -> Effect Unit
-- foreign import exitApp'       :: Int -> String -> Unit
-- -- foreign import _onEventWithCB :: Foreign -> (String -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit
-- foreign import getISOTime     :: Effect String
-- foreign import _getTime       :: Effect String
foreign import shuffle :: forall a. Array a -> Array a
foreign import generateUniqueId :: Unit -> String 
foreign import storeCallBackTime :: forall action. (action -> Effect Unit) -> (String -> String -> String -> action)  -> Effect Unit
foreign import getTime :: Unit -> Int
foreign import countDown :: forall action. Int -> String -> (action -> Effect Unit) -> (Int -> String -> String -> String-> action)  -> Effect Unit
-- -- foreign import getSessionInfo :: { android_id_raw :: String, android_id :: String, os_version :: String, package_name :: String, android_api_level :: String }
-- -- foreign import getKeyInSharedPrefKeys :: String -> String
-- -- foreign import setKeyInSharedPrefKeys' :: String -> String -> Effect Unit
-- -- foreign import removeKeysInSharedPrefs :: String -> Unit
-- -- foreign import createNonce    :: Int -> String
-- -- foreign import startGodel'    :: Foreign -> (String -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit
-- foreign import parseURL'      :: String -> Effect String
-- -- foreign import parseAddress      :: String -> Address
-- foreign import _getSessionId  :: Effect String
foreign import hideSplash :: Effect Unit
foreign import startTimer :: forall action. Int -> (action -> Effect Unit) -> (String -> action) -> Effect Unit
-- -- foreign import startLocationPollingAPI :: String -> String -> String -> String -> String -> Int -> Effect Unit
-- -- foreign import stopLocationPollingAPI :: Effect Unit

-- foreign import subscribeEvent' :: forall a b. String -> (a -> Effect b) -> Effect Unit
-- foreign import subscribeEventCB' :: forall a b. String -> (a -> (String -> Effect Unit) -> Effect b) -> Effect Unit
-- -- foreign import registerEvent' :: forall a b. String -> (a -> Effect b) -> Effect Unit
-- -- foreign import getExitFlows    :: forall a. Effect (Array (Flow GlobalState a))
-- foreign import addToExitFlows' :: forall a. (Flow GlobalState a) -> Effect Unit
-- -- foreign import showLoader'      :: String -> Effect Unit
-- -- foreign import hideLoader'      :: Effect Unit
-- -- foreign import showQrCode'      :: String -> String -> Effect Unit
-- -- foreign import scanQrCode       :: forall action. String -> (action -> Effect Unit) ->  (String -> action) -> Effect Unit
-- -- foreign import removePolyLine   :: String -> Effect Unit

-- -- foreign import timePicker       :: forall action. (action -> Effect Unit) -> (Int -> Int -> action) -> Effect Unit
-- -- foreign import datePicker       :: forall action. (action -> Effect Unit) -> (Int -> Int -> Int -> action) -> Effect Unit
-- -- foreign import requestLocation  :: Unit -> Effect Unit
-- -- foreign import ecBaseUrl :: String
-- --foreign import getSessionInfo :: String
-- -- foreign import getSinValue :: Number ->  Number
-- -- foreign import getCosValue :: Number ->  Number
-- -- foreign import getEnvironment :: Unit -> String
-- -- foreign import getPan :: String -> String
-- foreign import getMobileNo :: String -> String

-- -- foreign import addMarker' :: String -> Number -> Number -> Effect Boolean
-- -- foreign import removeMarker :: String -> Effect Unit
-- -- foreign import drawRoute :: String -> String -> Locations -> Effect Unit
-- -- foreign import drawActualRoute :: String -> String -> Locations -> Effect Int
-- -- foreign import showAndDrawRoute :: String -> String -> String -> Locations -> Effect Int
-- -- foreign import removePolyLineById :: Int -> Effect Unit
-- -- foreign import removeAllPolylines :: Effect Unit
-- -- foreign import addMarkers :: Markers -> Effect Unit
-- foreign import getuuid :: String -> String
-- foreign import base64Encode :: String -> String

-- -- foreign import animateCamera :: String -> String -> Int -> Effect Unit
-- -- foreign import moveCamera :: String -> String -> String -> String -> Effect Unit
-- -- foreign import addMarkerToCurrentLocation :: String -> String -> Effect Unit
-- foreign import showDialer          :: String -> Unit

-- foreign import calculateDist  :: Number -> Number -> Number -> Number -> Boolean

-- -- foreign import loadSVGImage :: String -> String -> Boolean -> Effect Unit
-- -- foreign import loadGIFImage :: String -> String -> Boolean -> Effect Unit
-- -- foreign import toggleLoader' :: Boolean -> Effect Unit
-- foreign import isMobile :: Boolean
-- -- foreign import loaderText' :: String -> String -> Effect Unit
-- -- foreign import getNearbyPlaces :: forall action. (action -> Effect Unit) -> (Place -> action) -> Effect Unit
-- -- foreign import dateISO :: String -> String

-- foreign import hasDriver :: String -> Boolean
-- foreign import driverRating :: String -> String
-- foreign import driverInfo :: String -> String
-- foreign import otp :: String -> String
-- foreign import vehicleInfo :: String -> String

-- foreign import vehicleNumber :: String -> String
-- foreign import vehicleType :: String -> String
-- foreign import modelNumber :: String -> String
-- foreign import vehicleId :: String -> String
-- foreign import vehicleColorAndModal :: String -> String

-- -- foreign import setFCMToken :: String -> Unit

foreign import convertKmToM :: String -> String
-- foreign import convertMToKm :: Number -> String
-- foreign import calculateETA :: Number -> String
-- -- foreign import dateTimePicker :: forall action. (action -> Effect Unit) -> (Int -> action) -> Effect Unit
-- foreign import epochToDate :: Int -> Date
-- foreign import epochToDateString :: Int ->String
-- foreign import formatDate :: Date -> String -> String
-- foreign import formatDateFix :: String -> String -> String
-- foreign import getCurrentUTC :: String -> String
-- foreign import getNextUTC :: Int -> String -> String
-- foreign import getCurrMinUTC :: Int -> String
-- foreign import convertISTtoUTC :: String -> String
foreign import convertUTCtoISC :: String -> String -> String
foreign import differenceBetweenTwoUTC :: String -> String -> Int
-- foreign import isUTCTimeValid :: String -> String -> Boolean
-- foreign import decodeDriverInfo :: String -> String
-- foreign import decodeDriver :: String -> String
-- foreign import decodeDriverInfoTransporter :: String -> String
-- foreign import decodeAgencyInfo :: String -> String
-- foreign import decodeAgencyName :: String -> String
-- foreign import decodeAgencyPhoneNo :: String -> String
-- foreign import completedRides :: String -> Int
-- foreign import totalAgencies :: String -> Int
-- foreign import declinedAgencies :: String -> Int
-- foreign import acceptedAgencies :: String -> Int
-- foreign import decodeDriverNo :: String -> String
-- foreign import decodeDriverNoTransporter :: String -> String
-- foreign import decodeVehicleIdTransporter :: String -> String
-- foreign import decodeDriverIdTransporter :: String -> String
-- foreign import decodeAddress :: String -> String
-- foreign import stringToDate :: String -> Date
-- -- foreign import showSnackBar :: forall action. String -> String -> (action -> Effect Unit) -> action -> Effect Unit
-- foreign import getAge :: String -> String
foreign import clearTimer :: String -> Unit
foreign import clearPopUpTimer :: String -> Unit 
foreign import clearAllTimer :: String -> Unit 
foreign import toString :: forall a. a-> String
foreign import toInt :: forall a. a -> String
foreign import setRefreshing :: String -> Boolean -> Unit
foreign import setEnabled :: String -> Boolean -> Unit
foreign import decodeErrorCode :: String -> String
foreign import decodeErrorMessage :: String -> String
foreign import storeCallBackForNotification :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit
foreign import secondsLeft :: String -> Int
foreign import setText' :: String -> String -> Effect Unit
foreign import parseFloat :: forall a. a -> Int -> String
foreign import objectToAllocationType :: String -> AllocationData
foreign import getcurrentdate :: String -> String
foreign import launchAppSettings :: Unit -> Effect Unit
foreign import setYoutubePlayer :: YoutubeData -> String -> String -> Unit
foreign import getTimeStampString :: String -> String
foreign import addMediaPlayer :: String -> String -> Effect Unit
foreign import removeMediaPlayer :: String -> Effect Unit
foreign import getVideoID :: String -> String
foreign import getImageUrl :: String -> String
-- foreign import decodeErrorPayload :: String -> String
-- foreign import debounceFunction :: forall action. Int -> (action -> Effect Unit) -> (String -> action) -> Effect Unit
-- foreign import updateInputString :: String -> Unit

-- -- ####### MAP FFI ######## -----
-- -- foreign import getCurrentPosition  :: forall action. (action -> Effect Unit) -> (String -> String -> action) -> Effect Unit
-- -- foreign import showMap' :: forall action. String -> String -> (action -> Effect Unit) -> action -> Effect Boolean
-- -- foreign import isLocationPermissionEnabled :: Unit -> Effect Boolean
-- -- foreign import isLocationEnabled :: Unit -> Effect Boolean


-- infixl 5 extract as <.>

-- foreign import saveScreenState' :: String -> String -> Effect Unit
-- foreign import fetchScreenState' :: String -> (String -> Maybe String) -> Maybe String -> Effect (Maybe String)

-- -- foreign import isNetworkAvailable :: Unit -> Boolean
-- -- foreign import goToUrl  :: String -> Effect Unit
-- -- foreign import openNavigation  :: Number -> Number -> Number -> Number -> Effect Unit
-- -- foreign import openUrlInApp  :: String -> Effect Unit
foreign import currentPosition  :: String -> Effect Unit
-- -- foreign import getVersionCode :: String -> Int
-- -- foreign import getVersionName :: String -> String

-- -- showMap :: forall action. String -> String -> (action -> Effect Unit) -> action -> Effect Boolean
-- -- showMap id mapType callback action = (showMap' id mapType callback action) --liftFlow (showMap' id mapType)

-- -- Give ImageView definite height & width as in case of MATCH_PARENT & WRAP_CONTENT function may not be able to handle it
-- -- showQrCode :: String -> String -> Effect Unit
-- -- showQrCode id str = showQrCode' id str

-- -- toggleLoader :: Boolean -> Flow GlobalState Unit
-- -- toggleLoader flag = liftFlow (toggleLoader' flag)

-- -- loaderText :: String -> String -> Flow GlobalState Unit
-- -- loaderText mainTxt subTxt = liftFlow (loaderText' mainTxt subTxt)

-- data Title = Title String
-- data SubTitle = SubTitle String

-- data LoaderMessage = LoaderMessage Title SubTitle

-- -- loader :: Boolean -> Maybe LoaderMessage -> Flow GlobalState Unit
-- -- loader flag message = do
-- --     _ <- pure $ hideKeyboardOnNavigation true
-- --     case message of
-- --         Just (LoaderMessage (Title title) (SubTitle subTitle)) -> do
-- --             _ <- liftFlow (loaderText' (title)  (subTitle))
-- --             liftFlow (toggleLoader' flag)
-- --         Nothing -> liftFlow (toggleLoader' flag)

-- -- addMarker :: String -> Number -> Number -> Effect Boolean
-- -- addMarker title lat lng = (addMarker' title lat lng) --liftFlow (addMarker' title lat lng)

-- -- showMarker :: String -> Number -> Number -> Effect Boolean
-- -- showMarker title lat lng = addMarker title lat lng

-- -- showLoader :: String -> Flow GlobalState Unit
-- -- showLoader str = liftFlow (showLoader' str)

-- -- hideLoader :: String -> Flow GlobalState Unit
-- -- hideLoader _ = liftFlow hideLoader'

-- -- setKeyInSharedPrefKeys :: String -> String -> Flow GlobalState Unit 
-- -- setKeyInSharedPrefKeys key val = liftFlow (setKeyInSharedPrefKeys' key val)


-- -- getCurrentLatLong :: Flow GlobalState LocationLatLong
-- -- getCurrentLatLong = do
-- --   str <- liftFlow $ getCurrentLatLong'
-- --   case (runExcept $ decodeJSON $ str) of
-- --     Right (a :: LocationLatLong) -> pure a
-- --     Left err -> pure $ LocationLatLong{"lat": "", "long": ""}

-- -- readFile :: String -> Flow GlobalState String
-- -- readFile = liftFlow <<< readFile'

-- eval :: String -> Flow GlobalState Unit
-- eval = liftFlow <<< eval'

-- getDateTime :: Flow GlobalState (Maybe DateTime)
-- getDateTime = liftFlow $ toDateTime <$> (getISOTime >>= parse)

-- -- onEventWithCB :: Foreign -> Flow GlobalState (Either String String)
-- -- onEventWithCB obj = doAff do
-- --   makeAff (\cb -> do
-- --     _onEventWithCB obj (Right >>> Right >>> cb) (Left >>> Right >>> cb)
-- --     pure nonCanceler)

-- extract :: forall t a b. Newtype t a => t -> (a -> b) -> b
-- extract a b = b $ unwrap a

-- -- startGodel :: Foreign -> Flow GlobalState String
-- -- startGodel p = doAff do
-- --   makeAff \cb -> startGodel' p (Left >>> Right >>> cb) (Right >>> Right >>> cb) *> pure nonCanceler
-- --   >>= either throwErr pure

-- subscribeEvent :: forall a b. String -> (a -> Flow GlobalState b) -> Flow GlobalState Unit
-- subscribeEvent event flow = liftFlow $ subscribeEvent' event (\x -> launchAff_ do flowRunner $ flow x)

-- subscribeEventCB :: forall a b. String -> (a -> (String -> Effect Unit) -> Flow GlobalState b) -> Flow GlobalState Unit
-- subscribeEventCB event flow = liftFlow $ subscribeEventCB' event (\x y -> launchAff_ do flowRunner $ flow x y)

-- exitApp :: Int -> String -> Flow GlobalState Unit
-- exitApp code status = pure $ exitApp' code status

-- parseURL :: String -> Flow GlobalState String
-- parseURL = liftFlow <<< parseURL'

-- addToExitFlows :: forall a. (Flow GlobalState a) -> Flow GlobalState Unit
-- addToExitFlows = liftFlow <<< addToExitFlows'

-- getSessionId :: Flow GlobalState String
-- getSessionId = liftFlow _getSessionId


-- wait :: Int -> Flow GlobalState (Effect Unit -> Effect TimeoutId)
-- wait waitTime = pure $ setTimeout waitTime

-- saveScreenState :: forall state. Encode state => Decode state => String -> state -> Flow GlobalState Unit
-- saveScreenState screenName state = liftFlow $ saveScreenState' screenName $ encodeJSON state

-- fetchScreenState :: forall state. Decode state => String -> Flow GlobalState (Maybe state)
-- fetchScreenState screenName = do
--     (maybeEncodedState :: Maybe String) <- liftFlow $ fetchScreenState' screenName Just Nothing
--     case maybeEncodedState of
--             Just encodedState -> do
--                 case runExcept (decodeJSON encodedState) of
--                     Right state -> pure $ Just state
--                     Left err -> do
--                         _ <- liftFlow (logShow $ "fetchScreenState: Error while decoding " <> (show err))
--                         pure Nothing
--             Nothing -> pure Nothing

otpRule :: Reader.OtpRule
otpRule = Reader.OtpRule {
  matches : {
    sender : [],
    message : "Your OTP for login to Yatri App is"
  },
  otp : "\\d{4}",
  group : Nothing
}

startOtpReciever :: forall action. (String -> action) -> (action -> Effect Unit) -> Effect (Effect Unit)
startOtpReciever action push = do
  fiber <- launchAff $ do
    otpListener <- traverse Readers.getOtpListener $ fromArray [ Readers.smsRetriever ] 
    _ <- traverse identity $ (otpListener <#> _.setOtpRules) <*> Just [otpRule]
    message <- traverse identity $ (otpListener <#> _.getNextOtp)
    case message of
      Just (Readers.Otp val _ _) -> liftEffect $ push $ action val
      _ -> pure unit
    void $ initiateSMSRetriever
    liftEffect $ startOtpReciever action push
  pure $ launchAff_ $ killFiber (error "Failed to Cancel") fiber


getCorrespondingErrorMessage :: String -> String
getCorrespondingErrorMessage errorCode = case errorCode of
  "IMAGE_VALIDATION_FAILED" -> (getString IMAGE_VALIDATION_FAILED)
  "IMAGE_NOT_READABLE" -> (getString IMAGE_NOT_READABLE)
  "IMAGE_LOW_QUALITY" -> (getString IMAGE_LOW_QUALITY)
  "IMAGE_INVALID_TYPE" -> (getString IMAGE_INVALID_TYPE)
  "IMAGE_DOCUMENT_NUMBER_MISMATCH" -> (getString IMAGE_DOCUMENT_NUMBER_MISMATCH)
  "IMAGE_EXTRACTION_FAILED" -> (getString IMAGE_EXTRACTION_FAILED)
  "IMAGE_NOT_FOUND" -> (getString IMAGE_NOT_FOUND)
  "IMAGE_NOT_VALID" -> (getString IMAGE_NOT_VALID)
  "DRIVER_ALREADY_LINKED" -> (getString DRIVER_ALREADY_LINKED)
  "DL_ALREADY_UPDATED" -> (getString DL_ALREADY_UPDATED)
  "DL_ALREADY_LINKED"  -> (getString DL_ALREADY_LINKED)
  "RC_ALREADY_LINKED" -> (getString RC_ALREADY_LINKED)
  "RC_ALREADY_UPDATED" -> (getString RC_ALREADY_UPDATED)
  "UNPROCESSABLE_ENTITY" -> (getString PLEASE_CHECK_FOR_IMAGE_IF_VALID_DOCUMENT_IMAGE_OR_NOT)
  _                      -> (getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER)

-- -- type Locations = {
-- --     paths :: Array Paths
-- -- }


-- -- type Paths = {
-- --     points :: Points
-- -- }

-- -- type Points = {
-- --     type :: String
-- -- ,   coordinates :: Array Point
-- -- }

-- -- type Point = Array Number

-- -- type Markers = {
-- --     markerObject :: Array MarkerObject
-- -- }

-- -- type MarkerObject = {
-- --     type :: String,
-- --     title :: String,
-- --     coordinates :: Array Number
-- -- }

-- -- newtype LocationLatLong = LocationLatLong
-- --   { lat :: String
-- --   , long :: String
-- --   }

-- -- derive instance genericLocationLatLong :: Generic LocationLatLong _
-- -- derive instance newtypeLocationLatLong :: Newtype LocationLatLong _
-- -- instance encodeLocationLatLong :: Encode LocationLatLong where encode = defaultEncode
-- -- instance decodeLocationLatLong :: Decode LocationLatLong where decode = defaultDecode

getDistanceBwCordinates :: Number -> Number -> Number -> Number -> Number
getDistanceBwCordinates lat1 long1 lat2 long2 = do
    let latPoint1 = toRad (lat1)
    let lngPoint1 = toRad (long1)
    let latPoint2 = toRad (lat2)
    let lngPoint2 = toRad (long2)
    let dlong = toRad (long2 - (long1))
    let lati1 = toRad (lat1)
    let lati2 = toRad (lat2)
    let dist = sin ((latPoint2 - latPoint1) / 2.0 ) * sin ((latPoint2 - latPoint1) / 2.0 ) + cos(latPoint1) * cos(latPoint2) * sin ((lngPoint2 - lngPoint1) / 2.0 ) * sin ((lngPoint2 - lngPoint1) / 2.0 )
    let dist1 = (2.0 * 6371.0 * asin ( sqrt dist))
    dist1

toRad :: Number -> Number
toRad n = (n * pi) / 180.0
