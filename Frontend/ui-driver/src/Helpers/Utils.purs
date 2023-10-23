{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Helpers.Utils
    ( module Helpers.Utils
    , module ReExport
    ) where

-- import Prelude (Unit, bind, discard, identity, pure, show, unit, void, ($), (<#>), (<$>), (<*>), (<<<), (<>), (>>=))
import Screens.Types (AllocationData, DisabilityType(..))
import Language.Strings (getString)
import Language.Types(STR(..))
import Data.Array ((!!), elemIndex) as DA
import Data.String (Pattern(..), split) as DS
import Data.Number (pi, sin, cos, asin, sqrt)

import MerchantConfig.Utils

import Common.Types.App (LazyCheck(..), CalendarDate, CalendarWeek, PaymentStatus(..))
import Types.App (FlowBT)
import Control.Monad.Except (runExcept)
import Data.Array ((!!), fold) as DA
import Data.Array.NonEmpty (fromArray)
import Data.Either (Either(..), hush)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Maybe (Maybe(..))
import Data.Number (pi, sin, cos, asin, sqrt)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split) as DS
import Data.String as DS
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff (..), error, killFiber, launchAff, launchAff_, makeAff, nonCanceler, Fiber)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (parseFloat, setText, getCurrentUTC, getPastDays, getPastWeeks) as ReExport
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode, decode)
import Juspay.OTP.Reader (initiateSMSRetriever)
import Juspay.OTP.Reader as Readers
import Juspay.OTP.Reader.Flow as Reader
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (class EuclideanRing, Unit, bind, discard, identity, pure, unit, void, ($), (+), (<#>), (<*>), (<>), (*>), (>>>), ($>), (/=), (&&), (<=), show, (>=), (>),(<))
import Prelude (class Eq, class Show, (<<<))
import Prelude (map, (*), (-), (/), (==))
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode)
import Data.Function.Uncurried (Fn4(..), Fn3(..), runFn4, runFn3)
import Effect.Uncurried (EffectFn1(..),EffectFn5(..), mkEffectFn1, mkEffectFn4, runEffectFn5)
import Common.Types.App (OptionButtonList)
import Engineering.Helpers.Commons (parseFloat, setText, convertUTCtoISC, getCurrentUTC) as ReExport
import Services.API(PaymentPagePayload)
import Presto.Core.Types.Language.Flow (Flow, doAff, loadS)
import Control.Monad.Except.Trans (lift)
import Foreign.Generic (Foreign, decodeJSON, encodeJSON)
import Data.Newtype (class Newtype)
import Presto.Core.Types.API (class StandardEncode, standardEncode)
import Services.API (PaymentPagePayload, PromotionPopupConfig)
import Storage (KeyStore) 
import JBridge (getCurrentPositionWithTimeout, firebaseLogEventWithParams, translateStringWithTimeout)
import Effect.Uncurried(EffectFn1, EffectFn4, EffectFn3,runEffectFn3)
import Storage (KeyStore(..), isOnFreeTrial, getValueToLocalNativeStore)
import Styles.Colors as Color
import Screens.Types (UpiApps(..), LocalStoreSubscriptionInfo)
import Data.Int (fromString, even, fromNumber)
import Data.Number.Format (fixed, toStringWith)
import Data.Function.Uncurried (Fn1)


foreign import shuffle :: forall a. Array a -> Array a
foreign import generateUniqueId :: Unit -> String
foreign import storeCallBackTime :: forall action. (action -> Effect Unit) -> (String -> String -> String -> action)  -> Effect Unit
foreign import getTime :: Unit -> Int
foreign import countDown :: forall action. Int -> String -> (action -> Effect Unit) -> (Int -> String -> String -> String-> action)  -> Effect Unit
foreign import hideSplash :: Effect Unit
foreign import startTimer :: forall action. Int -> Boolean -> (action -> Effect Unit) -> (String -> action) -> Effect Unit
foreign import convertKmToM :: String -> String
foreign import differenceBetweenTwoUTC :: String -> String -> Int
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
foreign import objectToAllocationType :: String -> AllocationData
foreign import getcurrentdate :: String -> String
foreign import getDatebyCount :: Int -> String
foreign import launchAppSettings :: Unit -> Effect Unit
foreign import getTimeStampString :: String -> String
foreign import addMediaPlayer :: String -> String -> Effect Unit
foreign import saveAudioFile :: String -> Effect String
foreign import clearFocus :: String -> Effect Unit
foreign import uploadMultiPartData :: String -> String -> String -> Effect String
foreign import startAudioRecording :: String -> Effect Boolean
foreign import stopAudioRecording :: String -> Effect String
foreign import renderBase64ImageFile :: String -> String -> Boolean -> String ->  Effect Unit
foreign import removeMediaPlayer :: String -> Effect Unit
foreign import getVideoID :: String -> String
foreign import getImageUrl :: String -> String
foreign import parseNumber :: Int -> String
foreign import getPixels :: Fn1 String Number
foreign import getDeviceDefaultDensity ::Fn1 String Number
foreign import isYesterday :: String -> Boolean

-- -- ####### MAP FFI ######## -----
foreign import currentPosition  :: String -> Effect Unit
foreign import getRideLabelConfig :: forall f a. Fn4 (f -> Maybe f) (Maybe f) String String (Maybe String)
foreign import getPeriod :: String -> Period
foreign import clampNumber :: Number -> Number -> Int -> Int
foreign import getPopupObject :: forall f a. Fn3 (f -> Maybe f) (Maybe f) String (Maybe PromotionPopupConfig)
foreign import countDownInMinutes :: forall action. EffectFn3 Int (action -> Effect Unit) (String -> String -> Int -> action) Unit
foreign import istToUtcDate :: String -> String

foreign import preFetch :: Effect (Array RenewFile)

foreign import renewFile :: EffectFn3 String String (AffSuccess Boolean) Unit

foreign import getDateAfterNDays :: Int -> String
foreign import  downloadQR  :: String -> Effect Unit

foreign import _generateQRCode :: EffectFn5 String String Int Int (AffSuccess String) Unit

generateQR:: EffectFn4 String String Int Int Unit
generateQR  = mkEffectFn4 \qrString viewId size margin ->  launchAff_  $ void $ makeAff $
  \cb ->
    (runEffectFn5 _generateQRCode qrString viewId size margin (Right >>> cb))
    $> nonCanceler

getPopupObjectFromSharedPrefs :: KeyStore -> Maybe PromotionPopupConfig
getPopupObjectFromSharedPrefs key = runFn3 getPopupObject Just Nothing (show key) 

type Period
  = { period :: Int
    , periodType :: String
    }

type RenewFile = {
  filePath :: String ,
  location :: String
}

otpRule :: Reader.OtpRule
otpRule = Reader.OtpRule {
  matches : {
    sender : [],
    message : (getValueFromConfig "OTP_MESSAGE_REGEX")
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

capitalizeFirstChar :: String -> String
capitalizeFirstChar inputStr =
  let splitedArray = DS.split (DS.Pattern " ") (inputStr)
      output = map (\item -> (DS.toUpper (DS.take 1 item)) <> (DS.toLower (DS.drop 1 item))) splitedArray
    in DS.joinWith " " output

getDowngradeOptions :: String -> Array String
getDowngradeOptions variant = case variant of
                                "TAXI"  -> []
                                "SUV"   -> ["SEDAN", "HATCHBACK"]
                                "SEDAN" -> ["TAXI", "HATCHBACK"] 
                                _       -> ["TAXI"]

getVehicleType :: String -> String
getVehicleType vehicleType =
  case vehicleType of
    "SEDAN" -> (getString SEDAN )
    "SUV"   -> (getString SUV)
    "HATCHBACK" -> (getString HATCHBACK)
    "AUTO_RICKSHAW" -> (getString AUTO_RICKSHAW)
    "TAXI" -> (getString TAXI)
    "TAXI_PLUS" -> (getString TAXI_PLUS)
    _ -> ""

getRideLabelData :: String -> Maybe String -> String
getRideLabelData prop tag = do
  case getRequiredTag prop tag of
    Nothing -> ""
    Just tag' -> tag'

getRequiredTag :: String -> Maybe String -> Maybe String
getRequiredTag prop tag = do
  case tag of
    Just "Accessibility" -> case (runFn4 getRideLabelConfig Just Nothing prop ("Accessibility")) of
                                Nothing -> Nothing
                                Just val -> Just val
                                
    Just "GOTO" -> case (runFn4 getRideLabelConfig Just Nothing prop "GOTO") of
                                Nothing -> Nothing
                                Just val -> Just val
    Just tag' -> do
        let arr = DS.split (DS.Pattern "_") tag'
        let pickup = fromMaybe "" (arr DA.!! 0)
        let drop = fromMaybe "" (arr DA.!! 1)
        let priority = fromMaybe "" (arr DA.!! 2)
        case priority of
          "PriorityPickup" -> case (runFn4 getRideLabelConfig Just Nothing prop (pickup <> "_Pickup")) of
                                Nothing -> Nothing
                                Just val -> Just val
          "PriorityDrop" -> case (runFn4 getRideLabelConfig Just Nothing prop (drop <> "_Drop")) of
                                Nothing -> Nothing
                                Just val -> Just val
          _ -> Nothing
    _ -> Nothing

getGenderIndex :: String -> Array OptionButtonList -> Maybe Int
getGenderIndex req arr = do
  let reqArray = map(\ele -> ele.reasonCode) arr
      reqIndex = DA.elemIndex req reqArray
  reqIndex

getMerchantVehicleSize :: Unit -> Int
getMerchantVehicleSize unit = 
  case getMerchant FunctionCall of 
    _ -> 90

getAssetStoreLink :: LazyCheck -> String
getAssetStoreLink lazy = case (getMerchant lazy) of
  NAMMAYATRI -> "https://assets.juspay.in/beckn/nammayatri/driver/images/"
  YATRISATHI -> "https://assets.juspay.in/beckn/jatrisaathi/driver/images/"
  YATRI -> "https://assets.juspay.in/beckn/yatri/driver/images/"
  MOBILITY_PM -> "https://assets.juspay.in/beckn/mobilitypaytm/driver/"
  PASSCULTURE -> "https://assets.juspay.in/beckn/passculture/driver/images"
  MOBILITY_RS -> "https://assets.juspay.in/beckn/passculture/driver/images"

getAssetsBaseUrl :: LazyCheck -> String
getAssetsBaseUrl lazy = case (getMerchant lazy) of
  NAMMAYATRI -> "https://assets.juspay.in/beckn/nammayatri/driver/"
  YATRISATHI -> "https://assets.juspay.in/beckn/jatrisaathi/driver/"
  YATRI -> "https://assets.juspay.in/beckn/yatri/driver/"
  MOBILITY_PM -> "https://assets.juspay.in/beckn/mobilitypaytm/"
  PASSCULTURE -> "https://assets.juspay.in/beckn/passculture/driver"
  MOBILITY_RS -> "https://assets.juspay.in/beckn/passculture/driver"

getCommonAssetStoreLink :: LazyCheck -> String
getCommonAssetStoreLink lazy = case (getMerchant lazy) of
  NAMMAYATRI -> "https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/"
  YATRISATHI -> "https://assets.juspay.in/beckn/jatrisaathi/jatrisaathicommon/images/"
  YATRI -> "https://assets.juspay.in/beckn/yatri/yatricommon/images/"
  MOBILITY_PM -> "https://assets.juspay.in/beckn/mobilitypaytm/mobilitypaytmcommon/"
  PASSCULTURE -> "https://assets.juspay.in/beckn/passculture/passculturecommon/"
  MOBILITY_RS -> "https://assets.juspay.in/beckn/passculture/passculturecommon/"

type AffSuccess s = (s -> Effect Unit)
type MicroAPPInvokeSignature = String -> (AffSuccess String) ->  Effect Unit


foreign import startPP :: MicroAPPInvokeSignature

foreign import initiatePP :: EffectFn1 Unit Unit

foreign import getAvailableUpiApps :: EffectFn1 ((Array UpiApps) -> Effect Unit) Unit

foreign import checkPPInitiateStatus :: EffectFn1 (Unit -> Effect Unit) Unit

foreign import consumeBP :: EffectFn1 Unit Unit

foreign import isDateGreaterThan :: String -> Boolean

paymentPageUI :: PaymentPagePayload -> FlowBT String String
paymentPageUI payload = lift $ lift $ doAff $ makeAff (\cb -> (startPP (encodeJSON payload) (Right >>> cb) ) *> pure nonCanceler)

getNegotiationUnit :: String -> String
getNegotiationUnit varient = case varient of
  "AUTO_RICKSHAW" -> "10"
  _ -> "20"
  
getValueBtwRange :: forall a. EuclideanRing a => a -> a -> a -> a -> a -> a
getValueBtwRange  x  in_min  in_max  out_min  out_max = (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min 

data LatLon = LatLon String String

data Translation = Translation String

getCurrentLocation :: Number -> Number -> Number -> Number -> Int -> FlowBT String LatLon
getCurrentLocation currentLat currentLon defaultLat defaultLon timeOut = do
  (LatLon startRideCurrentLat startRideCurrentLong) <- (lift $ lift $ doAff $ makeAff \cb -> getCurrentPositionWithTimeout (cb <<< Right) LatLon timeOut $> nonCanceler)
  if(startRideCurrentLat /= "0.0" && startRideCurrentLong /= "0.0") then
    pure (LatLon startRideCurrentLat startRideCurrentLong)
  else do
    if defaultLat /= 0.0 && defaultLon /= 0.0 && currentLat /= 0.0 && currentLon /= 0.0 then do
      let distanceDiff = (getDistanceBwCordinates currentLat currentLon defaultLat defaultLon)
          rideLat = show $ if distanceDiff <= 0.10 then  currentLat else defaultLat
          rideLong = show $ if distanceDiff <= 0.10 then currentLon else defaultLon
      pure (LatLon rideLat rideLong)
      else do
        pure (LatLon (show defaultLat) (show defaultLon))

translateString :: String -> Int -> FlowBT String String 
translateString toTranslate timeOut = do
  (Translation translation) <- (lift $ lift $ doAff $ makeAff \cb -> translateStringWithTimeout (cb <<< Right) Translation timeOut toTranslate $> nonCanceler )
  pure $ ( translation)


getRideTypeColor :: Maybe String -> String
getRideTypeColor variant = case getCategorizedVariant variant of
                              "AC Taxi" -> Color.blue800
                              "Non AC"  -> Color.orange900
                              _         -> Color.black800

getCategorizedVariant :: Maybe String -> String
getCategorizedVariant variant = case variant of
  Just var -> case (getMerchant FunctionCall) of
    YATRISATHI -> case var of
      "SEDAN"  -> "AC Taxi"
      "HATCHBACK"  -> "AC Taxi"
      "TAXI_PLUS"  -> "AC Taxi"
      "SUV" -> "AC Taxi"
      _ -> "Non AC"
    _ -> case var of
      "SEDAN"  -> "Sedan"
      "HATCHBACK"  -> "Hatchback"
      "TAXI_PLUS"  -> "AC Taxi"
      "SUV" -> "Suv"
      "AUTO_RICKSHAW" -> "Auto Rickshaw"
      _ -> var
  Nothing -> ""


fetchFiles :: Effect Unit
fetchFiles = do
  files <- preFetch
  DA.fold $ map (\item -> launchAff_ $ do 
    result <- download item.filePath item.location
    if result then pure unit else liftEffect $ firebaseLogEventWithParams "download_failed" "file_name" item.filePath) files
  

download :: String -> String -> Aff Boolean
download filepath location = makeAff \cb -> runEffectFn3 renewFile filepath location (cb <<< Right) $> nonCanceler

onBoardingSubscriptionScreenCheck :: Int -> Boolean -> Boolean
onBoardingSubscriptionScreenCheck onBoardingSubscriptionViewCount isEnabled = isEnabled && 
                                                                              getValueToLocalNativeStore DRIVER_SUBSCRIBED == "false" && 
                                                                              even onBoardingSubscriptionViewCount && 
                                                                              onBoardingSubscriptionViewCount <5 && 
                                                                              isOnFreeTrial FunctionCall

getVehicleVariantImage :: String -> String
getVehicleVariantImage variant =
  let url = getAssetStoreLink FunctionCall
      commonUrl = getCommonAssetStoreLink FunctionCall
  in case getMerchant FunctionCall of
        YATRISATHI -> case variant of
                        "TAXI" -> "ny_ic_taxi_side," <> commonUrl <> "ny_ic_taxi_side.png"
                        "SUV"  -> "ny_ic_suv_ac_side," <> commonUrl <> "ny_ic_suv_ac_side.png"
                        _      -> "ny_ic_sedan_ac_side," <> commonUrl <> "ny_ic_sedan_ac_side.png"
        _          -> case variant of
                        "SEDAN"     -> "ic_sedan," <> url <> "ic_sedan.png"
                        "SUV"       -> "ic_suv," <> url <> "ic_suv.png"
                        "HATCHBACK" -> "ic_hatchback," <> url <> "ic_hatchback.png"
                        "TAXI"      -> "ic_sedan_non_ac," <> url <> "ic_sedan_non_ac.png"
                        "TAXI_PLUS" -> "ic_sedan_ac," <> url <> "ic_sedan_ac.png"
                        _           -> "ic_sedan_ac," <> url <> "ic_sedan_ac.png"

getVariantRideType :: String -> String
getVariantRideType variant =
  case getMerchant FunctionCall of
    YATRISATHI -> case variant of
                    "TAXI" -> getString TAXI
                    "SUV"  -> getString AC_SUV
                    _      -> getString AC_CAB
    _          -> case variant of
                    "TAXI"          -> getString TAXI
                    "SEDAN"         -> getString SEDAN
                    "HATCHBACK"     -> getString HATCHBACK
                    "TAXI_PLUS"     -> getString TAXI_PLUS
                    "SUV"           -> getString SUV
                    "AUTO_RICKSHAW" -> getString AUTO_RICKSHAW
                    _               -> variant
                    
getStatus :: String -> PaymentStatus
getStatus status = case status of
  "Success" -> Success
  "Pending" -> Pending
  "Failed" -> Failed
  "Scheduled" -> Scheduled
  _ -> Pending

getFixedTwoDecimals :: Number -> String
getFixedTwoDecimals amount = case (fromNumber amount) of
                                Just value -> show value
                                Nothing ->  toStringWith (fixed 2) amount
