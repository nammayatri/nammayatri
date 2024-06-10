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
import Screens.Types (AllocationData, DisabilityType(..), DriverReferralType(..), DriverStatus(..))
import Language.Strings (getString)
import Language.Types(STR(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Array ((!!), elemIndex, length, slice, last, find, singleton, null) as DA
import Data.String (Pattern(..), split) as DS
import Data.Array as DA
import Data.String as DS
import Data.Newtype (class Newtype, unwrap)
import Data.Number (pi, sin, cos, asin, sqrt)
import Data.String.Common as DSC
import MerchantConfig.Utils
import Common.Types.App (LazyCheck(..), CalendarDate, CalendarWeek, Price(..), CategoryListType(..))
import Domain.Payments (PaymentStatus(..))
import Common.Types.Config (GeoJson, GeoJsonFeature, GeoJsonGeometry)
import Common.DefaultConfig as CC
import Types.App (FlowBT, defaultGlobalState)
import Control.Monad.Except (runExcept, runExceptT)
import Data.Array ((!!), fold, any, head, filter, foldr, sort, sortWith) as DA
import Data.Array.NonEmpty (fromArray)
import Data.Either (Either(..), hush, either)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number (pi, sin, cos, asin, sqrt)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split, take) as DS
import Data.String as DS
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff (..), error, killFiber, launchAff, launchAff_, makeAff, nonCanceler, Fiber)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (parseFloat, setText, getCurrentUTC, getPastDays, getPastWeeks, toStringJSON) as ReExport
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode, decode)
import Juspay.OTP.Reader (initiateSMSRetriever)
import Juspay.OTP.Reader as Readers
import Juspay.OTP.Reader.Flow as Reader
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (class EuclideanRing, Unit, bind, discard, identity, pure, unit, void, ($), (+), (<#>), (<*>), (<>), (*>), (>>>), ($>), (/=), (&&), (<=), show, (>=), (>),(<), not, (=<<))
import Prelude (class Eq, class Show, (<<<), compare)
import Prelude (map, (*), (-), (/), (==), div, mod, not)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode, defaultDecode, defaultEncode)
import Data.Function.Uncurried (Fn4(..), Fn3(..), runFn4, runFn3, Fn2, runFn1, runFn2)
import Effect.Uncurried (EffectFn1(..),EffectFn5(..), mkEffectFn1, mkEffectFn4, runEffectFn5, EffectFn2(..))
import Common.Types.App (OptionButtonList)
import Engineering.Helpers.Commons (parseFloat, setText, convertUTCtoISC, getCurrentUTC) as ReExport
import Engineering.Helpers.Commons (flowRunner, getCurrencyTypeFromSymbol)
import PaymentPage(PaymentPagePayload, UpiApps(..))
import Presto.Core.Types.Language.Flow (Flow, doAff, loadS)
import Control.Monad.Except.Trans (lift)
import Foreign.Generic (Foreign)
import Data.Newtype (class Newtype)
import Presto.Core.Types.API (class StandardEncode, standardEncode)
import Services.API (PromotionPopupConfig, BookingTypes(..), RidesInfo)
import Services.API as SA
import Storage (KeyStore) 
import JBridge (getCurrentPositionWithTimeout, firebaseLogEventWithParams, translateStringWithTimeout, openWhatsAppSupport, showDialer, getKeyInSharedPrefKeys, Location)
import Effect.Uncurried(EffectFn1, EffectFn4, EffectFn3, EffectFn7, runEffectFn3)
import Storage (KeyStore(..), isOnFreeTrial, getValueToLocalNativeStore)
import Styles.Colors as Color
import Screens.Types (LocalStoreSubscriptionInfo, HomeScreenState)
import Data.Int (fromString, even, fromNumber)
import Data.Int as Int
import Data.Function.Uncurried (Fn1)
import Storage (getValueToLocalStore)
import Services.Config (getWhatsAppSupportNo, getSupportNumber)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Control.Transformers.Back.Trans (runBackT)
import ConfigProvider
import Screens.Types as ST
import MerchantConfig.Types as MCT
import Locale.Utils
import Language.Types (STR(..))
import LocalStorage.Cache (getValueFromCache)
import Data.Map as DM
import Engineering.Helpers.Utils as EHU
import Engineering.Helpers.GeoHash as EHG
import Data.List as DL
import Data.Argonaut.Core
import Data.Argonaut.Decode.Error
import Data.Argonaut.Decode.Class as AD
import Data.Argonaut.Encode.Class as AE
import Data.Argonaut.Core as AC
import Data.Argonaut.Decode.Parser as ADP
import Common.Types.Config as CTC
import Common.Resources.Constants (assetDomain)
import Common.RemoteConfig.Utils (forwardBatchConfigData)
import Common.RemoteConfig.Types (ForwardBatchConfigData(..))
import DecodeUtil (getAnyFromWindow)
import Data.Foldable (foldl)
import MerchantConfig.DefaultConfig (defaultCityConfig)

type AffSuccess s = (s -> Effect Unit)

foreign import shuffle :: forall a. Array a -> Array a
foreign import generateUniqueId :: Unit -> String
foreign import storeCallBackTime :: forall action. (action -> Effect Unit) -> (String -> String -> String -> action)  -> Effect Unit
foreign import onMarkerClickCallbackMapper :: forall action. (action -> Effect Unit) -> (String -> String -> String -> action)  -> String
foreign import getTime :: Unit -> Int
foreign import hideSplash :: Effect Unit
foreign import startTimer :: forall action. Int -> Boolean -> (action -> Effect Unit) -> (String -> action) -> Effect Unit
foreign import convertKmToM :: String -> String
foreign import clearTimer :: String -> Unit
foreign import clearAllTimer :: String -> Unit
foreign import toInt :: forall a. a -> String
foreign import setRefreshing :: String -> Boolean -> Unit
foreign import setEnabled :: String -> Boolean -> Unit
foreign import decodeErrorCode :: String -> String
foreign import decodeErrorMessage :: String -> String
foreign import storeCallBackForNotification :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit
foreign import storeCallBackForAddRideStop :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit
foreign import secondsLeft :: String -> Int
foreign import objectToAllocationType :: String -> AllocationData
foreign import getcurrentdate :: String -> String
foreign import getDatebyCount :: Int -> String
foreign import launchAppSettings :: Unit -> Effect Unit
foreign import getTimeStampString :: String -> String
foreign import parseNumber :: Int -> String
foreign import getPixels :: Fn1 String Number
foreign import setValueToLocalStore :: Fn2 String String Unit
foreign import getDeviceDefaultDensity ::Fn1 String Number
foreign import isYesterday :: String -> Boolean
foreign import isMoreThan24Hours :: String -> Boolean

foreign import isToday :: String -> Boolean

-- -- ####### MAP FFI ######## -----
foreign import currentPosition  :: String -> Effect Unit
foreign import getPeriod :: String -> Period
foreign import clampNumber :: Number -> Number -> Int -> Int
foreign import getPopupObject :: forall f a. Fn3 (f -> Maybe f) (Maybe f) String (Maybe PromotionPopupConfig)
foreign import istToUtcDate :: String -> String

foreign import preFetch :: Effect (Array RenewFile)

foreign import renewFile :: EffectFn3 String String (AffSuccess Boolean) Unit

foreign import getDateAfterNDays :: Int -> String
foreign import downloadQR  :: String -> Effect Unit
foreign import startRideRequestMApp :: EffectFn1 String Unit

decodeGeoJson :: String -> Maybe GeoJson
decodeGeoJson stringGeoJson = 
  case (AD.decodeJson =<< ADP.parseJson stringGeoJson) of
    Right resp -> Just resp
    Left err   -> Nothing

decodeGeoJsonGeometry :: String -> Maybe GeoJsonGeometry
decodeGeoJsonGeometry stringGeometry =
  case (AD.decodeJson =<< ADP.parseJson stringGeometry) of
    Right resp -> Just resp
    Left err   -> Nothing

decodeSpecialLocationList :: String -> SpecialLocationMap 
decodeSpecialLocationList dummy = 
  case (AD.decodeJson =<< ADP.parseJson (getValueToLocalStore SPECIAL_LOCATION_LIST)) of
    Right resp -> resp
    Left err   -> DM.empty

type SliderConfig = { 
  id :: String,
  stepFunctionForCoinConversion :: Int,
  sliderConversionRate :: Number,
  sliderMinValue :: Int,
  sliderMaxValue :: Int,
  sliderDefaultValue :: Int,
  toolTipId :: String
}

foreign import _generateQRCode :: EffectFn5 String String Int Int (AffSuccess String) Unit
foreign import setPopupType :: ST.GoToPopUpType -> Unit
foreign import getPopupType :: forall f. Fn2 (f -> Maybe f) (Maybe f) (Maybe ST.GoToPopUpType)


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

type LabelConfig = { 
  label :: String,
  backgroundColor :: String,
  text :: String,
  secondaryText :: String,
  imageUrl :: String,
  cancelText :: String,
  cancelConfirmImage :: String,
  textColor :: String
}

type SpecialLocationMap = DM.Map String SpecialLocationList

type SpecialLocationList = {
    geoJson :: String
  , gates :: Array Location
  , locationName :: String
  , category :: String
  , city :: String
}

dummyLabelConfig = { 
  label : "",
  backgroundColor : "",
  text : "",
  secondaryText : "",
  imageUrl : "",
  cancelText : "",
  cancelConfirmImage : "",
  textColor : Color.white900
}

otpRule :: Reader.OtpRule
otpRule =
  let others = getAppConfig appConfig
  in Reader.OtpRule {
  matches : {
    sender : [],
    message : others.otpRegex
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

getDowngradeOptions :: String -> Array String
getDowngradeOptions variant = case (getMerchant FunctionCall) of
                                YATRISATHI -> case variant of
                                                "TAXI"  -> []
                                                "SUV"   -> ["SEDAN", "HATCHBACK"]
                                                "SEDAN" -> ["TAXI", "HATCHBACK"] 
                                                "BIKE"  -> []
                                                "AMBULANCE_TAXI" -> []
                                                "AMBULANCE_TAXI_OXY" -> []
                                                "AMBULANCE_AC" -> []
                                                "AMBULANCE_AC_OXY" -> []
                                                "AMBULANCE_VENTILATOR" -> []
                                                "SUV_PLUS" -> ["SUV", "SEDAN", "HATCHBACK"]
                                                _       -> ["TAXI"]
                                _ -> case variant of
                                        "SUV"   -> ["SEDAN", "HATCHBACK"]
                                        "SEDAN" -> ["HATCHBACK"]
                                        "BIKE"  -> []
                                        "AMBULANCE_TAXI" -> []
                                        "AMBULANCE_TAXI_OXY" -> []
                                        "AMBULANCE_AC" -> []
                                        "AMBULANCE_AC_OXY" -> []
                                        "AMBULANCE_VENTILATOR" -> []
                                        "SUV_PLUS" -> ["SUV", "SEDAN", "HATCHBACK"]
                                        _       -> []


getDowngradeOptionsText :: String -> String
getDowngradeOptionsText vehicleType = do
  let
    downgradeFrom = getVariantRideType vehicleType
    downgradeOptions = getUIDowngradeOptions vehicleType
    subsetArray = DA.slice 0 (DA.length downgradeOptions - 1) downgradeOptions
    lastElement = DA.last downgradeOptions
    modifiedArray = DS.joinWith ", " $  map (\item -> getVehicleType item) subsetArray
    prefixString = if (DA.length downgradeOptions > 1) then ", " else " "
    downgradedToString = ( prefixString
                        <> modifiedArray 
                        <> case lastElement of 
                            Just lastElem -> (getString AND) <> " " <> (getVehicleType lastElem)
                            _ -> "" )
  getString DOWNGRADING_VEHICLE_WILL_ALLOW_YOU_TO_TAKE_BOTH_1 <> downgradeFrom <> downgradedToString <> getString DOWNGRADING_VEHICLE_WILL_ALLOW_YOU_TO_TAKE_BOTH_3

getUIDowngradeOptions :: String -> Array String
getUIDowngradeOptions variant = case (getMerchant FunctionCall) of
                                YATRISATHI -> case variant of
                                                "TAXI"  -> []
                                                "BIKE"  -> []
                                                "SUV"   -> ["SEDAN", "HATCHBACK"]
                                                "SEDAN" -> ["TAXI"] 
                                                "AMBULANCE_TAXI" -> []
                                                "AMBULANCE_TAXI_OXY" -> []
                                                "AMBULANCE_AC" -> []
                                                "AMBULANCE_AC_OXY" -> []
                                                "AMBULANCE_VENTILATOR" -> []
                                                "SUV_PLUS" -> ["SUV", "SEDAN", "HATCHBACK"]
                                                _       -> []
                                _ -> case variant of
                                        "SUV"   -> ["SEDAN", "HATCHBACK"]
                                        "SEDAN" -> ["HATCHBACK"]
                                        "BIKE"  -> []
                                        "AMBULANCE_TAXI" -> []
                                        "AMBULANCE_TAXI_OXY" -> []
                                        "AMBULANCE_AC" -> []
                                        "AMBULANCE_AC_OXY" -> []
                                        "AMBULANCE_VENTILATOR" -> []
                                        "SUV_PLUS" -> ["SUV", "SEDAN", "HATCHBACK"]
                                        _       -> []
  
getVehicleType :: String -> String
getVehicleType vehicleType =
  case vehicleType of
    "SEDAN" -> (getString SEDAN )
    "SUV"   -> (getString SUV)
    "HATCHBACK" -> (getString HATCHBACK)
    "AUTO_RICKSHAW" -> (getString AUTO_RICKSHAW)
    "TAXI" -> (getString TAXI)
    "TAXI_PLUS" -> (getString TAXI_PLUS)
    "BIKE" -> (getString BIKE_TAXI)
    "AMBULANCE_TAXI" -> getString NON_AC <> "\x00B7" <> getString NO_OXYGEN
    "AMBULANCE_TAXI_OXY" -> getString NON_AC <> "\x00B7" <> getString OXYGEN
    "AMBULANCE_AC" -> getString AC <> "\x00B7" <> getString NO_OXYGEN
    "AMBULANCE_AC_OXY" -> getString AC <> "\x00B7" <> getString OXYGEN
    "AMBULANCE_VENTILATOR" -> getString VENTILATOR
    "SUV_PLUS" -> "XL Plus" -- getString XL_PLUS
    _ -> ""

getRideLabelData :: Maybe String -> LabelConfig
getRideLabelData maybeLabel = fromMaybe dummyLabelConfig (getRequiredTag maybeLabel)

getRequiredTag :: Maybe String -> Maybe LabelConfig
getRequiredTag maybeLabel  =
  case maybeLabel of
    Just label -> if DA.any (_ == label) ["Accessibility", "GOTO", "Safety", "SpecialZonePickup"] then
                    DA.head (DA.filter (\item -> item.label == label) (rideLabelConfig FunctionCall))
                  else do
                    let arr = DS.split (DS.Pattern "_") label
                    let pickup = fromMaybe "" (arr DA.!! 0)
                    let drop = fromMaybe "" (arr DA.!! 1)
                    let priority = fromMaybe "" (arr DA.!! 2)
                    DA.head (DA.filter (\item -> item.label == (pickup <> "_Pickup")) (rideLabelConfig FunctionCall))
    Nothing    -> Nothing

rideLabelConfig :: LazyCheck -> Array LabelConfig
rideLabelConfig _ = [
  dummyLabelConfig
    { label = "SureMetro_Pickup",
      backgroundColor = "#2194FF",
      text = "Metro Pickup",
      secondaryText = "",
      imageUrl = "ic_metro_white,https://" <> assetDomain <> "/beckn/nammayatri/driver/images/ic_metro_white.png",
      cancelText = "ZONE_CANCEL_TEXT_PICKUP",
      cancelConfirmImage = "ic_cancelride_metro_pickup,https://" <> assetDomain <> "/beckn/nammayatri/driver/images/ic_cancelride_metro_pickup.png"
    },
  dummyLabelConfig
    { label = "SureMetro_Drop",
      backgroundColor = "#2194FF",
      text = "Metro Drop",
      secondaryText = "",
      imageUrl = "ic_metro_white,https://" <> assetDomain <> "/beckn/nammayatri/driver/images/ic_metro_white.png",
      cancelText = "ZONE_CANCEL_TEXT_DROP",
      cancelConfirmImage = "ic_cancelride_metro_drop,https://" <> assetDomain <> "/beckn/nammayatri/driver/images/ic_cancelride_metro_drop.png"
    },
  dummyLabelConfig
    { label = "Accessibility",
      backgroundColor = "#9747FF",
      text = getString ASSISTANCE_REQUIRED,
      secondaryText = getString LEARN_MORE,
      imageUrl = "ny_ic_wheelchair,https://" <> assetDomain <> "/beckn/nammayatri/driver/images/ny_ic_wheelchair.png",
      cancelText = "FREQUENT_CANCELLATIONS_WILL_LEAD_TO_LESS_RIDES",
      cancelConfirmImage = "ic_cancel_prevention,https://" <> assetDomain <> "/beckn/nammayatri/driver/images/ic_cancel_prevention.png"
    },
  dummyLabelConfig
    { label = "Safety",
      backgroundColor = Color.green900,
      text = getString SAFETY_IS_OUR_RESPONSIBILITY,
      secondaryText = getString LEARN_MORE,
      imageUrl = fetchImage FF_ASSET  "ny_ic_user_safety_shield",
      cancelText = "FREQUENT_CANCELLATIONS_WILL_LEAD_TO_LESS_RIDES",
      cancelConfirmImage = fetchImage FF_ASSET  "ic_cancel_prevention"
    },
  dummyLabelConfig  
    { label = "GOTO",
      backgroundColor = "#2C2F3A",
      text = getString GO_TO,
      secondaryText = "",
      imageUrl = "ny_pin_check_white,",
      cancelText = "GO_TO_CANCELLATION_TITLE",
      cancelConfirmImage = "ny_ic_gotodriver_zero,"
    },
  dummyLabelConfig
    {
      label = "SpecialZonePickup",
      backgroundColor = Color.green900,
      text = getString SPECIAL_PICKUP_ZONE_RIDE,
      secondaryText = getString LEARN_MORE,
      imageUrl = "ny_ic_location_pin_white,",
      cancelText = "ZONE_CANCEL_TEXT_DROP",
      cancelConfirmImage = "ic_cancel_prevention,https://" <> assetDomain <> "/beckn/nammayatri/driver/images/ic_cancel_prevention.png"
    }
]

getGenderIndex :: String -> Array OptionButtonList -> Maybe Int
getGenderIndex req arr = do
  let reqArray = map(\ele -> ele.reasonCode) arr
      reqIndex = DA.elemIndex req reqArray
  reqIndex

getMerchantVehicleSize :: Unit -> Int
getMerchantVehicleSize unit = 90

getAssetLink :: LazyCheck -> String
getAssetLink lazy = getValueFromCache "getAssetLink" (\_ -> getAssetLinkByMerchant)
  where
    getAssetLinkByMerchant = 
      case (getMerchant lazy) of
        YATRISATHI -> "https://" <> assetDomain <> "/beckn/jatrisaathi/driver/images/"
        YATRI -> "https://" <> assetDomain <> "/beckn/yatri/driver/images/"
        MOBILITY_PM -> "https://" <> assetDomain <> "/beckn/mobilitypaytm/driver/"
        PASSCULTURE -> "https://" <> assetDomain <> "/beckn/passculture/driver/images"
        MOBILITY_RS -> "https://" <> assetDomain <> "/beckn/passculture/driver/images"
        _ -> "https://" <> assetDomain <> "/beckn/nammayatri/driver/images/"

getAssetsBaseUrl :: LazyCheck -> String
getAssetsBaseUrl lazy = getValueFromCache "getAssetsBaseUrl" (\_ -> getAssetsBaseUrlByMerchant)
  where 
    getAssetsBaseUrlByMerchant = 
      case (getMerchant lazy) of
      YATRISATHI -> "https://" <> assetDomain <> "/beckn/jatrisaathi/driver/"
      YATRI -> "https://" <> assetDomain <> "/beckn/yatri/driver/"
      MOBILITY_PM -> "https://" <> assetDomain <> "/beckn/mobilitypaytm/"
      PASSCULTURE -> "https://" <> assetDomain <> "/beckn/passculture/driver"
      MOBILITY_RS -> "https://" <> assetDomain <> "/beckn/passculture/driver"
      _ -> "https://" <> assetDomain <> "/beckn/nammayatri/driver/"

getCommonAssetLink :: LazyCheck -> String
getCommonAssetLink lazy = getValueFromCache "getCommonAssetLink" (\_ -> getCommonAssetLinkByMerchant)
  where 
    getCommonAssetLinkByMerchant = 
      case (getMerchant lazy) of
        YATRISATHI -> "https://" <> assetDomain <> "/beckn/jatrisaathi/jatrisaathicommon/images/"
        YATRI -> "https://" <> assetDomain <> "/beckn/yatri/yatricommon/images/"
        MOBILITY_PM -> "https://" <> assetDomain <> "/beckn/mobilitypaytm/mobilitypaytmcommon/"
        PASSCULTURE -> "https://" <> assetDomain <> "/beckn/passculture/passculturecommon/"
        MOBILITY_RS -> "https://" <> assetDomain <> "/beckn/passculture/passculturecommon/"
        _ -> "https://" <> assetDomain <> "/beckn/nammayatri/nammayatricommon/images/"

fetchImage :: FetchImageFrom -> String -> String
fetchImage fetchImageFrom imageName =   
  if imageName  == "" then ","
  else 
    case fetchImageFrom of
      FF_ASSET -> imageName <> "," <> (getAssetLink FunctionCall) <> imageName <> ".png"
      FF_COMMON_ASSET -> imageName <> "," <> (getCommonAssetLink FunctionCall) <> imageName <> ".png"
      COMMON_ASSET -> imageName <> "," <> "https://" <> assetDomain <> "/beckn/common/driver/images/" <> imageName <> ".png"
      GLOBAL_COMMON_ASSET -> imageName <> "," <> "https://" <> assetDomain <> "/beckn/common/common/images/" <> imageName <> ".png"

data FetchImageFrom = FF_ASSET | FF_COMMON_ASSET | COMMON_ASSET | GLOBAL_COMMON_ASSET

derive instance genericFetchImageFrom :: Generic FetchImageFrom _
instance eqFetchImageFrom :: Eq FetchImageFrom where eq = genericEq
instance showFetchImageFrom :: Show FetchImageFrom where show = genericShow
instance encodeFetchImageFrom :: Encode FetchImageFrom where encode = defaultEnumEncode
instance decodeFetchImageFrom :: Decode FetchImageFrom where decode = defaultEnumDecode

foreign import isDateGreaterThan :: String -> Boolean

getNegotiationUnit :: String -> MCT.NegotiationUnit -> String
getNegotiationUnit varient negotiationUnit = case varient of
  "AUTO_RICKSHAW" -> negotiationUnit.auto
  _ -> negotiationUnit.cab
  
getValueBtwRange :: forall a. EuclideanRing a => a -> a -> a -> a -> a -> a
getValueBtwRange  x  in_min  in_max  out_min  out_max = (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min 

data LatLon = LatLon String String String

data Translation = Translation String

getCurrentLocation :: Number -> Number -> Number -> Number -> Int -> Boolean -> Boolean -> FlowBT String LatLon
getCurrentLocation currentLat currentLon defaultLat defaultLon timeOut specialLocation shouldFallback = do
  (LatLon startRideCurrentLat startRideCurrentLong ts) <- (lift $ lift $ doAff $ makeAff \cb -> getCurrentPositionWithTimeout (cb <<< Right) LatLon timeOut shouldFallback $> nonCanceler)
  if (startRideCurrentLat /= "0.0" && startRideCurrentLong /= "0.0") then
    pure (LatLon startRideCurrentLat startRideCurrentLong ts)
  else do
    mbLastKnownTs <- lift $ lift $ loadS $ show LAST_KNOWN_LOCATION_TS
    let currentUtc = ReExport.getCurrentUTC ""
        lastKnownTs = fromMaybe currentUtc mbLastKnownTs
    if defaultLat /= 0.0 && defaultLon /= 0.0 && currentLat /= 0.0 && currentLon /= 0.0 then do
      let distanceDiff = (getDistanceBwCordinates currentLat currentLon defaultLat defaultLon)
          rideLat = show $ if distanceDiff <= 0.10 then  currentLat else defaultLat
          rideLong = show $ if distanceDiff <= 0.10 then currentLon else defaultLon
          timeStamp = show $ if distanceDiff <= 0.10 then lastKnownTs else currentUtc
      pure (LatLon rideLat rideLong timeStamp)
      else if specialLocation then do
        rideLat <- lift $ lift $ loadS $ show LAST_KNOWN_LAT 
        rideLong <- lift $ lift $ loadS $ show LAST_KNOWN_LON
        case rideLat,rideLong of
          Just lat, Just lon -> pure (LatLon lat lon lastKnownTs)
          _,_ -> pure (LatLon "0.0" "0.0" currentUtc)
        else pure (LatLon (show defaultLat) (show defaultLon) currentUtc)


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
  Just var -> case var of
                "SEDAN"  -> "Sedan"
                "HATCHBACK"  -> "Hatchback"
                "TAXI_PLUS"  -> "AC Taxi"
                "SUV" -> "Suv"
                "AUTO_RICKSHAW" -> "Auto Rickshaw"
                "BIKE" -> "Bike Taxi"
                "AMBULANCE_TAXI" -> "Ambulance_Taxi"
                "AMBULANCE_TAXI_OXY" -> "Ambulance_Taxi_Oxy"
                "AMBULANCE_AC" -> "Ambulance_AC" 
                "AMBULANCE_AC_OXY" -> "Ambulance_AC_Oxy"
                "AMBULANCE_VENTILATOR" -> "Ambulance_Ventilator"
                "SUV_PLUS" -> "XL Plus"
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
                                                                              isOnFreeTrial FunctionCall && 
                                                                              getValueToLocalNativeStore SHOW_SUBSCRIPTIONS == "true"

getVehicleVariantImage :: String -> String
getVehicleVariantImage variant =
  let url = getAssetLink FunctionCall
      commonUrl = getCommonAssetLink FunctionCall
  in case variant of
      "SEDAN"     -> "ny_ic_sedan_ac," <> commonUrl <> "ny_ic_sedan_ac.png"
      "SEDAN_TIER" -> "ny_ic_sedan_ac," <> commonUrl <> "ny_ic_sedan_ac.png"
      "SUV"       -> "ic_suv_ac," <> commonUrl <> "ic_suv_ac.png"
      "SUV_TIER"  -> "ic_suv_ac," <> commonUrl <> "ic_suv_ac.png"
      "HATCHBACK" -> "ic_hatchback_ac," <> commonUrl <> "ic_hatchback_ac.png"
      "HATCHBACK_TIER" -> "ic_hatchback_ac," <> commonUrl <> "ic_hatchback_ac.png"
      "RENTALS"   -> "ic_rentals," <> commonUrl <> "ic_rentals.png"
      "INTERCITY" -> "ic_intercity," <> commonUrl <> "ic_intercity.png"
      "TAXI"      -> "ic_taxi," <> commonUrl <> "ic_taxi.png"
      "PREMIUM"   -> "ic_cab_premium" <> commonUrl <> "ic_cab_premium.png"
      "TAXI_PLUS" -> "ny_ic_sedan_ac," <> commonUrl <> "ny_ic_sedan_ac.png"
      "ECO"       -> "ic_hatchback_ac," <> commonUrl <> "ic_hatchback_ac.png"
      "COMFY"     -> "ny_ic_sedan_ac," <> commonUrl <> "ny_ic_sedan_ac.png"
      "AUTO_RICKSHAW" -> 
        case (getValueFromCache (show DRIVER_LOCATION) getKeyInSharedPrefKeys) of
          "Hyderabad" -> fetchImage FF_ASSET "ny_ic_black_yellow_auto1"
          "Chennai"   -> fetchImage FF_ASSET "ny_ic_black_yellow_auto1"
          "Kochi"     -> fetchImage FF_ASSET "ny_ic_black_yellow_auto1"
          _           -> fetchImage FF_ASSET "ic_vehicle_front"
      "BIKE"      -> "ny_ic_bike_side," <> commonUrl <> "ny_ic_bike_side.png"
      "AMBULANCE_TAXI" -> "ny_ic_ambulance_side," <> commonUrl <> "ny_ic_ambulance_side.png"
      "AMBULANCE_TAXI_OXY" -> "ny_ic_ambulance_side," <> commonUrl <> "ny_ic_ambulance_side.png"
      "AMBULANCE_AC" -> "ny_ic_ambulance_side," <> commonUrl <> "ny_ic_ambulance_side.png"
      "AMBULANCE_AC_OXY" -> "ny_ic_ambulance_side," <> commonUrl <> "ny_ic_ambulance_side.png"
      "AMBULANCE_VENTILATOR" -> "ny_ic_ambulance_side," <> commonUrl <> "ny_ic_ambulance_side.png"
      "BIKE_TIER" -> "ny_ic_bike_side," <> commonUrl <> "ny_ic_bike_side.png"
      "SUV_PLUS"  -> "ny_ic_suv_plus_side," <> commonUrl <> "ny_ic_suv_plus_side.png"
      "SUV_PLUS_TIER" -> "ny_ic_suv_plus_side," <> commonUrl <> "ny_ic_suv_plus_side.png"
      _ -> fetchImage FF_ASSET "ic_vehicle_front"

getVariantRideType :: String -> String
getVariantRideType variant =
  case variant of
    "TAXI"          -> getString TAXI
    "SEDAN"         -> getString SEDAN
    "HATCHBACK"     -> getString HATCHBACK
    "TAXI_PLUS"     -> getString TAXI_PLUS
    "SUV"           -> getString SUV
    "AUTO_RICKSHAW" -> getString AUTO_RICKSHAW
    "BIKE"          -> getString BIKE_TAXI
    "AMBULANCE_TAXI" -> "Ambulance_Taxi"
    "AMBULANCE_TAXI_OXY" -> "Ambulance_Taxi_Oxy"
    "AMBULANCE_AC" -> "Ambulance_AC"
    "AMBULANCE_AC_OXY" -> "Ambulance_AC_Oxy"
    "AMBULANCE_VENTILATOR" -> "Ambulance_Ventilator"
    _               -> variant
                    
getStatus :: String -> PaymentStatus
getStatus status = case status of
  "Success" -> Success
  "Pending" -> Pending
  "Failed" -> Failed
  "Scheduled" -> Scheduled
  _ -> Pending

incrementValueOfLocalStoreKey :: KeyStore -> Effect Unit
incrementValueOfLocalStoreKey key = do
  let value = fromString $ runFn1 getValueToLocalNativeStore key
  case value of
    Just val -> do
      let _ = runFn2 setValueToLocalStore (show key) (show (val + 1))
      pure unit
    Nothing -> do
      let _ = runFn2 setValueToLocalStore (show key) "1"  
      pure unit

contactSupportNumber :: String -> Effect Unit
contactSupportNumber supportType = do
  void $ launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT do 
    config <- getAppConfigFlowBT appConfig
    let city = getCityConfig config.cityConfig (getValueToLocalStore DRIVER_LOCATION)
        supportNumber = if DSC.null city.supportNumber then getSupportNumber "" else city.supportNumber
    if supportType == "WHATSAPP" && DSC.null city.supportNumber then 
      liftFlowBT $ openWhatsAppSupport $ getWhatsAppSupportNo $ show (getMerchant FunctionCall) 
      else
        pure $ showDialer supportNumber false

setForwardBatchingData :: MCT.CityConfig -> MCT.CityConfig
setForwardBatchingData cityConf =
  let (ForwardBatchConfigData forwardBatchRemoteConfig) = forwardBatchConfigData cityConf.cityName
  in cityConf {enableAdvancedBooking = forwardBatchRemoteConfig.is_Forward_Dispatch_Feature_Enabled, advancedRidePopUpYoutubeLink = forwardBatchRemoteConfig.advancedRidePopUpYoutubeLink, callDriverInfoPost = forwardBatchRemoteConfig.callDriverInfoPost}

getCityConfig :: Array MCT.CityConfig -> String -> MCT.CityConfig
getCityConfig cityConfig cityName = do
  getValueFromCache cityName (\cityName -> maybe defaultCityConfig setForwardBatchingData $ DA.find (\item -> item.cityName == cityName) cityConfig)
    
getCityConfigFromCityCode :: Array MCT.CityConfig -> String -> MCT.CityConfig
getCityConfigFromCityCode cityConfigArr cityCode = getValueFromCache cityCode (\cityCode -> maybe defaultCityConfig setForwardBatchingData $ DA.find (\item -> item.cityCode == cityCode) cityConfigArr)

formatSecIntoMinSecs :: Int -> String
formatSecIntoMinSecs seconds = 
  let
    mins = seconds `div` 60
    secs = seconds `mod` 60
  in 
    show mins <> ":" <> (if secs < 10 then "0" else "") <> show secs

formatSecIntoHourMins :: Int -> String
formatSecIntoHourMins seconds =
  let 
    hours = seconds `div` 3600
    mins = (seconds `mod` 3600) `div` 60
  in (if hours > 0 then show hours <> " hr " else "") <> show mins <> " min"

splitBasedOnLanguage :: String -> String
splitBasedOnLanguage str = 
    let strArray = DS.split (DS.Pattern "-*$*-") str
    in
    fromMaybe "" (strArray DA.!! (getLanguage (DA.length strArray)))
    where 
        getLanguage len = do
            case getLanguageLocale languageKey of
                "KN_IN" | len > 1 -> 1
                "HI_IN" | len > 2 -> 2
                "BN_IN" | len > 3 -> 3
                "ML_IN" | len > 4 -> 4
                "TA_IN" | len > 5 -> 5
                "TE_IN" | len > 6 -> 6
                _ -> 0

generateReferralLink :: String -> String -> String -> String -> String -> DriverReferralType -> String
generateReferralLink source medium term content campaign driverReferralType =
  let config = getAppConfig appConfig 
      cityConfig = getCityConfig config.cityConfig source
      path = if driverReferralType == DRIVER then "/driverRefer" else "/refer"
      packageId = if driverReferralType == DRIVER then cityConfig.referral.driverAppId else cityConfig.referral.customerAppId
      domain = cityConfig.referral.domain
  in domain <> path <> "?referrer=" 
      <> "utm_source%3D" <> source 
      <> "%26utm_medium%3D" <> medium 
      <> "%26utm_term%3D" <> term 
      <> "%26utm_content%3D" <> content 
      <> "%26utm_campaign%3D" <> campaign 
      <> "%26anid%3Dadmob&id=" <> packageId

getLanguageTwoLetters :: Maybe String ->  String
getLanguageTwoLetters mbLanguage = 
  let language = fromMaybe (getLanguageLocale languageKey) mbLanguage
  in 
  case language of
    "HI_IN" -> "hi"
    "KN_IN" -> "kn"
    "TA_IN" -> "ta"
    "TE_IN" -> "te"
    "FR_FR" -> "fr"
    "ML_IN" -> "ml"
    "BN_IN" -> "bn"
    _       -> "en"


generateLanguageList :: Array String -> Array MCT.Language
generateLanguageList languages = map getLanguage languages
  where 
   getLanguage lang = case lang of
      "HINDI" -> {name : "हिंदी", value: "HI_IN", subtitle: "Hindi"}
      "KANNADA" -> {name : "ಕನ್ನಡ", value: "KN_IN", subtitle: "Kannada"}
      "TAMIL" -> {name :"தமிழ்", value : "TA_IN", subtitle : "Tamil"}
      "TELUGU" -> {name:"తెలుగు", value:"TE_IN", subtitle: "Telugu"}
      "FRENCH" -> {name:"Français", value:"FR_FR", subtitle: "French"}
      "MALAYALAM" -> {name:"മലയാളം", value:"ML_IN", subtitle: "Malayalam"}
      "BENGALI" -> {name:"বাংলা", value:"BN_IN", subtitle: "Bengali"}
      "ENGLISH" -> {name : "English", value: "EN_US", subtitle: "English"}
      _ -> {name : "English", value: "EN_US", subtitle: "English"}

getDriverStatus :: String -> DriverStatus
getDriverStatus dummy = do
  case getValueToLocalNativeStore DRIVER_STATUS_N of
    "Online" -> Online
    "Offline" -> Offline
    "Silent" -> Silent
    _ -> Online

getDriverStatusFromMode :: String -> DriverStatus
getDriverStatusFromMode mode = do
  case mode of
    "ONLINE" -> Online
    "OFFLINE" -> Offline
    "SILENT" -> Silent
    _ -> Online

updateDriverStatus :: Boolean -> DriverStatus
updateDriverStatus status = do
  if status && getValueToLocalNativeStore DRIVER_STATUS_N == "Silent" then Silent
    else if status then Online
      else Offline
      
transformSpecialLocationList :: SA.SpecialLocationFullRes -> Effect Unit
transformSpecialLocationList (SA.SpecialLocationFullRes specialLocations) = do
  let transformedList = DA.foldr (\(SA.SpecialLocationFull specialLocation) hashMap -> 
                                    if not $ DA.null specialLocation.gatesInfo then
                                      updateHashMap (SA.SpecialLocationFull specialLocation) hashMap
                                    else hashMap
                                  ) DM.empty specialLocations
      json = AE.encodeJson transformedList
      _ = runFn2 setValueToLocalStore (show SPECIAL_LOCATION_LIST) (AC.stringify json)
  pure unit

updateHashMap :: SA.SpecialLocationFull -> SpecialLocationMap -> SpecialLocationMap
updateHashMap (SA.SpecialLocationFull specialLocation) hashMap =
  let geoJson = transformGeoJsonFeature specialLocation.geoJson specialLocation.gatesInfo
      gates = transformGates specialLocation.gatesInfo
      locationName = specialLocation.locationName
      category = specialLocation.category
      city = ""
  in DA.foldr (\gate hashMap' -> DM.insert (runFn3 EHG.encodeGeohash gate.lat gate.lng 7) { geoJson : geoJson, gates : gates, locationName : locationName, category : category, city : city } hashMap') hashMap gates

transformGates :: Array SA.GateInfoFull -> Array Location
transformGates gatesInfoFulls = 
  DA.foldr  (\(SA.GateInfoFull gateInfoFull) locations ->
                case gateInfoFull.geoJson of
                  Just _ ->
                    let (SA.LatLong point) = gateInfoFull.point 
                    in locations <> [{ lat : point.lat, lng : point.lon, place : gateInfoFull.name, address : gateInfoFull.address, city : Nothing, isSpecialPickUp : Just true }]
                  Nothing -> locations
            ) [] gatesInfoFulls

transformGeoJsonFeature :: Maybe String -> Array SA.GateInfoFull -> String
transformGeoJsonFeature geoJson gateInfoFulls = 
  AC.stringify $ AE.encodeJson CC.defaultGeoJson { features = geoJsonFeatures }
  where
    geoJsonFeatures :: Array GeoJsonFeature
    geoJsonFeatures = 
      DA.foldr (\(SA.GateInfoFull gateInfoFull) specialZones -> 
                  case gateInfoFull.geoJson of
                    Just _ -> specialZones <> [createGeoJsonFeature (SA.GateInfoFull gateInfoFull)]
                    Nothing -> specialZones
               ) [] gateInfoFulls
      <> case geoJson of
            Just geoJson' -> DA.singleton CC.defaultGeoJsonFeature{ geometry = fromMaybe CC.defaultGeoJsonGeometry (decodeGeoJsonGeometry geoJson') }
            Nothing -> []
            
    createGeoJsonFeature :: SA.GateInfoFull ->  GeoJsonFeature
    createGeoJsonFeature (SA.GateInfoFull gateInfoFull) = 
      CC.defaultGeoJsonFeature {
          properties {
              name = gateInfoFull.name
            , defaultDriverExtra = fromMaybe 0 gateInfoFull.defaultDriverExtra
            , canQueueUpOnGate = fromMaybe false gateInfoFull.canQueueUpOnGate
          }
        , geometry = case gateInfoFull.geoJson of
                        Just geoJson -> fromMaybe CC.defaultGeoJsonGeometry (decodeGeoJsonGeometry geoJson)
                        Nothing      -> CC.defaultGeoJsonGeometry
      }

findNearestSpecialZone :: Number -> Number -> Maybe SpecialLocationList
findNearestSpecialZone lat lon =
  let currentGeoHash = runFn3 EHG.encodeGeohash lat lon 7
      hashMap = decodeSpecialLocationList ""
      neighbourGeoHash = (EHG.geohashNeighbours currentGeoHash) <> [currentGeoHash]
      specialZoneMaps = DM.filterWithKey (\key value -> (DM.member key hashMap) && (ifGatesWithInRadius lat lon value.gates) ) hashMap
  in DL.head (DM.values specialZoneMaps)
  where
    ifGatesWithInRadius :: Number -> Number -> Array Location -> Boolean
    ifGatesWithInRadius lat lon gates = not $ DA.null $ DA.filter (\gate -> (getDistanceBwCordinates lat lon gate.lat gate.lng)*1000.0 < 150.0) gates

findSpecialPickupZone :: Number -> Number -> Maybe SpecialLocationList
findSpecialPickupZone lat lon =
  let currentGeoHash = runFn3 EHG.encodeGeohash lat lon 7
      hashMap = decodeSpecialLocationList ""
      specialZone = DM.lookup currentGeoHash hashMap
      nearestGate = case specialZone of
                      Just zone -> ((DA.sortWith (\gate -> getDistanceBwCordinates lat lon gate.lat gate.lng) zone.gates) DA.!! 0)
                      Nothing -> Nothing
      pickupZone = case specialZone, nearestGate of
                    Just zone, Just gate -> 
                      case decodeGeoJson zone.geoJson of
                        Just geoJson -> 
                          let feature = (DA.filter (\feature -> feature.properties.name == gate.place) geoJson.features) DA.!! 0
                          in case feature of
                                Just feature' -> 
                                  let properties = feature'.properties
                                      gatesInfoFull = SA.GateInfoFull { address : Nothing, canQueueUpOnGate : Just properties.canQueueUpOnGate, defaultDriverExtra : Just properties.defaultDriverExtra, geoJson : Just (AC.stringify $ AE.encodeJson feature'.geometry), name : properties.name, point : SA.LatLong { lat : gate.lat, lon : gate.lng } }
                                  in Just zone{ gates = [gate], geoJson = transformGeoJsonFeature Nothing [gatesInfoFull]  }
                                Nothing -> Nothing
                        Nothing -> Nothing
                    _, _ -> Nothing
  in pickupZone

checkSpecialPickupZone :: Maybe String -> Boolean
checkSpecialPickupZone maybeLabel = 
  case maybeLabel of
    Just label -> let arr = DS.split (DS.Pattern "_") label
                      specialPickupZone = fromMaybe "" (arr DA.!! 3)
                  in specialPickupZone == "PickupZone"
    Nothing    -> false

getChargesOb :: ST.TripType -> MCT.CityConfig -> String -> CTC.ChargesEntity
getChargesOb tripType cityConfig driverVehicle = 
  if tripType == ST.Rental then
    getRentalChargesOb cityConfig driverVehicle
  else
    case driverVehicle of
      "AUTO_RICKSHAW" -> cityConfig.waitingChargesConfig.auto
      _ -> cityConfig.waitingChargesConfig.cab

getRentalChargesOb :: MCT.CityConfig -> String -> CTC.ChargesEntity
getRentalChargesOb cityConfig driverVehicle = 
  case driverVehicle of
    "AUTO_RICKSHAW" -> cityConfig.rentalWaitingChargesConfig.auto
    _ -> cityConfig.rentalWaitingChargesConfig.cab



getRideInfoEntityBasedOnBookingType :: HomeScreenState -> ST.ActiveRide
getRideInfoEntityBasedOnBookingType homeScreenState = 
  case homeScreenState.props.bookingStage of
    CURRENT -> homeScreenState.data.activeRide
    ADVANCED -> fromMaybe homeScreenState.data.activeRide homeScreenState.data.advancedRideData
    
transformBapName :: String -> String
transformBapName bapName =
  let lowerCase = DS.toLower bapName
      removedUnderScore = DS.replaceAll (DS.Pattern "_") (DS.Replacement " ") lowerCase
      firstLetterUpperCase = (DS.toUpper (DS.take 1 removedUnderScore)) <> DS.drop 1 removedUnderScore
  in firstLetterUpperCase


appName :: Boolean -> String
appName removeDriver = do
  let config = getAppConfig appConfig 
      driverAppName = fromMaybe config.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
  if removeDriver then
      foldl (\acc word -> DS.replaceAll (DS.Pattern word) (DS.Replacement "") acc) driverAppName ["Driver", "Partner"]
  else driverAppName

setPerKmRate :: SA.GetDriverRateCardRes -> ST.RidePreference -> ST.RidePreference
setPerKmRate (SA.GetDriverRateCardRes rateCardResp) prefOb = 
  let rateCardRespItem = DA.find (\(SA.RateCardRespItem item) -> item.serviceTierType == prefOb.serviceTierType) rateCardResp
  in case rateCardRespItem of
    Just (SA.RateCardRespItem rateCardRespItem) -> prefOb { perKmRate = Just $ rateCardRespItem.perKmRate}
    Nothing -> prefOb

getVehicleMapping :: SA.ServiceTierType -> String
getVehicleMapping serviceTierType = case serviceTierType of
  SA.COMFY -> "SEDAN"
  SA.ECO -> "HATCHBACK"
  SA.PREMIUM -> "SUV"
  SA.SUV_TIER -> "SUV"
  SA.AUTO_RICKSHAW -> "AUTO_RICKSHAW"
  SA.HATCHBACK_TIER -> "HATCHBACK"
  SA.SEDAN_TIER -> "SEDAN"
  SA.TAXI -> "TAXI"
  SA.TAXI_PLUS -> "TAXI_PLUS"
  SA.RENTALS -> "RENTALS"
  SA.INTERCITY -> "INTERCITY"
  SA.BIKE_TIER -> "BIKE"
  SA.SUV_PLUS_TIER -> "SUV_PLUS"

getLatestAndroidVersion :: Merchant -> Int
getLatestAndroidVersion merchant =
  case merchant of
    NAMMAYATRI -> 150
    YATRI -> 150
    YATRISATHI -> 133
    MOBILITY_PM -> 1
    MOBILITY_RS -> 1
    PASSCULTURE -> 1
    BRIDGE -> 129

shouldShowPurpleVideos :: ST.HomeScreenState -> Boolean
shouldShowPurpleVideos state = do
  let cityConfig = getCityConfig state.data.config.cityConfig (getValueToLocalNativeStore DRIVER_LOCATION) 
      purpleRideConfigForVehicle = getPurpleRideConfigForVehicle state.data.linkedVehicleVariant cityConfig.purpleRideConfig
  purpleRideConfigForVehicle.showVideo
  
getGenericAccessibilityVideo :: ST.HomeScreenState -> String
getGenericAccessibilityVideo state = do
  let cityConfig = getCityConfig state.data.config.cityConfig (getValueToLocalNativeStore DRIVER_LOCATION) 
      purpleRideConfigForVehicle = getPurpleRideConfigForVehicle state.data.linkedVehicleVariant cityConfig.purpleRideConfig
  purpleRideConfigForVehicle.genericVideoForVariant

getPurpleRideConfigForVehicle :: String -> MCT.PurpleRideConfigForVehicle -> MCT.VariantToDisabilityVideo
getPurpleRideConfigForVehicle linkedVehicleVariant purpleRideConfigForCity = 
  case linkedVehicleVariant of  
    "AUTO_RICKSHAW" -> purpleRideConfigForCity.purpleRideConfigForAuto
    "BIKE" -> purpleRideConfigForCity.purpleRideConfigForBikes
    _ -> purpleRideConfigForCity.purpleRideConfigForCabs
  
sortListBasedOnCreatedAt :: forall a t. Newtype t (Record (createdAt:: String | a)) => Array t -> Array t
sortListBasedOnCreatedAt = DA.sortBy (\a b -> compare ((unwrap b).createdAt) ((unwrap a).createdAt))

sortListBasedOnDOUpload :: forall a t. Newtype t (Record (dateOfUpload:: String | a)) => Array t -> Array t
sortListBasedOnDOUpload = DA.sortBy (\a b -> compare ((unwrap b).dateOfUpload) ((unwrap a).dateOfUpload))

dummyPriceForCity :: String -> Price
dummyPriceForCity cityName = do
  let currencySymobol = getCurrencyForCity cityName
  {amount: 0.0, currency: getCurrencyTypeFromSymbol currencySymobol}

getCurrencyForCity :: String -> String
getCurrencyForCity cityName = maybe "$" (\config -> config.currency) (DA.find (\item -> item.cityName == cityName) (getAppConfig "").cityConfig)

getDistanceUnitForCity :: String -> String
getDistanceUnitForCity cityName = maybe "mi" (\config -> config.distanceUnit) (DA.find (\item -> item.cityName == cityName) (getAppConfig "").cityConfig)

categoryTransformer :: Array SA.Category -> String -> Array CategoryListType 
categoryTransformer categories language = map (\(SA.Category catObj) ->
                                          { categoryName :
                                              if (language == "en")
                                              then EHU.capitalizeFirstChar catObj.category
                                              else catObj.category
                                          , categoryId       : catObj.issueCategoryId
                                          , categoryAction   : catObj.label
                                          , categoryImageUrl : catObj.logoUrl
                                          }) categories
