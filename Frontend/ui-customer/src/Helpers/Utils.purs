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
    )
    where

import Accessor (_distance_meters, _payload, _deeplinkOptions)
import Common.Types.App (EventPayload(..), GlobalPayload(..), LazyCheck(..), Payload(..), InnerPayload, CarouselModal, DeeplinkOptions(..))
import Components.LocationListItem.Controller (locationListStateObj)
import Control.Monad.Except (runExcept)
import Control.Monad.Free (resume)
import Data.Array (cons, deleteAt, drop, filter, head, length, null, sortBy, sortWith, tail, (!!), reverse)
import Data.Array.NonEmpty (fromArray)
import Data.Boolean (otherwise)
import Data.Date (Date)
import Data.Either (Either(..), hush)
import Data.Eq.Generic (genericEq)
import Data.Foldable (or)
import Data.Function.Uncurried (Fn2, runFn3, Fn3)
import Data.Generic.Rep (class Generic)
import Data.Int (round, toNumber, fromString)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (pi, sin, cos, sqrt, asin, abs)
import Data.Ord (comparing)
import Data.Profunctor.Strong (first)
import Data.Show.Generic (genericShow)
import Data.String (replace, split, Pattern(..), Replacement(..))
import Data.String as DS
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff(..), error, killFiber, launchAff, launchAff_, makeAff, nonCanceler, Fiber)
import Effect.Aff (error, killFiber, launchAff, launchAff_)
import Effect.Aff.Compat (EffectFn1, EffectFnAff, fromEffectFnAff, runEffectFn1, runEffectFn2, runEffectFn3, EffectFn2)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Effect.Uncurried (EffectFn1(..), EffectFn5(..), mkEffectFn1, mkEffectFn4, runEffectFn5)
import Effect.Uncurried (EffectFn1, EffectFn4, EffectFn3, runEffectFn3)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons (getWindowVariable, isPreviousVersion, liftFlow, os)
import Engineering.Helpers.Commons (parseFloat, setText) as ReExport
import Engineering.Helpers.Utils (class Serializable, serialize)
import Foreign (MultipleErrors, unsafeToForeign)
import Foreign.Class (class Decode, class Encode, encode)
import Foreign.Generic (Foreign, decodeJSON, encodeJSON)
import Foreign.Generic (decode)
import JBridge (emitJOSEvent)
import Juspay.OTP.Reader (initiateSMSRetriever)
import Juspay.OTP.Reader as Readers
import Juspay.OTP.Reader.Flow as Reader
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant, getValueFromConfig)
import Prelude (class Eq, class Ord, class Show, Unit, bind, compare, comparing, discard, identity, map, mod, not, pure, show, unit, void, ($), (&&), (*), (+), (-), (/), (/=), (<), (<#>), (<$>), (<*>), (<<<), (<=), (<>), (=<<), (==), (>), (>=), (>>>), (||))
import Prelude (class EuclideanRing, Unit, bind, discard, identity, pure, unit, void, ($), (+), (<#>), (<*>), (<>), (*>), (>>>), ($>), (/=), (&&), (<=), show, (>=), (>), (<))
import Presto.Core.Flow (Flow, doAff)
import Presto.Core.Types.Language.Flow (FlowWrapper(..), getState, modifyState)
import Screens.Types (RecentlySearchedObject,SuggestionsMap, SuggestionsData(..), HomeScreenState, AddNewAddressScreenState, LocationListItemState, PreviousCurrentLocations(..), CurrentLocationDetails, LocationItemType(..), NewContacts, Contacts, FareComponent)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode)
import PrestoDOM.Core (terminateUI)
import Screens.Types (AddNewAddressScreenState, Contacts, CurrentLocationDetails, FareComponent, HomeScreenState, LocationItemType(..), LocationListItemState, NewContacts, PreviousCurrentLocations, RecentlySearchedObject, Stage(..), Location)
import Screens.Types (RecentlySearchedObject, HomeScreenState, AddNewAddressScreenState, LocationListItemState, PreviousCurrentLocations(..), CurrentLocationDetails, LocationItemType(..), NewContacts, Contacts, FareComponent, SuggestionsMap, SuggestionsData(..),SourceGeoHash)
import Services.API (Prediction)
import Storage (KeyStore(..), getValueToLocalStore)
import Types.App (GlobalState(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import shuffle :: forall a. Array a -> Array a

foreign import withinTimeRange :: String -> String -> String -> Boolean

foreign import getNewTrackingId :: Unit -> String

foreign import storeCallBackCustomer :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit

foreign import getLocationName :: forall action. (action -> Effect Unit) -> Number -> Number -> String -> (Number -> Number -> String -> action) -> Effect Unit

foreign import getCurrentDate :: String -> String
foreign import getNextDate :: String -> String
foreign import getNextDateV2 :: String -> String
foreign import compareDate :: EffectFn2 String String Boolean
foreign import storeCallBackContacts :: forall action. (action -> Effect Unit) -> ((Array Contacts) -> action) -> Effect Unit
foreign import parseNewContacts :: String -> (Array NewContacts)
foreign import parseSourceHashArray :: String -> Array SourceGeoHash

foreign import secondsToHms :: Int -> String

foreign import getTime :: Unit -> Int

foreign import drawPolygon :: String -> String -> Effect Unit
foreign import getDifferenceBetweenDates :: Fn2 String String Int

foreign import removeLabelFromMarker :: EffectFn1 Number Unit
-- foreign import generateSessionToken :: String -> String
foreign import requestKeyboardShow :: String -> Effect Unit

foreign import factoryResetApp :: String -> Unit

foreign import validateEmail :: String -> Boolean

foreign import getUTCDay :: Date -> Int

foreign import makePascalCase :: String -> String

foreign import validateInputPattern :: String -> String -> Boolean

foreign import strLenWithSpecificCharacters :: String -> String -> Int

foreign import decodeError :: String -> String -> String

foreign import toStringJSON :: forall a. a -> String

foreign import setRefreshing :: String -> Boolean -> Unit

foreign import setEnabled :: String -> Boolean -> Unit

foreign import _generateQRCode :: EffectFn5 String String Int Int (AffSuccess String) Unit

generateQR:: EffectFn4 String String Int Int Unit
generateQR  = mkEffectFn4 \qrString viewId size margin ->  launchAff_  $ void $ makeAff $
  \cb ->
    (runEffectFn5 _generateQRCode qrString viewId size margin (Right >>> cb))
    $> nonCanceler

foreign import saveToLocalStoreImpl :: String -> String -> EffectFnAff Unit
saveToLocalStore' :: String -> String -> EffectFnAff Unit
saveToLocalStore' = saveToLocalStoreImpl

foreign import fetchFromLocalStoreImpl :: String -> (String -> Maybe String) -> Maybe String -> Effect (Maybe String)
fetchFromLocalStore' :: String -> (String -> Maybe String) -> Maybe String -> Effect (Maybe String)
fetchFromLocalStore' = fetchFromLocalStoreImpl

foreign import fetchFromLocalStoreTempImpl :: String -> (String -> Maybe String) -> Maybe String -> Effect (Maybe String)
fetchFromLocalStoreTemp' :: String -> (String -> Maybe String) -> Maybe String -> Effect (Maybe String)
fetchFromLocalStoreTemp' = fetchFromLocalStoreTempImpl

foreign import fetchAndUpdateCurrentLocation :: forall action. (action -> Effect Unit) -> (String -> String -> action) -> action -> Effect Unit

foreign import seperateByWhiteSpaces :: String -> String

foreign import getKeyInSharedPrefKeysConfigEff :: String -> Effect String
foreign import clearWaitingTimer :: String -> Unit
foreign import clearCountDownTimer :: String -> Unit
foreign import contactPermission :: Unit -> Effect Unit
foreign import performHapticFeedback :: Unit -> Effect Unit
foreign import adjustViewWithKeyboard :: String -> Effect Unit

foreign import getMobileNumber :: EffectFn2 String String String

foreign import extractKeyByRegex :: Fn2 String String String

data TimeUnit
  = HOUR
  | MINUTE
  | SECOND

convertUTCToISTAnd12HourFormat :: String -> Maybe String
convertUTCToISTAnd12HourFormat inputTime = do
  -- Convert the input time to a 24-hour format if it's in 12-hour format (AM/PM)
  let adjustedInputTime = replace (Pattern "PM") (Replacement "") $ replace (Pattern "AM") (Replacement "") inputTime

  case split (Pattern ":") adjustedInputTime of
    [h, m, _] -> do
      hours <- fromString h
      minutes <- fromString m
      
      -- Add 5 hours and 30 minutes
      let adjustRemainder = if minutes >= 30 then 1 else 0
          adjustedHours24 = (hours + 5 + adjustRemainder) `mod` 24
          adjustedMinutes = (minutes + 30) `mod` 60
      
      -- Convert to 12-hour format with AM/PM
      let {adjustedHours, period} = if adjustedHours24 < 12 then {adjustedHours: adjustedHours24, period: "AM"} else {adjustedHours: adjustedHours24 - 12, period: "PM"}
      
      let paddingHours = if adjustedHours < 10 then "0" else ""
          paddingMinutes = if adjustedMinutes < 10 then "0" else ""

      -- Format the adjusted time
      let adjustedTime = paddingHours <> show adjustedHours <> ":" <> paddingMinutes <> show adjustedMinutes <> " " <> period
  
      pure adjustedTime
    _ -> Nothing
  
otpRule :: Reader.OtpRule
otpRule =
  Reader.OtpRule
    { matches:
        { sender: []
        , message : (getValueFromConfig "OTP_MESSAGE_REGEX")
        }
    , otp: "\\d{4}"
    , group: Nothing
    }

startOtpReciever :: forall action. (String -> action) -> (action -> Effect Unit) -> Effect (Effect Unit)
startOtpReciever action push = do
  fiber <-
    launchAff
      $ do
          otpListener <- traverse Readers.getOtpListener $ fromArray [ Readers.smsRetriever ]
          _ <- traverse identity $ (otpListener <#> _.setOtpRules) <*> Just [ otpRule ]
          message <- traverse identity $ (otpListener <#> _.getNextOtp)
          case message of
            Just (Readers.Otp val _ _) -> liftEffect $ push $ action val
            _ -> pure unit
          void $ initiateSMSRetriever
          liftEffect $ startOtpReciever action push
  pure $ launchAff_ $ killFiber (error "Failed to Cancel") fiber

derive instance genericTimeUnit :: Generic TimeUnit _

instance showTimeUnit :: Show TimeUnit where
  show = genericShow


fetchRecents :: Decode RecentlySearchedObject => String -> Flow GlobalState (Maybe RecentlySearchedObject)
fetchRecents objName = do
  (maybeEncodedState :: Maybe String) <- liftFlow $ fetchFromLocalStore' objName Just Nothing
  case maybeEncodedState of
    Just encodedState -> do
      case runExcept (decodeJSON encodedState) of
        Right obj -> pure $ Just obj
        Left err -> fetchOldRecents objName
    Nothing -> pure Nothing

fetchOldRecents :: Decode RecentlySearchedObject => String -> Flow GlobalState (Maybe RecentlySearchedObject)
fetchOldRecents objName = do
  (maybeEncodedState :: Maybe String) <- liftFlow $ fetchFromLocalStoreTemp' objName Just Nothing
  case maybeEncodedState of
    Just encodedState -> do
      case runExcept (decodeJSON encodedState) of
        Right obj -> pure $ Just obj
        Left err -> do
          _ <- liftFlow (logShow $ "fetchRecentsTemp: Error while decoding " <> (show err))
          pure Nothing
    Nothing -> pure Nothing

getObjFromLocal :: HomeScreenState -> Flow GlobalState RecentlySearchedObject
getObjFromLocal homeScreenState = do
  (recentlySearched :: Maybe RecentlySearchedObject) <- (fetchRecents "RECENT_SEARCHES")
  case recentlySearched of
    Just recents -> pure $ recents{predictionArray =  map (\item -> item{prefixImageUrl = "ny_ic_recent_search," <> (getAssetLink FunctionCall) <> "ny_ic_recent_search.png"}) (recents.predictionArray)}
    Nothing -> pure homeScreenState.data.recentSearchs

getRecentSearches :: AddNewAddressScreenState -> Flow GlobalState RecentlySearchedObject
getRecentSearches addNewAddressScreenState = do
      (recentlySearched :: Maybe RecentlySearchedObject) <- (fetchRecents "RECENT_SEARCHES")
      case recentlySearched of
        Just recents  -> pure recents
        Nothing -> pure addNewAddressScreenState.data.recentSearchs

--------------------------------------------------------------------------------------------------

saveCurrentLocations :: forall s. Serializable s => String -> s -> Flow GlobalState Unit
saveCurrentLocations objName obj =
  doAff do
    (fromEffectFnAff <<< saveToLocalStore' objName $ (serialize obj))

fetchCurrentLocations :: Decode PreviousCurrentLocations => String -> Flow GlobalState (Maybe PreviousCurrentLocations)
fetchCurrentLocations objName = do
  (maybeEncodedState :: Maybe String) <- liftFlow $ fetchFromLocalStore' objName Just Nothing
  case maybeEncodedState of
    Just encodedState -> do
      case runExcept (decodeJSON encodedState) of
        Right obj -> pure $ Just obj
        Left err -> do
          _ <- liftFlow (logShow $ "fetchCurrentLocations: Error while decoding " <> (show err))
          pure Nothing
    Nothing -> pure Nothing

getCurrentLocationsObjFromLocal :: HomeScreenState -> Flow GlobalState PreviousCurrentLocations
getCurrentLocationsObjFromLocal homeScreenState = do
  (currentLocations :: Maybe PreviousCurrentLocations) <- (fetchCurrentLocations "PREVIOUS_CURRENT_LOCATION")
  case currentLocations of
    Just recents -> pure recents
    Nothing -> pure homeScreenState.data.previousCurrentLocations

checkCurrLoc :: CurrentLocationDetails -> Array CurrentLocationDetails -> Boolean
checkCurrLoc currLoc currLocArr = or ( map (\item -> (getDistanceBwCordinates currLoc.lat currLoc.lon item.lat item.lon < 0.05)) currLocArr)

addToPrevCurrLoc :: CurrentLocationDetails -> Array CurrentLocationDetails -> Array CurrentLocationDetails
addToPrevCurrLoc currLoc currLocArr =
  if (not (checkCurrLoc currLoc currLocArr))
    then if (length currLocArr == 10)
            then (fromMaybe [] (deleteAt 10 (cons currLoc currLocArr)))
            else (cons currLoc currLocArr)
    else currLocArr

 --------------------------------------------------------------------------------------------------

checkPrediction :: LocationListItemState -> Array LocationListItemState -> Boolean
checkPrediction prediction predictionArr = if (length (filter (\ ( item) -> (item.placeId) == (prediction.placeId))(predictionArr)) > 0) then false else true

getPrediction :: LocationListItemState -> Array LocationListItemState -> LocationListItemState
getPrediction prediction predictionArr = (fromMaybe locationListStateObj ((filter (\ ( item) -> (item.placeId) == (prediction.placeId))(predictionArr)) !! 0))

addSearchOnTop :: LocationListItemState -> Array LocationListItemState -> Array LocationListItemState
addSearchOnTop prediction predictionArr = cons prediction (filter (\ ( item) -> (item.placeId) /= (prediction.placeId))(predictionArr))

addToRecentSearches :: LocationListItemState -> Array LocationListItemState -> Array LocationListItemState
addToRecentSearches prediction predictionArr = 
    let prediction' = prediction {prefixImageUrl = "ny_ic_recent_search," <> (getAssetLink FunctionCall) <> "ny_ic_recent_search.png", locationItemType = Just RECENTS}
      in (if (checkPrediction prediction' predictionArr) 
           then (if length predictionArr == 30 then (fromMaybe [] (deleteAt 30 (cons prediction' predictionArr)))
          else (cons  prediction' predictionArr)) else addSearchOnTop prediction' predictionArr)

differenceOfLocationLists :: Array LocationListItemState -> Array LocationListItemState -> Array LocationListItemState
differenceOfLocationLists arr1 arr2 = filter ( \item1 -> length (filter( \ (item2) -> (item2.placeId == item1.placeId)) arr2) == 0) arr1

filterRecentSearches :: Array LocationListItemState -> Array LocationListItemState -> Array LocationListItemState
filterRecentSearches arr1 arr2 = filter ( \item1 -> length (filter( \ (item2) -> (item2.placeId /= item1.placeId)) arr2) /= (length arr2)) arr1

getDistanceBwCordinates :: Number -> Number -> Number -> Number -> Number
getDistanceBwCordinates lat1 long1 lat2 long2 = do
  let latPoint1 = toRad (lat1)
  let lngPoint1 = toRad (long1)
  let latPoint2 = toRad (lat2)
  let lngPoint2 = toRad (long2)
  let dlong = toRad (long2 -  (long1))
  let lati1 = toRad (lat1)
  let lati2 = toRad (lat2)
  let dist =  sin ((latPoint2 - latPoint1) / 2.0 ) * sin ((latPoint2 - latPoint1) / 2.0 ) + cos(latPoint1) * cos(latPoint2) * sin ((lngPoint2 - lngPoint1) / 2.0 ) * sin ((lngPoint2 - lngPoint1) / 2.0 )
  let dist1 = (2.0 * 6371.0 * asin ( sqrt dist))
  dist1

toRad :: Number -> Number
toRad n = (n * pi) / 180.0

getCurrentLocationMarker :: String -> String
getCurrentLocationMarker currentVersion = "ny_ic_customer_current_location"

rotateArray :: forall a. Array a -> Int -> Array a
rotateArray arr times =
  if times > 0 then case head arr of
    Just ele ->
      rotateArray
        ( ( case tail arr of
              Just tailArray -> tailArray
              Nothing -> []
          )
            <> [ ele ]
        )
        (times - 1)
    Nothing -> arr
  else
    arr

type AffSuccess s = (s -> Effect Unit)

isHaveFare :: String -> Array FareComponent -> Boolean
isHaveFare fare = not null <<< filter (\item -> item.fareType == fare)

sortPredctionByDistance :: Array Prediction -> Array Prediction
sortPredctionByDistance arr = sortBy (comparing (_^._distance_meters)) arr


getDistanceString :: Int -> Int -> String
getDistanceString distanceInMeters decimalPoint
  | distanceInMeters >= 1000 = ReExport.parseFloat (toNumber distanceInMeters / 1000.0) decimalPoint <> " km"
  | otherwise = show distanceInMeters <> " m"

recentDistance :: Array LocationListItemState -> Number -> Number -> Array LocationListItemState
recentDistance arr currLat currLon = map (\item -> item{actualDistance = Just ( round ( ((getDistanceBwCordinates currLat currLon (fromMaybe 0.0 item.lat) (fromMaybe 0.0 item.lon) ))*1000.0)), distance = Just $ getDistanceString (round ( ((getDistanceBwCordinates currLat currLon (fromMaybe 0.0 item.lat) (fromMaybe 0.0 item.lon) ))*1000.0) )1}) arr

getAssetLink :: LazyCheck -> String
getAssetLink lazy = case (getMerchant lazy) of
  NAMMAYATRI -> "https://assets.juspay.in/beckn/nammayatri/user/images/"
  YATRISATHI -> "https://assets.juspay.in/beckn/jatrisaathi/user/images/"
  YATRI -> "https://assets.juspay.in/beckn/yatri/user/images/"
  MOBILITY_PM -> "https://assets.juspay.in/beckn/mobilitypaytm/user/"
  PASSCULTURE -> "https://assets.juspay.in/beckn/passculture/user/images/"
  MOBILITY_RS -> "https://assets.juspay.in/beckn/mobilityredbus/user/images/"

getCommonAssetLink :: LazyCheck -> String
getCommonAssetLink lazy = case (getMerchant lazy) of
  NAMMAYATRI -> "https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/"
  YATRISATHI -> "https://assets.juspay.in/beckn/jatrisaathi/jatrisaathicommon/images/"
  YATRI -> "https://assets.juspay.in/beckn/yatri/yatricommon/images/"
  MOBILITY_PM -> "https://assets.juspay.in/beckn/mobilitypaytm/mobilitypaytmcommon/"
  PASSCULTURE -> "https://assets.juspay.in/beckn/passculture/passculturecommon/"
  MOBILITY_RS -> "https://assets.juspay.in/beckn/mobilityredbus/mobilityredbuscommon/"

getAssetsBaseUrl :: LazyCheck -> String
getAssetsBaseUrl lazy = case (getMerchant lazy) of
  NAMMAYATRI -> "https://assets.juspay.in/beckn/nammayatri/user/"
  YATRISATHI -> "https://assets.juspay.in/beckn/jatrisaathi/user/"
  YATRI -> "https://assets.juspay.in/beckn/yatri/user/"
  MOBILITY_PM -> "https://assets.juspay.in/beckn/mobilitypaytm/user/"
  PASSCULTURE -> "https://assets.juspay.in/beckn/passculture/user/"
  MOBILITY_RS -> "https://assets.juspay.in/beckn/mobilityredbus/user/"

fetchImage :: FetchImageFrom -> String -> String
fetchImage fetchImageFrom imageName = do
  if imageName  == "" then ","
  else case fetchImageFrom of
    FF_ASSET -> imageName <> "," <> (getAssetLink FunctionCall) <> imageName <> ".png"
    FF_COMMON_ASSET -> imageName <> "," <> (getCommonAssetLink FunctionCall) <> imageName <> ".png"

data FetchImageFrom = FF_ASSET | FF_COMMON_ASSET

derive instance genericFetchImageFrom :: Generic FetchImageFrom _
instance eqFetchImageFrom :: Eq FetchImageFrom where eq = genericEq
instance showFetchImageFrom :: Show FetchImageFrom where show = genericShow
instance encodeFetchImageFrom :: Encode FetchImageFrom where encode = defaultEnumEncode
instance decodeFetchImageFrom :: Decode FetchImageFrom where decode = defaultEnumDecode

showCarouselScreen :: LazyCheck -> Boolean
showCarouselScreen a = if os == "IOS" then not ( isPreviousVersion (getValueToLocalStore VERSION_NAME) "1.3.1" ) && getMerchant FunctionCall == NAMMAYATRI else getMerchant FunctionCall == NAMMAYATRI

terminateApp :: Stage -> Boolean -> Unit
terminateApp stage exitApp = emitTerminateApp (Just $ getScreenFromStage stage) exitApp

emitTerminateApp :: Maybe String -> Boolean -> Unit
emitTerminateApp screen exitApp = runFn3 emitJOSEvent "java" "onEvent" $ encode $  EventPayload {
    event : "process_result"
  , payload : Just {
    action : "terminate"
  , trip_amount : Nothing
  , ride_status : Nothing
  , trip_id : Nothing
  , screen : screen
  , exit_app : exitApp
  }
}

makeNumber :: String -> String
makeNumber number = (DS.take 2 number) <> " " <> (DS.drop 2 (DS.take 4 number)) <> " " <>  reverse' (DS.drop 4 (reverse' (DS.drop 4 number))) <> " " <>  reverse' (DS.take 4 (reverse' number))

reverse' :: String -> String
reverse' = fromCharArray <<< reverse <<< toCharArray

getVehicleSize :: Unit -> Int
getVehicleSize unit = (getValueFromConfig "vehicleMarkerSize")

getScreenFromStage :: Stage -> String
getScreenFromStage stage = case stage of
  HomeScreen -> "home_screen"
  SettingPrice -> "estimate_screen"
  FindingEstimate -> "finding_driver_loader"
  ConfirmingRide -> "confirm_ride_loader"
  RideAccepted -> "trip_accepted_screen"
  RideStarted -> "trip_started_screen"
  RideCompleted -> "trip_completed_screen"
  PricingTutorial -> "estimate_screen"
  SearchLocationModel -> "search_location_screen"
  FindingQuotes -> "finding_rides_screen"
  QuoteList -> "no_rides_screen"
  PreviousRating -> "previous_ride_rating_screen"
  ConfirmingLocation -> "confirm_location_screen"
  RideRating -> "ride_rating_screen"
  FavouriteLocationModel -> "search_location_screen"
  ChatWithDriver -> "trip_accepted_screen"
  FindEstimateAndSearch -> "finding_rides_screen"
  RetryFindingQuote -> "finding_rides_screen"
  DistanceOutsideLimits -> "finding_driver_loader"
  ShortDistance -> "finding_driver_loader"
  TryAgain -> "finding_rides_screen"
  PickUpFarFromCurrentLocation -> "finding_driver_loader"

getGlobalPayload :: Unit -> Effect (Maybe GlobalPayload)
getGlobalPayload _ = do
  payload  ::  Either MultipleErrors GlobalPayload  <- runExcept <<< decode <<< fromMaybe (unsafeToForeign {}) <$> (getWindowVariable "__payload" Just Nothing)
  pure $ hush payload

getSearchType :: Unit -> String
getSearchType _ = do 
  let payload = unsafePerformEffect $ getGlobalPayload unit
  case payload of
    Just (GlobalPayload payload') -> do
      let (Payload innerPayload) = payload'.payload
      case innerPayload.search_type of
        Just a -> a 
        Nothing -> "normal_search"
    Nothing -> "normal_search"

getDeepLinkOptions :: LazyCheck -> Maybe DeeplinkOptions
getDeepLinkOptions _ = do 
  let mBPayload = unsafePerformEffect $ getGlobalPayload unit
  maybe Nothing (\payload -> payload ^. _payload ^. _deeplinkOptions) mBPayload

isParentView :: LazyCheck -> Boolean
isParentView lazy = maybe false (\(DeeplinkOptions options) -> fromMaybe false options.parent_view) $ getDeepLinkOptions lazy

showTitle :: LazyCheck -> Boolean
showTitle lazy = maybe true (\(DeeplinkOptions options) -> fromMaybe false options.show_title) $ getDeepLinkOptions lazy 

getPaymentMethod :: Unit -> String
getPaymentMethod _ = do 
  let payload = unsafePerformEffect $ getGlobalPayload unit
  case payload of
    Just (GlobalPayload payload') -> do
      let (Payload innerPayload) = payload'.payload
      case innerPayload.payment_method of
        Just a -> a 
        Nothing -> "cash"
    Nothing -> "cash"


triggerRideStatusEvent :: String -> Maybe Int -> Maybe String -> String -> Flow GlobalState Unit 
triggerRideStatusEvent status amount bookingId screen = do
  let (payload :: InnerPayload) = { action : "trip_status"
    , ride_status : Just status
    , trip_amount : amount
    , trip_id : bookingId
    , screen : Just screen
    , exit_app : false
    }
  pure $ runFn3 emitJOSEvent "java" "onEvent" $ encode $ EventPayload {
    event : "process_result"
  , payload : Just payload
  }

fetchDefaultPickupPoint :: Array Location -> Number -> Number -> String
fetchDefaultPickupPoint locations lati longi =
  case filter (\loc -> abs(loc.lat - lati) <= 0.0001 && abs(loc.lng - longi) <= 0.0001) locations of
    [foundLocation] -> foundLocation.place
    _ -> ""

getVehicleVariantImage :: String -> String
getVehicleVariantImage variant =
  let url = getAssetLink FunctionCall
      commonUrl = getCommonAssetLink FunctionCall
  in case getMerchant FunctionCall of
        YATRISATHI -> case variant of
                        "TAXI" -> "ny_ic_taxi_side," <> commonUrl <> "ny_ic_taxi_side.png"
                        "SUV"  -> "ny_ic_suv_ac_side," <> commonUrl <> "ny_ic_suv_ac_side.png"
                        _      -> "ny_ic_sedan_ac_side," <> commonUrl <> "ny_ic_sedan_ac_side.png"
        _          -> case variant of
                        "TAXI"          -> "ic_sedan,"<> url <>"ic_sedan.png"
                        "TAXI_PLUS"     -> "ic_sedan_ac,"<> url <>"ic_sedan_ac.png"
                        "SEDAN"         -> "ic_sedan,"<> url <>"ic_sedan.png"
                        "SUV"           -> "ic_suv,"<> url <>"ic_suv.png"
                        "HATCHBACK"     -> "ic_hatchback,"<> url <>"ic_hatchback.png"
                        "AUTO_RICKSHAW" -> "ny_ic_auto_quote_list,"<> url <>"ic_auto_side_view.png"
                        _               -> "ic_sedan_non_ac,"<> url <>"ic_sedan_non_ac.png"

getVariantRideType :: String -> String
getVariantRideType variant =
  case getMerchant FunctionCall of
    YATRISATHI -> case variant of
                    "TAXI" -> getString NON_AC_TAXI
                    "SUV"  -> getString AC_SUV
                    _      -> getString AC_CAB
    _          -> getString AC_CAB
