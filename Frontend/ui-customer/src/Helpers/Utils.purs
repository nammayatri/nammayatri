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

import Accessor (_distance_meters)
import Accessor (_distance_meters)
import Common.Types.App (EventPayload(..), GlobalPayload(..), LazyCheck(..), Payload(..), InnerPayload)
import Components.LocationListItem.Controller (dummyLocationListState)
import Control.Monad.Except (runExcept)
import Control.Monad.Free (resume)
import Data.Array (cons, deleteAt, drop, filter, head, length, null, sortBy, sortWith, tail, (!!), reverse)
import Data.Array (sortBy)
import Data.Array.NonEmpty (fromArray)
import Data.Date (Date)
import Data.Either (Either(..), hush)
import Data.Eq.Generic (genericEq)
import Data.Foldable (or)
import Data.Function.Uncurried (runFn3)
import Data.Generic.Rep (class Generic)
import Data.Lens ((^.))
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString, pi, sin, cos, sqrt, asin, abs)
import Data.Ord (comparing)
import Data.Profunctor.Strong (first)
import Data.Show.Generic (genericShow)
import Data.String as DS
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (error, killFiber, launchAff, launchAff_)
import Effect.Aff.Compat (EffectFn1, EffectFnAff, fromEffectFnAff, runEffectFn1, runEffectFn2, runEffectFn3, EffectFn2)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons (getWindowVariable, isPreviousVersion, liftFlow, os)
import Engineering.Helpers.Commons (parseFloat, setText) as ReExport
import Foreign (MultipleErrors, unsafeToForeign)
import Foreign.Class (class Decode, class Encode, encode)
import Foreign.Generic (Foreign, decodeJSON, encodeJSON)
import Foreign.Generic (decode)
import JBridge (emitJOSEvent)
import Juspay.OTP.Reader (initiateSMSRetriever)
import Juspay.OTP.Reader as Readers
import Juspay.OTP.Reader.Flow as Reader
import MerchantConfig.Utils (Merchant(..), getMerchant, getValueFromConfig)
import Prelude (class Eq, class Ord, class Show, Unit, bind, compare, comparing, discard, identity, map, not, pure, show, unit, void, ($), (*), (+), (-), (/), (/=), (<), (<#>), (<*>), (<<<), (<=), (<>), (=<<), (==), (>), (>>>), (||), (&&), (<$>))
import Presto.Core.Flow (Flow, doAff)
import Presto.Core.Types.Language.Flow (FlowWrapper(..), getState, modifyState)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode)
import PrestoDOM.Core (terminateUI)
import Screens.Types (AddNewAddressScreenState, Contacts, CurrentLocationDetails, FareComponent, HomeScreenState, LocationItemType(..), LocationListItemState, NewContacts, PreviousCurrentLocations, RecentlySearchedObject, Stage(..), Location)
import Screens.Types (RecentlySearchedObject, HomeScreenState, AddNewAddressScreenState, LocationListItemState, PreviousCurrentLocations(..), CurrentLocationDetails, LocationItemType(..), NewContacts, Contacts, FareComponent, CarouselModel)
import Services.API (Prediction)
import Services.API (Prediction)
import Types.App (GlobalState(..))
import Types.App (GlobalState)
import Storage (KeyStore(..), getValueToLocalStore)
import Unsafe.Coerce (unsafeCoerce)
import Language.Strings (getString)
import Language.Types (STR(..))

-- shuffle' :: forall a. Array a -> Effect (Array a)
-- shuffle' array = do
--   arrayWithRandom <- addRandom array
--   let sortWithRandom = sortWith (\b -> b.randomNum) arrayWithRandom
--   arrayWithoutRandom <- removeRandom sortWithRandom
--   pure arrayWithoutRandom
--   where
--     addRandom :: Array a -> Effect (Array {randomNum :: Number, value :: a})
--     addRandom arr = do
--       randomValue <- random
--       case head arr of
--         Just x -> do
--           future <- (addRandom (drop 1 arr))
--           pure $ [{randomNum : randomValue, value : x}] <> future
--         Nothing -> pure []
--     removeRandom :: Array {randomNum :: Number, value :: a} -> Effect (Array a)
--     removeRandom arr = do
--       case head arr of
--         Just x -> do
--           future <- (removeRandom (drop 1 arr))
--           pure $ [x.value] <> future
--         Nothing -> pure []
foreign import shuffle :: forall a. Array a -> Array a

foreign import withinTimeRange :: String -> String -> String -> Boolean

foreign import getNewTrackingId :: Unit -> String

foreign import storeCallBackLocateOnMap :: forall action. (action -> Effect Unit) -> (String -> String -> String -> action) -> Effect Unit

foreign import storeCallBackCustomer :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit

foreign import getLocationName :: forall action. (action -> Effect Unit) -> Number -> Number -> String -> (Number -> Number -> String -> action) -> Effect Unit

foreign import getCurrentDate :: String -> String
foreign import storeCallBackContacts :: forall action. (action -> Effect Unit) -> ((Array Contacts) -> action) -> Effect Unit
foreign import parseNewContacts :: String -> (Array NewContacts)

foreign import secondsToHms :: Int -> String

foreign import getTime :: Unit -> Int

foreign import drawPolygon :: String -> String -> Effect Unit

foreign import removeLabelFromMarker :: Unit -> Effect Unit
-- foreign import generateSessionToken :: String -> String
foreign import requestKeyboardShow :: String -> Effect Unit

foreign import factoryResetApp :: String -> Unit

foreign import validateEmail :: String -> Boolean

foreign import getUTCDay :: Date -> Int

foreign import makePascalCase :: String -> String

foreign import validateInputPattern :: String -> String -> Boolean

foreign import strLenWithSpecificCharacters :: String -> String -> Int

foreign import decodeError :: String -> String -> String

foreign import toString :: forall a. a -> String

foreign import zoneOtpExpiryTimer :: forall action. Int -> Int -> (action -> Effect Unit) -> (String -> String -> Int -> action) -> Effect Unit

foreign import setRefreshing :: String -> Boolean -> Unit

foreign import setEnabled :: String -> Boolean -> Unit

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

foreign import updateInputString :: String -> Unit

foreign import debounceFunction :: forall action. Int -> (action -> Effect Unit) -> (String -> Boolean -> action) -> Boolean -> Effect Unit

foreign import clearWaitingTimer :: String -> Unit
foreign import clearCountDownTimer :: String -> Unit
foreign import contactPermission :: Unit -> Effect Unit
foreign import performHapticFeedback :: Unit -> Effect Unit
foreign import adjustViewWithKeyboard :: String -> Effect Unit
foreign import storeOnResumeCallback :: forall action. (action -> Effect Unit) -> action -> Effect Unit
foreign import addCarousel :: Array CarouselModel -> String -> Effect Unit
-- foreign import debounceFunction :: forall action. Int -> (action -> Effect Unit) -> (String -> action) -> Effect Unit

foreign import getMobileNumber :: EffectFn2 String String String

data TimeUnit
  = HOUR
  | MINUTE
  | SECOND

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

class Serializable a where
  serialize :: a -> String
  deserialize :: String -> Maybe a

instance genericSerializable :: (Encode a, Decode a) => Serializable a where
  serialize = encodeJSON
  deserialize = decodeJSON >>> runExcept >>> hush

saveRecents :: forall s. Serializable s => String -> s -> Flow GlobalState Unit
saveRecents objName obj =
  doAff do
    (fromEffectFnAff <<< saveToLocalStore' objName $ (serialize obj))

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
    Just recents -> pure $ recents{predictionArray =  map (\item -> item{prefixImageUrl = "ny_ic_recent_search," <> (getAssetStoreLink FunctionCall) <> "ny_ic_recent_search.png"}) (recents.predictionArray)}
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
getPrediction prediction predictionArr = (fromMaybe dummyLocationListState ((filter (\ ( item) -> (item.placeId) == (prediction.placeId))(predictionArr)) !! 0))

addSearchOnTop :: LocationListItemState -> Array LocationListItemState -> Array LocationListItemState
addSearchOnTop prediction predictionArr = cons prediction (filter (\ ( item) -> (item.placeId) /= (prediction.placeId))(predictionArr))

addToRecentSearches :: LocationListItemState -> Array LocationListItemState -> Array LocationListItemState
addToRecentSearches prediction predictionArr = 
    let prediction' = prediction {prefixImageUrl = "ny_ic_recent_search," <> (getAssetStoreLink FunctionCall) <> "ny_ic_recent_search.png", locationItemType = Just RECENTS}
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
getCurrentLocationMarker currentVersion = if isPreviousVersion currentVersion (getPreviousVersion "") then "ic_customer_current_location" else "ny_ic_customer_current_location"

getPreviousVersion :: String -> String
getPreviousVersion _ =
  if os == "IOS" then
    case getMerchant FunctionCall of
      NAMMAYATRI -> "1.2.5"
      YATRISATHI -> "0.0.0"
      _ -> "1.0.0"
    else case getMerchant FunctionCall of
        YATRISATHI -> "0.0.0"
        _ -> "0.0.0"

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

isHaveFare :: String -> Array FareComponent -> Boolean
isHaveFare fare = not null <<< filter (\item -> item.fareType == fare)

sortPredctionByDistance :: Array Prediction -> Array Prediction
sortPredctionByDistance arr = sortBy (comparing (_^._distance_meters)) arr

getAssetStoreLink :: LazyCheck -> String
getAssetStoreLink lazy = case (getMerchant lazy) of
  NAMMAYATRI -> "https://assets.juspay.in/beckn/nammayatri/user/images/"
  YATRISATHI -> "https://assets.juspay.in/beckn/jatrisaathi/user/images/"
  YATRI -> "https://assets.juspay.in/beckn/yatri/user/images/"
  MOBILITY_PM -> "https://assets.juspay.in/beckn/mobilitypaytm/user/"
  PASSCULTURE -> "https://assets.juspay.in/beckn/passculture/user/images/"
  MOBILITY_RS -> "https://assets.juspay.in/beckn/mobilityredbus/user/images/"

getCommonAssetStoreLink :: LazyCheck -> String
getCommonAssetStoreLink lazy = case (getMerchant lazy) of
  NAMMAYATRI -> "https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/"
  YATRISATHI -> "https://assets.juspay.in/beckn/jatrisaathi/jatrisaathicommon/images/"
  YATRI -> "https://assets.juspay.in/beckn/yatri/yatricommon/images/"
  MOBILITY_PM -> "https://assets.juspay.in/beckn/mobilitypaytm/mobilitypaytmcommon/"
  PASSCULTURE -> "https://assets.juspay.in/beckn/passculture/passculturecommon/"
  MOBILITY_RS -> "https://assets.juspay.in/beckn/mobilityredbuscommon/user/"

getAssetsBaseUrl :: LazyCheck -> String
getAssetsBaseUrl lazy = case (getMerchant lazy) of
  NAMMAYATRI -> "https://assets.juspay.in/beckn/nammayatri/user/"
  YATRISATHI -> "https://assets.juspay.in/beckn/jatrisaathi/user/"
  YATRI -> "https://assets.juspay.in/beckn/yatri/user/"
  MOBILITY_PM -> "https://assets.juspay.in/beckn/mobilitypaytm/user/"
  PASSCULTURE -> "https://assets.juspay.in/beckn/passculture/user/"
  MOBILITY_RS -> "https://assets.juspay.in/beckn/mobilityredbus/user/"

showCarouselScreen :: LazyCheck -> Boolean
showCarouselScreen a = if os == "IOS" then not ( isPreviousVersion (getValueToLocalStore VERSION_NAME) "1.3.1" ) && getMerchant FunctionCall == NAMMAYATRI else getMerchant FunctionCall == NAMMAYATRI

terminateApp :: Stage -> Boolean -> Unit
terminateApp stage exitApp = runFn3 emitJOSEvent "java" "onEvent" $ encode $  EventPayload {
    event : "process_result"
  , payload : Just {
    action : "terminate"
  , trip_amount : Nothing
  , ride_status : Nothing
  , trip_id : Nothing
  , screen : Just (getScreenFromStage stage)
  , exit_app : exitApp
  }
}

makeNumber :: String -> String
makeNumber number = (DS.take 2 number) <> " " <> (DS.drop 2 (DS.take 4 number)) <> " " <>  reverse' (DS.drop 4 (reverse' (DS.drop 4 number))) <> " " <>  reverse' (DS.take 4 (reverse' number))

reverse' :: String -> String
reverse' = fromCharArray <<< reverse <<< toCharArray

getMerchantVechicleSize :: Unit -> Int
getMerchantVechicleSize unit = 
 case getMerchant FunctionCall of
   YATRI -> 160
   _ -> 90


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
  let url = getAssetStoreLink FunctionCall
      commonUrl = getCommonAssetStoreLink FunctionCall
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