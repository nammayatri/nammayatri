{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Helpers.Utils where

import Data.Date (Date)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Prelude (class Show, class Ord, Unit, bind, discard, pure, unit, void, identity, not, (<*>), (<#>), (<<<), (>>>), ($), (<>), (>), show, (==), (/=),(/), (*), (-), (+), map, compare, (<), (=<<), (<=), ($))
import Data.Traversable (traverse)
import Effect.Aff (error, killFiber, launchAff, launchAff_)
import Juspay.OTP.Reader (initiateSMSRetriever)
import Juspay.OTP.Reader.Flow as Reader
import Juspay.OTP.Reader as Readers
import Data.Array.NonEmpty (fromArray)
import Effect.Class (liftEffect)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign.Class (class Decode, class Encode)
import Data.Maybe (Maybe(..), fromMaybe)
import Presto.Core.Flow (Flow, doAff)
import Types.App (GlobalState)
import Screens.Types (RecentlySearchedObject, HomeScreenState, AddNewAddressScreenState, LocationListItemState, PreviousCurrentLocations(..), CurrentLocationDetails, LocationItemType(..))
import Engineering.Helpers.Commons (liftFlow, os)
import Control.Monad.Except (runExcept)
import Foreign.Generic (decodeJSON, encodeJSON)
import Data.Either (Either(..), hush)
import Effect.Console (logShow)
import Data.Array(length,filter,cons,deleteAt, sortWith, drop, head, (!!))
import Debug.Trace(spy)
import Math(pi, sin, cos, sqrt, asin)
import Data.Number (fromString)
import Data.Foldable ( or )
import Data.String as DS
import Components.LocationListItem.Controller (dummyLocationListState)
import Data.Int as INT
-- import Effect.Random (random)
import Effect (Effect)

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

foreign import withinTimeRange :: String -> String -> Boolean

foreign import getNewTrackingId :: Unit -> String

foreign import storeCallBackLocateOnMap :: forall action. (action -> Effect Unit) -> (String -> String -> String -> action) -> Effect Unit

foreign import storeCallBackCustomer :: forall action. (action -> Effect Unit) -> (String -> action) -> Effect Unit

foreign import getLocationName :: forall action. (action -> Effect Unit) -> String -> String -> String -> (String -> String -> String -> action) -> Effect Unit

foreign import getCurrentDate :: String -> String

foreign import dateCompare :: Date -> Date -> Boolean

foreign import secondsToHms :: Int -> String

foreign import getTime :: Unit -> Int

-- foreign import generateSessionToken :: String -> String
foreign import requestKeyboardShow :: String -> Effect Unit

foreign import setText' :: String -> String -> Effect Unit

foreign import addTimeToDate :: Date -> Number -> String -> Date

foreign import factoryResetApp :: String -> Unit

foreign import validateEmail :: String -> Boolean

foreign import getUTCDay :: Date -> Int

foreign import makePascalCase :: String -> String

foreign import hideSplash :: Effect Unit

foreign import validateInputPattern :: String -> String -> Boolean

foreign import decodeErrorCode :: String -> String

foreign import decodeErrorMessage :: String -> String

foreign import toString :: forall a. a -> String

foreign import waitingCountdownTimer :: forall action. Int -> (action -> Effect Unit) -> (String -> String -> Int -> action) -> Effect Unit

foreign import convertUTCtoISC :: String -> String -> String

foreign import setRefreshing :: String -> Boolean -> Unit

foreign import setEnabled :: String -> Boolean -> Unit

foreign import saveToLocalStore' :: String -> String -> EffectFnAff Unit

foreign import fetchFromLocalStore' :: String -> (String -> Maybe String) -> Maybe String -> Effect (Maybe String)

foreign import fetchFromLocalStoreTemp' :: String -> (String -> Maybe String) -> Maybe String -> Effect (Maybe String)

foreign import fetchAndUpdateCurrentLocation :: forall action. (action -> Effect Unit) -> (String -> String -> action) -> action -> Effect Unit

foreign import getCurrentUTC :: String -> String

foreign import getExpiryTime :: String -> String -> Boolean -> Int

foreign import seperateByWhiteSpaces :: String -> String

foreign import getKeyInSharedPrefKeysConfigEff :: String -> Effect String

foreign import updateInputString :: String -> Unit

foreign import debounceFunction :: forall action. Int -> (action -> Effect Unit) -> (String -> action) -> Effect Unit

foreign import parseFloat :: forall a. a -> Int -> String

foreign import clearWaitingTimer :: String -> Unit

foreign import adjustViewWithKeyboard :: String -> Effect Unit
-- foreign import debounceFunction :: forall action. Int -> (action -> Effect Unit) -> (String -> action) -> Effect Unit
data TimeUnit
  = HOUR
  | MINUTE
  | SECOND

otpRule :: Reader.OtpRule
otpRule =
  Reader.OtpRule
    { matches:
        { sender: []
        , message: "Your OTP for login to Yatri App is"
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
    Just recents -> pure $ recents{predictionArray =  map (\item -> item{prefixImageUrl = "ny_ic_recent_search,https://assets.juspay.in/nammayatri/images/user/ny_ic_recent_search.png"}) (recents.predictionArray)}
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
    let prediction' = prediction {prefixImageUrl = "ny_ic_recent_search,https://assets.juspay.in/nammayatri/images/user/ny_ic_recent_search.png", locationItemType = Just RECENTS}
      in (if (checkPrediction prediction' predictionArr) 
           then (if length predictionArr == 5 then (fromMaybe [] (deleteAt 5 (cons prediction' predictionArr)))
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

isPreviousVersion :: String -> String -> Boolean 
isPreviousVersion currentVersion previousVersion = numericVersion currentVersion <= numericVersion previousVersion

numericVersion :: String -> Int 
numericVersion versionName = do 
  let versionArray = (DS.split (DS.Pattern ".") versionName)
      majorUpdateIndex = fromMaybe (0) $ INT.fromString $ fromMaybe "NA" $ versionArray !! 0
      minorUpdateIndex = fromMaybe (0) $ INT.fromString $ fromMaybe "NA" $ versionArray !! 1
      patchUpdateIndex = fromMaybe (0) $ INT.fromString $ fromMaybe "NA" $ versionArray !! 2
  (majorUpdateIndex * 100 + minorUpdateIndex * 10 + patchUpdateIndex)
  
getCurrentLocationMarker :: String -> String
getCurrentLocationMarker currentVersion = if isPreviousVersion currentVersion (getPreviousVersion "") then "ic_customer_current_location" else "ny_ic_customer_current_location"

getPreviousVersion :: String -> String 
getPreviousVersion _ = if os == "IOS" then "1.2.5" else "1.2.0"