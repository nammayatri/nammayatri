{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module SuggestionUtils where

import Data.Map (Map, insert, update, lookup, member, delete, keys, isEmpty, empty, values)
import Data.Tuple.Nested ((/\))
import Engineering.Helpers.Commons (getCurrentUTC, getNewIDWithTag, convertUTCtoISC, compareUTCDate)
import Data.Maybe
import Prelude
import Data.Array(singleton,catMaybes, any, sortWith, reverse, take, filter, (:), length, (!!), fromFoldable, toUnfoldable, snoc, cons, concat, null, head)
import Data.Ord (comparing)
import Screens.Types (LocationListItemState(..),SourceGeoHash, DestinationGeoHash,SuggestionsMap(..), Suggestions(..), Trip(..), LocationItemType(..), HomeScreenState(..), Address, LocationType(..), RecentlySearchedObject(..))
import Helpers.Utils(getDistanceBwCordinates, parseSourceHashArray, toStringJSON, fetchImage, FetchImageFrom(..), differenceOfLocationLists, checkPrediction, updateLocListWithDistance)
import Data.Int(toNumber)
import Storage (getValueToLocalStore, setValueToLocalStore, KeyStore(..), getValueToLocalNativeStore)
import MerchantConfig.Types (SuggestedDestinationAndTripsConfig)
import Data.Function.Uncurried (runFn2, Fn3)
import Data.Argonaut.Core
import Data.Argonaut.Decode.Class as Decode
import Data.Foldable (foldMap, maximumBy)
import Data.Argonaut.Encode.Class as Encode
import Data.Argonaut.Decode.Error
import Data.Either (Either(..))
import Common.Types.App(LazyCheck(..))
import Data.Either(either)
import Data.Function.Uncurried (runFn3)
import Data.Number(fromString)
import ConfigProvider
import Services.API (RideBookingRes(..))
import Resources.Constants (DecodeAddress(..), decodeAddress, getAddressFromBooking)
import Data.Foldable (foldl,maximum)
import Data.Lens((^.))
import Accessor (_contents, _lat, _lon, _toLocation, _otpCode)
import Data.String as DS
import Data.Array as DA
import Engineering.Helpers.GeoHash (encodeGeohash, geohashNeighbours)
import Data.List as DL
-- import Screens.HomeScreen.Transformer (dummyRideAPIEntity)
import Accessor (_vehicleVariant)
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails)
import RemoteConfig (FamousDestination(..), getFamousDestinations)
import Screens.HomeScreen.ScreenData (dummyAddress)
import RemoteConfig as RC
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Debug
import Data.Array (sort, group, head,groupBy)
import Data.Foldable (maximumBy)
import Data.Maybe (Maybe(..))
import Data.Ord (comparing)
import Data.Array.NonEmpty as DAN
import Data.Map as DM
import JBridge as JB
import Data.Function.Uncurried (runFn2)
import Engineering.Helpers.Commons (flowRunner, getNewIDWithTag, liftFlow, os, safeMarginBottom, safeMarginTop, screenHeight, isPreviousVersion, screenWidth, camelCaseToSentenceCase, truncate, getExpiryTime, getDeviceHeight, getScreenPpi, safeMarginTopWithDefault, markPerformance, getValueFromIdMap, updatePushInIdMap, getCurrentUTC, convertUTCtoISC, compareUTCDate, getCurrentUTC, getMarkerCallback)
import Data.Tuple (Tuple(..))
import Mobility.Prelude

foreign import setSuggestionsMapInJson :: Json -> Json
foreign import getSuggestedDestinationsJsonFromLocal :: String -> Json
fetchSuggestionsFromLocal :: String -> Either JsonDecodeError SuggestionsMap
fetchSuggestionsFromLocal key = Decode.decodeJson $ getSuggestedDestinationsJsonFromLocal key
setSuggestionsMap :: SuggestionsMap -> Either JsonDecodeError SuggestionsMap
setSuggestionsMap destinations = Decode.decodeJson $ setSuggestionsMapInJson $ Encode.encodeJson destinations

addOrUpdateSuggestedDestination ::
  SourceGeoHash ->
  LocationListItemState ->
  SuggestionsMap ->
  SuggestedDestinationAndTripsConfig ->
  SuggestionsMap
addOrUpdateSuggestedDestination sourceGeohash destination suggestionsMap config = 
    if member sourceGeohash suggestionsMap
    then update updateSuggestions sourceGeohash suggestionsMap
    else insertSuggestionInMap 
          sourceGeohash 
          { destinationSuggestions :
              (singleton 
                destination{
                    recencyDate = Just $ getCurrentUTC "",
                    frequencyCount = Just 1,
                    locationScore = Just $ calculateScore (toNumber 1) (getCurrentUTC "") config.frequencyWeight,
                    prefixImageUrl = fetchImage FF_ASSET "ny_ic_recent_search",
                    locationItemType = Just SUGGESTED_DESTINATIONS
                  }),
            tripSuggestions : []
          } 
          suggestionsMap 
          config.geohashLimitForMap
    where
      updateSuggestions :: Suggestions -> Maybe Suggestions
      updateSuggestions suggestion = Just $ suggestion {destinationSuggestions = updateDestinations suggestion.destinationSuggestions} 

      updateDestinations ::  (Array LocationListItemState) -> Array LocationListItemState
      updateDestinations destinations = (updateDestination destinations)

      updateDestination :: Array LocationListItemState -> Array LocationListItemState
      updateDestination destinations =
        let
          locationsToExclude = RC.getLocationSuggestionsToExclude $ DS.toLower $ getValueToLocalStore CUSTOMER_LOCATION
          updateExisting :: LocationListItemState -> LocationListItemState
          updateExisting existingDestination = do
            if destination.placeId == existingDestination.placeId
            then existingDestination
                  { frequencyCount = Just $ (fromMaybe 0 existingDestination.frequencyCount) +  1
                  , recencyDate = Just $ (getCurrentUTC "")
                  , locationScore = Just $ calculateScore (toNumber ((fromMaybe 0 existingDestination.frequencyCount) +  1)) (getCurrentUTC "") config.frequencyWeight
                  }
            else existingDestination
                  { locationScore = Just $ calculateScore (toNumber (fromMaybe 0 existingDestination.frequencyCount)) (fromMaybe (getCurrentUTC "") existingDestination.recencyDate) config.frequencyWeight }

          updatedDestinations = DA.mapMaybe (\item -> transformSuggestion locationsToExclude $ updateExisting item) destinations
          destinationExists = any (\destinationItem -> destinationItem.placeId == destination.placeId) destinations
          sortedDestinations = sortDestinationsByScore updatedDestinations
        in
          if destinationExists
          then sortedDestinations
          else  (take (config.locationsToBeStored - 1) sortedDestinations) <> ( singleton destination{recencyDate = (Just $ (getCurrentUTC "")),
                                                      frequencyCount = Just 1,
                                                      locationScore = Just $ calculateScore (toNumber 1) (getCurrentUTC "") config.frequencyWeight,
                                                      prefixImageUrl = fetchImage FF_ASSET "ny_ic_recent_search", 
                                                      locationItemType = Just SUGGESTED_DESTINATIONS
                                                      })

addOrUpdateSuggestedTrips ::
  SourceGeoHash ->
  Trip ->
  Boolean ->
  SuggestionsMap ->
  SuggestedDestinationAndTripsConfig ->
  Boolean ->
  SuggestionsMap
addOrUpdateSuggestedTrips sourceGeohash trip isPastTrip suggestionsMap config isBackFilling =
    if member sourceGeohash suggestionsMap
    then update (\item -> updateSuggestions isBackFilling item) sourceGeohash suggestionsMap
    else insertSuggestionInMap 
          sourceGeohash 
          { destinationSuggestions:[],
            tripSuggestions : 
              (singleton 
                trip { 
                  recencyDate = Just $ getCurrentUTC "",
                  frequencyCount = Just 1,
                  locationScore = Just $ calculateScore (toNumber 1) (getCurrentUTC "") config.frequencyWeight
                })
          } 
          suggestionsMap 
          config.geohashLimitForMap
    where
      updateSuggestions :: Boolean -> Suggestions -> Maybe Suggestions
      updateSuggestions isBackFilling suggestion = Just $ suggestion {tripSuggestions = updateTrip suggestion.tripSuggestions isBackFilling} 

      updateTrip :: Array Trip -> Boolean -> Array Trip
      updateTrip trips isBackFilling =
        let
          locationsToExclude = RC.getLocationSuggestionsToExclude $ DS.toLower $ getValueToLocalStore CUSTOMER_LOCATION
          updateExisting :: Trip -> Trip
          updateExisting existingTrip = do
            if (getDistanceBwCordinates trip.sourceLat trip.sourceLong existingTrip.sourceLat existingTrip.sourceLong) < config.tripDistanceThreshold
            && (getDistanceBwCordinates trip.destLat trip.destLong existingTrip.destLat existingTrip.destLong) < config.tripDistanceThreshold
            && (existingTrip.serviceTierNameV2 == trip.serviceTierNameV2 || isNothing existingTrip.serviceTierNameV2)
            then existingTrip
                  { frequencyCount = Just $ (fromMaybe 0 existingTrip.frequencyCount) +  1
                  , recencyDate = if isPastTrip then existingTrip.recencyDate else Just $ getCurrentUTC ""
                  , locationScore = Just $ calculateScore (toNumber ((fromMaybe 0 existingTrip.frequencyCount) +  1)) (getCurrentUTC "") config.frequencyWeight
                  , serviceTierNameV2 = if isBackFilling || isJust existingTrip.serviceTierNameV2 then existingTrip.serviceTierNameV2 else trip.serviceTierNameV2
                  , vehicleVariant = if isBackFilling || isJust existingTrip.serviceTierNameV2 then existingTrip.vehicleVariant else trip.vehicleVariant
                  }
            else existingTrip
                  { locationScore = Just $ calculateScore (toNumber (fromMaybe 0 existingTrip.frequencyCount)) (fromMaybe (getCurrentUTC "") existingTrip.recencyDate) config.frequencyWeight
                  }
          updatedTrips = DA.mapMaybe (\item -> transformTrip locationsToExclude $ updateExisting item) trips
          validTrips = filter (\trip -> 
              (trip.sourceLat /= trip.destLat || trip.sourceLong /= trip.destLong)
            ) updatedTrips
          tripExists = any (\tripItem -> (getDistanceBwCordinates tripItem.sourceLat tripItem.sourceLong trip.sourceLat trip.sourceLong) < config.tripDistanceThreshold
            && (getDistanceBwCordinates tripItem.destLat tripItem.destLong trip.destLat trip.destLong) < config.tripDistanceThreshold && tripItem.serviceTierNameV2 == trip.serviceTierNameV2) updatedTrips
          sortedTrips = sortTripsByScore $ updateVariantInfo validTrips
        in
          if tripExists
          then sortedTrips
          else (take (config.tripsToBeStored - 1) sortedTrips) 
              <> (singleton 
                    trip
                      { recencyDate = if isPastTrip then trip.recencyDate else Just $ getCurrentUTC "" 
                      , frequencyCount = Just 1
                      , locationScore = Just $ calculateScore (toNumber 1) (getCurrentUTC "") config.frequencyWeight
                      })

getSuggestedRidesAndLocations :: SourceGeoHash -> SuggestionsMap -> Int -> Maybe Suggestions
getSuggestedRidesAndLocations sourceGeohash suggestionsMap geoHashLimit = do
  if (member sourceGeohash suggestionsMap && getValueToLocalStore SOURCE_GEOHASHES /= "__failed" && getValueToLocalStore SOURCE_GEOHASHES /= "(null)") then do
    sourceHashList <- pure $ getValueToLocalStore SOURCE_GEOHASHES
    parsedHashList <- pure $ parseSourceHashArray sourceHashList
    sourceHashListInString <- pure $ toStringJSON (updateSourceGeohash sourceGeohash (take geoHashLimit parsedHashList))
    void $ pure $ setValueToLocalStore SOURCE_GEOHASHES sourceHashListInString
  else do pure unit
  lookup sourceGeohash suggestionsMap

updateSourceGeohash :: SourceGeoHash -> Array SourceGeoHash -> Array SourceGeoHash
updateSourceGeohash sourceHash hashList = sourceHash : (filter (\hash -> hash /= sourceHash) hashList)

calculateScore :: Number -> String -> Number -> Number
calculateScore frequency recencyDate frequencyConfig =
  let
    frequencyWeight = frequencyConfig
    recencyWeight = 1.0 - frequencyWeight
    currentDate = (getCurrentUTC "")
    recencyInSeconds = compareUTCDate currentDate recencyDate
    normalizedFrequency = frequency / (frequency + 1.0)

    normalizedRecency = 1.0 - (toNumber $ (recencyInSeconds / recencyInSeconds + 1))

    score = (frequencyWeight * normalizedFrequency) + (recencyWeight * normalizedRecency)
  in
    score


insertSuggestionInMap :: SourceGeoHash -> Suggestions -> SuggestionsMap -> Int -> SuggestionsMap
insertSuggestionInMap sourceGeohash suggestionItem suggestionsMap geohashLimit = 
  let hashList =  getValueToLocalStore SOURCE_GEOHASHES
      parsedHashList = parseSourceHashArray if (hashList == "__failed" || hashList == "(null)") then "[]" else hashList
      toDelete = length parsedHashList > (geohashLimit - 1)
      updatedHashList = if toDelete then snoc (take (geohashLimit - 1) parsedHashList) sourceGeohash else snoc parsedHashList sourceGeohash
      updatedMap = if toDelete then delete (fromMaybe "" (parsedHashList !! (geohashLimit - 1))) suggestionsMap else suggestionsMap
      _ = setValueToLocalStore SOURCE_GEOHASHES (toStringJSON updatedHashList)

    in insert sourceGeohash suggestionItem updatedMap

getTripsFromCurrLatLng :: Number -> Number -> SuggestedDestinationAndTripsConfig -> SuggestionsMap -> Array Trip
getTripsFromCurrLatLng srcLat srcLng suggestionsConfig suggestionsMap = 
  let encodedSourceHash = runFn3 encodeGeohash srcLat srcLng suggestionsConfig.geohashPrecision
      geohashNeighbors = cons encodedSourceHash $ geohashNeighbours encodedSourceHash
      tripArrWithNeighbors = concat (map (\hash -> (fromMaybe dummySuggestionsObject (getSuggestedRidesAndLocations hash suggestionsMap suggestionsConfig.geohashLimitForMap)).tripSuggestions) geohashNeighbors)
      sortedTripList = take 30 (reverse (sortWith (\trip -> fromMaybe 0.0 trip.locationScore) tripArrWithNeighbors))
  in sortedTripList

getDestinationsFromCurrLatLng :: Number -> Number -> Array LocationListItemState -> Array LocationListItemState -> Array LocationListItemState
getDestinationsFromCurrLatLng srcLat srcLng savedLocations recentSearches = 
  let globalConfig = getAppConfig appConfig
      suggestionsConfig = globalConfig.suggestedTripsAndLocationConfig
      encodedSourceHash = getGeoHash srcLat srcLng suggestionsConfig.geohashPrecision
      geohashNeighbors = cons encodedSourceHash $ geohashNeighbours encodedSourceHash
      suggestionsMap = getSuggestionsMapFromLocal FunctionCall
      destinationArrWithNeighbors = concat (map (\hash -> (fromMaybe dummySuggestionsObject (getSuggestedRidesAndLocations hash suggestionsMap suggestionsConfig.geohashLimitForMap)).destinationSuggestions) geohashNeighbors)
      sortedDestinationList = take 30 (reverse (sortWith (\destination -> fromMaybe 0.0 destination.locationScore) destinationArrWithNeighbors))
  in sortedDestinationList

sortDestinationsByScore :: Array LocationListItemState -> Array LocationListItemState
sortDestinationsByScore destinations = reverse (sortWith (\destination -> fromMaybe 0.0 destination.locationScore) destinations)

sortTripsByScore :: Array Trip -> Array Trip
sortTripsByScore trips = reverse (sortWith (\trip -> fromMaybe 0.0 trip.locationScore) trips)

getSuggestionsMapFromLocal :: LazyCheck -> SuggestionsMap 
getSuggestionsMapFromLocal lazycheck =
  either (\err -> empty) (\val -> val) (fetchSuggestionsFromLocal (show SUGGESTIONS_MAP))

dummySuggestionsObject :: Suggestions
dummySuggestionsObject = {
  destinationSuggestions : [],
  tripSuggestions : []
}

getGeoHash :: Number -> Number -> Int -> String
getGeoHash latitude longitude precision = 
  let lat = if latitude /= 0.0 then latitude else lastKnowLat
      long = if longitude /= 0.0 then longitude else lastKnowLong
  in runFn3 encodeGeohash lat long precision
    where
      lastKnowLat = (fromMaybe 0.0 $ fromString $ getValueToLocalNativeStore LAST_KNOWN_LAT)
      lastKnowLong = (fromMaybe 0.0 $ fromString $ getValueToLocalNativeStore LAST_KNOWN_LON)

rideListToTripsTransformer :: Array RideBookingRes -> Array Trip
rideListToTripsTransformer listRes =
    map transformBooking (sortByCreatedAtDesc listRes)
  where
    sortByCreatedAtDesc = reverse <<< sortWith (\(RideBookingRes ride) -> ride.createdAt)

    transformBooking :: RideBookingRes -> Trip
    transformBooking (RideBookingRes ride) =
      let
       sourceAddressTransformed = getAddressFromBooking ride.fromLocation
       toLocationTransformed = fromMaybe dummyBookingDetails (((ride.bookingDetails)^._contents)^._toLocation)
       destinationAddressTransformed = getAddressFromBooking $ toLocationTransformed
       in {  sourceLat : (ride.fromLocation)^._lat,
             sourceLong : (ride.fromLocation)^._lon,
             destLat : toLocationTransformed^._lat,
             destLong : toLocationTransformed^._lon,
             recencyDate : Just  (fromMaybe ride.createdAt ride.rideStartTime),
             source :  decodeAddress (Booking ride.fromLocation),
             destination : decodeAddress (Booking (fromMaybe dummyBookingDetails (ride.bookingDetails ^._contents^._toLocation))),
             sourceAddress : sourceAddressTransformed,
             destinationAddress : destinationAddressTransformed,
             isSpecialZone : (null ride.rideList || isJust (ride.bookingDetails ^._contents^._otpCode)),
             locationScore : Nothing,
             frequencyCount : Nothing,
             vehicleVariant : case (head ride.rideList) of
                                Just rideEntity -> Just $ rideEntity^._vehicleVariant
                                Nothing -> Nothing,
             serviceTierNameV2 : correctServiceTierName ride.serviceTierName
         }

updateMapWithPastTrips :: Array Trip -> HomeScreenState -> SuggestionsMap
updateMapWithPastTrips trips state = foldl updateMap (getSuggestionsMapFromLocal FunctionCall) trips
  where
    config = state.data.config.suggestedTripsAndLocationConfig
    geohashPrecision = config.geohashPrecision

    encodeTripGeohash :: Trip -> Int -> String
    encodeTripGeohash trip precision = runFn3 encodeGeohash trip.sourceLat trip.sourceLong precision

    updateMap :: SuggestionsMap -> Trip -> SuggestionsMap
    updateMap suggestionsMap trip =
      let geohash = encodeTripGeohash trip geohashPrecision
      in addOrUpdateSuggestedTrips geohash trip true suggestionsMap config true

getLocationTitle :: String -> String
getLocationTitle singleLineAddress = 
  maybe "" identity $ DA.head $ DS.split (DS.Pattern ",") singleLineAddress

getLocationSubTitle :: String -> String
getLocationSubTitle singleLineAddress = 
  (DS.drop ((fromMaybe 0 (DS.indexOf (DS.Pattern ",") (singleLineAddress))) + 2) (singleLineAddress))

getLocationFromTrip :: LocationType -> Trip -> Number -> Number -> LocationListItemState
getLocationFromTrip locationType trip sourceLat sourceLong = 
  let lat = if locationType == Destination then trip.destLat else trip.sourceLat
      lon = if locationType == Destination then trip.destLong else trip.sourceLong
      fullAddress = if locationType == Destination then trip.destinationAddress else trip.sourceAddress
      singleLineAddress = if locationType == Destination then trip.destination else trip.source
      distanceFromSource = getDistanceBwCordinates lat lon sourceLat sourceLong
  in 
    { prefixImageUrl : fetchImage FF_ASSET "ny_ic_recent_search"
    , postfixImageUrl : ""
    , postfixImageVisibility : true
    , title : getLocationTitle singleLineAddress
    , subTitle : getLocationSubTitle singleLineAddress
    , placeId : Nothing
    , lat : Just lat
    , lon : Just lon
    , description : singleLineAddress
    , tag : ""
    , tagType : Nothing
    , cardType : Nothing
    , address : ""
    , tagName : ""
    , isEditEnabled : true
    , savedLocation : ""
    , placeName : ""
    , isClickable : true
    , alpha : 1.0
    , fullAddress : fullAddress
    , locationItemType : Just SUGGESTED_DESTINATIONS
    , distance : Just $ show distanceFromSource
    , showDistance : Just true
    , actualDistance : Nothing
    , frequencyCount : trip.frequencyCount
    , recencyDate : trip.recencyDate
    , locationScore : trip.locationScore
    , dynamicAction : Nothing
    , types : Nothing
    }

transformTrip :: Array String -> Trip -> Maybe Trip 
transformTrip locationsToExclude trip = let 
  shouldInclude = not $ any (\item -> DS.contains (DS.Pattern $ DS.toLower item) $ DS.toLower $ trip.destination) locationsToExclude
  in if shouldInclude then Just $ fixVariantForTrip $ trip { serviceTierNameV2 = correctServiceTierName trip.serviceTierNameV2 } else Nothing

transformSuggestion :: Array String -> LocationListItemState -> Maybe LocationListItemState
transformSuggestion locationsToExclude destination = let 
    shouldInclude = not $ any (\item -> DS.contains (DS.Pattern $ DS.toLower item) $ DS.toLower destination.title) locationsToExclude
    in if shouldInclude then Just destination else Nothing

correctServiceTierName :: Maybe String -> Maybe String
correctServiceTierName serviceTierName =
  case serviceTierName of
    Just "SUV" -> Just "SUV"
    Just "SEDAN" -> Just "Sedan"
    Just "AUTO_RICKSHAW" -> Just "Auto"
    Just "HATCHBACK" -> Just "Hatchback"
    Just "DELIVERY_BIKE" -> Just "Bike Parcel"
    _ -> serviceTierName

removeDuplicateTrips :: Array Trip -> Int -> Array Trip
removeDuplicateTrips trips precision = 
  let validTrips = filter (\trip -> 
          (trip.sourceLat /= trip.destLat || trip.sourceLong /= trip.destLong) && (getDistanceBwCordinates trip.sourceLat trip.sourceLong trip.destLat trip.destLong /= 0.0)
        ) trips
      grouped = DA.groupBy 
        (\trip1 trip2 -> 
          (getGeoHash trip1.destLat trip1.destLong precision) 
          == 
          (getGeoHash trip2.destLat trip2.destLong precision)
          && trip1.serviceTierNameV2 == trip2.serviceTierNameV2
        ) 
        validTrips

      maxScoreTrips = map 
        (maximumBy (comparing (\trip -> trip.locationScore))) 
        grouped
    in 
      catMaybes maxScoreTrips

isPointWithinXDist :: Trip -> Number -> Number -> Number -> Boolean
isPointWithinXDist item lat lon thresholdDist =
  let sourceLat = if lat == 0.0 then fromMaybe 0.0 $ fromString $ getValueToLocalNativeStore LAST_KNOWN_LAT else lat
      sourceLong = if lon == 0.0 then fromMaybe 0.0 $ fromString $ getValueToLocalNativeStore LAST_KNOWN_LON else lon
  in
    getDistanceBwCordinates 
      item.sourceLat 
      item.sourceLong 
      sourceLat
      sourceLong
      <= thresholdDist
      
locationEquality :: LocationListItemState -> LocationListItemState -> Boolean
locationEquality a b = a.lat == b.lat && a.lon == b.lon


getMapValuesArray :: forall k v. Map k v -> Array v
getMapValuesArray = foldMap singleton

type GetSuggestionsObject = {
  savedLocationsWithOtherTag :: Array LocationListItemState,
  recentlySearchedLocations :: Array LocationListItemState,
  suggestionsMap :: SuggestionsMap, 
  trips :: Array Trip, 
  suggestedDestinations :: Array LocationListItemState
}

getHelperLists :: Array LocationListItemState -> RecentlySearchedObject -> HomeScreenState -> Number -> Number -> GetSuggestionsObject
getHelperLists savedLocationResp recentPredictionsObject state lat lon = 
  let suggestionsConfig = state.data.config.suggestedTripsAndLocationConfig
      locationsToExclude = RC.getLocationSuggestionsToExclude $ DS.toLower $ getValueToLocalStore CUSTOMER_LOCATION
      homeWorkImages = [fetchImage FF_ASSET "ny_ic_home_blue", fetchImage FF_ASSET "ny_ic_work_blue"]
      isHomeOrWorkImage = \listItem -> any (_ == listItem.prefixImageUrl) homeWorkImages
      savedLocationWithHomeOrWorkTag = filter isHomeOrWorkImage savedLocationResp
      recentlySearchedLocations = differenceOfLocationLists recentPredictionsObject.predictionArray savedLocationWithHomeOrWorkTag
      savedLocationsWithOtherTag = filter (not <<< isHomeOrWorkImage) savedLocationResp
      suggestionsMap = getSuggestionsMapFromLocal FunctionCall
      currentGeoHash = getGeoHash lat lon suggestionsConfig.geohashPrecision
      geohashNeighbors = DA.cons currentGeoHash $ geohashNeighbours currentGeoHash
      currentGeoHashDestinations = fromMaybe dummySuggestionsObject (getSuggestedRidesAndLocations currentGeoHash suggestionsMap suggestionsConfig.geohashLimitForMap)
      arrWithNeighbors = concat (map (\hash -> (fromMaybe dummySuggestionsObject (getSuggestedRidesAndLocations hash suggestionsMap suggestionsConfig.geohashLimitForMap)).destinationSuggestions) geohashNeighbors)
      tripArrWithNeighbors = concat (map (\hash -> (fromMaybe dummySuggestionsObject (getSuggestedRidesAndLocations hash suggestionsMap suggestionsConfig.geohashLimitForMap)).tripSuggestions) geohashNeighbors)
      sortedDestinationsList = DA.take 30 (DA.reverse (DA.sortWith (\d -> fromMaybe 0.0 d.locationScore) arrWithNeighbors))
      suggestedDestinationsArr = differenceOfLocationLists sortedDestinationsList savedLocationWithHomeOrWorkTag

      allValuesFromMap = concat $ map (\item -> item.tripSuggestions)(getMapValuesArray suggestionsMap)
      sortedValues = DA.sortWith (\d -> fromMaybe 0.0 d.locationScore) allValuesFromMap
      reversedValues = DA.reverse sortedValues
      topValues = DA.take 30 reversedValues
      topTripDestinatiions = map (\item -> getLocationFromTrip Destination item lat lon) topValues
      
      recentSearchesWithoutSuggested =  differenceOfLocationLists recentlySearchedLocations suggestedDestinationsArr
      topTripDestinatiionsWoutSuggested = differenceOfLocationLists (differenceOfLocationLists topTripDestinatiions suggestedDestinationsArr) savedLocationWithHomeOrWorkTag
      smartSuggestions = if null suggestedDestinationsArr then topTripDestinatiionsWoutSuggested else suggestedDestinationsArr
      sugestedFinalList =  DA.nubByEq locationEquality $ smartSuggestions <> (DA.take (suggestionsConfig.locationsToBeStored - (length smartSuggestions)) recentSearchesWithoutSuggested)
      

      updateFavIcon = 
        map (\item ->
            item { postfixImageUrl =  
                    if not (checkPrediction item savedLocationsWithOtherTag) 
                      then fetchImage FF_ASSET "ny_ic_fav_red"
                      else fetchImage FF_ASSET "ny_ic_fav" 
                }
            ) sugestedFinalList
      suggestedDestinations = updateLocListWithDistance updateFavIcon lat lon true state.data.config.suggestedTripsAndLocationConfig.locationWithinXDist
      sortedTripList =  
          DA.take 30 
            $ filter 
                (\item -> isPointWithinXDist item lat lon state.data.config.suggestedTripsAndLocationConfig.tripWithinXDist) 
            $ DA.reverse 
                (DA.sortWith (\d -> fromMaybe 0.0 d.locationScore) tripArrWithNeighbors)
      validTrips = filter (\trip -> 
              (trip.sourceLat /= trip.destLat || trip.sourceLong /= trip.destLong)
            ) sortedTripList 
      trips = DA.mapMaybe (\item -> transformTrip locationsToExclude item) validTrips
      filteredDestinations = DA.mapMaybe (\item -> transformSuggestion locationsToExclude item) suggestedDestinations
  in {savedLocationsWithOtherTag, recentlySearchedLocations, suggestionsMap, trips, suggestedDestinations : filteredDestinations}

updateVariantInfo :: Array Trip -> Array Trip
updateVariantInfo trips = 
  map fixVariantForTrip trips 

fixVariantForTrip :: Trip -> Trip
fixVariantForTrip trip = 
  if (DA.notElem trip.serviceTierNameV2 [Just "Auto", Just "AUTO_RICKSHAW"]) && trip.vehicleVariant == Just "AUTO_RICKSHAW"
    then trip {vehicleVariant = getVariant trip.serviceTierNameV2 trip.vehicleVariant}
    else trip

getVariant :: Maybe String -> Maybe String -> Maybe String
getVariant serviceTier variant = 
  case serviceTier of
    Just "Auto" -> Just "AUTO_RICKSHAW"
    Just "EV Auto" -> Just "EV_AUTO_RICKSHAW"
    Just "Sedan" -> Just "SEDAN"
    Just "AC Mini" -> Just "HATCHBACK"
    Just "XL Cab" -> Just "SUV"
    Just "Non-AC Mini" -> Just "TAXI"
    Just "Bike Taxi" -> Just "BIKE"
    Just "XL Plus" -> Just "SUV_PLUS"
    Just "2 Wheeler" -> Just "DELIVERY_BIKE"
    Just "Heritage Cab" -> Just "HERITAGE_CAB"
    _ -> variant

fetchFamousDestinations :: LazyCheck -> Array LocationListItemState
fetchFamousDestinations _ = do
  let destinations = getFamousDestinations $ DS.toLower $ getValueToLocalStore CUSTOMER_LOCATION
  map getFamousCityDestination destinations


fetchLocationBasedPreferredVariant :: SourceGeoHash -> SuggestionsMap -> Maybe String
fetchLocationBasedPreferredVariant sourceGeohash suggestionsMap = 
  let 
    mbSuggestion = lookup sourceGeohash suggestionsMap
    suggestion =  fromMaybe dummySuggestionsObject mbSuggestion
    monthInSecond = convertMonthsToSeconds 1
    tripSuggestionsOfLast30days = filter (\x -> (runFn2 JB.differenceBetweenTwoUTC (fromMaybe "" x.recencyDate) (getCurrentUTC "")) <= monthInSecond) suggestion.tripSuggestions
    sortedRecencyGroup = DA.sortBy (\a b -> compare b.recencyDate a.recencyDate) tripSuggestionsOfLast30days
    lastFiveTrips  =  getLast5Trips sortedRecencyGroup dummyFreqencyCount 
    sortedTripList =  ( (DA.sortBy (\(Tuple varient frequency) (Tuple varient1 frequency1) ->compare frequency1 frequency ) lastFiveTrips.serviceTierNameV2))
  in 
    case head sortedTripList of 
      Just (Tuple varient frequency) -> varient 
      Nothing -> Nothing 

getPreferredVariant :: SuggestionsMap -> Maybe String
getPreferredVariant suggestionsMap = 
  let
    suggestionsMap =  getSuggestionsMapFromLocal FunctionCall
    suggestionMapValues =  values suggestionsMap
    suggestions =  suggestionMapValues
    tripSuggestion  = foldl (\acc item -> acc <> item.tripSuggestions) [] suggestions
    monthInSecond = convertMonthsToSeconds 1
    suggestionsOfLast30days = filter (\x -> (runFn2 JB.differenceBetweenTwoUTC (fromMaybe "" x.recencyDate) (getCurrentUTC "")) <= monthInSecond) tripSuggestion
    sortedRecencyGroup =  DA.sortBy (\a b -> compare b.recencyDate a.recencyDate) suggestionsOfLast30days
    lastFiveTrips  = getLast5Trips sortedRecencyGroup dummyFreqencyCount 
    sortedTripList = ( (DA.sortBy (\(Tuple varient frequency) (Tuple varient1 frequency1) ->compare frequency1 frequency ) lastFiveTrips.serviceTierNameV2))
  in 
    case head sortedTripList of 
      Just (Tuple varient frequency) -> varient 
      Nothing -> Nothing



mostFrequentElement :: forall a. (Ord a) => Array a -> Maybe a
mostFrequentElement arr =
  let
    grouped = groupBy (==) arr
    mbLargestGroup = maximumBy (\x y -> compare (DAN.length y) (DAN.length x)) grouped
  in 
    maybe Nothing (\largestGroup -> Just (DAN.head largestGroup)) mbLargestGroup

getFamousCityDestination :: FamousDestination -> LocationListItemState
getFamousCityDestination (FamousDestination destnData)  = 
  { prefixImageUrl : ""
    , postfixImageUrl : destnData.imageUrl
    , postfixImageVisibility : false
    , title : destnData.name
    , subTitle : destnData.address
    , placeId : Nothing
    , lat : Just destnData.lat
    , lon : Just destnData.lon
    , description : destnData.description
    , tag : ""
    , tagType : Nothing
    , cardType : Nothing
    , address : destnData.nameBasedOnLanguage
    , tagName : ""
    , isEditEnabled : true
    , savedLocation : ""
    , placeName : ""
    , isClickable : true
    , alpha : 1.0
    , fullAddress : dummyAddress
    , locationItemType : Just SUGGESTED_DESTINATIONS
    , distance : Nothing
    , showDistance : Nothing
    , actualDistance : Nothing
    , frequencyCount : Nothing
    , recencyDate : Nothing
    , locationScore : Nothing
    , dynamicAction : destnData.dynamic_action
    , types : Nothing
    }

type FrequencyCount = {
  frequency :: Int,
  serviceTierNameV2 :: Array (Tuple (Maybe String) Int)
}

dummyFreqencyCount :: FrequencyCount 
dummyFreqencyCount = {
  frequency: 5, 
  serviceTierNameV2: []
}

findAndUpdate :: Array (Tuple (Maybe String) Int) -> Maybe String -> Int -> Array (Tuple (Maybe String) Int)
findAndUpdate serviceTierList serviceTierNameV2 freq =
  case DA.find (\(Tuple tierName _) -> tierName == serviceTierNameV2) serviceTierList of
    Just (Tuple tierName oldFreq) ->
      map (\(Tuple tn f) -> if tn == serviceTierNameV2 then Tuple tn (f + freq) else Tuple tn f) serviceTierList
    Nothing ->
      serviceTierList <> [Tuple serviceTierNameV2 freq]

getLast5Trips :: Array Trip -> FrequencyCount -> FrequencyCount
getLast5Trips sortedRecencyGroup dummyFrequencyCount = 
  foldl (\acc item -> 
    let
      frequency = fromMaybe 0 item.frequencyCount
      serviceTierNameV2 = item.serviceTierNameV2 
      updatedServiceTierNameV2 = if frequency >= acc.frequency then
        findAndUpdate acc.serviceTierNameV2 serviceTierNameV2 acc.frequency
      else
        findAndUpdate acc.serviceTierNameV2 serviceTierNameV2 frequency

      updatedFrequency = if frequency >= acc.frequency then 0 else acc.frequency - frequency  

    in
      acc { frequency = updatedFrequency, serviceTierNameV2 = updatedServiceTierNameV2 }
  ) dummyFrequencyCount sortedRecencyGroup

