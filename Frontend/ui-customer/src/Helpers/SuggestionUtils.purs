{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module SuggestionUtils where

import Data.Map (Map, insert, update, lookup, member, delete, keys, isEmpty, empty)
import Data.Tuple.Nested ((/\))
import Engineering.Helpers.Commons (getCurrentUTC, getNewIDWithTag, convertUTCtoISC, compareUTCDate)
import Data.Maybe
import Prelude
import Data.Array(singleton,catMaybes, any, sortWith, reverse, take, filter, (:), length, (!!), fromFoldable, toUnfoldable, snoc, cons, concat, null, head)
import Data.Ord (comparing)
import Screens.Types (LocationListItemState(..),SourceGeoHash, DestinationGeoHash,SuggestionsMap(..), Suggestions(..), Trip(..), LocationItemType(..), HomeScreenState(..), Address, LocationType(..))
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
import Data.Foldable (foldl)
import Data.Lens((^.))
import Accessor (_contents, _lat, _lon, _toLocation, _otpCode)
import Data.String as DS
import Data.Array as DA
import Engineering.Helpers.GeoHash (encodeGeohash, geohashNeighbours)
import Screens.HomeScreen.Transformer (dummyRideAPIEntity)
import Accessor (_vehicleVariant)
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails)

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
          updateExisting :: LocationListItemState -> LocationListItemState
          updateExisting existingDestination =
            if destination.placeId == existingDestination.placeId
            then existingDestination
                  { frequencyCount = Just $ (fromMaybe 0 existingDestination.frequencyCount) +  1
                  , recencyDate = Just $ (getCurrentUTC "")
                  , locationScore = Just $ calculateScore (toNumber ((fromMaybe 0 existingDestination.frequencyCount) +  1)) (getCurrentUTC "") config.frequencyWeight
                  }
            else existingDestination
                  { locationScore = Just $ calculateScore (toNumber (fromMaybe 0 existingDestination.frequencyCount)) (fromMaybe (getCurrentUTC "") existingDestination.recencyDate) config.frequencyWeight }

          updatedDestinations = map updateExisting destinations
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
          updatedTrips = map updateExisting trips
          tripExists = any (\tripItem -> (getDistanceBwCordinates tripItem.sourceLat tripItem.sourceLong trip.sourceLat trip.sourceLong) < config.tripDistanceThreshold
            && (getDistanceBwCordinates tripItem.destLat tripItem.destLong trip.destLat trip.destLong) < config.tripDistanceThreshold && tripItem.serviceTierNameV2 == trip.serviceTierNameV2) updatedTrips
          sortedTrips = sortTripsByScore $ updateVariantInfo updatedTrips
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
    , description : ""
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
    }

transformTrip :: Trip -> Trip 
transformTrip trip = fixVariantForTrip $ trip { serviceTierNameV2 = correctServiceTierName trip.serviceTierNameV2 }

correctServiceTierName :: Maybe String -> Maybe String
correctServiceTierName serviceTierName = 
  case serviceTierName of
    Just "SUV" -> Just "SUV"
    Just "SEDAN" -> Just "Sedan"
    Just "AUTO_RICKSHAW" -> Just "Auto"
    Just "HATCHBACK" -> Just "Hatchback"
    _ -> serviceTierName

removeDuplicateTrips :: Array Trip -> Int -> Array Trip
removeDuplicateTrips trips precision = 
  let 
    grouped = DA.groupBy 
      (\trip1 trip2 -> 
        (getGeoHash trip1.destLat trip1.destLong precision) 
        == 
        (getGeoHash trip2.destLat trip2.destLong precision)
        && trip1.serviceTierNameV2 == trip2.serviceTierNameV2
      ) 
      trips

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

getHelperLists savedLocationResp recentPredictionsObject state lat lon = 
  let suggestionsConfig = state.data.config.suggestedTripsAndLocationConfig
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
        
      trips = map (\item -> transformTrip item) sortedTripList
  in {savedLocationsWithOtherTag, recentlySearchedLocations, suggestionsMap, trips, suggestedDestinations}

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
    Just "Sedan" -> Just "SEDAN"
    Just "AC Mini" -> Just "HATCHBACK"
    Just "XL Cab" -> Just "SUV"
    Just "Non-AC Mini" -> Just "TAXI"
    _ -> variant