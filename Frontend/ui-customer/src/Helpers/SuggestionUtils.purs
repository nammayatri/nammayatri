module SuggestionUtils where

import Data.Map (Map, insert, update, lookup, member, delete, keys, isEmpty, empty)
import Data.Tuple.Nested ((/\))
import Engineering.Helpers.Commons (clearTimer, getCurrentUTC, getNewIDWithTag, convertUTCtoISC)
import Data.Maybe
import Prelude
import Data.Array(singleton,catMaybes, any, sortWith, reverse, take, filter, (:), length, (!!), fromFoldable, toUnfoldable, snoc)
import Data.Ord (comparing)
import Screens.Types (LocationListItemState(..),SourceGeoHash, DestinationGeoHash,SuggestionsMap(..), Suggestions(..), Trip(..), LocationItemType(..))
import Helpers.Utils(getDistanceBwCordinates, getDifferenceBetweenDates, parseSourceHashArray, toStringJSON, fetchImage, FetchImageFrom(..))
import Data.Int(toNumber)
import Storage (getValueToLocalStore, setValueToLocalStore, KeyStore(..))
import MerchantConfig.Types (SuggestedDestinationAndTripsConfig)
import Data.Function.Uncurried (runFn2, Fn3)
import Data.Argonaut.Core
import Data.Argonaut.Decode.Class as Decode
import Data.Argonaut.Encode.Class as Encode
import Data.Argonaut.Decode.Error
import Data.Either (Either(..))
import Common.Types.App(LazyCheck(..))
import Data.Either(either)

foreign import encodeGeohash :: Fn3 Number Number Int String
foreign import geohashNeighbours :: String -> Array String
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
          destinationExists = any (\destination -> destination.placeId == destination.placeId) destinations
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
  SuggestionsMap ->
  SuggestedDestinationAndTripsConfig ->
  SuggestionsMap
addOrUpdateSuggestedTrips sourceGeohash trip suggestionsMap config =
    if member sourceGeohash suggestionsMap
    then update updateSuggestions sourceGeohash suggestionsMap
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
      updateSuggestions :: Suggestions -> Maybe Suggestions
      updateSuggestions suggestion = Just $ suggestion {tripSuggestions = updateTrips suggestion.tripSuggestions} 

      updateTrips ::  Array Trip -> Array Trip
      updateTrips trips = updateTrip trips

      updateTrip :: Array Trip -> Array Trip
      updateTrip trips =
        let
          updateExisting :: Trip -> Trip
          updateExisting existingTrip =
            if (getDistanceBwCordinates trip.sourceLat trip.sourceLong existingTrip.sourceLat existingTrip.sourceLong) < config.tripDistanceThreshold
            && (getDistanceBwCordinates trip.destLat trip.destLong existingTrip.destLat existingTrip.destLong) < config.tripDistanceThreshold
            then existingTrip
                  { frequencyCount = Just $ (fromMaybe 0 existingTrip.frequencyCount) +  1
                  , recencyDate = Just $ getCurrentUTC ""
                  , locationScore = Just $ calculateScore (toNumber ((fromMaybe 0 existingTrip.frequencyCount) +  1)) (getCurrentUTC "") config.frequencyWeight
                  }
            else existingTrip
                  { locationScore = Just $ calculateScore (toNumber (fromMaybe 0 existingTrip.frequencyCount)) (fromMaybe (getCurrentUTC "") existingTrip.recencyDate) config.frequencyWeight }

          updatedTrips = map updateExisting trips
          tripExists = any (\trip -> (getDistanceBwCordinates trip.sourceLat trip.sourceLong trip.sourceLat trip.sourceLong) < config.tripDistanceThreshold
            && (getDistanceBwCordinates trip.destLat trip.destLong trip.destLat trip.destLong) < config.tripDistanceThreshold) trips
          sortedTrips = sortTripsByScore updatedTrips
        in
          if tripExists
          then sortedTrips
          else  (take (config.tripsToBeStored - 1) sortedTrips) <> ( singleton trip{recencyDate = Just $ getCurrentUTC "",
                                                      frequencyCount = Just 1,
                                                      locationScore = Just $ calculateScore (toNumber 1) (getCurrentUTC "") config.frequencyWeight
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
    recencyInSeconds = runFn2 getDifferenceBetweenDates currentDate recencyDate
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


sortDestinationsByScore :: Array LocationListItemState -> Array LocationListItemState
sortDestinationsByScore destinations = reverse (sortWith (\destination -> fromMaybe 0.0 destination.locationScore) destinations)

sortTripsByScore :: Array Trip -> Array Trip
sortTripsByScore trips = reverse (sortWith (\trip -> fromMaybe 0.0 trip.locationScore) trips)

getSuggestionsMapFromLocal :: LazyCheck -> SuggestionsMap 
getSuggestionsMapFromLocal lazycheck =
  either (\err -> empty) (\val -> val) (fetchSuggestionsFromLocal (show SUGGESTIONS_MAP))

