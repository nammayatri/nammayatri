module SuggestedDestinations where

import Data.Map (Map, insert, update, lookup, member, delete, keys, isEmpty)
import Data.Tuple.Nested ((/\))
import Engineering.Helpers.Commons (clearTimer, getCurrentUTC, getNewIDWithTag, convertUTCtoISC)
import Data.Maybe
import Prelude
import Data.Array(singleton,catMaybes, any, sortWith, reverse, take, filter, (:), length, (!!), fromFoldable, toUnfoldable, snoc)
import Data.Ord (comparing)
import Screens.Types (LocationListItemState(..),SourceGeoHash, DestinationGeoHash,SuggestedDestinations(..), Suggestions(..), Trip(..), LocationItemType(..))
import Helpers.Utils(getDistanceBwCordinates, getDifferenceBetweenDates, parseSourceHashArray, toString)
import Data.Int(toNumber)
import Storage (getValueToLocalStore, setValueToLocalStore, KeyStore(..))

addOrUpdateSuggestedDestination ::
  SourceGeoHash ->
  LocationListItemState ->
  SuggestedDestinations ->
  SuggestedDestinations
addOrUpdateSuggestedDestination sourceGeohash destination suggestedDestinations =
  let
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
                 , recencyDate = Just $ (convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD")
                 , locationScore = Just $ calculateScore (toNumber ((fromMaybe 0 existingDestination.frequencyCount) +  1)) (convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD")
                 }
          else existingDestination

        updatedDestinations = map updateExisting destinations
        destinationExists = any (\d -> d.placeId == destination.placeId) destinations
        sortedDestinations = sortDestinationsByScore updatedDestinations
      in
        if destinationExists
        then sortedDestinations
        else  (take 29 sortedDestinations) <> ( singleton destination{recencyDate = Just $ (convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD"),
                                                     frequencyCount = Just 1,
                                                     locationScore = Just $ calculateScore (toNumber 1) (convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD"),
                                                     prefixImageUrl = "ny_ic_fav,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav.png", 
                                                     locationItemType = Just SUGGESTED_DESTINATIONS
                                                     }) 
  in
    if member sourceGeohash suggestedDestinations
    then update updateSuggestions sourceGeohash suggestedDestinations
    else insertLocationInMap sourceGeohash {destinationSuggestions:(singleton destination{recencyDate = Just $ (convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD"),
                                                     frequencyCount = Just 1,
                                                     locationScore = Just $ calculateScore (toNumber 1) (convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD"),
                                                     prefixImageUrl = "ny_ic_fav,https://assets.juspay.in/nammayatri/images/user/ny_ic_fav.png",
                                                     locationItemType = Just SUGGESTED_DESTINATIONS
                                                     }),
                                                     tripSuggestions : []} suggestedDestinations

insertLocationInMap :: SourceGeoHash -> Suggestions -> SuggestedDestinations -> SuggestedDestinations
insertLocationInMap sourceGeohash suggestionItem suggestedDestinations = 
  let hashList =  getValueToLocalStore SOURCE_GEOHASHES
      parsedHashList = parseSourceHashArray if (hashList == "__failed" || hashList == "(null)") then "[]" else hashList
      toDelete = length parsedHashList > 59
      updatedHashList = if toDelete then snoc (take 59 parsedHashList) sourceGeohash else snoc parsedHashList sourceGeohash
      updatedMap = if toDelete then delete (fromMaybe "" (parsedHashList !! 59)) suggestedDestinations else suggestedDestinations
      _ = setValueToLocalStore SOURCE_GEOHASHES (toString updatedHashList)

    in insert sourceGeohash suggestionItem updatedMap


addOrUpdateSuggestedTrips ::
  SourceGeoHash ->
  Trip ->
  SuggestedDestinations ->
  SuggestedDestinations
addOrUpdateSuggestedTrips sourceGeohash trip suggestedDestinations =
  let
    updateSuggestions :: Suggestions -> Maybe Suggestions
    updateSuggestions suggestion = Just $ suggestion {tripSuggestions = updateTrips suggestion.tripSuggestions} 

    updateTrips ::  (Array Trip) -> Array Trip
    updateTrips trips = (updateTrip trips)

    updateTrip :: Array Trip -> Array Trip
    updateTrip trips =
      let
        updateExisting :: Trip -> Trip
        updateExisting existingDestination =
          if (getDistanceBwCordinates trip.sourceLat trip.sourceLong existingDestination.sourceLat existingDestination.sourceLong) < 0.011
          && (getDistanceBwCordinates trip.destLat trip.destLong existingDestination.destLat existingDestination.destLong) < 0.011
          then existingDestination
                 { frequencyCount = Just $ (fromMaybe 0 existingDestination.frequencyCount) +  1
                 , recencyDate = Just $ (convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD")
                 , locationScore = Just $ calculateScore (toNumber ((fromMaybe 0 existingDestination.frequencyCount) +  1)) (convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD")
                 }
          else existingDestination

        updatedDestinations = map updateExisting trips
        destinationExists = any (\d -> (getDistanceBwCordinates trip.sourceLat trip.sourceLong d.sourceLat d.sourceLong) < 0.011
          && (getDistanceBwCordinates trip.destLat trip.destLong d.destLat d.destLong) < 0.011) trips
        sortedDestinations = sortTripsByScore updatedDestinations
      in
        if destinationExists
        then sortedDestinations
        else  (take 29 sortedDestinations) <> ( singleton trip{recencyDate = Just $ (convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD"),
                                                     frequencyCount = Just 1,
                                                     locationScore = Just $ calculateScore (toNumber 1) (convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD")
                                                     }) 
  in
    if member sourceGeohash suggestedDestinations
    then update updateSuggestions sourceGeohash suggestedDestinations
    else insert sourceGeohash ({destinationSuggestions:[],
                              tripSuggestions : (singleton trip{recencyDate = Just $ (convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD"),
                                                     frequencyCount = Just 1,
                                                     locationScore = Just $ calculateScore (toNumber 1) (convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD")
                                                     })}) suggestedDestinations


getSuggestedDestinations :: SourceGeoHash -> SuggestedDestinations -> Maybe Suggestions
getSuggestedDestinations sourceGeohash suggestedDestinations = do
  if (member sourceGeohash suggestedDestinations && not (getValueToLocalStore SOURCE_GEOHASHES == "__failed" || getValueToLocalStore SOURCE_GEOHASHES == "(null)")) then do
    sourceHashList <- pure $ getValueToLocalStore SOURCE_GEOHASHES
    parsedHashList <- pure $ parseSourceHashArray sourceHashList
    sourceHashListInString <- pure $ toString (updateSourceGeohash sourceGeohash parsedHashList)
    _ <- pure $ setValueToLocalStore SOURCE_GEOHASHES sourceHashListInString
    pure unit
  else do pure unit
  lookup sourceGeohash suggestedDestinations

updateSourceGeohash :: SourceGeoHash -> Array SourceGeoHash -> Array SourceGeoHash
updateSourceGeohash sourceHash hashList = sourceHash : (filter (\hash -> hash /= sourceHash) hashList)

calculateScore :: Number -> String -> Number
calculateScore frequency recencyDate =
  let
    frequencyWeight = 0.7
    recencyWeight = 1.0 - frequencyWeight
    currentDate = (convertUTCtoISC (getCurrentUTC "") "YYYY-MM-DD")
    recencyInDays = getDifferenceBetweenDates currentDate recencyDate

    normalizedFrequency = frequency / (frequency + 1.0)

    normalizedRecency = 1.0 - (toNumber $ (recencyInDays / recencyInDays + 1))

    score = (frequencyWeight * normalizedFrequency) + (recencyWeight * normalizedRecency)
  in
    score

sortDestinationsByScore :: Array LocationListItemState -> Array LocationListItemState
sortDestinationsByScore destinations = reverse (sortWith (\d -> fromMaybe 0.0 d.locationScore) destinations)

sortTripsByScore :: Array Trip -> Array Trip
sortTripsByScore trips = reverse (sortWith (\d -> fromMaybe 0.0 d.locationScore) trips)

fetchKeys :: forall k v. Map k v -> Array k
fetchKeys = fromFoldable <<< keys

addKeys :: SuggestedDestinations -> Array SourceGeoHash
addKeys = fetchKeys
