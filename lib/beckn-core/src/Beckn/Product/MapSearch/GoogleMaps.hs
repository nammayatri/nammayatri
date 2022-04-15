module Beckn.Product.MapSearch.GoogleMaps
  ( GetDistanceResult (..),
    GetDistanceResultInfo (..),
    getDistance,
    getDistances,
  )
where

import qualified Beckn.External.GoogleMaps.Client as GoogleMaps
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Prelude
import Beckn.Tools.Metrics.CoreMetrics (CoreMetrics)
import Beckn.Types.Common hiding (id)
import Beckn.Types.Error
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Common hiding (id)
import GHC.Float

data GetDistanceResult = GetDistanceResult
  { origin :: GoogleMaps.Place,
    destination :: GoogleMaps.Place,
    info :: GetDistanceResultInfo
  }
  deriving (Generic)

data GetDistanceResultInfo = GetDistanceResultInfo
  { distance :: Double,
    duration :: NominalDiffTime,
    duration_in_traffic :: NominalDiffTime,
    status :: Text
  }
  deriving (Generic)

getDistance ::
  ( MonadFlow m,
    CoreMetrics m,
    GoogleMaps.HasGoogleMaps m r c
  ) =>
  Maybe MapSearch.TravelMode ->
  MapSearch.LatLong ->
  MapSearch.LatLong ->
  Maybe UTCTime ->
  m GetDistanceResult
getDistance travelMode origin destination utcDepartureTime =
  getDistances travelMode (origin :| []) (destination :| []) utcDepartureTime >>= \case
    [] -> throwError (InternalError "Empty GoogleMaps.getDistance result.")
    [a] -> return a
    _ -> throwError (InternalError "Exactly one GoogleMaps.getDistance result expected.")

getDistances ::
  ( MonadFlow m,
    CoreMetrics m,
    GoogleMaps.HasGoogleMaps m r c
  ) =>
  Maybe MapSearch.TravelMode ->
  NonEmpty MapSearch.LatLong ->
  NonEmpty MapSearch.LatLong ->
  Maybe UTCTime ->
  m [GetDistanceResult]
getDistances travelMode origins destinations utcDepartureTime = do
  googleMapsUrl <- asks (.googleMapsUrl)
  key <- asks (.googleMapsKey)
  originPlaces <- map latLongToPlace <$> capListAsPerGoogleLimits "origins" origins
  destinationPlaces <- map latLongToPlace <$> capListAsPerGoogleLimits "destinations" destinations
  let departureTime = case utcDepartureTime of
        Nothing -> Just GoogleMaps.Now
        Just time -> Just $ GoogleMaps.FutureTime time
  GoogleMaps.distanceMatrix googleMapsUrl originPlaces destinationPlaces key departureTime mode
    >>= parseDistanceMatrixResp originPlaces destinationPlaces
  where
    mode = mapToMode <$> travelMode

    -- Constraints on Distance matrix API: https://developers.google.com/maps/documentation/distance-matrix/usage-and-billing#other-usage-limits
    capListAsPerGoogleLimits listName inputList = do
      when (length inputList > 25) $
        logWarning ("Capping " <> listName <> " to maximum 25 elements as per Distance matrix API limits")
      return $ take 25 $ toList inputList

latLongToPlace :: MapSearch.LatLong -> GoogleMaps.Place
latLongToPlace MapSearch.LatLong {..} =
  GoogleMaps.Location $ GoogleMaps.LocationS {lat = lat, lng = lon}

mapToMode :: MapSearch.TravelMode -> GoogleMaps.Mode
mapToMode MapSearch.CAR = GoogleMaps.DRIVING
mapToMode MapSearch.MOTORCYCLE = GoogleMaps.DRIVING
mapToMode MapSearch.BICYCLE = GoogleMaps.BICYCLING
mapToMode MapSearch.FOOT = GoogleMaps.WALKING

parseDistanceMatrixResp ::
  (MonadThrow m, MonadIO m, Log m) =>
  [GoogleMaps.Place] ->
  [GoogleMaps.Place] ->
  GoogleMaps.DistanceMatrixResp ->
  m [GetDistanceResult]
parseDistanceMatrixResp origins destinations distanceMatrixResp = do
  mapM buildGetDistanceResult origDestAndElemList
  where
    origDestAndElemList = do
      (orig, row) <- zip origins distanceMatrixResp.rows
      (dest, element) <- zip destinations row.elements
      return (orig, dest, element)

    buildGetDistanceResultInfo element = do
      void $ GoogleMaps.validateResponseStatus element
      distance <- parseDistances element
      duration <- parseDuration element
      durationInTraffic <- parseDurationInTraffic element
      return $ GetDistanceResultInfo distance duration durationInTraffic element.status

    buildGetDistanceResult (orig, dest, element) = do
      info <- buildGetDistanceResultInfo element
      return $ GetDistanceResult orig dest info

parseDistances :: (MonadThrow m, Log m) => GoogleMaps.DistanceMatrixElement -> m Double
parseDistances distanceMatrixElement = do
  distance <-
    distanceMatrixElement.distance
      & fromMaybeM (InternalError "No distance value provided in distance matrix API response")
  pure $ int2Double distance.value / 1000

parseDuration :: (MonadThrow m, Log m) => GoogleMaps.DistanceMatrixElement -> m NominalDiffTime
parseDuration distanceMatrixElement = do
  durationInTraffic <-
    distanceMatrixElement.duration
      & fromMaybeM (InternalError "No duration value provided in distance matrix API response")
  pure $ intToNominalDiffTime durationInTraffic.value

parseDurationInTraffic :: (MonadThrow m, Log m) => GoogleMaps.DistanceMatrixElement -> m NominalDiffTime
parseDurationInTraffic distanceMatrixElement = do
  durationInTraffic <-
    distanceMatrixElement.duration_in_traffic
      & fromMaybeM (InternalError "No duration_in_traffic value provided in distance matrix API response")
  pure $ intToNominalDiffTime durationInTraffic.value
