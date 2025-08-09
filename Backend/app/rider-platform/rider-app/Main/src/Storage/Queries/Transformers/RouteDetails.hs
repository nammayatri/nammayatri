module Storage.Queries.Transformers.RouteDetails where

import Data.List (sortOn)
import qualified Domain.Types.JourneyRouteDetails as JRD
import qualified Domain.Types.RouteDetails as RD
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.JourneyRouteDetails as QJRD
import qualified Storage.Queries.RouteDetails as QRD

getJourneyRouteDetails :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> Maybe Text -> m [RD.RouteDetails]
getJourneyRouteDetails searchRequestId journeyLegId = do
  routeDetails <-
    case journeyLegId of
      Just jlId -> do
        QRD.findAllByJourneyLegId jlId
          >>= \case
            rd@(_ : _) -> return rd
            -- TODO :: For Backward Compatibility, remove this once we have all the journeyLegId in the database
            [] -> do
              journeyRouteDetails <- QJRD.findAllBySearchId (Id searchRequestId)
              return (transformJourneyRouteDetails <$> journeyRouteDetails)
      -- TODO :: For Backward Compatibility, remove this once we have all the journeyLegId in the database
      Nothing -> do
        journeyRouteDetails <- QJRD.findAllBySearchId (Id searchRequestId)
        return (transformJourneyRouteDetails <$> journeyRouteDetails)
  return $ sortOn (.subLegOrder) routeDetails
  where
    -- TODO :: For Backward Compatibility, remove this once we have all the journeyLegId in the database
    transformJourneyRouteDetails :: JRD.JourneyRouteDetails -> RD.RouteDetails
    transformJourneyRouteDetails JRD.JourneyRouteDetails {..} =
      RD.RouteDetails
        { agencyGtfsId = Nothing,
          agencyName = Nothing,
          endLocationLat = 0.0,
          endLocationLon = 0.0,
          fromArrivalTime = Nothing,
          fromDepartureTime = Nothing,
          fromStopCode = fromStationCode,
          fromStopGtfsId = fromStationCode,
          fromStopName = fromStationCode,
          fromStopPlatformCode = platformNumber,
          id = cast id,
          journeyLegId = fromMaybe "BackwardCompatibility" journeyLegId,
          routeColorCode = lineColorCode,
          routeColorName = lineColor,
          routeGtfsId = routeCode,
          routeCode = routeCode,
          routeLongName = routeLongName,
          routeShortName = routeCode,
          startLocationLat = 0.0,
          startLocationLon = 0.0,
          toArrivalTime = Nothing,
          toDepartureTime = Nothing,
          toStopCode = toStationCode,
          toStopGtfsId = toStationCode,
          toStopName = toStationCode,
          toStopPlatformCode = platformNumber,
          trackingStatus = Nothing,
          journeyStatus, -- TODO :: Remove this field when `trackingStatus` is added to the database and consumed
          alternateShortNames = fromMaybe [] alternateShortNames,
          ..
        }
