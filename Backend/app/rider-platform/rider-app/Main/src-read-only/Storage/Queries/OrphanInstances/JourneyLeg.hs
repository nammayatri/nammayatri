{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.JourneyLeg where

import qualified Domain.Types.FRFSRouteDetails
import qualified Domain.Types.JourneyLeg
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Google.MapsClient
import qualified Kernel.External.MultiModal.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.JourneyLeg as Beam
import qualified Storage.Queries.RouteDetails
import qualified Storage.Queries.Transformers.RouteDetails

instance FromTType' Beam.JourneyLeg Domain.Types.JourneyLeg.JourneyLeg where
  fromTType' (Beam.JourneyLegT {..}) = do
    routeDetailsList <- Storage.Queries.RouteDetails.findAllByJourneyLegId (Kernel.Types.Id.Id id)
    let agencyDetails = Storage.Queries.Transformers.RouteDetails.getAgencyDetails routeDetailsList
    pure $
      Just
        Domain.Types.JourneyLeg.JourneyLeg
          { agency = agencyDetails,
            distance = Kernel.Types.Common.Distance <$> distance <*> distanceUnit,
            duration = duration,
            endLocation = Kernel.External.Maps.Google.MapsClient.LatLngV2 endLocationLat endLocationLon,
            estimatedMaxFare = estimatedMaxFare,
            estimatedMinFare = estimatedMinFare,
            fromArrivalTime = fromArrivalTime,
            fromDepartureTime = fromDepartureTime,
            fromStopDetails = Just $ Kernel.External.MultiModal.Interface.Types.MultiModalStopDetails fromStopCode fromStopPlatformCode fromStopName fromStopGtfsId,
            id = Kernel.Types.Id.Id id,
            isDeleted = isDeleted,
            isSkipped = isSkipped,
            journeyId = Kernel.Types.Id.Id journeyId,
            legSearchId = legId,
            mode = mode,
            routeDetails = Storage.Queries.Transformers.RouteDetails.getTransformedRouteDetails routeDetailsList,
            sequenceNumber = sequenceNumber,
            startLocation = Kernel.External.Maps.Google.MapsClient.LatLngV2 startLocationLat startLocationLon,
            toArrivalTime = toArrivalTime,
            toDepartureTime = toDepartureTime,
            toStopDetails = Just $ Kernel.External.MultiModal.Interface.Types.MultiModalStopDetails toStopCode toStopPlatformCode toStopName toStopGtfsId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.JourneyLeg Domain.Types.JourneyLeg.JourneyLeg where
  toTType' (Domain.Types.JourneyLeg.JourneyLeg {..}) = do
    Beam.JourneyLegT
      { Beam.distance = (.value) <$> distance,
        Beam.distanceUnit = (.unit) <$> distance,
        Beam.duration = duration,
        Beam.endLocationLat = endLocation & (.latitude),
        Beam.endLocationLon = endLocation & (.longitude),
        Beam.estimatedMaxFare = estimatedMaxFare,
        Beam.estimatedMinFare = estimatedMinFare,
        Beam.fromArrivalTime = fromArrivalTime,
        Beam.fromDepartureTime = fromDepartureTime,
        Beam.fromStopCode = fromStopDetails >>= (.stopCode),
        Beam.fromStopGtfsId = (fromStopDetails >>= (.gtfsId)) <&> Domain.Types.FRFSRouteDetails.gtfsIdtoDomainCode,
        Beam.fromStopName = fromStopDetails >>= (.name),
        Beam.fromStopPlatformCode = fromStopDetails >>= (.platformCode),
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isDeleted = isDeleted,
        Beam.isSkipped = isSkipped,
        Beam.journeyId = Kernel.Types.Id.getId journeyId,
        Beam.legId = legSearchId,
        Beam.mode = mode,
        Beam.sequenceNumber = sequenceNumber,
        Beam.startLocationLat = startLocation & (.latitude),
        Beam.startLocationLon = startLocation & (.longitude),
        Beam.toArrivalTime = toArrivalTime,
        Beam.toDepartureTime = toDepartureTime,
        Beam.toStopCode = toStopDetails >>= (.stopCode),
        Beam.toStopGtfsId = (toStopDetails >>= (.gtfsId)) <&> Domain.Types.FRFSRouteDetails.gtfsIdtoDomainCode,
        Beam.toStopName = toStopDetails >>= (.name),
        Beam.toStopPlatformCode = toStopDetails >>= (.platformCode),
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
