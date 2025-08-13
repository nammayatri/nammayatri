{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.JourneyLeg where

import qualified Data.Aeson
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
import qualified Kernel.Utils.JSON
import qualified Storage.Beam.JourneyLeg as Beam
import qualified Storage.Queries.RouteDetails

instance FromTType' Beam.JourneyLeg Domain.Types.JourneyLeg.JourneyLeg where
  fromTType' (Beam.JourneyLegT {..}) = do
    routeDetails' <- Storage.Queries.RouteDetails.findAllByJourneyLegId id
    pure $
      Just
        Domain.Types.JourneyLeg.JourneyLeg
          { agency = Kernel.External.MultiModal.Interface.Types.MultiModalAgency agencyGtfsId <$> agencyName,
            changedBusesInSequence = changedBusesInSequence,
            distance = Kernel.Types.Common.Distance <$> distance <*> distanceUnit,
            duration = duration,
            endLocation = Kernel.External.Maps.Google.MapsClient.LatLngV2 endLocationLat endLocationLon,
            estimatedMaxFare = estimatedMaxFare,
            estimatedMinFare = estimatedMinFare,
            finalBoardedBusNumber = finalBoardedBusNumber,
            fromArrivalTime = fromArrivalTime,
            fromDepartureTime = fromDepartureTime,
            fromStopDetails = Just $ Kernel.External.MultiModal.Interface.Types.MultiModalStopDetails fromStopCode fromStopPlatformCode fromStopName fromStopGtfsId,
            id = Kernel.Types.Id.Id id,
            isDeleted = isDeleted,
            journeyId = Kernel.Types.Id.Id journeyId,
            legSearchId = legId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            mode = mode,
            osmEntrance = osmEntrance >>= Kernel.Utils.JSON.valueToMaybe,
            osmExit = osmExit >>= Kernel.Utils.JSON.valueToMaybe,
            routeDetails = routeDetails',
            sequenceNumber = sequenceNumber,
            serviceTypes = serviceTypes,
            startLocation = Kernel.External.Maps.Google.MapsClient.LatLngV2 startLocationLat startLocationLon,
            straightLineEntrance = straightLineEntrance >>= Kernel.Utils.JSON.valueToMaybe,
            straightLineExit = straightLineExit >>= Kernel.Utils.JSON.valueToMaybe,
            toArrivalTime = toArrivalTime,
            toDepartureTime = toDepartureTime,
            toStopDetails = Just $ Kernel.External.MultiModal.Interface.Types.MultiModalStopDetails toStopCode toStopPlatformCode toStopName toStopGtfsId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.JourneyLeg Domain.Types.JourneyLeg.JourneyLeg where
  toTType' (Domain.Types.JourneyLeg.JourneyLeg {..}) = do
    Beam.JourneyLegT
      { Beam.agencyGtfsId = agency >>= (.gtfsId),
        Beam.agencyName = agency <&> (.name),
        Beam.changedBusesInSequence = changedBusesInSequence,
        Beam.distance = (.value) <$> distance,
        Beam.distanceUnit = (.unit) <$> distance,
        Beam.duration = duration,
        Beam.endLocationLat = endLocation & (.latitude),
        Beam.endLocationLon = endLocation & (.longitude),
        Beam.estimatedMaxFare = estimatedMaxFare,
        Beam.estimatedMinFare = estimatedMinFare,
        Beam.finalBoardedBusNumber = finalBoardedBusNumber,
        Beam.fromArrivalTime = fromArrivalTime,
        Beam.fromDepartureTime = fromDepartureTime,
        Beam.fromStopCode = fromStopDetails >>= (.stopCode),
        Beam.fromStopGtfsId = (fromStopDetails >>= (.gtfsId)) <&> Domain.Types.FRFSRouteDetails.gtfsIdtoDomainCode,
        Beam.fromStopName = fromStopDetails >>= (.name),
        Beam.fromStopPlatformCode = fromStopDetails >>= (.platformCode),
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isDeleted = isDeleted,
        Beam.journeyId = Kernel.Types.Id.getId journeyId,
        Beam.legId = legSearchId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.mode = mode,
        Beam.osmEntrance = osmEntrance >>= Just . Data.Aeson.toJSON,
        Beam.osmExit = osmExit >>= Just . Data.Aeson.toJSON,
        Beam.sequenceNumber = sequenceNumber,
        Beam.serviceTypes = serviceTypes,
        Beam.startLocationLat = startLocation & (.latitude),
        Beam.startLocationLon = startLocation & (.longitude),
        Beam.straightLineEntrance = straightLineEntrance >>= Just . Data.Aeson.toJSON,
        Beam.straightLineExit = straightLineExit >>= Just . Data.Aeson.toJSON,
        Beam.toArrivalTime = toArrivalTime,
        Beam.toDepartureTime = toDepartureTime,
        Beam.toStopCode = toStopDetails >>= (.stopCode),
        Beam.toStopGtfsId = (toStopDetails >>= (.gtfsId)) <&> Domain.Types.FRFSRouteDetails.gtfsIdtoDomainCode,
        Beam.toStopName = toStopDetails >>= (.name),
        Beam.toStopPlatformCode = toStopDetails >>= (.platformCode),
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
