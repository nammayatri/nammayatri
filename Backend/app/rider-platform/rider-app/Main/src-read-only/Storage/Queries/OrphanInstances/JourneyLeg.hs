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
import qualified Storage.Queries.JourneyLegMapping
import qualified Storage.Queries.RouteDetails
import qualified Storage.Queries.Transformers.JourneyLeg

instance FromTType' Beam.JourneyLeg Domain.Types.JourneyLeg.JourneyLeg where
  fromTType' (Beam.JourneyLegT {..}) = do
    mbJourneyLegMapping <- Storage.Queries.JourneyLegMapping.findByJourneyLegId (Kernel.Types.Id.Id id)
    isDeleted' <- Storage.Queries.Transformers.JourneyLeg.getIsDeleted mbJourneyLegMapping isDeleted
    journeyId' <- Storage.Queries.Transformers.JourneyLeg.getJourneyId mbJourneyLegMapping journeyId
    routeDetails' <- Storage.Queries.RouteDetails.findAllByJourneyLegId Nothing Nothing id
    sequenceNumber' <- Storage.Queries.Transformers.JourneyLeg.getSequenceNumber mbJourneyLegMapping sequenceNumber
    pure $
      Just
        Domain.Types.JourneyLeg.JourneyLeg
          { agency = Kernel.External.MultiModal.Interface.Types.MultiModalAgency agencyGtfsId <$> agencyName,
            busConductorId = busConductorId,
            busDriverId = busDriverId,
            busLocationData = fromMaybe [] (Kernel.Utils.JSON.valueToMaybe =<< busLocationData),
            changedBusesInSequence = changedBusesInSequence,
            distance = Kernel.Types.Common.Distance <$> distance <*> distanceUnit,
            duration = duration,
            endLocation = Kernel.External.Maps.Google.MapsClient.LatLngV2 endLocationLat endLocationLon,
            estimatedMaxFare = estimatedMaxFare,
            estimatedMinFare = estimatedMinFare,
            finalBoardedBusNumber = finalBoardedBusNumber,
            finalBoardedBusNumberSource = finalBoardedBusNumberSource,
            finalBoardedBusServiceTierType = finalBoardedBusServiceTierType,
            finalBoardedDepotNo = finalBoardedDepotNo,
            finalBoardedScheduleNo = finalBoardedScheduleNo,
            finalBoardedWaybillId = finalBoardedWaybillId,
            fromArrivalTime = fromArrivalTime,
            fromDepartureTime = fromDepartureTime,
            fromStopDetails = Just $ Kernel.External.MultiModal.Interface.Types.MultiModalStopDetails fromStopCode fromStopPlatformCode fromStopName fromStopGtfsId,
            groupCode = groupCode,
            id = Kernel.Types.Id.Id id,
            isDeleted = isDeleted',
            journeyId = journeyId',
            legPricingId = legPricingId,
            legSearchId = legId,
            liveVehicleAvailableServiceTypes = serviceTypes,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            mode = mode,
            multimodalSearchRequestId = multimodalSearchRequestId,
            osmEntrance = osmEntrance >>= Kernel.Utils.JSON.valueToMaybe,
            osmExit = osmExit >>= Kernel.Utils.JSON.valueToMaybe,
            providerRouteId = providerRouteId,
            routeDetails = routeDetails',
            sequenceNumber = sequenceNumber',
            startLocation = Kernel.External.Maps.Google.MapsClient.LatLngV2 startLocationLat startLocationLon,
            straightLineEntrance = straightLineEntrance >>= Kernel.Utils.JSON.valueToMaybe,
            straightLineExit = straightLineExit >>= Kernel.Utils.JSON.valueToMaybe,
            toArrivalTime = toArrivalTime,
            toDepartureTime = toDepartureTime,
            toStopDetails = Just $ Kernel.External.MultiModal.Interface.Types.MultiModalStopDetails toStopCode toStopPlatformCode toStopName toStopGtfsId,
            userBookedBusServiceTierType = userBookedBusServiceTierType,
            userPreferredServiceTier = userPreferredServiceTier,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.JourneyLeg Domain.Types.JourneyLeg.JourneyLeg where
  toTType' (Domain.Types.JourneyLeg.JourneyLeg {..}) = do
    Beam.JourneyLegT
      { Beam.agencyGtfsId = agency >>= (.gtfsId),
        Beam.agencyName = agency <&> (.name),
        Beam.busConductorId = busConductorId,
        Beam.busDriverId = busDriverId,
        Beam.busLocationData = Just $ toJSON busLocationData,
        Beam.changedBusesInSequence = changedBusesInSequence,
        Beam.distance = (.value) <$> distance,
        Beam.distanceUnit = (.unit) <$> distance,
        Beam.duration = duration,
        Beam.endLocationLat = endLocation & (.latitude),
        Beam.endLocationLon = endLocation & (.longitude),
        Beam.estimatedMaxFare = estimatedMaxFare,
        Beam.estimatedMinFare = estimatedMinFare,
        Beam.finalBoardedBusNumber = finalBoardedBusNumber,
        Beam.finalBoardedBusNumberSource = finalBoardedBusNumberSource,
        Beam.finalBoardedBusServiceTierType = finalBoardedBusServiceTierType,
        Beam.finalBoardedDepotNo = finalBoardedDepotNo,
        Beam.finalBoardedScheduleNo = finalBoardedScheduleNo,
        Beam.finalBoardedWaybillId = finalBoardedWaybillId,
        Beam.fromArrivalTime = fromArrivalTime,
        Beam.fromDepartureTime = fromDepartureTime,
        Beam.fromStopCode = fromStopDetails >>= (.stopCode),
        Beam.fromStopGtfsId = (fromStopDetails >>= (.gtfsId)) <&> Domain.Types.FRFSRouteDetails.gtfsIdtoDomainCode,
        Beam.fromStopName = fromStopDetails >>= (.name),
        Beam.fromStopPlatformCode = fromStopDetails >>= (.platformCode),
        Beam.groupCode = groupCode,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isDeleted = isDeleted,
        Beam.journeyId = Just $ Kernel.Types.Id.getId journeyId,
        Beam.legPricingId = legPricingId,
        Beam.legId = legSearchId,
        Beam.serviceTypes = liveVehicleAvailableServiceTypes,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.mode = mode,
        Beam.multimodalSearchRequestId = multimodalSearchRequestId,
        Beam.osmEntrance = osmEntrance >>= Just . Data.Aeson.toJSON,
        Beam.osmExit = osmExit >>= Just . Data.Aeson.toJSON,
        Beam.providerRouteId = providerRouteId,
        Beam.sequenceNumber = Just sequenceNumber,
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
        Beam.userBookedBusServiceTierType = userBookedBusServiceTierType,
        Beam.userPreferredServiceTier = userPreferredServiceTier,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
