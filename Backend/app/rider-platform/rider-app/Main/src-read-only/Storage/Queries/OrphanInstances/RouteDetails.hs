{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.RouteDetails where

import qualified Domain.Types.RouteDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.RouteDetails as Beam

instance FromTType' Beam.RouteDetails Domain.Types.RouteDetails.RouteDetails where
  fromTType' (Beam.RouteDetailsT {..}) = do
    pure $
      Just
        Domain.Types.RouteDetails.RouteDetails
          { agencyGtfsId = agencyGtfsId,
            agencyName = agencyName,
            alternateRouteIds = alternateRouteIds,
            alternateShortNames = alternateShortNames,
            endLocationLat = endLocationLat,
            endLocationLon = endLocationLon,
            frequency = frequency,
            fromArrivalTime = fromArrivalTime,
            fromDepartureTime = fromDepartureTime,
            fromStopCode = fromStopCode,
            fromStopGtfsId = fromStopGtfsId,
            fromStopName = fromStopName,
            fromStopPlatformCode = fromStopPlatformCode,
            id = Kernel.Types.Id.Id id,
            journeyLegId = journeyLegId,
            legEndTime = legEndTime,
            legStartTime = legStartTime,
            routeCode = routeCode,
            routeColorCode = routeColorCode,
            routeColorName = routeColorName,
            routeGtfsId = routeGtfsId,
            routeLongName = routeLongName,
            routeShortName = routeShortName,
            startLocationLat = startLocationLat,
            startLocationLon = startLocationLon,
            subLegOrder = subLegOrder,
            toArrivalTime = toArrivalTime,
            toDepartureTime = toDepartureTime,
            toStopCode = toStopCode,
            toStopGtfsId = toStopGtfsId,
            toStopName = toStopName,
            toStopPlatformCode = toStopPlatformCode,
            trackingStatus = trackingStatus,
            trackingStatusLastUpdatedAt = trackingStatusLastUpdatedAt,
            userBookedRouteShortName = userBookedRouteShortName,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RouteDetails Domain.Types.RouteDetails.RouteDetails where
  toTType' (Domain.Types.RouteDetails.RouteDetails {..}) = do
    Beam.RouteDetailsT
      { Beam.agencyGtfsId = agencyGtfsId,
        Beam.agencyName = agencyName,
        Beam.alternateRouteIds = alternateRouteIds,
        Beam.alternateShortNames = alternateShortNames,
        Beam.endLocationLat = endLocationLat,
        Beam.endLocationLon = endLocationLon,
        Beam.frequency = frequency,
        Beam.fromArrivalTime = fromArrivalTime,
        Beam.fromDepartureTime = fromDepartureTime,
        Beam.fromStopCode = fromStopCode,
        Beam.fromStopGtfsId = fromStopGtfsId,
        Beam.fromStopName = fromStopName,
        Beam.fromStopPlatformCode = fromStopPlatformCode,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.journeyLegId = journeyLegId,
        Beam.legEndTime = legEndTime,
        Beam.legStartTime = legStartTime,
        Beam.routeCode = routeCode,
        Beam.routeColorCode = routeColorCode,
        Beam.routeColorName = routeColorName,
        Beam.routeGtfsId = routeGtfsId,
        Beam.routeLongName = routeLongName,
        Beam.routeShortName = routeShortName,
        Beam.startLocationLat = startLocationLat,
        Beam.startLocationLon = startLocationLon,
        Beam.subLegOrder = subLegOrder,
        Beam.toArrivalTime = toArrivalTime,
        Beam.toDepartureTime = toDepartureTime,
        Beam.toStopCode = toStopCode,
        Beam.toStopGtfsId = toStopGtfsId,
        Beam.toStopName = toStopName,
        Beam.toStopPlatformCode = toStopPlatformCode,
        Beam.trackingStatus = trackingStatus,
        Beam.trackingStatusLastUpdatedAt = trackingStatusLastUpdatedAt,
        Beam.userBookedRouteShortName = userBookedRouteShortName,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
