{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RouteDetails where

import qualified Domain.Types.JourneyLeg
import qualified Domain.Types.RouteDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RouteDetails as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RouteDetails.RouteDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RouteDetails.RouteDetails] -> m ())
createMany = traverse_ create

findAllByJourneyLegId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg -> m ([Domain.Types.RouteDetails.RouteDetails]))
findAllByJourneyLegId journeyLegId = do findAllWithKV [Se.Is Beam.journeyLegId $ Se.Eq (Kernel.Types.Id.getId journeyLegId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RouteDetails.RouteDetails -> m (Maybe Domain.Types.RouteDetails.RouteDetails))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RouteDetails.RouteDetails -> m ())
updateByPrimaryKey (Domain.Types.RouteDetails.RouteDetails {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.agencyGtfsId agencyGtfsId,
      Se.Set Beam.agencyName agencyName,
      Se.Set Beam.endLocationLat endLocationLat,
      Se.Set Beam.endLocationLon endLocationLon,
      Se.Set Beam.frequency frequency,
      Se.Set Beam.fromArrivalTime fromArrivalTime,
      Se.Set Beam.fromDepartureTime fromDepartureTime,
      Se.Set Beam.fromStopCode fromStopCode,
      Se.Set Beam.fromStopGtfsId fromStopGtfsId,
      Se.Set Beam.fromStopName fromStopName,
      Se.Set Beam.fromStopPlatformCode fromStopPlatformCode,
      Se.Set Beam.journeyLegId (Kernel.Types.Id.getId journeyLegId),
      Se.Set Beam.routeColorCode routeColorCode,
      Se.Set Beam.routeColorName routeColorName,
      Se.Set Beam.routeGtfsId routeGtfsId,
      Se.Set Beam.routeLongName routeLongName,
      Se.Set Beam.routeShortName routeShortName,
      Se.Set Beam.startLocationLat startLocationLat,
      Se.Set Beam.startLocationLon startLocationLon,
      Se.Set Beam.subLegOrder subLegOrder,
      Se.Set Beam.toArrivalTime toArrivalTime,
      Se.Set Beam.toDepartureTime toDepartureTime,
      Se.Set Beam.toStopCode toStopCode,
      Se.Set Beam.toStopGtfsId toStopGtfsId,
      Se.Set Beam.toStopName toStopName,
      Se.Set Beam.toStopPlatformCode toStopPlatformCode,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.RouteDetails Domain.Types.RouteDetails.RouteDetails where
  fromTType' (Beam.RouteDetailsT {..}) = do
    pure $
      Just
        Domain.Types.RouteDetails.RouteDetails
          { agencyGtfsId = agencyGtfsId,
            agencyName = agencyName,
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
            journeyLegId = Kernel.Types.Id.Id journeyLegId,
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
        Beam.journeyLegId = Kernel.Types.Id.getId journeyLegId,
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
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
