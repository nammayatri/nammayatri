{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.JourneyLegExtra where

import Domain.Types.FRFSRouteDetails
import Domain.Types.JourneyLeg
import qualified Domain.Types.JourneyLeg as JL
import qualified Domain.Types.RouteDetails as RouteDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.MultiModal.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Common as Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.JourneyLeg as Beam
import Storage.Queries.OrphanInstances.JourneyLeg
import qualified Storage.Queries.RouteDetails as RD

-- Extra code goes here --
create' :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyLeg.JourneyLeg -> m ())
create' = createWithKV

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyLeg.JourneyLeg -> m ())
create journeyLeg = do
  forM_ (JL.routeDetails journeyLeg) $ \routeDetail -> do
    _now <- getCurrentTime
    newId <- Common.generateGUID
    let fromStopDetails' = fromMaybe (MultiModalStopDetails Nothing Nothing Nothing Nothing) (routeDetail.fromStopDetails)
        toStopDetails' = fromMaybe (MultiModalStopDetails Nothing Nothing Nothing Nothing) (routeDetail.toStopDetails)
    let routeDetails =
          RouteDetails.RouteDetails
            { routeGtfsId = routeDetail.gtfsId <&> gtfsIdtoDomainCode,
              id = newId,
              routeLongName = routeDetail.longName,
              routeShortName = routeDetail.shortName,
              routeColorName = routeDetail.shortName,
              routeColorCode = routeDetail.color,
              frequency = routeDetail.frequency,
              journeyLegId = journeyLeg.id,
              agencyGtfsId = routeDetail.gtfsId <&> gtfsIdtoDomainCode,
              agencyName = routeDetail.longName,
              subLegOrder = Just routeDetail.subLegOrder,
              --fromStopDetails:
              fromStopCode = fromStopDetails'.stopCode,
              fromStopName = fromStopDetails'.name,
              fromStopGtfsId = fromStopDetails'.gtfsId <&> gtfsIdtoDomainCode,
              fromStopPlatformCode = fromStopDetails'.platformCode,
              --toStopDetails:
              toStopCode = toStopDetails'.stopCode,
              toStopName = toStopDetails'.name,
              toStopGtfsId = toStopDetails'.gtfsId <&> gtfsIdtoDomainCode,
              toStopPlatformCode = toStopDetails'.platformCode,
              --Times --
              fromArrivalTime = routeDetail.fromArrivalTime,
              fromDepartureTime = routeDetail.fromDepartureTime,
              toArrivalTime = routeDetail.toArrivalTime,
              toDepartureTime = routeDetail.toDepartureTime,
              --startLocation:
              startLocationLat = Just routeDetail.startLocation.latLng.latitude,
              startLocationLon = Just routeDetail.startLocation.latLng.longitude,
              --endLocation:
              endLocationLat = Just routeDetail.endLocation.latLng.latitude,
              endLocationLon = Just routeDetail.endLocation.latLng.longitude,
              merchantId = journeyLeg.merchantId,
              merchantOperatingCityId = journeyLeg.merchantOperatingCityId,
              createdAt = _now,
              updatedAt = _now
            }
    RD.create routeDetails

  create' journeyLeg

upsert :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyLeg.JourneyLeg -> m ())
upsert journeyLeg = do
  findByPrimaryKey journeyLeg.id >>= \case
    Just _ -> updateByPrimaryKey journeyLeg
    Nothing -> create journeyLeg

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyLeg.JourneyLeg -> m ())
updateByPrimaryKey (Domain.Types.JourneyLeg.JourneyLeg {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.distance ((.value) <$> distance),
      Se.Set Beam.distanceUnit ((.unit) <$> distance),
      Se.Set Beam.duration duration,
      Se.Set Beam.endLocationLat (endLocation & (.latitude)),
      Se.Set Beam.endLocationLon (endLocation & (.longitude)),
      Se.Set Beam.estimatedMaxFare estimatedMaxFare,
      Se.Set Beam.estimatedMinFare estimatedMinFare,
      Se.Set Beam.fromArrivalTime fromArrivalTime,
      Se.Set Beam.fromDepartureTime fromDepartureTime,
      Se.Set Beam.fromStopCode (fromStopDetails >>= (.stopCode)),
      Se.Set Beam.fromStopGtfsId ((fromStopDetails >>= (.gtfsId)) <&> Domain.Types.FRFSRouteDetails.gtfsIdtoDomainCode),
      Se.Set Beam.fromStopName (fromStopDetails >>= (.name)),
      Se.Set Beam.fromStopPlatformCode (fromStopDetails >>= (.platformCode)),
      Se.Set Beam.isDeleted isDeleted,
      Se.Set Beam.isSkipped isSkipped,
      Se.Set Beam.journeyId (Kernel.Types.Id.getId journeyId),
      Se.Set Beam.legId legSearchId,
      Se.Set Beam.mode mode,
      Se.Set Beam.sequenceNumber sequenceNumber,
      Se.Set Beam.startLocationLat (startLocation & (.latitude)),
      Se.Set Beam.startLocationLon (startLocation & (.longitude)),
      Se.Set Beam.toArrivalTime toArrivalTime,
      Se.Set Beam.toDepartureTime toDepartureTime,
      Se.Set Beam.toStopCode (toStopDetails >>= (.stopCode)),
      Se.Set Beam.toStopGtfsId ((toStopDetails >>= (.gtfsId)) <&> Domain.Types.FRFSRouteDetails.gtfsIdtoDomainCode),
      Se.Set Beam.toStopName (toStopDetails >>= (.name)),
      Se.Set Beam.toStopPlatformCode (toStopDetails >>= (.platformCode)),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg -> m (Maybe Domain.Types.JourneyLeg.JourneyLeg))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
