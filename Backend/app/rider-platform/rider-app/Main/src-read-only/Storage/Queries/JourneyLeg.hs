{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.JourneyLeg where

import qualified Domain.Types.Common
import qualified Domain.Types.Journey
import qualified Domain.Types.JourneyLeg
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Google.MapsClient
import qualified Kernel.External.Maps.Google.MapsClient.Types
import qualified Kernel.External.MultiModal.Interface.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.JourneyLeg as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyLeg.JourneyLeg -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.JourneyLeg.JourneyLeg] -> m ())
createMany = traverse_ create

findAllByJourneyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Journey.Journey -> m [Domain.Types.JourneyLeg.JourneyLeg])
findAllByJourneyId journeyId = do findAllWithKV [Se.Is Beam.journeyId $ Se.Eq (Kernel.Types.Id.getId journeyId)]

findByLegSearchId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.JourneyLeg.JourneyLeg))
findByLegSearchId legSearchId = do findOneWithKV [Se.Is Beam.legId $ Se.Eq legSearchId]

updateAfterEditLocation ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds -> Kernel.Prelude.Maybe Kernel.Types.Common.Distance -> Kernel.External.Maps.Google.MapsClient.Types.LatLngV2 -> Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg -> m ())
updateAfterEditLocation duration distance endLocation id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.duration duration,
      Se.Set Beam.distance ((.value) <$> distance),
      Se.Set Beam.distanceUnit ((.unit) <$> distance),
      Se.Set Beam.endLocationLat (endLocation & (.latitude)),
      Se.Set Beam.endLocationLon (endLocation & (.longitude)),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateDistanceAndDuration ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Types.Common.Distance -> Kernel.Prelude.Maybe Kernel.Types.Common.Seconds -> Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg -> m ())
updateDistanceAndDuration distance duration id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.distance ((.value) <$> distance),
      Se.Set Beam.distanceUnit ((.unit) <$> distance),
      Se.Set Beam.duration duration,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateIsDeleted :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ())
updateIsDeleted isDeleted legSearchId = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.isDeleted isDeleted, Se.Set Beam.updatedAt _now] [Se.Is Beam.legId $ Se.Eq legSearchId]

updateIsSkipped :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ())
updateIsSkipped isSkipped legSearchId = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.isSkipped isSkipped, Se.Set Beam.updatedAt _now] [Se.Is Beam.legId $ Se.Eq legSearchId]

updateLegSearchId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg -> m ())
updateLegSearchId legSearchId id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.legId legSearchId, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateMode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Common.MultimodalTravelMode -> Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg -> m ())
updateMode mode id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.mode mode, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg -> m (Maybe Domain.Types.JourneyLeg.JourneyLeg))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyLeg.JourneyLeg -> m ())
updateByPrimaryKey (Domain.Types.JourneyLeg.JourneyLeg {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.agencyGtfsId (routeDetails >>= (.gtfsId)),
      Se.Set Beam.agencyName (routeDetails >>= (.longName)),
      Se.Set Beam.distance ((.value) <$> distance),
      Se.Set Beam.distanceUnit ((.unit) <$> distance),
      Se.Set Beam.duration duration,
      Se.Set Beam.endLocationLat (endLocation & (.latitude)),
      Se.Set Beam.endLocationLon (endLocation & (.longitude)),
      Se.Set Beam.estimatedMaxFare estimatedMaxFare,
      Se.Set Beam.estimatedMinFare estimatedMinFare,
      Se.Set Beam.fromArrivalTime fromArrivalTime,
      Se.Set Beam.fromDepartureTime fromDepartureTime,
      Se.Set Beam.fromStopCode (fromStopDetails >>= (.stopCode)),
      Se.Set Beam.fromStopGtfsId (fromStopDetails >>= (.gtfsId)),
      Se.Set Beam.fromStopName (fromStopDetails >>= (.name)),
      Se.Set Beam.fromStopPlatformCode (fromStopDetails >>= (.platformCode)),
      Se.Set Beam.isDeleted isDeleted,
      Se.Set Beam.isSkipped isSkipped,
      Se.Set Beam.journeyId (Kernel.Types.Id.getId journeyId),
      Se.Set Beam.legId legSearchId,
      Se.Set Beam.mode mode,
      Se.Set Beam.frequency Nothing,
      Se.Set Beam.routeColorCode (routeDetails >>= (.color)),
      Se.Set Beam.routeColorName (routeDetails >>= (.shortName)),
      Se.Set Beam.routeGtfsId (routeDetails >>= (.gtfsId)),
      Se.Set Beam.routeLongName (routeDetails >>= (.longName)),
      Se.Set Beam.routeShortName (routeDetails >>= (.shortName)),
      Se.Set Beam.sequenceNumber sequenceNumber,
      Se.Set Beam.startLocationLat (startLocation & (.latitude)),
      Se.Set Beam.startLocationLon (startLocation & (.longitude)),
      Se.Set Beam.toArrivalTime toArrivalTime,
      Se.Set Beam.toDepartureTime toDepartureTime,
      Se.Set Beam.toStopCode (toStopDetails >>= (.stopCode)),
      Se.Set Beam.toStopGtfsId (toStopDetails >>= (.gtfsId)),
      Se.Set Beam.toStopName (toStopDetails >>= (.name)),
      Se.Set Beam.toStopPlatformCode (toStopDetails >>= (.platformCode)),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.JourneyLeg Domain.Types.JourneyLeg.JourneyLeg where
  fromTType' (Beam.JourneyLegT {..}) = do
    pure $
      Just
        Domain.Types.JourneyLeg.JourneyLeg
          { agency = Kernel.External.MultiModal.Interface.Types.MultiModalAgency agencyGtfsId <$> agencyName,
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
            routeDetails = Just $ Kernel.External.MultiModal.Interface.Types.MultiModalRouteDetails routeGtfsId routeLongName routeShortName routeColorCode,
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
      { Beam.agencyGtfsId = routeDetails >>= (.gtfsId),
        Beam.agencyName = routeDetails >>= (.longName),
        Beam.distance = (.value) <$> distance,
        Beam.distanceUnit = (.unit) <$> distance,
        Beam.duration = duration,
        Beam.endLocationLat = endLocation & (.latitude),
        Beam.endLocationLon = endLocation & (.longitude),
        Beam.estimatedMaxFare = estimatedMaxFare,
        Beam.estimatedMinFare = estimatedMinFare,
        Beam.fromArrivalTime = fromArrivalTime,
        Beam.fromDepartureTime = fromDepartureTime,
        Beam.fromStopCode = fromStopDetails >>= (.stopCode),
        Beam.fromStopGtfsId = fromStopDetails >>= (.gtfsId),
        Beam.fromStopName = fromStopDetails >>= (.name),
        Beam.fromStopPlatformCode = fromStopDetails >>= (.platformCode),
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isDeleted = isDeleted,
        Beam.isSkipped = isSkipped,
        Beam.journeyId = Kernel.Types.Id.getId journeyId,
        Beam.legId = legSearchId,
        Beam.mode = mode,
        Beam.frequency = Nothing,
        Beam.routeColorCode = routeDetails >>= (.color),
        Beam.routeColorName = routeDetails >>= (.shortName),
        Beam.routeGtfsId = routeDetails >>= (.gtfsId),
        Beam.routeLongName = routeDetails >>= (.longName),
        Beam.routeShortName = routeDetails >>= (.shortName),
        Beam.sequenceNumber = sequenceNumber,
        Beam.startLocationLat = startLocation & (.latitude),
        Beam.startLocationLon = startLocation & (.longitude),
        Beam.toArrivalTime = toArrivalTime,
        Beam.toDepartureTime = toDepartureTime,
        Beam.toStopCode = toStopDetails >>= (.stopCode),
        Beam.toStopGtfsId = toStopDetails >>= (.gtfsId),
        Beam.toStopName = toStopDetails >>= (.name),
        Beam.toStopPlatformCode = toStopDetails >>= (.platformCode),
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
