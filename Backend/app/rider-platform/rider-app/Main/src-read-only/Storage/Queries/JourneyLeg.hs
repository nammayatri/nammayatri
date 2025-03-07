{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.JourneyLeg (module Storage.Queries.JourneyLeg, module ReExport) where

import qualified Domain.Types.Common
import qualified Domain.Types.FRFSRouteDetails
import qualified Domain.Types.Journey
import qualified Domain.Types.JourneyLeg
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Google.MapsClient
import qualified Kernel.External.Maps.Google.MapsClient.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.JourneyLeg as Beam
import Storage.Queries.JourneyLegExtra as ReExport

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
