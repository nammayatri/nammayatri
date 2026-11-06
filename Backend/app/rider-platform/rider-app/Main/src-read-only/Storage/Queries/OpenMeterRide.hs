{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OpenMeterRide (module Storage.Queries.OpenMeterRide, module ReExport) where

import qualified Domain.Types.OpenMeterRide
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.OpenMeterRide as Beam
import Storage.Queries.OpenMeterRideExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.OpenMeterRide.OpenMeterRide -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.OpenMeterRide.OpenMeterRide] -> m ())
createMany = traverse_ create

deleteAllByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteAllByPersonId personId = do deleteWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findAllByPersonIdAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.OpenMeterRide.OpenMeterRideStatus -> m ([Domain.Types.OpenMeterRide.OpenMeterRide]))
findAllByPersonIdAndStatus personId status = do findAllWithKV [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId), Se.Is Beam.status $ Se.Eq status]]

updateEndById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.OpenMeterRide.OpenMeterRideStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong -> Kernel.Types.Common.Meters -> Kernel.Prelude.Maybe Kernel.Types.Common.Meters -> Kernel.Types.Common.Seconds -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Int -> Kernel.Types.Common.Meters -> Kernel.Prelude.Maybe Domain.Types.OpenMeterRide.OpenMeterEndReason -> Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide -> m ())
updateEndById status endTime endLatLong distanceMeters serverLinearDistanceMeters waitingTimeSeconds fare currency encodedPolyline pointsCount estimatedGapMeters endReason id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.endTime endTime,
      Se.Set Beam.endLat ((endLatLong <&> (.lat))),
      Se.Set Beam.endLon ((endLatLong <&> (.lon))),
      Se.Set Beam.distanceMeters distanceMeters,
      Se.Set Beam.serverLinearDistanceMeters serverLinearDistanceMeters,
      Se.Set Beam.waitingTimeSeconds waitingTimeSeconds,
      Se.Set Beam.fare fare,
      Se.Set Beam.currency currency,
      Se.Set Beam.encodedPolyline encodedPolyline,
      Se.Set Beam.pointsCount pointsCount,
      Se.Set Beam.estimatedGapMeters estimatedGapMeters,
      Se.Set Beam.endReason endReason,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateHeartbeatById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Common.Meters -> Kernel.Types.Common.Seconds -> Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide -> m ())
updateHeartbeatById distanceMeters waitingTimeSeconds lastLatLong fare lastHeartbeatAt id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.distanceMeters distanceMeters,
      Se.Set Beam.waitingTimeSeconds waitingTimeSeconds,
      Se.Set Beam.lastLat ((lastLatLong <&> (.lat))),
      Se.Set Beam.lastLon ((lastLatLong <&> (.lon))),
      Se.Set Beam.fare fare,
      Se.Set Beam.lastHeartbeatAt lastHeartbeatAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.OpenMeterRide.OpenMeterRideStatus -> Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide -> m ())
updateStatusById status id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.OpenMeterRide.OpenMeterRide -> m (Maybe Domain.Types.OpenMeterRide.OpenMeterRide))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.OpenMeterRide.OpenMeterRide -> m ())
updateByPrimaryKey (Domain.Types.OpenMeterRide.OpenMeterRide {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currency currency,
      Se.Set Beam.distanceMeters distanceMeters,
      Se.Set Beam.encodedPolyline encodedPolyline,
      Se.Set Beam.endLat ((endLatLong <&> (.lat))),
      Se.Set Beam.endLon ((endLatLong <&> (.lon))),
      Se.Set Beam.endReason endReason,
      Se.Set Beam.endTime endTime,
      Se.Set Beam.estimatedGapMeters estimatedGapMeters,
      Se.Set Beam.fare fare,
      Se.Set Beam.lastHeartbeatAt lastHeartbeatAt,
      Se.Set Beam.lastLat ((lastLatLong <&> (.lat))),
      Se.Set Beam.lastLon ((lastLatLong <&> (.lon))),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.pointsCount pointsCount,
      Se.Set Beam.serverLinearDistanceMeters serverLinearDistanceMeters,
      Se.Set Beam.serverStartedAt serverStartedAt,
      Se.Set Beam.startLat ((startLatLong <&> (.lat))),
      Se.Set Beam.startLon ((startLatLong <&> (.lon))),
      Se.Set Beam.startTime startTime,
      Se.Set Beam.status status,
      Se.Set Beam.tariffSnapshot tariffSnapshot,
      Se.Set Beam.trackingConfig trackingConfig,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.waitingTimeSeconds waitingTimeSeconds
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
