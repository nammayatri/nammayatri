{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverNudge (module Storage.Queries.DriverNudge, module ReExport) where

import qualified Domain.Types.DriverNudge
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverNudge as Beam
import Storage.Queries.DriverNudgeExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverNudge.DriverNudge -> m ())
create = createWithKV

findActiveByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.DriverNudge.NudgeStatus -> m (Maybe Domain.Types.DriverNudge.DriverNudge))
findActiveByDriverId driverId status = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.status $ Se.Eq status]]

findByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.DriverNudge.DriverNudge])
findByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverNudge.DriverNudge -> m (Maybe Domain.Types.DriverNudge.DriverNudge))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateActualDistance :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters -> Kernel.Types.Id.Id Domain.Types.DriverNudge.DriverNudge -> m ())
updateActualDistance actualDistance id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.actualDistance actualDistance, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverNudge.NudgeStatus -> Kernel.Types.Id.Id Domain.Types.DriverNudge.DriverNudge -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverNudge.DriverNudge -> m (Maybe Domain.Types.DriverNudge.DriverNudge))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverNudge.DriverNudge -> m ())
updateByPrimaryKey (Domain.Types.DriverNudge.DriverNudge {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.actualDistance actualDistance,
      Se.Set Beam.destination (Kernel.Prelude.toJSON <$> destination),
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.estimatedDistance estimatedDistance,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
