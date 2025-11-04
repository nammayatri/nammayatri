{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverGoHomeRequest (module Storage.Queries.DriverGoHomeRequest, module ReExport) where

import qualified Domain.Types.DriverGoHomeRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverGoHomeRequest as Beam
import Storage.Queries.DriverGoHomeRequestExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverGoHomeRequest.DriverGoHomeRequest -> m ())
create = createWithKV

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverGoHomeRequest.DriverGoHomeRequest -> m (Maybe Domain.Types.DriverGoHomeRequest.DriverGoHomeRequest))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

finishWithStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.DriverGoHomeRequest.DriverGoHomeRequestStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.DriverGoHomeRequest.DriverGoHomeRequest -> m ())
finishWithStatus status mbReachedHome id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.reachedHome mbReachedHome, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateCancellationCount :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.DriverGoHomeRequest.DriverGoHomeRequest -> m ())
updateCancellationCount numCancellation id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.numCancellation numCancellation, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.DriverGoHomeRequest.DriverGoHomeRequest -> m (Maybe Domain.Types.DriverGoHomeRequest.DriverGoHomeRequest))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverGoHomeRequest.DriverGoHomeRequest -> m ())
updateByPrimaryKey (Domain.Types.DriverGoHomeRequest.DriverGoHomeRequest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.lat lat,
      Se.Set Beam.lon lon,
      Se.Set Beam.reachedHome mbReachedHome,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.numCancellation numCancellation,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
