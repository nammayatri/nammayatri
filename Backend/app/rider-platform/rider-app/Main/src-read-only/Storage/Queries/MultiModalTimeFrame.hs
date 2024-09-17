{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MultiModalTimeFrame where

import qualified Domain.Types.MultiModalTimeFrame
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MultiModalTimeFrame as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MultiModalTimeFrame.MultiModalTimeFrame -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MultiModalTimeFrame.MultiModalTimeFrame] -> m ())
createMany = traverse_ create

findByEndTime :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.TimeOfDay -> m ([Domain.Types.MultiModalTimeFrame.MultiModalTimeFrame]))
findByEndTime endTime = do findAllWithKV [Se.Is Beam.endTime $ Se.Eq endTime]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MultiModalTimeFrame.MultiModalTimeFrame -> m (Maybe Domain.Types.MultiModalTimeFrame.MultiModalTimeFrame))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByStartTime :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.TimeOfDay -> m ([Domain.Types.MultiModalTimeFrame.MultiModalTimeFrame]))
findByStartTime startTime = do findAllWithKV [Se.Is Beam.startTime $ Se.Eq startTime]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MultiModalTimeFrame.MultiModalTimeFrame -> m (Maybe Domain.Types.MultiModalTimeFrame.MultiModalTimeFrame))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MultiModalTimeFrame.MultiModalTimeFrame -> m ())
updateByPrimaryKey (Domain.Types.MultiModalTimeFrame.MultiModalTimeFrame {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.endTime endTime,
      Se.Set Beam.serviceId (Kernel.Types.Id.getId serviceId),
      Se.Set Beam.startTime startTime,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MultiModalTimeFrame Domain.Types.MultiModalTimeFrame.MultiModalTimeFrame where
  fromTType' (Beam.MultiModalTimeFrameT {..}) = do
    pure $
      Just
        Domain.Types.MultiModalTimeFrame.MultiModalTimeFrame
          { endTime = endTime,
            id = Kernel.Types.Id.Id id,
            serviceId = Kernel.Types.Id.Id serviceId,
            startTime = startTime,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MultiModalTimeFrame Domain.Types.MultiModalTimeFrame.MultiModalTimeFrame where
  toTType' (Domain.Types.MultiModalTimeFrame.MultiModalTimeFrame {..}) = do
    Beam.MultiModalTimeFrameT
      { Beam.endTime = endTime,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.serviceId = Kernel.Types.Id.getId serviceId,
        Beam.startTime = startTime,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
