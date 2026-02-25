{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SeatLayout where

import qualified Domain.Types.SeatLayout
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SeatLayout as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SeatLayout.SeatLayout -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SeatLayout.SeatLayout] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout -> m (Maybe Domain.Types.SeatLayout.SeatLayout))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout -> m (Maybe Domain.Types.SeatLayout.SeatLayout))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SeatLayout.SeatLayout -> m ())
updateByPrimaryKey (Domain.Types.SeatLayout.SeatLayout {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.columns columns,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.rows rows,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SeatLayout Domain.Types.SeatLayout.SeatLayout where
  fromTType' (Beam.SeatLayoutT {..}) = do
    pure $
      Just
        Domain.Types.SeatLayout.SeatLayout
          { columns = columns,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            rows = rows,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SeatLayout Domain.Types.SeatLayout.SeatLayout where
  toTType' (Domain.Types.SeatLayout.SeatLayout {..}) = do
    Beam.SeatLayoutT
      { Beam.columns = columns,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.rows = rows,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
