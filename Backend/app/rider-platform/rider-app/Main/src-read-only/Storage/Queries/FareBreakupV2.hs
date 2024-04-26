{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareBreakupV2 where

import qualified Domain.Types.FareBreakupV2
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FareBreakupV2 as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareBreakupV2.FareBreakupV2 -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FareBreakupV2.FareBreakupV2] -> m ())
createMany = traverse_ create

findAllByEntityIdAndTag :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Domain.Types.FareBreakupV2.FareBreakupV2Tags -> m ([Domain.Types.FareBreakupV2.FareBreakupV2]))
findAllByEntityIdAndTag entityId tag = do findAllWithKV [Se.And [Se.Is Beam.entityId $ Se.Eq entityId, Se.Is Beam.tag $ Se.Eq tag]]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FareBreakupV2.FareBreakupV2 -> m (Maybe Domain.Types.FareBreakupV2.FareBreakupV2))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FareBreakupV2.FareBreakupV2 -> m (Maybe Domain.Types.FareBreakupV2.FareBreakupV2))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareBreakupV2.FareBreakupV2 -> m ())
updateByPrimaryKey (Domain.Types.FareBreakupV2.FareBreakupV2 {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.description description,
      Se.Set Beam.entityId entityId,
      Se.Set Beam.tag tag,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FareBreakupV2 Domain.Types.FareBreakupV2.FareBreakupV2 where
  fromTType' (Beam.FareBreakupV2T {..}) = do
    pure $
      Just
        Domain.Types.FareBreakupV2.FareBreakupV2
          { amount = amount,
            createdAt = createdAt,
            description = description,
            entityId = entityId,
            id = Kernel.Types.Id.Id id,
            tag = tag,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.FareBreakupV2 Domain.Types.FareBreakupV2.FareBreakupV2 where
  toTType' (Domain.Types.FareBreakupV2.FareBreakupV2 {..}) = do
    Beam.FareBreakupV2T
      { Beam.amount = amount,
        Beam.createdAt = createdAt,
        Beam.description = description,
        Beam.entityId = entityId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.tag = tag,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
