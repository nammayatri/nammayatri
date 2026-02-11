{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.EntityInfo where

import qualified Domain.Types.EntityInfo
import qualified Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.EntityInfo as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.EntityInfo.EntityInfo -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.EntityInfo.EntityInfo] -> m ())
createMany = traverse_ create

deleteAllByEntityIdAndType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m ())
deleteAllByEntityIdAndType entityId entityType merchantId = do
  deleteWithKV
    [ Se.And
        [ Se.Is Beam.entityId $ Se.Eq entityId,
          Se.Is Beam.entityType $ Se.Eq entityType,
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)
        ]
    ]

findAllByEntityIdAndType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m [Domain.Types.EntityInfo.EntityInfo])
findAllByEntityIdAndType entityId entityType merchantId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.entityId $ Se.Eq entityId,
          Se.Is Beam.entityType $ Se.Eq entityType,
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Prelude.Text -> m (Maybe Domain.Types.EntityInfo.EntityInfo))
findByPrimaryKey entityId entityType merchantId questionId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.entityId $ Se.Eq entityId,
          Se.Is Beam.entityType $ Se.Eq entityType,
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.questionId $ Se.Eq questionId
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.EntityInfo.EntityInfo -> m ())
updateByPrimaryKey (Domain.Types.EntityInfo.EntityInfo {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.answer answer,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.question question,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.entityId $ Se.Eq entityId,
          Se.Is Beam.entityType $ Se.Eq entityType,
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.questionId $ Se.Eq questionId
        ]
    ]

instance FromTType' Beam.EntityInfo Domain.Types.EntityInfo.EntityInfo where
  fromTType' (Beam.EntityInfoT {..}) = do
    pure $
      Just
        Domain.Types.EntityInfo.EntityInfo
          { answer = answer,
            createdAt = createdAt,
            entityId = entityId,
            entityType = entityType,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            question = question,
            questionId = questionId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.EntityInfo Domain.Types.EntityInfo.EntityInfo where
  toTType' (Domain.Types.EntityInfo.EntityInfo {..}) = do
    Beam.EntityInfoT
      { Beam.answer = answer,
        Beam.createdAt = createdAt,
        Beam.entityId = entityId,
        Beam.entityType = entityType,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.question = question,
        Beam.questionId = questionId,
        Beam.updatedAt = updatedAt
      }
