{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.EntityInfo where

import qualified Domain.Types.EntityInfo
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.EntityInfo as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.EntityInfo.EntityInfo -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.EntityInfo.EntityInfo] -> m ())
createMany = traverse_ create

deleteAllByEntityIdAndType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ())
deleteAllByEntityIdAndType entityId entityType = do deleteWithKV [Se.And [Se.Is Beam.entityId $ Se.Eq entityId, Se.Is Beam.entityType $ Se.Eq entityType]]

findAllByEntityIdAndType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ([Domain.Types.EntityInfo.EntityInfo]))
findAllByEntityIdAndType entityId entityType = do findAllWithKV [Se.And [Se.Is Beam.entityId $ Se.Eq entityId, Se.Is Beam.entityType $ Se.Eq entityType]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.EntityInfo.EntityInfo))
findByPrimaryKey entityId entityType questionId = do findOneWithKV [Se.And [Se.Is Beam.entityId $ Se.Eq entityId, Se.Is Beam.entityType $ Se.Eq entityType, Se.Is Beam.questionId $ Se.Eq questionId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.EntityInfo.EntityInfo -> m ())
updateByPrimaryKey (Domain.Types.EntityInfo.EntityInfo {..}) = do
  updateWithKV
    [Se.Set Beam.answer answer, Se.Set Beam.question question]
    [ Se.And
        [ Se.Is Beam.entityId $ Se.Eq entityId,
          Se.Is Beam.entityType $ Se.Eq entityType,
          Se.Is Beam.questionId $ Se.Eq questionId
        ]
    ]

instance FromTType' Beam.EntityInfo Domain.Types.EntityInfo.EntityInfo where
  fromTType' (Beam.EntityInfoT {..}) = do
    pure $
      Just
        Domain.Types.EntityInfo.EntityInfo
          { answer = answer,
            entityId = entityId,
            entityType = entityType,
            question = question,
            questionId = questionId
          }

instance ToTType' Beam.EntityInfo Domain.Types.EntityInfo.EntityInfo where
  toTType' (Domain.Types.EntityInfo.EntityInfo {..}) = do
    Beam.EntityInfoT
      { Beam.answer = answer,
        Beam.entityId = entityId,
        Beam.entityType = entityType,
        Beam.question = question,
        Beam.questionId = questionId
      }
