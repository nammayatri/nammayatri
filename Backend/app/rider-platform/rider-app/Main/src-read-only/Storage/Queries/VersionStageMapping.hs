{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VersionStageMapping where

import qualified Domain.Types.VersionStageMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VersionStageMapping as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VersionStageMapping.VersionStageMapping -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VersionStageMapping.VersionStageMapping] -> m ())
createMany = traverse_ create

findAllByVersionId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.VersionStageMapping.VersionStageMapping])
findAllByVersionId versionId = do findAllWithKV [Se.Is Beam.versionId $ Se.Eq versionId]

findByVersionIdAndStageId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.VersionStageMapping.VersionStageMapping))
findByVersionIdAndStageId versionId stageId = do findOneWithKV [Se.And [Se.Is Beam.versionId $ Se.Eq versionId, Se.Is Beam.stageId $ Se.Eq stageId]]

updateFailureById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.VersionStageMapping.Status -> Kernel.Types.Id.Id Domain.Types.VersionStageMapping.VersionStageMapping -> m ())
updateFailureById failureReason status id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.failureReason failureReason, Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateSuccessById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VersionStageMapping.Status -> Kernel.Types.Id.Id Domain.Types.VersionStageMapping.VersionStageMapping -> m ())
updateSuccessById status id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VersionStageMapping.VersionStageMapping -> m (Maybe Domain.Types.VersionStageMapping.VersionStageMapping))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VersionStageMapping.VersionStageMapping -> m ())
updateByPrimaryKey (Domain.Types.VersionStageMapping.VersionStageMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.failureReason failureReason,
      Se.Set Beam.stageData stageData,
      Se.Set Beam.stageId stageId,
      Se.Set Beam.stageName stageName,
      Se.Set Beam.status status,
      Se.Set Beam.versionId versionId,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.VersionStageMapping Domain.Types.VersionStageMapping.VersionStageMapping where
  fromTType' (Beam.VersionStageMappingT {..}) = do
    pure $
      Just
        Domain.Types.VersionStageMapping.VersionStageMapping
          { failureReason = failureReason,
            id = Kernel.Types.Id.Id id,
            stageData = stageData,
            stageId = stageId,
            stageName = stageName,
            status = status,
            versionId = versionId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.VersionStageMapping Domain.Types.VersionStageMapping.VersionStageMapping where
  toTType' (Domain.Types.VersionStageMapping.VersionStageMapping {..}) = do
    Beam.VersionStageMappingT
      { Beam.failureReason = failureReason,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.stageData = stageData,
        Beam.stageId = stageId,
        Beam.stageName = stageName,
        Beam.status = status,
        Beam.versionId = versionId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
