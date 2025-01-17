{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VersionStopMapping where

import qualified Domain.Types.VersionStopMapping
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VersionStopMapping as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VersionStopMapping.VersionStopMapping -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VersionStopMapping.VersionStopMapping] -> m ())
createMany = traverse_ create

findByVersionIdAndStageId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.VersionStopMapping.VersionStopMapping))
findByVersionIdAndStageId versionId stageId = do findOneWithKV [Se.And [Se.Is Beam.versionId $ Se.Eq versionId, Se.Is Beam.stageId $ Se.Eq stageId]]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VersionStopMapping.VersionStopMapping -> m (Maybe Domain.Types.VersionStopMapping.VersionStopMapping))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VersionStopMapping.VersionStopMapping -> m ())
updateByPrimaryKey (Domain.Types.VersionStopMapping.VersionStopMapping {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.failureReason failureReason,
      Se.Set Beam.stageData stageData,
      Se.Set Beam.stageId stageId,
      Se.Set Beam.status status,
      Se.Set Beam.versionId versionId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.VersionStopMapping Domain.Types.VersionStopMapping.VersionStopMapping where
  fromTType' (Beam.VersionStopMappingT {..}) = do
    pure $
      Just
        Domain.Types.VersionStopMapping.VersionStopMapping
          { failureReason = failureReason,
            id = Kernel.Types.Id.Id id,
            stageData = stageData,
            stageId = stageId,
            status = status,
            versionId = versionId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.VersionStopMapping Domain.Types.VersionStopMapping.VersionStopMapping where
  toTType' (Domain.Types.VersionStopMapping.VersionStopMapping {..}) = do
    Beam.VersionStopMappingT
      { Beam.failureReason = failureReason,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.stageData = stageData,
        Beam.stageId = stageId,
        Beam.status = status,
        Beam.versionId = versionId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
