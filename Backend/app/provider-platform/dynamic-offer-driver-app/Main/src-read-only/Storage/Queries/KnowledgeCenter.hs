{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.KnowledgeCenter (module Storage.Queries.KnowledgeCenter, module ReExport) where

import qualified Domain.Types.KnowledgeCenter
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.KnowledgeCenter as Beam
import Storage.Queries.KnowledgeCenterExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.KnowledgeCenter.KnowledgeCenter -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.KnowledgeCenter.KnowledgeCenter] -> m ())
createMany = traverse_ create

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.KnowledgeCenter.KnowledgeCenter -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

deleteBySopType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteBySopType sopType = do deleteWithKV [Se.Is Beam.sopType $ Se.Eq sopType]

findAllBySopType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Maybe Int -> Maybe Int -> Kernel.Prelude.Text -> m ([Domain.Types.KnowledgeCenter.KnowledgeCenter]))
findAllBySopType limit offset sopType = do findAllWithOptionsKV [Se.Is Beam.sopType $ Se.Eq sopType] (Se.Desc Beam.createdAt) limit offset

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.KnowledgeCenter.KnowledgeCenter -> m (Maybe Domain.Types.KnowledgeCenter.KnowledgeCenter))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateSopTypeBySopType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
updateSopTypeBySopType sopType = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.sopType sopType, Se.Set Beam.updatedAt _now] [Se.Is Beam.sopType $ Se.Eq sopType]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.KnowledgeCenter.KnowledgeCenter -> m (Maybe Domain.Types.KnowledgeCenter.KnowledgeCenter))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.KnowledgeCenter.KnowledgeCenter -> m ())
updateByPrimaryKey (Domain.Types.KnowledgeCenter.KnowledgeCenter {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.fileType fileType,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.s3Path s3Path,
      Se.Set Beam.sopType sopType,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
