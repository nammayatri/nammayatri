{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.KnowledgeCenter where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.KnowledgeCenter
import qualified Storage.Beam.KnowledgeCenter as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Domain.Types.MerchantOperatingCity
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.KnowledgeCenter.KnowledgeCenter -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.KnowledgeCenter.KnowledgeCenter] -> m ())
createMany = traverse_ create
deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.KnowledgeCenter.KnowledgeCenter -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
deleteBySopType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteBySopType sopType = do deleteWithKV [Se.Is Beam.sopType $ Se.Eq sopType]
deleteBySopTypeAndMerchantOperatingCityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ())
deleteBySopTypeAndMerchantOperatingCityId sopType merchantOperatingCityId = do deleteWithKV [Se.And [Se.Is Beam.sopType $ Se.Eq sopType,
                                                                                                     Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]
findAllBySopType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Maybe Int -> Maybe Int -> Kernel.Prelude.Text -> m ([Domain.Types.KnowledgeCenter.KnowledgeCenter]))
findAllBySopType limit offset sopType = do findAllWithOptionsKV [Se.Is Beam.sopType $ Se.Eq sopType] (Se.Desc Beam.createdAt) limit offset
findAllBySopTypeAndMerchantOperatingCityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                              (Maybe Int -> Maybe Int -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.KnowledgeCenter.KnowledgeCenter]))
findAllBySopTypeAndMerchantOperatingCityId limit offset sopType merchantOperatingCityId = do findAllWithOptionsKV [Se.And [Se.Is Beam.sopType $ Se.Eq sopType,
                                                                                                                           Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]] (Se.Desc Beam.createdAt) limit offset
findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.KnowledgeCenter.KnowledgeCenter -> m (Maybe Domain.Types.KnowledgeCenter.KnowledgeCenter))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.KnowledgeCenter.KnowledgeCenter -> m (Maybe Domain.Types.KnowledgeCenter.KnowledgeCenter))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.KnowledgeCenter.KnowledgeCenter -> m ())
updateByPrimaryKey (Domain.Types.KnowledgeCenter.KnowledgeCenter {..}) = do {_now <- getCurrentTime;
                                                                             updateWithKV [Se.Set Beam.documentName documentName,
                                                                                           Se.Set Beam.fileType fileType,
                                                                                           Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
                                                                                           Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                           Se.Set Beam.s3Path s3Path,
                                                                                           Se.Set Beam.sopType sopType,
                                                                                           Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



instance FromTType' Beam.KnowledgeCenter Domain.Types.KnowledgeCenter.KnowledgeCenter
    where fromTType' (Beam.KnowledgeCenterT {..}) = do pure $ Just Domain.Types.KnowledgeCenter.KnowledgeCenter{createdAt = createdAt,
                                                                                                                documentName = documentName,
                                                                                                                fileType = fileType,
                                                                                                                id = Kernel.Types.Id.Id id,
                                                                                                                merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                                                merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                                s3Path = s3Path,
                                                                                                                sopType = sopType,
                                                                                                                updatedAt = updatedAt}
instance ToTType' Beam.KnowledgeCenter Domain.Types.KnowledgeCenter.KnowledgeCenter
    where toTType' (Domain.Types.KnowledgeCenter.KnowledgeCenter {..}) = do Beam.KnowledgeCenterT{Beam.createdAt = createdAt,
                                                                                                  Beam.documentName = documentName,
                                                                                                  Beam.fileType = fileType,
                                                                                                  Beam.id = Kernel.Types.Id.getId id,
                                                                                                  Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                                                  Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                                  Beam.s3Path = s3Path,
                                                                                                  Beam.sopType = sopType,
                                                                                                  Beam.updatedAt = updatedAt}



