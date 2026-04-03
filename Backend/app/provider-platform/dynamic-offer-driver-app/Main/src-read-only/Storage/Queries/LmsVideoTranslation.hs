{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.LmsVideoTranslation where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.LmsVideoTranslation
import qualified Storage.Beam.LmsVideoTranslation as Beam
import qualified Kernel.Prelude
import qualified Kernel.External.Types
import qualified Kernel.Types.Id
import qualified Domain.Types.LmsModuleVideoInformation
import qualified Storage.Queries.Transformers.ReelsData
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.LmsVideoTranslation.LmsVideoTranslation -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.LmsVideoTranslation.LmsVideoTranslation] -> m ())
createMany = traverse_ create
getAllTranslationsForVideoId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                (Kernel.Types.Id.Id Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation -> m ([Domain.Types.LmsVideoTranslation.LmsVideoTranslation]))
getAllTranslationsForVideoId videoId = do findAllWithKV [Se.Is Beam.videoId $ Se.Eq (Kernel.Types.Id.getId videoId)]
getVideoByLanguageAndVideoId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                (Kernel.Types.Id.Id Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation -> Kernel.External.Types.Language -> m (Maybe Domain.Types.LmsVideoTranslation.LmsVideoTranslation))
getVideoByLanguageAndVideoId videoId language = do findOneWithKV [Se.And [Se.Is Beam.videoId $ Se.Eq (Kernel.Types.Id.getId videoId), Se.Is Beam.language $ Se.Eq language]]
updateCompletedWatchCount :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                             (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation -> Kernel.External.Types.Language -> m ())
updateCompletedWatchCount completedWatchCount videoId language = do {_now <- getCurrentTime;
                                                                     updateOneWithKV [Se.Set Beam.completedWatchCount completedWatchCount, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.videoId $ Se.Eq (Kernel.Types.Id.getId videoId),
                                                                                                                                                                                Se.Is Beam.language $ Se.Eq language]]}
updateViewCount :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                   (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation -> Kernel.External.Types.Language -> m ())
updateViewCount viewCount videoId language = do {_now <- getCurrentTime;
                                                 updateOneWithKV [Se.Set Beam.viewCount viewCount, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.videoId $ Se.Eq (Kernel.Types.Id.getId videoId), Se.Is Beam.language $ Se.Eq language]]}
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.External.Types.Language -> Kernel.Types.Id.Id Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation -> m (Maybe Domain.Types.LmsVideoTranslation.LmsVideoTranslation))
findByPrimaryKey language videoId = do findOneWithKV [Se.And [Se.Is Beam.language $ Se.Eq language, Se.Is Beam.videoId $ Se.Eq (Kernel.Types.Id.getId videoId)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.LmsVideoTranslation.LmsVideoTranslation -> m ())
updateByPrimaryKey (Domain.Types.LmsVideoTranslation.LmsVideoTranslation {..}) = do {_now <- getCurrentTime;
                                                                                     updateWithKV [Se.Set Beam.bottomButtonConfig (Storage.Queries.Transformers.ReelsData.convertBottomButtonConfigToTable bottomButtonConfig),
                                                                                                   Se.Set Beam.completedThresholdInPercentage completedThresholdInPercentage,
                                                                                                   Se.Set Beam.completedWatchCount completedWatchCount,
                                                                                                   Se.Set Beam.description description,
                                                                                                   Se.Set Beam.duration duration,
                                                                                                   Se.Set Beam.sideButtonConfig (Storage.Queries.Transformers.ReelsData.convertSideButtonConfigToTable sideButtonConfig),
                                                                                                   Se.Set Beam.startThresholdInPercentage startThresholdInPercentage,
                                                                                                   Se.Set Beam.thresholdEnabled thresholdEnabled,
                                                                                                   Se.Set Beam.thumbnailImage thumbnailImage,
                                                                                                   Se.Set Beam.title title,
                                                                                                   Se.Set Beam.url url,
                                                                                                   Se.Set Beam.useMerchantOperatingCityDefaultLanguageVideoUrl useMerchantOperatingCityDefaultLanguageVideoUrl,
                                                                                                   Se.Set Beam.viewCount viewCount,
                                                                                                   Se.Set Beam.ytVideoId ytVideoId,
                                                                                                   Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.language $ Se.Eq language, Se.Is Beam.videoId $ Se.Eq (Kernel.Types.Id.getId videoId)]]}



instance FromTType' Beam.LmsVideoTranslation Domain.Types.LmsVideoTranslation.LmsVideoTranslation
    where fromTType' (Beam.LmsVideoTranslationT {..}) = do pure $ Just Domain.Types.LmsVideoTranslation.LmsVideoTranslation{bottomButtonConfig = Storage.Queries.Transformers.ReelsData.getBottomButtonConfigFromTable bottomButtonConfig,
                                                                                                                            completedThresholdInPercentage = completedThresholdInPercentage,
                                                                                                                            completedWatchCount = completedWatchCount,
                                                                                                                            description = description,
                                                                                                                            duration = duration,
                                                                                                                            language = language,
                                                                                                                            sideButtonConfig = Storage.Queries.Transformers.ReelsData.getSideButtonConfigFromTable sideButtonConfig,
                                                                                                                            startThresholdInPercentage = startThresholdInPercentage,
                                                                                                                            thresholdEnabled = thresholdEnabled,
                                                                                                                            thumbnailImage = thumbnailImage,
                                                                                                                            title = title,
                                                                                                                            url = url,
                                                                                                                            useMerchantOperatingCityDefaultLanguageVideoUrl = useMerchantOperatingCityDefaultLanguageVideoUrl,
                                                                                                                            videoId = Kernel.Types.Id.Id videoId,
                                                                                                                            viewCount = viewCount,
                                                                                                                            ytVideoId = ytVideoId,
                                                                                                                            createdAt = createdAt,
                                                                                                                            updatedAt = updatedAt}
instance ToTType' Beam.LmsVideoTranslation Domain.Types.LmsVideoTranslation.LmsVideoTranslation
    where toTType' (Domain.Types.LmsVideoTranslation.LmsVideoTranslation {..}) = do Beam.LmsVideoTranslationT{Beam.bottomButtonConfig = Storage.Queries.Transformers.ReelsData.convertBottomButtonConfigToTable bottomButtonConfig,
                                                                                                              Beam.completedThresholdInPercentage = completedThresholdInPercentage,
                                                                                                              Beam.completedWatchCount = completedWatchCount,
                                                                                                              Beam.description = description,
                                                                                                              Beam.duration = duration,
                                                                                                              Beam.language = language,
                                                                                                              Beam.sideButtonConfig = Storage.Queries.Transformers.ReelsData.convertSideButtonConfigToTable sideButtonConfig,
                                                                                                              Beam.startThresholdInPercentage = startThresholdInPercentage,
                                                                                                              Beam.thresholdEnabled = thresholdEnabled,
                                                                                                              Beam.thumbnailImage = thumbnailImage,
                                                                                                              Beam.title = title,
                                                                                                              Beam.url = url,
                                                                                                              Beam.useMerchantOperatingCityDefaultLanguageVideoUrl = useMerchantOperatingCityDefaultLanguageVideoUrl,
                                                                                                              Beam.videoId = Kernel.Types.Id.getId videoId,
                                                                                                              Beam.viewCount = viewCount,
                                                                                                              Beam.ytVideoId = ytVideoId,
                                                                                                              Beam.createdAt = createdAt,
                                                                                                              Beam.updatedAt = updatedAt}



