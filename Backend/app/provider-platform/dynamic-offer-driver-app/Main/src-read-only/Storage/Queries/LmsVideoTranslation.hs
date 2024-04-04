{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.LmsVideoTranslation where

import qualified Domain.Types.LmsModuleVideoInformation
import qualified Domain.Types.LmsVideoTranslation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.LmsVideoTranslation as Beam
import qualified Storage.Queries.Transformers.ReelsData

create :: KvDbFlow m r => (Domain.Types.LmsVideoTranslation.LmsVideoTranslation -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.LmsVideoTranslation.LmsVideoTranslation] -> m ())
createMany = traverse_ create

getAllTranslationsForVideoId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation -> m [Domain.Types.LmsVideoTranslation.LmsVideoTranslation])
getAllTranslationsForVideoId (Kernel.Types.Id.Id videoId) = do findAllWithKV [Se.Is Beam.videoId $ Se.Eq videoId]

getVideoByLanguageAndVideoId ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation -> Kernel.External.Types.Language -> m (Maybe Domain.Types.LmsVideoTranslation.LmsVideoTranslation))
getVideoByLanguageAndVideoId (Kernel.Types.Id.Id videoId) language = do findOneWithKV [Se.And [Se.Is Beam.videoId $ Se.Eq videoId, Se.Is Beam.language $ Se.Eq language]]

updateCompletedWatchCount :: KvDbFlow m r => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation -> Kernel.External.Types.Language -> m ())
updateCompletedWatchCount completedWatchCount (Kernel.Types.Id.Id videoId) language = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.completedWatchCount completedWatchCount, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.videoId $ Se.Eq videoId, Se.Is Beam.language $ Se.Eq language]]

updateViewCount :: KvDbFlow m r => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation -> Kernel.External.Types.Language -> m ())
updateViewCount viewCount (Kernel.Types.Id.Id videoId) language = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.viewCount viewCount, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.videoId $ Se.Eq videoId, Se.Is Beam.language $ Se.Eq language]]

findByPrimaryKey ::
  KvDbFlow m r =>
  (Kernel.External.Types.Language -> Kernel.Types.Id.Id Domain.Types.LmsModuleVideoInformation.LmsModuleVideoInformation -> m (Maybe Domain.Types.LmsVideoTranslation.LmsVideoTranslation))
findByPrimaryKey language (Kernel.Types.Id.Id videoId) = do findOneWithKV [Se.And [Se.Is Beam.language $ Se.Eq language, Se.Is Beam.videoId $ Se.Eq videoId]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.LmsVideoTranslation.LmsVideoTranslation -> m ())
updateByPrimaryKey (Domain.Types.LmsVideoTranslation.LmsVideoTranslation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bottomButtonConfig (Storage.Queries.Transformers.ReelsData.convertBottomButtonConfigToTable bottomButtonConfig),
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
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.language $ Se.Eq language, Se.Is Beam.videoId $ Se.Eq (Kernel.Types.Id.getId videoId)]]

instance FromTType' Beam.LmsVideoTranslation Domain.Types.LmsVideoTranslation.LmsVideoTranslation where
  fromTType' (Beam.LmsVideoTranslationT {..}) = do
    pure $
      Just
        Domain.Types.LmsVideoTranslation.LmsVideoTranslation
          { bottomButtonConfig = Storage.Queries.Transformers.ReelsData.getBottomButtonConfigFromTable bottomButtonConfig,
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
            updatedAt = updatedAt
          }

instance ToTType' Beam.LmsVideoTranslation Domain.Types.LmsVideoTranslation.LmsVideoTranslation where
  toTType' (Domain.Types.LmsVideoTranslation.LmsVideoTranslation {..}) = do
    Beam.LmsVideoTranslationT
      { Beam.bottomButtonConfig = Storage.Queries.Transformers.ReelsData.convertBottomButtonConfigToTable bottomButtonConfig,
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
        Beam.updatedAt = updatedAt
      }
