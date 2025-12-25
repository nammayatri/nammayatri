{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ReelsData where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ReelsData
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ReelsData as Beam
import Storage.Queries.Transformers.ReelsData

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ReelsData.ReelsData -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.ReelsData.ReelsData] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityIdLanguageAndKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.External.Types.Language -> Kernel.Prelude.Text -> m [Domain.Types.ReelsData.ReelsData])
findAllByMerchantOpCityIdLanguageAndKey merchantOperatingCityId language reelKey = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.language $ Se.Eq language,
          Se.Is Beam.reelKey $ Se.Eq reelKey
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.ReelsData.ReelsData -> m (Maybe Domain.Types.ReelsData.ReelsData))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ReelsData.ReelsData -> m ())
updateByPrimaryKey (Domain.Types.ReelsData.ReelsData {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bottomButtonConfig (convertBottomButtonConfigToTable bottomButtonConfig),
      Se.Set Beam.carouselBigImageUrl carouselBigImageUrl,
      Se.Set Beam.carouselSmallImageUrl carouselSmallImageUrl,
      Se.Set Beam.carouselTextColor carouselTextColor,
      Se.Set Beam.carouselTextString carouselTextString,
      Se.Set Beam.description description,
      Se.Set Beam.language language,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.rank rank,
      Se.Set Beam.reelKey reelKey,
      Se.Set Beam.shareLink shareLink,
      Se.Set Beam.sideButtonConfig (convertSideButtonConfigToTable sideButtonConfig),
      Se.Set Beam.thresholdConfig thresholdConfig,
      Se.Set Beam.thumbnailImageUrl thumbnailImageUrl,
      Se.Set Beam.title title,
      Se.Set Beam.videoUrl videoUrl,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.ReelsData Domain.Types.ReelsData.ReelsData where
  fromTType' (Beam.ReelsDataT {..}) = do
    pure $
      Just
        Domain.Types.ReelsData.ReelsData
          { bottomButtonConfig = getBottomButtonConfigFromTable bottomButtonConfig,
            carouselBigImageUrl = carouselBigImageUrl,
            carouselSmallImageUrl = carouselSmallImageUrl,
            carouselTextColor = carouselTextColor,
            carouselTextString = carouselTextString,
            description = description,
            id = Kernel.Types.Id.Id id,
            language = language,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            rank = rank,
            reelKey = reelKey,
            shareLink = shareLink,
            sideButtonConfig = getSideButtonConfigFromTable sideButtonConfig,
            thresholdConfig = thresholdConfig,
            thumbnailImageUrl = thumbnailImageUrl,
            title = title,
            videoUrl = videoUrl,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ReelsData Domain.Types.ReelsData.ReelsData where
  toTType' (Domain.Types.ReelsData.ReelsData {..}) = do
    Beam.ReelsDataT
      { Beam.bottomButtonConfig = convertBottomButtonConfigToTable bottomButtonConfig,
        Beam.carouselBigImageUrl = carouselBigImageUrl,
        Beam.carouselSmallImageUrl = carouselSmallImageUrl,
        Beam.carouselTextColor = carouselTextColor,
        Beam.carouselTextString = carouselTextString,
        Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.language = language,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.rank = rank,
        Beam.reelKey = reelKey,
        Beam.shareLink = shareLink,
        Beam.sideButtonConfig = convertSideButtonConfigToTable sideButtonConfig,
        Beam.thresholdConfig = thresholdConfig,
        Beam.thumbnailImageUrl = thumbnailImageUrl,
        Beam.title = title,
        Beam.videoUrl = videoUrl,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
