{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Message where

import qualified Data.Time
import qualified Domain.Types.Message
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Message as Beam
import qualified Storage.Queries.Transformers.Message

instance FromTType' Beam.Message Domain.Types.Message.Message where
  fromTType' (Beam.MessageT {..}) = do
    merchantOperatingCityId' <- Storage.Queries.Transformers.Message.getMerchantOperatingCityId merchantId
    messageTranslations' <- Storage.Queries.Transformers.Message.getMsgTranslations id
    pure $
      Just
        Domain.Types.Message.Message
          { _type = messageType,
            alwaysTriggerOnOnboarding = Kernel.Prelude.fromMaybe False alwaysTriggerOnOnboarding,
            createdAt = Data.Time.localTimeToUTC Data.Time.utc createdAt,
            description = description,
            id = Kernel.Types.Id.Id id,
            label = label,
            likeCount = likeCount,
            mediaFiles = Kernel.Types.Id.Id <$> mediaFiles,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            messageTranslations = messageTranslations',
            shareable = Kernel.Prelude.fromMaybe False shareable,
            shortDescription = shortDescription,
            title = title,
            viewCount = viewCount
          }

instance ToTType' Beam.Message Domain.Types.Message.Message where
  toTType' (Domain.Types.Message.Message {..}) = do
    Beam.MessageT
      { Beam.messageType = _type,
        Beam.alwaysTriggerOnOnboarding = Kernel.Prelude.Just alwaysTriggerOnOnboarding,
        Beam.createdAt = Data.Time.utcToLocalTime Data.Time.utc createdAt,
        Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.label = label,
        Beam.likeCount = likeCount,
        Beam.mediaFiles = Kernel.Types.Id.getId <$> mediaFiles,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Prelude.Just (Kernel.Types.Id.getId merchantOperatingCityId),
        Beam.shareable = Kernel.Prelude.Just shareable,
        Beam.shortDescription = shortDescription,
        Beam.title = title,
        Beam.viewCount = viewCount
      }
