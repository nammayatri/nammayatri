{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Communication where

import qualified Data.Aeson
import qualified Domain.Types.Communication
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Storage.Beam.Communication as Beam

instance FromTType' Beam.Communication Domain.Types.Communication.Communication where
  fromTType' (Beam.CommunicationT {..}) = do
    pure $
      Just
        Domain.Types.Communication.Communication
          { body = body,
            channels = fromMaybe [] (Kernel.Utils.JSON.valueToMaybe =<< channels),
            contentType = contentType,
            createdAt = createdAt,
            ctaButton = ctaButton >>= Kernel.Utils.JSON.valueToMaybe,
            domain = domain,
            htmlBody = htmlBody,
            id = Kernel.Types.Id.Id id,
            mediaUrls = mediaUrls,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            scheduledAt = scheduledAt,
            senderDisplayName = senderDisplayName,
            senderId = Kernel.Types.Id.Id senderId,
            senderRole = senderRole,
            status = status,
            templateId = templateId,
            templateName = templateName,
            title = title,
            triggerType = triggerType,
            updatedAt = updatedAt,
            variables = variables
          }

instance ToTType' Beam.Communication Domain.Types.Communication.Communication where
  toTType' (Domain.Types.Communication.Communication {..}) = do
    Beam.CommunicationT
      { Beam.body = body,
        Beam.channels = Just $ Data.Aeson.toJSON channels,
        Beam.contentType = contentType,
        Beam.createdAt = createdAt,
        Beam.ctaButton = Data.Aeson.toJSON <$> ctaButton,
        Beam.domain = domain,
        Beam.htmlBody = htmlBody,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.mediaUrls = mediaUrls,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.scheduledAt = scheduledAt,
        Beam.senderDisplayName = senderDisplayName,
        Beam.senderId = Kernel.Types.Id.getId senderId,
        Beam.senderRole = senderRole,
        Beam.status = status,
        Beam.templateId = templateId,
        Beam.templateName = templateName,
        Beam.title = title,
        Beam.triggerType = triggerType,
        Beam.updatedAt = updatedAt,
        Beam.variables = variables
      }
