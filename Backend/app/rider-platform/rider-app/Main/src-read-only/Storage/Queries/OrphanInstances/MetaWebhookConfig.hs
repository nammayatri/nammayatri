{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.MetaWebhookConfig where

import qualified Data.Aeson
import qualified Domain.Types.MetaWebhookConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.MetaWebhookConfig as Beam
import Storage.Queries.Transformers.MetaWebhookConfig

instance FromTType' Beam.MetaWebhookConfig Domain.Types.MetaWebhookConfig.MetaWebhookConfig where
  fromTType' (Beam.MetaWebhookConfigT {..}) = do
    botConfig' <- readBotConfig botConfig
    pure $
      Just
        Domain.Types.MetaWebhookConfig.MetaWebhookConfig
          { accessToken = Encrypted accessToken,
            apiVersion = apiVersion,
            appSecret = Encrypted appSecret,
            baseUrl = baseUrl,
            botConfig = botConfig',
            createdAt = createdAt,
            enabled = enabled,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            phoneNumberId = phoneNumberId,
            updatedAt = updatedAt,
            verifyToken = Encrypted verifyToken
          }

instance ToTType' Beam.MetaWebhookConfig Domain.Types.MetaWebhookConfig.MetaWebhookConfig where
  toTType' (Domain.Types.MetaWebhookConfig.MetaWebhookConfig {..}) = do
    Beam.MetaWebhookConfigT
      { Beam.accessToken = accessToken & unEncrypted,
        Beam.apiVersion = apiVersion,
        Beam.appSecret = appSecret & unEncrypted,
        Beam.baseUrl = baseUrl,
        Beam.botConfig = Data.Aeson.toJSON botConfig,
        Beam.createdAt = createdAt,
        Beam.enabled = enabled,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.phoneNumberId = phoneNumberId,
        Beam.updatedAt = updatedAt,
        Beam.verifyToken = verifyToken & unEncrypted
      }
