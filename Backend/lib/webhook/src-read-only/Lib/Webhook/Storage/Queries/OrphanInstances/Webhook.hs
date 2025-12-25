{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Webhook.Storage.Queries.OrphanInstances.Webhook where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Webhook.Storage.Beam.Webhook as Beam
import qualified Lib.Webhook.Types.Webhook

instance FromTType' Beam.Webhook Lib.Webhook.Types.Webhook.Webhook where
  fromTType' (Beam.WebhookT {..}) = do
    pure $
      Just
        Lib.Webhook.Types.Webhook.Webhook
          { batchId = batchId,
            city = city,
            createdAt = createdAt,
            eventName = eventName,
            extMerchantName = extMerchantName,
            id = Kernel.Types.Id.Id id,
            lastTriedAt = lastTriedAt,
            merchantId = merchantId,
            mode = mode,
            responseCode = responseCode,
            responseMessage = responseMessage,
            retryCount = retryCount,
            shortId = Kernel.Types.Id.ShortId shortId,
            status = status,
            webhookData = webhookData,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Webhook Lib.Webhook.Types.Webhook.Webhook where
  toTType' (Lib.Webhook.Types.Webhook.Webhook {..}) = do
    Beam.WebhookT
      { Beam.batchId = batchId,
        Beam.city = city,
        Beam.createdAt = createdAt,
        Beam.eventName = eventName,
        Beam.extMerchantName = extMerchantName,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lastTriedAt = lastTriedAt,
        Beam.merchantId = merchantId,
        Beam.mode = mode,
        Beam.responseCode = responseCode,
        Beam.responseMessage = responseMessage,
        Beam.retryCount = retryCount,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.status = status,
        Beam.webhookData = webhookData,
        Beam.updatedAt = updatedAt
      }
