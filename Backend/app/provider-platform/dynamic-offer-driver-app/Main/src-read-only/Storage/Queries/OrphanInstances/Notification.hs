{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Notification where

import qualified Domain.Types.Notification
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Notification as Beam
import qualified Storage.Queries.Transformers.Notification

instance FromTType' Beam.Notification Domain.Types.Notification.Notification where
  fromTType' (Beam.NotificationT {..}) = do
    merchantOperatingCityId' <- Storage.Queries.Transformers.Notification.getMerchantOperatingCityId merchantOperatingCityId id driverFeeId
    pure $
      Just
        Domain.Types.Notification.Notification
          { createdAt = createdAt,
            dateCreated = dateCreated,
            description = description,
            driverFeeId = Kernel.Types.Id.Id driverFeeId,
            id = Kernel.Types.Id.Id id,
            juspayProvidedId = juspayProvidedId,
            lastStatusCheckedAt = lastStatusCheckedAt,
            lastUpdated = lastUpdated,
            mandateId = Kernel.Types.Id.Id mandateId,
            merchantOperatingCityId = merchantOperatingCityId',
            notificationType = notificationType,
            providerName = providerName,
            responseCode = responseCode,
            responseMessage = responseMessage,
            shortId = shortId,
            sourceAmount = sourceAmount,
            status = status,
            txnDate = txnDate,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId
          }

instance ToTType' Beam.Notification Domain.Types.Notification.Notification where
  toTType' (Domain.Types.Notification.Notification {..}) = do
    Beam.NotificationT
      { Beam.createdAt = createdAt,
        Beam.dateCreated = dateCreated,
        Beam.description = description,
        Beam.driverFeeId = Kernel.Types.Id.getId driverFeeId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.juspayProvidedId = juspayProvidedId,
        Beam.lastStatusCheckedAt = lastStatusCheckedAt,
        Beam.lastUpdated = lastUpdated,
        Beam.mandateId = Kernel.Types.Id.getId mandateId,
        Beam.merchantOperatingCityId = Kernel.Prelude.Just (Kernel.Types.Id.getId merchantOperatingCityId),
        Beam.notificationType = notificationType,
        Beam.providerName = providerName,
        Beam.responseCode = responseCode,
        Beam.responseMessage = responseMessage,
        Beam.shortId = shortId,
        Beam.sourceAmount = sourceAmount,
        Beam.status = status,
        Beam.txnDate = txnDate,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId
      }
