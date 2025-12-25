{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.OrphanInstances.PayoutOrder where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PayoutOrder
import qualified Lib.Payment.Storage.Beam.PayoutOrder as Beam

instance FromTType' Beam.PayoutOrder Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder where
  fromTType' (Beam.PayoutOrderT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder
          { accountDetailsType = accountDetailsType,
            amount = Kernel.Types.Common.mkPrice currency price,
            city = city,
            createdAt = createdAt,
            customerEmail = EncryptedHashed (Encrypted customerEmailEncrypted) customerEmailHash,
            customerId = customerId,
            entityIds = entityIds,
            entityName = entityName,
            id = Kernel.Types.Id.Id id,
            lastStatusCheckedAt = lastStatusCheckedAt,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            mobileNo = EncryptedHashed (Encrypted mobileNoEncrypted) mobileNoHash,
            orderId = orderId,
            responseCode = responseCode,
            responseMessage = responseMessage,
            retriedOrderId = retriedOrderId,
            shortId = Kernel.Types.Id.ShortId <$> shortId,
            status = status,
            updatedAt = updatedAt,
            vpa = vpa
          }

instance ToTType' Beam.PayoutOrder Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder where
  toTType' (Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder {..}) = do
    Beam.PayoutOrderT
      { Beam.accountDetailsType = accountDetailsType,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) amount,
        Beam.price = (.amount) amount,
        Beam.city = city,
        Beam.createdAt = createdAt,
        Beam.customerEmailEncrypted = customerEmail & unEncrypted . encrypted,
        Beam.customerEmailHash = customerEmail & hash,
        Beam.customerId = customerId,
        Beam.entityIds = entityIds,
        Beam.entityName = entityName,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lastStatusCheckedAt = lastStatusCheckedAt,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.mobileNoEncrypted = mobileNo & unEncrypted . encrypted,
        Beam.mobileNoHash = mobileNo & hash,
        Beam.orderId = orderId,
        Beam.responseCode = responseCode,
        Beam.responseMessage = responseMessage,
        Beam.retriedOrderId = retriedOrderId,
        Beam.shortId = Kernel.Types.Id.getShortId <$> shortId,
        Beam.status = status,
        Beam.updatedAt = updatedAt,
        Beam.vpa = vpa
      }
