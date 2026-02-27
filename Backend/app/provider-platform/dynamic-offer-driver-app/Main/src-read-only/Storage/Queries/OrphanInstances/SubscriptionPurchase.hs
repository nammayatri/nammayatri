{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.SubscriptionPurchase where

import qualified Domain.Types.SubscriptionPurchase
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.SubscriptionPurchase as Beam

instance FromTType' Beam.SubscriptionPurchase Domain.Types.SubscriptionPurchase.SubscriptionPurchase where
  fromTType' (Beam.SubscriptionPurchaseT {..}) = do
    pure $
      Just
        Domain.Types.SubscriptionPurchase.SubscriptionPurchase
          { enableServiceUsageCharge = enableServiceUsageCharge,
            expiryDate = expiryDate,
            financeInvoiceId = Kernel.Types.Id.Id <$> financeInvoiceId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            ownerId = ownerId,
            ownerType = ownerType,
            paymentOrderId = Kernel.Types.Id.Id paymentOrderId,
            planFee = planFee,
            planFrequency = planFrequency,
            planId = Kernel.Types.Id.Id planId,
            planRideCredit = planRideCredit,
            purchaseTimestamp = purchaseTimestamp,
            reconciliationStatus = reconciliationStatus,
            serviceName = serviceName,
            status = status,
            vehicleCategory = vehicleCategory,
            waiveOfMode = waiveOfMode,
            waiveOffEnabledOn = waiveOffEnabledOn,
            waiveOffValidTill = waiveOffValidTill,
            waiverOffPercentage = waiverOffPercentage,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SubscriptionPurchase Domain.Types.SubscriptionPurchase.SubscriptionPurchase where
  toTType' (Domain.Types.SubscriptionPurchase.SubscriptionPurchase {..}) = do
    Beam.SubscriptionPurchaseT
      { Beam.enableServiceUsageCharge = enableServiceUsageCharge,
        Beam.expiryDate = expiryDate,
        Beam.financeInvoiceId = Kernel.Types.Id.getId <$> financeInvoiceId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.ownerId = ownerId,
        Beam.ownerType = ownerType,
        Beam.paymentOrderId = Kernel.Types.Id.getId paymentOrderId,
        Beam.planFee = planFee,
        Beam.planFrequency = planFrequency,
        Beam.planId = Kernel.Types.Id.getId planId,
        Beam.planRideCredit = planRideCredit,
        Beam.purchaseTimestamp = purchaseTimestamp,
        Beam.reconciliationStatus = reconciliationStatus,
        Beam.serviceName = serviceName,
        Beam.status = status,
        Beam.vehicleCategory = vehicleCategory,
        Beam.waiveOfMode = waiveOfMode,
        Beam.waiveOffEnabledOn = waiveOffEnabledOn,
        Beam.waiveOffValidTill = waiveOffValidTill,
        Beam.waiverOffPercentage = waiverOffPercentage,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
