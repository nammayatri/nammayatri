{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Invoice where

import qualified Domain.Types.Invoice
import qualified Domain.Types.Plan
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Invoice as Beam
import qualified Storage.Queries.Transformers.Invoice

instance FromTType' Beam.Invoice Domain.Types.Invoice.Invoice where
  fromTType' (Beam.InvoiceT {..}) = do
    merchantOperatingCityId' <- Storage.Queries.Transformers.Invoice.getMerchantOperatingCityId merchantOperatingCityId driverFeeId id
    pure $
      Just
        Domain.Types.Invoice.Invoice
          { bankErrorCode = bankErrorCode,
            bankErrorMessage = bankErrorMessage,
            bankErrorUpdatedAt = bankErrorUpdatedAt,
            createdAt = createdAt,
            driverFeeId = Kernel.Types.Id.Id driverFeeId,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            invoiceShortId = invoiceShortId,
            invoiceStatus = invoiceStatus,
            lastStatusCheckedAt = lastStatusCheckedAt,
            maxMandateAmount = maxMandateAmount,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            paymentMode = paymentMode,
            serviceName = fromMaybe Domain.Types.Plan.YATRI_SUBSCRIPTION serviceName,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Invoice Domain.Types.Invoice.Invoice where
  toTType' (Domain.Types.Invoice.Invoice {..}) = do
    Beam.InvoiceT
      { Beam.bankErrorCode = bankErrorCode,
        Beam.bankErrorMessage = bankErrorMessage,
        Beam.bankErrorUpdatedAt = bankErrorUpdatedAt,
        Beam.createdAt = createdAt,
        Beam.driverFeeId = Kernel.Types.Id.getId driverFeeId,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.invoiceShortId = invoiceShortId,
        Beam.invoiceStatus = invoiceStatus,
        Beam.lastStatusCheckedAt = lastStatusCheckedAt,
        Beam.maxMandateAmount = maxMandateAmount,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Just (Kernel.Types.Id.getId merchantOperatingCityId),
        Beam.paymentMode = paymentMode,
        Beam.serviceName = Just serviceName,
        Beam.updatedAt = updatedAt
      }
