{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.OrphanInstances.Invoice where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Lib.Finance.Storage.Beam.Invoice as Beam

instance FromTType' Beam.Invoice Lib.Finance.Domain.Types.Invoice.Invoice where
  fromTType' (Beam.InvoiceT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.Invoice.Invoice
          { createdAt = createdAt,
            createdBy = createdBy,
            currency = currency,
            dueAt = dueAt,
            id = Kernel.Types.Id.Id id,
            invoiceNumber = invoiceNumber,
            invoiceType = invoiceType,
            irn = irn,
            issuedAt = issuedAt,
            issuedByAddress = issuedByAddress,
            issuedById = issuedById,
            issuedByName = issuedByName,
            issuedByType = issuedByType,
            issuedToAddress = issuedToAddress,
            issuedToId = issuedToId,
            issuedToName = issuedToName,
            issuedToType = issuedToType,
            lineItems = lineItems,
            merchantGstin = merchantGstin,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            paymentMode = paymentMode,
            paymentOrderId = paymentOrderId,
            periodEnd = periodEnd,
            periodStart = periodStart,
            referenceId = referenceId,
            signedQRCode = signedQRCode,
            status = status,
            subtotal = subtotal,
            supplierAddress = supplierAddress,
            supplierGSTIN = supplierGSTIN,
            supplierId = supplierId,
            supplierName = supplierName,
            supplierTaxNo = supplierTaxNo,
            taxBreakdown = taxBreakdown,
            totalAmount = totalAmount,
            updatedBy = updatedBy,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Invoice Lib.Finance.Domain.Types.Invoice.Invoice where
  toTType' (Lib.Finance.Domain.Types.Invoice.Invoice {..}) = do
    Beam.InvoiceT
      { Beam.createdAt = createdAt,
        Beam.createdBy = createdBy,
        Beam.currency = currency,
        Beam.dueAt = dueAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.invoiceNumber = invoiceNumber,
        Beam.invoiceType = invoiceType,
        Beam.irn = irn,
        Beam.issuedAt = issuedAt,
        Beam.issuedByAddress = issuedByAddress,
        Beam.issuedById = issuedById,
        Beam.issuedByName = issuedByName,
        Beam.issuedByType = issuedByType,
        Beam.issuedToAddress = issuedToAddress,
        Beam.issuedToId = issuedToId,
        Beam.issuedToName = issuedToName,
        Beam.issuedToType = issuedToType,
        Beam.lineItems = lineItems,
        Beam.merchantGstin = merchantGstin,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.paymentMode = paymentMode,
        Beam.paymentOrderId = paymentOrderId,
        Beam.periodEnd = periodEnd,
        Beam.periodStart = periodStart,
        Beam.referenceId = referenceId,
        Beam.signedQRCode = signedQRCode,
        Beam.status = status,
        Beam.subtotal = subtotal,
        Beam.supplierAddress = supplierAddress,
        Beam.supplierGSTIN = supplierGSTIN,
        Beam.supplierId = supplierId,
        Beam.supplierName = supplierName,
        Beam.supplierTaxNo = supplierTaxNo,
        Beam.taxBreakdown = taxBreakdown,
        Beam.totalAmount = totalAmount,
        Beam.updatedBy = updatedBy,
        Beam.updatedAt = updatedAt
      }
