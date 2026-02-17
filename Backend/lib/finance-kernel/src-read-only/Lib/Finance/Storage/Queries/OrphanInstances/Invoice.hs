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
            currency = currency,
            dueAt = dueAt,
            id = Kernel.Types.Id.Id id,
            invoiceNumber = invoiceNumber,
            invoiceType = invoiceType,
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
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            paymentOrderId = paymentOrderId,
            status = status,
            subtotal = subtotal,
            supplierAddress = supplierAddress,
            supplierGSTIN = supplierGSTIN,
            supplierId = supplierId,
            supplierName = supplierName,
            taxBreakdown = taxBreakdown,
            totalAmount = totalAmount,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Invoice Lib.Finance.Domain.Types.Invoice.Invoice where
  toTType' (Lib.Finance.Domain.Types.Invoice.Invoice {..}) = do
    Beam.InvoiceT
      { Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.dueAt = dueAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.invoiceNumber = invoiceNumber,
        Beam.invoiceType = invoiceType,
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
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.paymentOrderId = paymentOrderId,
        Beam.status = status,
        Beam.subtotal = subtotal,
        Beam.supplierAddress = supplierAddress,
        Beam.supplierGSTIN = supplierGSTIN,
        Beam.supplierId = supplierId,
        Beam.supplierName = supplierName,
        Beam.taxBreakdown = taxBreakdown,
        Beam.totalAmount = totalAmount,
        Beam.updatedAt = updatedAt
      }
