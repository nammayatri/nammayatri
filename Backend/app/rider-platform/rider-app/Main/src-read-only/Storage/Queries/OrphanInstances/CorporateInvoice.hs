{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.CorporateInvoice where

import qualified Domain.Types.CorporateInvoice
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.CorporateInvoice as Beam

instance FromTType' Beam.CorporateInvoice Domain.Types.CorporateInvoice.CorporateInvoice where
  fromTType' (Beam.CorporateInvoiceT {..}) = do
    pure $
      Just
        Domain.Types.CorporateInvoice.CorporateInvoice
          { id = Kernel.Types.Id.Id id,
            corporateEntityId = Kernel.Types.Id.Id corporateEntityId,
            invoiceNumber = invoiceNumber,
            periodStart = periodStart,
            periodEnd = periodEnd,
            totalTrips = totalTrips,
            baseAmount = Kernel.Prelude.realToFrac baseAmount,
            cgstRate = cgstRate,
            cgstAmount = Kernel.Prelude.realToFrac <$> cgstAmount,
            sgstRate = sgstRate,
            sgstAmount = Kernel.Prelude.realToFrac <$> sgstAmount,
            igstRate = igstRate,
            igstAmount = Kernel.Prelude.realToFrac <$> igstAmount,
            totalTaxAmount = Kernel.Prelude.realToFrac totalTaxAmount,
            netAmount = Kernel.Prelude.realToFrac netAmount,
            currency = Kernel.Prelude.fromMaybe Kernel.Prelude.INR (Kernel.Prelude.readMaybe (Kernel.Prelude.toString currency)),
            sacCode = sacCode,
            placeOfSupply = placeOfSupply,
            supplierGstin = supplierGstin,
            recipientGstin = recipientGstin,
            eInvoiceIrn = eInvoiceIrn,
            status = Kernel.Prelude.fromMaybe Domain.Types.CorporateInvoice.DRAFT (Kernel.Prelude.readMaybe (Kernel.Prelude.toString status)),
            pdfUrl = pdfUrl,
            generatedAt = generatedAt,
            paidAt = paidAt,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CorporateInvoice Domain.Types.CorporateInvoice.CorporateInvoice where
  toTType' (Domain.Types.CorporateInvoice.CorporateInvoice {..}) = do
    Beam.CorporateInvoiceT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.corporateEntityId = Kernel.Types.Id.getId corporateEntityId,
        Beam.invoiceNumber = invoiceNumber,
        Beam.periodStart = periodStart,
        Beam.periodEnd = periodEnd,
        Beam.totalTrips = totalTrips,
        Beam.baseAmount = Kernel.Prelude.realToFrac baseAmount,
        Beam.cgstRate = cgstRate,
        Beam.cgstAmount = Kernel.Prelude.realToFrac <$> cgstAmount,
        Beam.sgstRate = sgstRate,
        Beam.sgstAmount = Kernel.Prelude.realToFrac <$> sgstAmount,
        Beam.igstRate = igstRate,
        Beam.igstAmount = Kernel.Prelude.realToFrac <$> igstAmount,
        Beam.totalTaxAmount = Kernel.Prelude.realToFrac totalTaxAmount,
        Beam.netAmount = Kernel.Prelude.realToFrac netAmount,
        Beam.currency = Kernel.Prelude.show currency,
        Beam.sacCode = sacCode,
        Beam.placeOfSupply = placeOfSupply,
        Beam.supplierGstin = supplierGstin,
        Beam.recipientGstin = recipientGstin,
        Beam.eInvoiceIrn = eInvoiceIrn,
        Beam.status = Kernel.Prelude.show status,
        Beam.pdfUrl = pdfUrl,
        Beam.generatedAt = generatedAt,
        Beam.paidAt = paidAt,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
