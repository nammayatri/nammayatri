{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.OrphanInstances.DirectTaxTransaction where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.DirectTaxTransaction
import qualified Lib.Finance.Storage.Beam.DirectTaxTransaction as Beam

instance FromTType' Beam.DirectTaxTransaction Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction where
  fromTType' (Beam.DirectTaxTransactionT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction
          { counterpartyId = counterpartyId,
            createdAt = createdAt,
            createdBy = createdBy,
            createdById = createdById,
            grossAmount = grossAmount,
            id = Kernel.Types.Id.Id id,
            invoiceNumber = invoiceNumber,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            netAmountPaid = netAmountPaid,
            panOfParty = panOfParty,
            panType = panType,
            paymentDate = paymentDate,
            referenceId = referenceId,
            tanOfDeductee = tanOfDeductee,
            tdsAmount = tdsAmount,
            tdsRate = tdsRate,
            tdsRateReason = tdsRateReason,
            tdsSection = tdsSection,
            tdsTreatment = tdsTreatment,
            transactionDate = transactionDate,
            transactionType = transactionType,
            updatedBy = updatedBy,
            updatedById = updatedById,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DirectTaxTransaction Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction where
  toTType' (Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction {..}) = do
    Beam.DirectTaxTransactionT
      { Beam.counterpartyId = counterpartyId,
        Beam.createdAt = createdAt,
        Beam.createdBy = createdBy,
        Beam.createdById = createdById,
        Beam.grossAmount = grossAmount,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.invoiceNumber = invoiceNumber,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.netAmountPaid = netAmountPaid,
        Beam.panOfParty = panOfParty,
        Beam.panType = panType,
        Beam.paymentDate = paymentDate,
        Beam.referenceId = referenceId,
        Beam.tanOfDeductee = tanOfDeductee,
        Beam.tdsAmount = tdsAmount,
        Beam.tdsRate = tdsRate,
        Beam.tdsRateReason = tdsRateReason,
        Beam.tdsSection = tdsSection,
        Beam.tdsTreatment = tdsTreatment,
        Beam.transactionDate = transactionDate,
        Beam.transactionType = transactionType,
        Beam.updatedBy = updatedBy,
        Beam.updatedById = updatedById,
        Beam.updatedAt = updatedAt
      }
