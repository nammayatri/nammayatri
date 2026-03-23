{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.IndirectTaxTransaction where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.IndirectTaxTransaction as Beam
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.IndirectTaxTransaction.IndirectTaxTransaction -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.IndirectTaxTransaction.IndirectTaxTransaction] -> m ())
createMany = traverse_ create

findByCounterparty :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m [Lib.Finance.Domain.Types.IndirectTaxTransaction.IndirectTaxTransaction])
findByCounterparty counterpartyId = do findAllWithKV [Se.Is Beam.counterpartyId $ Se.Eq counterpartyId]

findById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.IndirectTaxTransaction.IndirectTaxTransaction -> m (Maybe Lib.Finance.Domain.Types.IndirectTaxTransaction.IndirectTaxTransaction))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByTransactionDate :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.UTCTime -> m [Lib.Finance.Domain.Types.IndirectTaxTransaction.IndirectTaxTransaction])
findByTransactionDate transactionDate = do findAllWithKV [Se.Is Beam.transactionDate $ Se.Eq transactionDate]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.IndirectTaxTransaction.IndirectTaxTransaction -> m (Maybe Lib.Finance.Domain.Types.IndirectTaxTransaction.IndirectTaxTransaction))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.IndirectTaxTransaction.IndirectTaxTransaction -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.IndirectTaxTransaction.IndirectTaxTransaction {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.cgstAmount cgstAmount,
      Se.Set Beam.counterpartyId counterpartyId,
      Se.Set Beam.creditOrDebitNoteNumber creditOrDebitNoteNumber,
      Se.Set Beam.externalCharges externalCharges,
      Se.Set Beam.gstCreditType gstCreditType,
      Se.Set Beam.gstRate gstRate,
      Se.Set Beam.gstinOfParty gstinOfParty,
      Se.Set Beam.igstAmount igstAmount,
      Se.Set Beam.invoiceNumber invoiceNumber,
      Se.Set Beam.issuedByTaxNo issuedByTaxNo,
      Se.Set Beam.issuedToTaxNo issuedToTaxNo,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.referenceId referenceId,
      Se.Set Beam.sacCode sacCode,
      Se.Set Beam.saleType saleType,
      Se.Set Beam.sgstAmount sgstAmount,
      Se.Set Beam.taxCreditType taxCreditType,
      Se.Set Beam.taxRate taxRate,
      Se.Set Beam.taxableValue taxableValue,
      Se.Set Beam.totalGstAmount totalGstAmount,
      Se.Set Beam.totalTaxAmount totalTaxAmount,
      Se.Set Beam.transactionDate transactionDate,
      Se.Set Beam.transactionType transactionType,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.IndirectTaxTransaction Lib.Finance.Domain.Types.IndirectTaxTransaction.IndirectTaxTransaction where
  fromTType' (Beam.IndirectTaxTransactionT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.IndirectTaxTransaction.IndirectTaxTransaction
          { cgstAmount = cgstAmount,
            counterpartyId = counterpartyId,
            createdAt = createdAt,
            creditOrDebitNoteNumber = creditOrDebitNoteNumber,
            externalCharges = externalCharges,
            gstCreditType = gstCreditType,
            gstRate = gstRate,
            gstinOfParty = gstinOfParty,
            id = Kernel.Types.Id.Id id,
            igstAmount = igstAmount,
            invoiceNumber = invoiceNumber,
            issuedByTaxNo = issuedByTaxNo,
            issuedToTaxNo = issuedToTaxNo,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            referenceId = referenceId,
            sacCode = sacCode,
            saleType = saleType,
            sgstAmount = sgstAmount,
            taxCreditType = taxCreditType,
            taxRate = taxRate,
            taxableValue = taxableValue,
            totalGstAmount = totalGstAmount,
            totalTaxAmount = totalTaxAmount,
            transactionDate = transactionDate,
            transactionType = transactionType,
            updatedAt = updatedAt
          }

instance ToTType' Beam.IndirectTaxTransaction Lib.Finance.Domain.Types.IndirectTaxTransaction.IndirectTaxTransaction where
  toTType' (Lib.Finance.Domain.Types.IndirectTaxTransaction.IndirectTaxTransaction {..}) = do
    Beam.IndirectTaxTransactionT
      { Beam.cgstAmount = cgstAmount,
        Beam.counterpartyId = counterpartyId,
        Beam.createdAt = createdAt,
        Beam.creditOrDebitNoteNumber = creditOrDebitNoteNumber,
        Beam.externalCharges = externalCharges,
        Beam.gstCreditType = gstCreditType,
        Beam.gstRate = gstRate,
        Beam.gstinOfParty = gstinOfParty,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.igstAmount = igstAmount,
        Beam.invoiceNumber = invoiceNumber,
        Beam.issuedByTaxNo = issuedByTaxNo,
        Beam.issuedToTaxNo = issuedToTaxNo,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.referenceId = referenceId,
        Beam.sacCode = sacCode,
        Beam.saleType = saleType,
        Beam.sgstAmount = sgstAmount,
        Beam.taxCreditType = taxCreditType,
        Beam.taxRate = taxRate,
        Beam.taxableValue = taxableValue,
        Beam.totalGstAmount = totalGstAmount,
        Beam.totalTaxAmount = totalTaxAmount,
        Beam.transactionDate = transactionDate,
        Beam.transactionType = transactionType,
        Beam.updatedAt = updatedAt
      }
