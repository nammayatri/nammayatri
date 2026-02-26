{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.DirectTaxTransaction where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.DirectTaxTransaction
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.DirectTaxTransaction as Beam
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction] -> m ())
createMany = traverse_ create

findByCounterparty :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction]))
findByCounterparty counterpartyId = do findAllWithKV [Se.Is Beam.counterpartyId $ Se.Eq counterpartyId]

findById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction -> m (Maybe Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByTransactionDate :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.UTCTime -> m ([Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction]))
findByTransactionDate transactionDate = do findAllWithKV [Se.Is Beam.transactionDate $ Se.Eq transactionDate]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction -> m (Maybe Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.counterpartyId counterpartyId,
      Se.Set Beam.grossAmount grossAmount,
      Se.Set Beam.invoiceNumber invoiceNumber,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.netAmountPaid netAmountPaid,
      Se.Set Beam.panOfParty panOfParty,
      Se.Set Beam.paymentDate paymentDate,
      Se.Set Beam.referenceId referenceId,
      Se.Set Beam.tanOfDeductee tanOfDeductee,
      Se.Set Beam.tdsAmount tdsAmount,
      Se.Set Beam.tdsRate tdsRate,
      Se.Set Beam.tdsTreatment tdsTreatment,
      Se.Set Beam.transactionDate transactionDate,
      Se.Set Beam.transactionType transactionType,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DirectTaxTransaction Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction where
  fromTType' (Beam.DirectTaxTransactionT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction
          { counterpartyId = counterpartyId,
            createdAt = createdAt,
            grossAmount = grossAmount,
            id = Kernel.Types.Id.Id id,
            invoiceNumber = invoiceNumber,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            netAmountPaid = netAmountPaid,
            panOfParty = panOfParty,
            paymentDate = paymentDate,
            referenceId = referenceId,
            tanOfDeductee = tanOfDeductee,
            tdsAmount = tdsAmount,
            tdsRate = tdsRate,
            tdsTreatment = tdsTreatment,
            transactionDate = transactionDate,
            transactionType = transactionType,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DirectTaxTransaction Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction where
  toTType' (Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction {..}) = do
    Beam.DirectTaxTransactionT
      { Beam.counterpartyId = counterpartyId,
        Beam.createdAt = createdAt,
        Beam.grossAmount = grossAmount,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.invoiceNumber = invoiceNumber,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.netAmountPaid = netAmountPaid,
        Beam.panOfParty = panOfParty,
        Beam.paymentDate = paymentDate,
        Beam.referenceId = referenceId,
        Beam.tanOfDeductee = tanOfDeductee,
        Beam.tdsAmount = tdsAmount,
        Beam.tdsRate = tdsRate,
        Beam.tdsTreatment = tdsTreatment,
        Beam.transactionDate = transactionDate,
        Beam.transactionType = transactionType,
        Beam.updatedAt = updatedAt
      }
