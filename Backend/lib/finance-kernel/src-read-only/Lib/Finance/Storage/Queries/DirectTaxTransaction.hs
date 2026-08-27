{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.DirectTaxTransaction (module Lib.Finance.Storage.Queries.DirectTaxTransaction, module ReExport) where

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
import Lib.Finance.Storage.Queries.DirectTaxTransactionExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction] -> m ())
createMany = traverse_ create

findById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction -> m (Maybe Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByInvoiceNumber :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m [Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction])
findByInvoiceNumber invoiceNumber = do findAllWithKV [Se.Is Beam.invoiceNumber $ Se.Eq invoiceNumber]

findByReferenceId :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m [Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction])
findByReferenceId referenceId = do findAllWithKV [Se.Is Beam.referenceId $ Se.Eq referenceId]

findByTdsTreatmentAndDateRange ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Prelude.Text -> Lib.Finance.Domain.Types.DirectTaxTransaction.TdsTreatment -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> m [Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction])
findByTdsTreatmentAndDateRange limit offset merchantOperatingCityId tdsTreatment startTime endTime = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.tdsTreatment $ Se.Eq tdsTreatment,
          Se.Is Beam.transactionDate $ Se.GreaterThanOrEq startTime,
          Se.Is Beam.transactionDate $ Se.LessThanOrEq endTime
        ]
    ]
    (Se.Desc Beam.transactionDate)
    limit
    offset

findByTransactionDate :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.UTCTime -> m [Lib.Finance.Domain.Types.DirectTaxTransaction.DirectTaxTransaction])
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
      Se.Set Beam.createdBy createdBy,
      Se.Set Beam.createdById createdById,
      Se.Set Beam.grossAmount grossAmount,
      Se.Set Beam.invoiceNumber invoiceNumber,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.netAmountPaid netAmountPaid,
      Se.Set Beam.panOfParty panOfParty,
      Se.Set Beam.panType panType,
      Se.Set Beam.paymentDate paymentDate,
      Se.Set Beam.referenceId referenceId,
      Se.Set Beam.tanOfDeductee tanOfDeductee,
      Se.Set Beam.tdsAmount tdsAmount,
      Se.Set Beam.tdsRate tdsRate,
      Se.Set Beam.tdsRateReason tdsRateReason,
      Se.Set Beam.tdsSection tdsSection,
      Se.Set Beam.tdsTreatment tdsTreatment,
      Se.Set Beam.transactionDate transactionDate,
      Se.Set Beam.transactionType transactionType,
      Se.Set Beam.updatedBy updatedBy,
      Se.Set Beam.updatedById updatedById,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
