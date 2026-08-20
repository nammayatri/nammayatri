{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.FinanceTdsReimbursementInvoiceMapping where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.FinanceTdsReimbursementInvoiceMapping
import qualified Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.FinanceTdsReimbursementInvoiceMapping as Beam
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.FinanceTdsReimbursementInvoiceMapping.FinanceTdsReimbursementInvoiceMapping -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.FinanceTdsReimbursementInvoiceMapping.FinanceTdsReimbursementInvoiceMapping] -> m ())
createMany = traverse_ create

findAllByInvoiceId ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.Invoice.Invoice -> m ([Lib.Finance.Domain.Types.FinanceTdsReimbursementInvoiceMapping.FinanceTdsReimbursementInvoiceMapping]))
findAllByInvoiceId invoiceId = do findAllWithKV [Se.Is Beam.invoiceId $ Se.Eq (Kernel.Types.Id.getId invoiceId)]

findAllByRequestId ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest -> m ([Lib.Finance.Domain.Types.FinanceTdsReimbursementInvoiceMapping.FinanceTdsReimbursementInvoiceMapping]))
findAllByRequestId requestId = do findAllWithKV [Se.Is Beam.requestId $ Se.Eq (Kernel.Types.Id.getId requestId)]

findByRequestIdAndInvoiceId ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.Invoice.Invoice -> m (Maybe Lib.Finance.Domain.Types.FinanceTdsReimbursementInvoiceMapping.FinanceTdsReimbursementInvoiceMapping))
findByRequestIdAndInvoiceId requestId invoiceId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.requestId $ Se.Eq (Kernel.Types.Id.getId requestId),
          Se.Is Beam.invoiceId $ Se.Eq (Kernel.Types.Id.getId invoiceId)
        ]
    ]

updateTdsCreditReceivable ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.FinanceTdsReimbursementInvoiceMapping.FinanceTdsReimbursementInvoiceMapping -> m ())
updateTdsCreditReceivable tdsCreditReceivable id = do updateOneWithKV [Se.Set Beam.tdsCreditReceivable tdsCreditReceivable] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.FinanceTdsReimbursementInvoiceMapping.FinanceTdsReimbursementInvoiceMapping -> m (Maybe Lib.Finance.Domain.Types.FinanceTdsReimbursementInvoiceMapping.FinanceTdsReimbursementInvoiceMapping))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.FinanceTdsReimbursementInvoiceMapping.FinanceTdsReimbursementInvoiceMapping -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.FinanceTdsReimbursementInvoiceMapping.FinanceTdsReimbursementInvoiceMapping {..}) = do
  updateWithKV
    [ Se.Set Beam.invoiceId (Kernel.Types.Id.getId invoiceId),
      Se.Set Beam.requestId (Kernel.Types.Id.getId requestId),
      Se.Set Beam.revenueRecognisedSnapshot revenueRecognisedSnapshot,
      Se.Set Beam.tdsAmount tdsAmount,
      Se.Set Beam.tdsCreditReceivable tdsCreditReceivable
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FinanceTdsReimbursementInvoiceMapping Lib.Finance.Domain.Types.FinanceTdsReimbursementInvoiceMapping.FinanceTdsReimbursementInvoiceMapping where
  fromTType' (Beam.FinanceTdsReimbursementInvoiceMappingT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.FinanceTdsReimbursementInvoiceMapping.FinanceTdsReimbursementInvoiceMapping
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            invoiceId = Kernel.Types.Id.Id invoiceId,
            requestId = Kernel.Types.Id.Id requestId,
            revenueRecognisedSnapshot = revenueRecognisedSnapshot,
            tdsAmount = tdsAmount,
            tdsCreditReceivable = tdsCreditReceivable
          }

instance ToTType' Beam.FinanceTdsReimbursementInvoiceMapping Lib.Finance.Domain.Types.FinanceTdsReimbursementInvoiceMapping.FinanceTdsReimbursementInvoiceMapping where
  toTType' (Lib.Finance.Domain.Types.FinanceTdsReimbursementInvoiceMapping.FinanceTdsReimbursementInvoiceMapping {..}) = do
    Beam.FinanceTdsReimbursementInvoiceMappingT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.invoiceId = Kernel.Types.Id.getId invoiceId,
        Beam.requestId = Kernel.Types.Id.getId requestId,
        Beam.revenueRecognisedSnapshot = revenueRecognisedSnapshot,
        Beam.tdsAmount = tdsAmount,
        Beam.tdsCreditReceivable = tdsCreditReceivable
      }
