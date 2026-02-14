{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.Invoice (module Lib.Finance.Storage.Queries.Invoice, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.Invoice as Beam
import Lib.Finance.Storage.Queries.InvoiceExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.Invoice.Invoice -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.Invoice.Invoice] -> m ())
createMany = traverse_ create

findById :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Finance.Domain.Types.Invoice.Invoice -> m (Maybe Lib.Finance.Domain.Types.Invoice.Invoice))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByIssuedTo :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.Invoice.Invoice]))
findByIssuedTo issuedToType issuedToId = do findAllWithKV [Se.And [Se.Is Beam.issuedToType $ Se.Eq issuedToType, Se.Is Beam.issuedToId $ Se.Eq issuedToId]]

findByNumber :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m (Maybe Lib.Finance.Domain.Types.Invoice.Invoice))
findByNumber invoiceNumber = do findOneWithKV [Se.Is Beam.invoiceNumber $ Se.Eq invoiceNumber]

updateStatus :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.Invoice.InvoiceStatus -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.Invoice.Invoice -> m ())
updateStatus status id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Finance.Domain.Types.Invoice.Invoice -> m (Maybe Lib.Finance.Domain.Types.Invoice.Invoice))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.Invoice.Invoice -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.Invoice.Invoice {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currency currency,
      Se.Set Beam.dueAt dueAt,
      Se.Set Beam.invoiceNumber invoiceNumber,
      Se.Set Beam.invoiceType invoiceType,
      Se.Set Beam.issuedAt issuedAt,
      Se.Set Beam.issuedByAddress issuedByAddress,
      Se.Set Beam.issuedById issuedById,
      Se.Set Beam.issuedByName issuedByName,
      Se.Set Beam.issuedByType issuedByType,
      Se.Set Beam.issuedToAddress issuedToAddress,
      Se.Set Beam.issuedToId issuedToId,
      Se.Set Beam.issuedToName issuedToName,
      Se.Set Beam.issuedToType issuedToType,
      Se.Set Beam.lineItems lineItems,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.paymentOrderId paymentOrderId,
      Se.Set Beam.status status,
      Se.Set Beam.subtotal subtotal,
      Se.Set Beam.taxBreakdown taxBreakdown,
      Se.Set Beam.totalAmount totalAmount,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
