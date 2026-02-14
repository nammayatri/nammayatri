{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.Invoice where

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
      Se.Set Beam.issuedById issuedById,
      Se.Set Beam.issuedByName issuedByName,
      Se.Set Beam.issuedByType issuedByType,
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
            issuedById = issuedById,
            issuedByName = issuedByName,
            issuedByType = issuedByType,
            issuedToId = issuedToId,
            issuedToName = issuedToName,
            issuedToType = issuedToType,
            lineItems = lineItems,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            paymentOrderId = paymentOrderId,
            status = status,
            subtotal = subtotal,
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
        Beam.issuedById = issuedById,
        Beam.issuedByName = issuedByName,
        Beam.issuedByType = issuedByType,
        Beam.issuedToId = issuedToId,
        Beam.issuedToName = issuedToName,
        Beam.issuedToType = issuedToType,
        Beam.lineItems = lineItems,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.paymentOrderId = paymentOrderId,
        Beam.status = status,
        Beam.subtotal = subtotal,
        Beam.taxBreakdown = taxBreakdown,
        Beam.totalAmount = totalAmount,
        Beam.updatedAt = updatedAt
      }
