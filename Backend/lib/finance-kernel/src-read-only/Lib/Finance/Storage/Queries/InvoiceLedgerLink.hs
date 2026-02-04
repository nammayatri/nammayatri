{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.InvoiceLedgerLink where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Lib.Finance.Domain.Types.InvoiceLedgerLink
import qualified Lib.Finance.Domain.Types.LedgerEntry
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.InvoiceLedgerLink as Beam
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.InvoiceLedgerLink.InvoiceLedgerLink -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.InvoiceLedgerLink.InvoiceLedgerLink] -> m ())
createMany = traverse_ create

findByInvoice :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Finance.Domain.Types.Invoice.Invoice -> m ([Lib.Finance.Domain.Types.InvoiceLedgerLink.InvoiceLedgerLink]))
findByInvoice invoiceId = do findAllWithKV [Se.Is Beam.invoiceId $ Se.Eq (Kernel.Types.Id.getId invoiceId)]

findByLedgerEntry ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry -> m (Maybe Lib.Finance.Domain.Types.InvoiceLedgerLink.InvoiceLedgerLink))
findByLedgerEntry ledgerEntryId = do findOneWithKV [Se.Is Beam.ledgerEntryId $ Se.Eq (Kernel.Types.Id.getId ledgerEntryId)]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.InvoiceLedgerLink.InvoiceLedgerLink -> m (Maybe Lib.Finance.Domain.Types.InvoiceLedgerLink.InvoiceLedgerLink))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.InvoiceLedgerLink.InvoiceLedgerLink -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.InvoiceLedgerLink.InvoiceLedgerLink {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.invoiceId (Kernel.Types.Id.getId invoiceId),
      Se.Set Beam.ledgerEntryId (Kernel.Types.Id.getId ledgerEntryId),
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.InvoiceLedgerLink Lib.Finance.Domain.Types.InvoiceLedgerLink.InvoiceLedgerLink where
  fromTType' (Beam.InvoiceLedgerLinkT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.InvoiceLedgerLink.InvoiceLedgerLink
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            invoiceId = Kernel.Types.Id.Id invoiceId,
            ledgerEntryId = Kernel.Types.Id.Id ledgerEntryId,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.InvoiceLedgerLink Lib.Finance.Domain.Types.InvoiceLedgerLink.InvoiceLedgerLink where
  toTType' (Lib.Finance.Domain.Types.InvoiceLedgerLink.InvoiceLedgerLink {..}) = do
    Beam.InvoiceLedgerLinkT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.invoiceId = Kernel.Types.Id.getId invoiceId,
        Beam.ledgerEntryId = Kernel.Types.Id.getId ledgerEntryId,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.updatedAt = updatedAt
      }
