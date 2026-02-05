{-
  Finance.Invoice.Service

  Concrete invoice operations for domain use.
  Uses generated Beam queries internally.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Invoice.Service
  ( -- * Invoice operations
    createInvoice,
    getInvoice,
    getByNumber,
    updateInvoiceStatus,

    -- * Invoice-Ledger linking
    getEntriesForInvoice,
    getInvoiceForEntry,

    -- * Queries
    findByIssuedTo,

    -- * Input types (re-export from Interface)
    module Lib.Finance.Invoice.Interface,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Text
import Data.Text (pack)
import Data.Time (defaultTimeLocale, formatTime)
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.Domain.Types.Invoice
import Lib.Finance.Domain.Types.InvoiceLedgerLink
import Lib.Finance.Domain.Types.LedgerEntry (LedgerEntry)
import Lib.Finance.Error.Types
import Lib.Finance.Invoice.Interface
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.Invoice as QInvoice
import qualified Lib.Finance.Storage.Queries.InvoiceLedgerLink as QLink
import qualified Lib.Finance.Storage.Queries.LedgerEntry as QLedger

-- | Create an invoice and link to ledger entries
createInvoice ::
  (BeamFlow.BeamFlow m r) =>
  InvoiceInput ->
  [Id LedgerEntry] -> -- Ledger entries to link
  m (Either FinanceError Invoice)
createInvoice input entryIds = do
  now <- getCurrentTime
  invoiceId <- generateGUID
  invoiceNum <- generateInvoiceNumber now

  -- Calculate totals from line items
  let lineItemsJson = Aeson.toJSON input.lineItems
      subtotal = sum $ map (.lineTotal) input.lineItems
      totalAmount = subtotal -- Could add tax calculation here
  let invoice =
        Invoice
          { id = Id invoiceId,
            invoiceNumber = invoiceNum,
            invoiceType = input.invoiceType,
            issuedToType = input.issuedToType,
            issuedToId = input.issuedToId,
            issuedToName = input.issuedToName,
            issuedByType = input.issuedByType,
            issuedById = input.issuedById,
            issuedByName = input.issuedByName,
            lineItems = lineItemsJson,
            subtotal = subtotal,
            taxBreakdown = Nothing,
            totalAmount = totalAmount,
            currency = input.currency,
            status = Draft,
            issuedAt = now,
            dueAt = input.dueAt,
            merchantId = input.merchantId,
            merchantOperatingCityId = input.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }

  QInvoice.create invoice

  -- Create links to ledger entries
  forM_ entryIds $ \entryId -> do
    linkId <- generateGUID
    let link =
          InvoiceLedgerLink
            { id = Id linkId,
              invoiceId = Id invoiceId,
              ledgerEntryId = entryId,
              merchantId = input.merchantId,
              merchantOperatingCityId = input.merchantOperatingCityId,
              createdAt = now,
              updatedAt = now
            }
    QLink.create link

  pure $ Right invoice

-- | Get invoice by ID
getInvoice ::
  (BeamFlow.BeamFlow m r) =>
  Id Invoice ->
  m (Maybe Invoice)
getInvoice = QInvoice.findById

-- | Get invoice by number
getByNumber ::
  (BeamFlow.BeamFlow m r) =>
  Text ->
  m (Maybe Invoice)
getByNumber = QInvoice.findByNumber

-- | Update invoice status
updateInvoiceStatus ::
  (BeamFlow.BeamFlow m r) =>
  Id Invoice ->
  InvoiceStatus ->
  m ()
updateInvoiceStatus invoiceId newStatus = do
  QInvoice.updateStatus newStatus invoiceId

-- | Get all ledger entries linked to an invoice
getEntriesForInvoice ::
  (BeamFlow.BeamFlow m r) =>
  Id Invoice ->
  m [LedgerEntry]
getEntriesForInvoice invoiceId = do
  links <- QLink.findByInvoice invoiceId
  catMaybes <$> mapM (QLedger.findById . (.ledgerEntryId)) links

-- | Get invoice for a ledger entry (if linked)
getInvoiceForEntry ::
  (BeamFlow.BeamFlow m r) =>
  Id LedgerEntry ->
  m (Maybe Invoice)
getInvoiceForEntry entryId = do
  mbLink <- QLink.findByLedgerEntry entryId
  case mbLink of
    Nothing -> pure Nothing
    Just link -> QInvoice.findById link.invoiceId

-- | Find invoices by issued-to (e.g., all invoices for a driver)
findByIssuedTo ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- Issued to type
  Text -> -- Issued to ID
  m [Invoice]
findByIssuedTo = QInvoice.findByIssuedTo

-- | Generate invoice number (format: INV-YYYYMMDD-XXXXXX)
generateInvoiceNumber :: (MonadFlow m) => UTCTime -> m Text
generateInvoiceNumber now = do
  suffix <- generateGUID
  let dateStr = formatTime defaultTimeLocale "%Y%m%d" now
  pure $ "INV-" <> pack dateStr <> "-" <> Data.Text.take 6 suffix
