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
import Data.Text (pack)
import qualified Data.Text
import Data.Time (defaultTimeLocale, formatTime)
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.Domain.Types.Account (CounterpartyType (..))
import Lib.Finance.Domain.Types.IndirectTaxTransaction
import Lib.Finance.Domain.Types.Invoice
import Lib.Finance.Domain.Types.InvoiceLedgerLink
import Lib.Finance.Domain.Types.LedgerEntry (LedgerEntry)
import Lib.Finance.Error.Types
import Lib.Finance.Invoice.Interface
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.Account as QAccount
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransaction as QIndirectTax
import qualified Lib.Finance.Storage.Queries.Invoice as QInvoice
import qualified Lib.Finance.Storage.Queries.InvoiceLedgerLink as QLink
import qualified Lib.Finance.Storage.Queries.LedgerEntry as QLedger

-- | Create an invoice and link to ledger entries.
--   If any linked entry targets a GOVERNMENT_INDIRECT account (GST),
--   an IndirectTaxTransaction record is also created.
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
      totalAmount = subtotal
  let invoice =
        Invoice
          { id = Id invoiceId,
            invoiceNumber = invoiceNum,
            invoiceType = input.invoiceType,
            paymentOrderId = input.paymentOrderId,
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

  -- Create IndirectTaxTransaction for GST entries (toAccount is GOVERNMENT_INDIRECT)
  entries <- catMaybes <$> mapM QLedger.findById entryIds
  forM_ entries $ \entry -> do
    mbToAccount <- QAccount.findById entry.toAccountId
    case mbToAccount of
      Just toAccount
        | toAccount.counterpartyType == Just GOVERNMENT_INDIRECT -> do
          taxTxnId <- generateGUID
          let gstAmount = entry.amount
              halfGst = gstAmount / 2.0
              taxableValue = subtotal - gstAmount
              gstRate = if taxableValue > 0 then realToFrac (gstAmount / taxableValue) * 100.0 else 0.0
              txnType = invoiceTypeToTransactionType input.invoiceType
          let taxTxn =
                IndirectTaxTransaction
                  { id = Id taxTxnId,
                    transactionDate = now,
                    transactionType = txnType,
                    referenceId = entry.referenceId,
                    taxableValue = taxableValue,
                    gstRate = gstRate,
                    cgstAmount = halfGst,
                    sgstAmount = halfGst,
                    igstAmount = 0,
                    totalGstAmount = gstAmount,
                    gstCreditType = Output,
                    counterpartyId = input.issuedToId,
                    gstinOfParty = Nothing,
                    saleType = B2C,
                    invoiceNumber = Just invoiceNum,
                    creditOrDebitNoteNumber = Nothing,
                    sacCode = Just (sacCodeForTransactionType txnType),
                    merchantId = input.merchantId,
                    merchantOperatingCityId = input.merchantOperatingCityId,
                    createdAt = now,
                    updatedAt = now
                  }
          QIndirectTax.create taxTxn
      _ -> pure ()

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

-- | Map invoiceType text to TransactionType
invoiceTypeToTransactionType :: Text -> TransactionType
invoiceTypeToTransactionType invoiceType = case invoiceType of
  "SubscriptionPurchase" -> Subscription
  "Subscription" -> Subscription
  "RideFare" -> RideFare
  "Incentive" -> Incentive
  "Cancellation" -> Cancellation
  "BuyerCommission" -> BuyerCommission
  "CreditNote" -> CreditNote
  "DebitNote" -> DebitNote
  _ -> Subscription -- default fallback

-- | SAC code mapping per transaction type
sacCodeForTransactionType :: TransactionType -> Text
sacCodeForTransactionType = \case
  Subscription -> "998314"
  RideFare -> "996412"
  Incentive -> "998314"
  Cancellation -> "996412"
  BuyerCommission -> "998314"
  CreditNote -> "998314"
  DebitNote -> "998314"
