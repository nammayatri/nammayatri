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

    -- * Standalone tax entry creation
    createIndirectTaxEntry,
    createDirectTaxEntry,

    -- * Invoice-Ledger linking
    getEntriesForInvoice,
    getInvoiceForEntry,

    -- * Queries
    findByIssuedTo,

    -- * Input types (re-export from Interface)
    module Lib.Finance.Invoice.Interface,

    -- * Invoice number generation (re-export from InvoiceNumber)
    module Lib.Finance.Invoice.InvoiceNumber,
  )
where

import qualified Data.Aeson as Aeson
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.Domain.Types.Account (CounterpartyType (..))
import Lib.Finance.Domain.Types.DirectTaxTransaction (DirectTaxTransaction (..))
import qualified Lib.Finance.Domain.Types.DirectTaxTransaction as DirectTax
import Lib.Finance.Domain.Types.IndirectTaxTransaction
import qualified Lib.Finance.Domain.Types.IndirectTaxTransaction as IndirectTax
import Lib.Finance.Domain.Types.Invoice
import Lib.Finance.Domain.Types.InvoiceLedgerLink
import Lib.Finance.Domain.Types.LedgerEntry (LedgerEntry)
import Lib.Finance.Error.Types
import Lib.Finance.Invoice.Interface
import Lib.Finance.Invoice.InvoiceNumber
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.Account as QAccount
import qualified Lib.Finance.Storage.Queries.DirectTaxTransaction as QDirectTax
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransaction as QIndirectTax
import qualified Lib.Finance.Storage.Queries.Invoice as QInvoice
import qualified Lib.Finance.Storage.Queries.InvoiceLedgerLink as QLink
import qualified Lib.Finance.Storage.Queries.LedgerEntry as QLedger

-- | Create an invoice and link to ledger entries.
--   If any linked entry targets a GOVERNMENT_INDIRECT account (GST),
--   an IndirectTaxTransaction record is also created.
createInvoice ::
  (BeamFlow.BeamFlow m r, Redis.HedisFlow m r) =>
  InvoiceInput ->
  [Id LedgerEntry] -> -- Ledger entries to link
  m (Either FinanceError Invoice)
createInvoice input entryIds = do
  now <- getCurrentTime
  invoiceId <- generateGUID
  let dbFallback = fmap (.invoiceNumber) <$> QInvoice.findLatestByCreatedAt now
      purposeAbbr = invoiceTypeToPurpose input.invoiceType
  invoiceNum <- generateInvoiceNumber input.merchantShortId purposeAbbr typePayment now dbFallback

  -- Calculate totals from line items
  -- subtotal: excludes external charges (toll, parking) and tax line items
  -- totalAmount: sum of all line items (what rider pays)
  let lineItemsJson = Aeson.toJSON input.lineItems
      totalAmount = sum $ map (.lineTotal) input.lineItems
      externalTotal = sum $ map (.lineTotal) $ filter (.isExternalCharge) input.lineItems
      taxTotal = sum $ map (.lineTotal) $ filter (\li -> li.description == "Tax") input.lineItems
      subtotal = totalAmount - externalTotal - taxTotal
  let invoice =
        Invoice
          { id = Id invoiceId,
            invoiceNumber = invoiceNum,
            invoiceType = input.invoiceType,
            paymentOrderId = input.paymentOrderId,
            issuedToType = input.issuedToType,
            issuedToId = input.issuedToId,
            issuedToName = input.issuedToName,
            issuedToAddress = input.issuedToAddress,
            issuedByType = input.issuedByType,
            issuedById = input.issuedById,
            issuedByName = input.issuedByName,
            issuedByAddress = input.issuedByAddress,
            supplierName = input.supplierName,
            supplierAddress = input.supplierAddress,
            supplierGSTIN = input.supplierGSTIN,
            supplierTaxNo = input.supplierTaxNo,
            supplierId = input.supplierId,
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

  -- Create IndirectTaxTransaction for GST/VAT entries (toAccount is GOVERNMENT_INDIRECT)
  -- Create DirectTaxTransaction for TDS entries (toAccount is GOVERNMENT_DIRECT)
  entries <- catMaybes <$> mapM QLedger.findById entryIds
  forM_ entries $ \entry -> do
    mbToAccount <- QAccount.findById entry.toAccountId
    case mbToAccount of
      Just toAccount
        | toAccount.counterpartyType == Just GOVERNMENT_INDIRECT -> do
          let extCharges = sum $ map (.lineTotal) $ filter (.isExternalCharge) input.lineItems
              txnType = invoiceTypeToTransactionType input.invoiceType
              isVat = input.isVat
              indirectTaxInput =
                IndirectTaxInput
                  { transactionType = txnType,
                    referenceId = entry.referenceId,
                    taxableValue = totalAmount - entry.amount - extCharges,
                    totalTaxAmount = entry.amount,
                    gstBreakdown = input.gstBreakdown,
                    taxCreditType = Output,
                    counterpartyId = input.issuedToId,
                    gstinOfParty = if isVat then Nothing else input.gstinOfParty,
                    sacCode = if isVat then Nothing else Just (sacCodeForTransactionType txnType),
                    externalCharges = Just extCharges,
                    invoiceNumber = Just invoiceNum,
                    merchantId = input.merchantId,
                    merchantOperatingCityId = input.merchantOperatingCityId,
                    isVat = isVat,
                    issuedToTaxNo = input.issuedToTaxNo,
                    issuedByTaxNo = input.issuedByTaxNo
                  }
          void $ createIndirectTaxEntry indirectTaxInput
        | toAccount.counterpartyType == Just GOVERNMENT_DIRECT -> do
          let extCharges = sum $ map (.lineTotal) $ filter (.isExternalCharge) input.lineItems
              gstAmount = case input.gstBreakdown of
                Just breakdown -> fromMaybe 0 breakdown.cgstAmount + fromMaybe 0 breakdown.sgstAmount + fromMaybe 0 breakdown.igstAmount
                Nothing -> 0
              txnType = invoiceTypeToDirectTransactionType input.invoiceType
              directTaxInput =
                DirectTaxInput
                  { transactionType = txnType,
                    referenceId = entry.referenceId,
                    grossAmount = totalAmount - extCharges - gstAmount,
                    tdsAmount = entry.amount,
                    tdsTreatment = DirectTax.Deducted,
                    counterpartyId = input.counterpartyId,
                    panOfParty = input.panOfParty,
                    panType = input.panType,
                    tdsRateReason = input.tdsRateReason,
                    tanOfDeductee = input.tanOfDeductee,
                    tdsSection = transactionTypeToTdsSection txnType,
                    invoiceNumber = Just invoiceNum,
                    merchantId = input.merchantId,
                    merchantOperatingCityId = input.merchantOperatingCityId
                  }
          void $ createDirectTaxEntry directTaxInput
      _ -> pure ()

  pure $ Right invoice

-- | Create a standalone indirect tax (GST/VAT) transaction without an invoice.
--   When isVat=True, GST-specific columns are zeroed out and new generic columns are populated.
--   When isVat=False, both old GST columns and new generic columns are populated with the same values.
createIndirectTaxEntry ::
  (BeamFlow.BeamFlow m r) =>
  IndirectTaxInput ->
  m IndirectTaxTransaction
createIndirectTaxEntry input = do
  now <- getCurrentTime
  taxTxnId <- generateGUID
  let taxAmount = input.totalTaxAmount
      -- GST-specific fields: zero for VAT, normal split for GST
      (cgstAmt, sgstAmt, igstAmt) =
        if input.isVat
          then (0, 0, 0) -- no CGST/SGST/IGST for VAT
          else case input.gstBreakdown of
            Just breakdown ->
              ( fromMaybe 0 breakdown.cgstAmount,
                fromMaybe 0 breakdown.sgstAmount,
                fromMaybe 0 breakdown.igstAmount
              )
            Nothing ->
              let cg = taxAmount / 2.0
               in (cg, taxAmount - cg, 0)
      computedTaxRate = if input.taxableValue > 0 then realToFrac (taxAmount / input.taxableValue) * 100.0 else 0.0
      saleType = if input.isVat then Nothing else Just (if isJust input.gstinOfParty then B2B else B2C)
      taxTxn =
        IndirectTaxTransaction
          { id = Id taxTxnId,
            transactionDate = now,
            transactionType = input.transactionType,
            referenceId = input.referenceId,
            taxableValue = input.taxableValue,
            -- OLD GST columns (backward compat: zeroed for VAT)
            gstRate = if input.isVat then 0 else computedTaxRate,
            cgstAmount = cgstAmt,
            sgstAmount = sgstAmt,
            igstAmount = igstAmt,
            totalGstAmount = if input.isVat then 0 else taxAmount,
            gstCreditType = input.taxCreditType,
            gstinOfParty = input.gstinOfParty,
            saleType = saleType,
            sacCode = input.sacCode,
            -- NEW generic columns (filled for both GST and VAT)
            taxRate = Just computedTaxRate,
            taxCreditType = Just input.taxCreditType,
            issuedToTaxNo = input.issuedToTaxNo,
            issuedByTaxNo = input.issuedByTaxNo,
            totalTaxAmount = Just taxAmount,
            -- Unchanged fields
            counterpartyId = input.counterpartyId,
            invoiceNumber = input.invoiceNumber,
            creditOrDebitNoteNumber = Nothing,
            externalCharges = input.externalCharges,
            merchantId = input.merchantId,
            merchantOperatingCityId = input.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }
  QIndirectTax.create taxTxn
  pure taxTxn

-- | Create a standalone direct tax (TDS) transaction without an invoice.
createDirectTaxEntry ::
  (BeamFlow.BeamFlow m r) =>
  DirectTaxInput ->
  m DirectTaxTransaction
createDirectTaxEntry input = do
  now <- getCurrentTime
  taxTxnId <- generateGUID
  let netAmountPaid = input.grossAmount - input.tdsAmount
      tdsRate = if input.grossAmount > 0 then realToFrac (input.tdsAmount / input.grossAmount) * 100.0 else 0.0
      directTaxTxn =
        DirectTaxTransaction
          { id = Id taxTxnId,
            transactionDate = now,
            transactionType = input.transactionType,
            referenceId = input.referenceId,
            grossAmount = input.grossAmount,
            tdsRate = tdsRate,
            tdsAmount = input.tdsAmount,
            netAmountPaid = netAmountPaid,
            tdsTreatment = input.tdsTreatment,
            tdsSection = input.tdsSection,
            counterpartyId = input.counterpartyId,
            panOfParty = input.panOfParty,
            panType = input.panType,
            tdsRateReason = input.tdsRateReason,
            tanOfDeductee = input.tanOfDeductee,
            paymentDate = Just now,
            invoiceNumber = input.invoiceNumber,
            merchantId = input.merchantId,
            merchantOperatingCityId = input.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }
  QDirectTax.create directTaxTxn
  pure directTaxTxn

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

-- | Map invoiceType text to TransactionType (Indirect Tax)
invoiceTypeToTransactionType :: InvoiceType -> TransactionType
invoiceTypeToTransactionType invoiceType = case invoiceType of
  SubscriptionPurchase -> Subscription
  Ride -> IndirectTax.RideFare
  RideCancellation -> Cancellation

-- | Map invoiceType to DirectTax TransactionType (Direct Tax / TDS)
invoiceTypeToDirectTransactionType :: InvoiceType -> DirectTax.TransactionType
invoiceTypeToDirectTransactionType invoiceType = case invoiceType of
  SubscriptionPurchase -> DirectTax.Subscription
  Ride -> DirectTax.RideFare
  RideCancellation -> DirectTax.Cancellation

-- | SAC code mapping per transaction type
sacCodeForTransactionType :: TransactionType -> Text
sacCodeForTransactionType = \case
  Subscription -> "998314"
  IndirectTax.RideFare -> "996412"
  Incentive -> "998314"
  Cancellation -> "996412"
  BuyerCommission -> "998314"
  CreditNote -> "998314"
  DebitNote -> "998314"
  PGFee -> "998516" -- Payment processing services

-- | Map InvoiceType to purpose abbreviation for invoice number generation
invoiceTypeToPurpose :: InvoiceType -> Text
invoiceTypeToPurpose = \case
  SubscriptionPurchase -> purposeSubscription
  Ride -> purposeRideFare
  RideCancellation -> purposeCancellation

-- | Map DirectTax TransactionType to TDS section
transactionTypeToTdsSection :: DirectTax.TransactionType -> Maybe Text
transactionTypeToTdsSection = \case
  DirectTax.RideFare -> Just "194O"
  DirectTax.Cancellation -> Just "194O"
  DirectTax.BuyerCommission -> Just "194H"
  _ -> Nothing
