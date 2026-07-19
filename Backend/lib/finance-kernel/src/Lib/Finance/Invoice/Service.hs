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
import Domain.Types.Invoice (InvoiceType (..), IssuedToType)
import Kernel.Beam.Functions (ToTType' (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.Audit.Interface (AuditInput (..))
import qualified Lib.Finance.Audit.Service as Audit
import Lib.Finance.Domain.Types.Account (CounterpartyType (..))
import Lib.Finance.Domain.Types.AuditEntry (AuditAction (..))
import qualified Lib.Finance.Domain.Types.AuditEntry as DAuditEntry
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
import qualified Lib.Finance.Storage.Beam.DirectTaxTransaction as BeamDirectTax
import qualified Lib.Finance.Storage.Beam.IndirectTaxTransaction as BeamIndirectTax
import qualified Lib.Finance.Storage.Beam.Invoice as BeamInvoice
import qualified Lib.Finance.Storage.Queries.Account as QAccount
import qualified Lib.Finance.Storage.Queries.DirectTaxTransaction as QDirectTax
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransaction as QIndirectTax
import qualified Lib.Finance.Storage.Queries.Invoice as QInvoice
import qualified Lib.Finance.Storage.Queries.InvoiceLedgerLink as QLink
import qualified Lib.Finance.Storage.Queries.LedgerEntry as QLedger
import qualified Lib.Finance.Utils.SensitiveData as SD

--------------------------------------------------------------------------------
-- AUDIT HELPERS
--------------------------------------------------------------------------------

invoiceToAuditValue :: Invoice -> Aeson.Value
invoiceToAuditValue = Aeson.toJSON . toTType' @BeamInvoice.Invoice . hideInvoiceSensitiveFields
  where
    hideInvoiceSensitiveFields :: Invoice -> Invoice
    hideInvoiceSensitiveFields Invoice {..} =
      Invoice
        { issuedToName = Nothing,
          issuedToAddress = Nothing,
          supplierGSTIN = SD.maskTaxNo <$> supplierGSTIN,
          supplierTaxNo = SD.maskTaxNo <$> supplierTaxNo,
          irn = Nothing,
          signedQRCode = Nothing,
          ..
        }

indirectTaxToAuditValue :: IndirectTaxTransaction -> Aeson.Value
indirectTaxToAuditValue = Aeson.toJSON . toTType' @BeamIndirectTax.IndirectTaxTransaction . hideIndirectTaxTransactionSensitiveFields
  where
    hideIndirectTaxTransactionSensitiveFields :: IndirectTaxTransaction -> IndirectTaxTransaction
    hideIndirectTaxTransactionSensitiveFields IndirectTaxTransaction {..} =
      IndirectTaxTransaction
        { gstinOfParty = SD.maskTaxNo <$> gstinOfParty,
          issuedToTaxNo = SD.maskTaxNo <$> issuedToTaxNo,
          ..
        }

directTaxToAuditValue :: DirectTaxTransaction -> Aeson.Value
directTaxToAuditValue = Aeson.toJSON . toTType' @BeamDirectTax.DirectTaxTransaction . hideDirectTaxTransactionSensitiveFields
  where
    hideDirectTaxTransactionSensitiveFields :: DirectTaxTransaction -> DirectTaxTransaction
    hideDirectTaxTransactionSensitiveFields DirectTaxTransaction {..} =
      DirectTaxTransaction
        { panOfParty = SD.maskTaxNo <$> panOfParty,
          tanOfDeductee = SD.maskTaxNo <$> tanOfDeductee,
          ..
        }

logFinanceEntityAudit ::
  BeamFlow.BeamFlow m r =>
  DAuditEntry.AuditEntityType ->
  Text ->
  AuditAction ->
  ActorInfo ->
  Maybe Aeson.Value ->
  Aeson.Value ->
  Text ->
  Text ->
  m ()
logFinanceEntityAudit entityType entityId action actorInfo mbBefore afterState merchantId merchantOperatingCityId = do
  auditResult <-
    Audit.logAudit
      AuditInput
        { entityType = entityType,
          entityId = entityId,
          action = action,
          actorType = actorInfo.actorType,
          actorId = actorInfo.actorId,
          beforeState = mbBefore,
          afterState = Just afterState,
          merchantId = merchantId,
          merchantOperatingCityId = merchantOperatingCityId
        }
  case auditResult of
    Left err -> logWarning $ "Failed to audit " <> show entityType <> " (" <> show action <> "): " <> show err
    Right _ -> pure ()

auditInvoiceCreate ::
  BeamFlow.BeamFlow m r =>
  ActorInfo ->
  Invoice ->
  m ()
auditInvoiceCreate actorInfo invoice =
  logFinanceEntityAudit
    DAuditEntry.Invoice
    invoice.id.getId
    Created
    actorInfo
    Nothing
    (invoiceToAuditValue invoice)
    invoice.merchantId
    invoice.merchantOperatingCityId

auditInvoiceUpdate ::
  BeamFlow.BeamFlow m r =>
  ActorInfo ->
  AuditAction ->
  Invoice ->
  Invoice ->
  m ()
auditInvoiceUpdate actorInfo action before after =
  logFinanceEntityAudit
    DAuditEntry.Invoice
    after.id.getId
    action
    actorInfo
    (Just $ invoiceToAuditValue before)
    (invoiceToAuditValue after)
    after.merchantId
    after.merchantOperatingCityId

auditIndirectTaxCreate ::
  BeamFlow.BeamFlow m r =>
  ActorInfo ->
  IndirectTaxTransaction ->
  m ()
auditIndirectTaxCreate actorInfo txn =
  logFinanceEntityAudit
    DAuditEntry.IndirectTaxTransaction
    txn.id.getId
    Created
    actorInfo
    Nothing
    (indirectTaxToAuditValue txn)
    txn.merchantId
    txn.merchantOperatingCityId

auditDirectTaxCreate ::
  BeamFlow.BeamFlow m r =>
  ActorInfo ->
  DirectTaxTransaction ->
  m ()
auditDirectTaxCreate actorInfo txn =
  logFinanceEntityAudit
    DAuditEntry.DirectTaxTransaction
    txn.id.getId
    Created
    actorInfo
    Nothing
    (directTaxToAuditValue txn)
    txn.merchantId
    txn.merchantOperatingCityId

--------------------------------------------------------------------------------
-- CREATE OPERATIONS
--------------------------------------------------------------------------------

-- | Create an invoice and link to ledger entries.
--   If any linked entry targets a GOVERNMENT_INDIRECT account (GST),
--   an IndirectTaxTransaction record is also created.
--   If any linked entry targets a GOVERNMENT_DIRECT account (TDS),
--   a DirectTaxTransaction record is also created.
--   E-invoice (IRN) generation is NOT performed here; callers that need it
--   (e.g. driver-app) should invoke their own e-invoice helper after this.
createInvoice ::
  ( BeamFlow.BeamFlow m r,
    Redis.HedisFlow m r,
    HasActorInfo m r
  ) =>
  InvoiceInput ->
  [Id LedgerEntry] -> -- Ledger entries to link
  m (Either FinanceError Invoice)
createInvoice input entryIds = do
  actorInfo <- asks (.actorInfo)
  now <- getCurrentTime
  invoiceId <- generateGUID
  -- AggregatedCommission uses an isolated CMB counter (gap-less customer-facing
  -- series); others share the global counter. dbFallback must match the isolation.
  let purposeAbbr = invoiceTypeToPurpose input.invoiceType
      (mbKeySuffix, dbFallback) = case input.invoiceType of
        AggregatedCommission ->
          (Just purposeAggregatedCommission, fmap (.invoiceNumber) <$> QInvoice.findLatestAggregatedCommissionByCreatedAt now)
        _ ->
          (Nothing, fmap (.invoiceNumber) <$> QInvoice.findLatestByCreatedAt now)
  invoiceNum <- generateInvoiceNumber purposeAbbr mbKeySuffix now dbFallback

  -- Calculate totals from line items
  -- subtotal: excludes external charges (toll, parking) and tax line items
  -- totalAmount: sum of all line items (what rider pays)
  let lineItemsJson = Aeson.toJSON input.lineItems
      totalAmount = sum $ map (.lineTotal) input.lineItems
      externalTotal = sum $ map (.lineTotal) $ filter (.isExternalCharge) input.lineItems
      taxTotal = sum $ map (.lineTotal) $ filter (\li -> not li.isExternalCharge && li.itemType == Just Tax) input.lineItems
      subtotal = totalAmount - externalTotal - taxTotal
  logDebug $ "Sum of unit price of all line items: " <> show (sum $ map (.unitPrice) input.lineItems)
  let invoice =
        Invoice
          { id = Id invoiceId,
            invoiceNumber = invoiceNum,
            invoiceType = input.invoiceType,
            entityReferenceId = input.entityReferenceId,
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
            merchantGstin = input.merchantGstin,
            referenceId = input.referenceId,
            referenceInvoiceNumber = input.referenceInvoiceNumber,
            lineItems = lineItemsJson,
            subtotal = subtotal,
            taxBreakdown = Nothing,
            totalAmount = totalAmount,
            currency = input.currency,
            status = Draft,
            issuedAt = now,
            dueAt = input.dueAt,
            periodStart = input.periodStart,
            periodEnd = input.periodEnd,
            merchantId = input.merchantId,
            merchantOperatingCityId = input.merchantOperatingCityId,
            createdBy = Just actorInfo.actorType,
            createdById = actorInfo.actorId,
            updatedBy = Just actorInfo.actorType,
            updatedById = actorInfo.actorId,
            createdAt = now,
            updatedAt = now,
            irn = Nothing,
            signedQRCode = Nothing,
            paymentMode = input.paymentMode
          }

  QInvoice.create invoice
  auditInvoiceCreate actorInfo invoice

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
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  IndirectTaxInput ->
  m IndirectTaxTransaction
createIndirectTaxEntry input = do
  actorInfo <- asks (.actorInfo)
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
            createdBy = Just actorInfo.actorType,
            createdById = actorInfo.actorId,
            updatedBy = Just actorInfo.actorType,
            updatedById = actorInfo.actorId,
            createdAt = now,
            updatedAt = now
          }
  QIndirectTax.create taxTxn
  auditIndirectTaxCreate actorInfo taxTxn
  pure taxTxn

-- | Create a standalone direct tax (TDS) transaction without an invoice.
createDirectTaxEntry ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  DirectTaxInput ->
  m DirectTaxTransaction
createDirectTaxEntry input = do
  actorInfo <- asks (.actorInfo)
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
            createdBy = Just actorInfo.actorType,
            createdById = actorInfo.actorId,
            updatedBy = Just actorInfo.actorType,
            updatedById = actorInfo.actorId,
            createdAt = now,
            updatedAt = now
          }
  QDirectTax.create directTaxTxn
  auditDirectTaxCreate actorInfo directTaxTxn
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
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  Id Invoice ->
  InvoiceStatus ->
  m ()
updateInvoiceStatus invoiceId newStatus = do
  actorInfo <- asks (.actorInfo)
  mbBefore <- QInvoice.findById invoiceId
  forM_ mbBefore $ \before ->
    when (before.status /= newStatus) $ do
      QInvoice.updateStatus newStatus (Just actorInfo.actorType) actorInfo.actorId invoiceId
      mbAfter <- QInvoice.findById invoiceId
      forM_ mbAfter $ \after ->
        auditInvoiceUpdate actorInfo StatusChanged before after

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
  IssuedToType -> -- Issued to type
  Text -> -- Issued to ID
  m [Invoice]
findByIssuedTo = QInvoice.findByIssuedTo

-- | Map invoiceType text to TransactionType (Indirect Tax)
invoiceTypeToTransactionType :: InvoiceType -> TransactionType
invoiceTypeToTransactionType invoiceType = case invoiceType of
  SubscriptionPurchase -> Subscription
  Ride -> IndirectTax.RideFare
  RideCancellation -> Cancellation
  Commission -> BuyerCommission
  AggregatedCommission -> BuyerCommission
  Refund -> CreditNote

-- | Map invoiceType to DirectTax TransactionType (Direct Tax / TDS)
invoiceTypeToDirectTransactionType :: InvoiceType -> DirectTax.TransactionType
invoiceTypeToDirectTransactionType invoiceType = case invoiceType of
  SubscriptionPurchase -> DirectTax.Subscription
  Ride -> DirectTax.RideFare
  RideCancellation -> DirectTax.Cancellation
  Commission -> DirectTax.BuyerCommission
  AggregatedCommission -> DirectTax.BuyerCommission
  Refund -> DirectTax.RideFare

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
  Commission -> purposeCommission
  AggregatedCommission -> purposeAggregatedCommission
  Refund -> purposeRefund

-- | Map DirectTax TransactionType to TDS section
transactionTypeToTdsSection :: DirectTax.TransactionType -> Maybe Text
transactionTypeToTdsSection = \case
  DirectTax.RideFare -> Just "194O"
  DirectTax.Cancellation -> Just "194O"
  DirectTax.BuyerCommission -> Just "194H"
  _ -> Nothing
