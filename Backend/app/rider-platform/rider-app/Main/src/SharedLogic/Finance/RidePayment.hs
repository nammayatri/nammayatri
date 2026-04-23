{-
  Finance integration for rider-side (BAP) ride payments.

  Online rider-obligation:
    * Create (PENDING): BuyerAsset → OwnerLiability (Dr A/R, Cr suspense).
    * Settle (2-leg capture):
        BuyerExternal  → BuyerAsset    (cash-in clears A/R)
        OwnerLiability → BuyerExternal (cash-out drains suspense to BPP)

  Cash rider-obligation:
    * Create (PENDING): BuyerControl → OwnerControl (Control accounts;
      memo-only record of rider→driver cash that bypasses BAP).
    * Settle: mark SETTLED only — no cash flowed through BAP, so no
      capture-side legs are posted. The Control balances persist as the
      per-booking memo.

  Offer discount / cashback topology is unchanged from the pre-existing
  implementation (deferred until VAT/offer restructuring lands).

  On cancel / recreate: 'voidRidePaymentLedger' voids the pending ledger
  entries AND the invoice(s) linked to them, so stale invoices don't
  persist on retry flows.
-}
module SharedLogic.Finance.RidePayment
  ( -- * Reference type constants
    ridePaymentRefRideFare,
    ridePaymentRefGST,
    ridePaymentRefPlatformFee,
    ridePaymentRefTip,
    ridePaymentRefCancellationFee,
    ridePaymentRefCancellationGST,
    ridePaymentRefOfferDiscount,
    ridePaymentRefCashbackPayout,

    -- * Settlement reason constants
    settledReasonRidePayment,
    settledReasonDebtSettlement,
    settledReasonTipPayment,

    -- * Context builder
    buildRiderFinanceCtx,

    -- * Ledger operations
    createRidePaymentLedger,
    createFullyDiscountedRidePaymentLedger,
    upsertCoreRidePaymentLedger,
    UpsertCoreLedgerResult (..),
    settleRidePaymentLedger,
    findUnsettledCashbackEntriesForRefs,
    markCashbackEntriesAsPaidOut,
    voidRidePaymentLedger,
    createTipLedger,
    createCancellationFeeLedger,

    -- * Query helpers (replaces PaymentInvoice reads)
    findRidePaymentEntries,
    findPendingRidePaymentEntries,
    findUnsettledRidePaymentEntries,
    findDueRidePaymentEntries,
    markEntriesAsDue,
    isRidePaymentSettled,
    coreRidePaymentRefTypes,
    allRidePaymentRefTypes,
  )
where

import qualified Data.List as List
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (MonadFlow, logError, logInfo)
import Lib.Finance
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Finance.Invoice.Service as InvoiceSvc
import Lib.Finance.Ledger.Service (getEntry)
import qualified Lib.Finance.Ledger.Service
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.Invoice as QInvoice

-- ---------------------------------------------------------------------------
-- Reference type constants (no hardcoded strings)
-- ---------------------------------------------------------------------------

ridePaymentRefRideFare :: Text
ridePaymentRefRideFare = "RideFare"

ridePaymentRefGST :: Text
ridePaymentRefGST = "RideGST"

ridePaymentRefPlatformFee :: Text
ridePaymentRefPlatformFee = "PlatformFee"

ridePaymentRefTip :: Text
ridePaymentRefTip = "RideTip"

ridePaymentRefCancellationFee :: Text
ridePaymentRefCancellationFee = "CancellationFee"

ridePaymentRefCancellationGST :: Text
ridePaymentRefCancellationGST = "CancellationGST"

ridePaymentRefOfferDiscount :: Text
ridePaymentRefOfferDiscount = "OfferDiscount"

ridePaymentRefCashbackPayout :: Text
ridePaymentRefCashbackPayout = "CashbackPayout"

-- ---------------------------------------------------------------------------
-- Settlement reason constants
-- ---------------------------------------------------------------------------

settledReasonRidePayment :: Text
settledReasonRidePayment = "RidePaymentCaptured"

settledReasonDebtSettlement :: Text
settledReasonDebtSettlement = "DebtSettlementCaptured"

settledReasonTipPayment :: Text
settledReasonTipPayment = "TipPaymentCaptured"

-- ---------------------------------------------------------------------------
-- Ledger entry results
-- ---------------------------------------------------------------------------

data RidePaymentLedgerResult = RidePaymentLedgerResult
  { invoiceId :: Maybe (Id FInvoice.Invoice),
    entryIds :: [Id LE.LedgerEntry]
  }
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Build FinanceCtx for rider-side
-- ---------------------------------------------------------------------------

buildRiderFinanceCtx ::
  Text -> -- merchantId
  Text -> -- merchantOperatingCityId
  Currency ->
  Bool -> -- isOnline (True = online/card/platform-wallet, False = cash)
  Text -> -- riderId (person ID)
  Text -> -- referenceId (ride ID)
  Maybe Text -> -- merchantName
  Maybe Text -> -- merchantShortId
  FinanceCtx
buildRiderFinanceCtx merchantId merchantOpCityId currency isOnline riderId referenceId merchantName merchantShortId =
  FinanceCtx
    { merchantId = merchantId,
      merchantOpCityId = merchantOpCityId,
      currency = currency,
      isOnline = isOnline,
      counterpartyType = RIDER,
      counterpartyId = riderId,
      referenceId = referenceId,
      merchantName = merchantName,
      merchantShortId = merchantShortId,
      issuedByAddress = Nothing,
      supplierName = Nothing,
      supplierGSTIN = Nothing,
      merchantGstin = Nothing,
      supplierVatNumber = Nothing,
      supplierAddress = Nothing,
      merchantVatNumber = Nothing,
      supplierId = Nothing,
      panOfParty = Nothing,
      panType = Nothing,
      tdsRateReason = Nothing
    }

-- | Reference types that carry real rider → BPP rider-obligation movement.
--   Online settle uses the 2-leg capture pattern; cash settle is mark-only.
riderObligationRefTypes :: [Text]
riderObligationRefTypes =
  [ ridePaymentRefRideFare,
    ridePaymentRefGST,
    ridePaymentRefPlatformFee,
    ridePaymentRefTip,
    ridePaymentRefCancellationFee,
    ridePaymentRefCancellationGST
  ]

-- ---------------------------------------------------------------------------
-- 1. Create PENDING ledger entries after payment order creation
-- ---------------------------------------------------------------------------

-- | Create PENDING ledger entries after successful payment order creation.
--   Online: BuyerAsset → OwnerLiability (Dr A/R, Cr per-rider suspense).
--   Cash:   BuyerControl → OwnerControl (Control memo — no cash through BAP).
--   Offer discount / cashback topology is unchanged from the pre-existing flow.
createRidePaymentLedger :: -- RIDE ASSIGNED
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  FinanceCtx ->
  HighPrecMoney -> -- rideFare (without GST)
  HighPrecMoney -> -- gstAmount
  HighPrecMoney -> -- platformFee (application fee / commission)
  HighPrecMoney -> -- offerDiscountAmount (charge reduction, 0 for CASHBACK)
  HighPrecMoney -> -- cashbackPayoutAmount (amount to pay back to rider, 0 for DISCOUNT)
  m (Either FinanceError RidePaymentLedgerResult)
createRidePaymentLedger ctx rideFare gstAmount platformFee offerDiscountAmount cashbackPayoutAmount = do
  result <- runFinance ctx $ do
    -- Rider obligations — online uses real A/R + suspense; cash uses Control pair.
    let (riderSrc, riderDst) =
          if ctx.isOnline
            then (BuyerAsset, OwnerLiability)
            else (BuyerControl, OwnerControl)
        postRiderObligation amt ref = void $ transferPending riderSrc riderDst amt ref
    postRiderObligation rideFare ridePaymentRefRideFare
    postRiderObligation gstAmount ridePaymentRefGST
    postRiderObligation platformFee ridePaymentRefPlatformFee

    when (offerDiscountAmount > 0) $
      void $ transferPending BuyerExpense BuyerAsset offerDiscountAmount ridePaymentRefOfferDiscount

    when (cashbackPayoutAmount > 0) $
      void $ transferPending BuyerExpense OwnerLiability cashbackPayoutAmount ridePaymentRefCashbackPayout

    -- Create Draft invoice with line items
    invoice
      InvoiceConfig
        { invoiceType = FInvoice.Ride,
          issuedToType = "RIDER",
          issuedToId = ctx.counterpartyId,
          issuedToName = Nothing,
          issuedToAddress = Nothing,
          lineItems =
            filter
              (\li -> li.lineTotal > 0)
              [ InvoiceLineItem
                  { description = ridePaymentRefRideFare,
                    quantity = 1,
                    unitPrice = rideFare,
                    lineTotal = rideFare,
                    invoiceLineItemType = RideFareType,
                    isExternalCharge = False
                  },
                InvoiceLineItem
                  { description = ridePaymentRefGST,
                    quantity = 1,
                    unitPrice = gstAmount,
                    lineTotal = gstAmount,
                    invoiceLineItemType = RideGST,
                    isExternalCharge = False
                  },
                InvoiceLineItem
                  { description = ridePaymentRefPlatformFee,
                    quantity = 1,
                    unitPrice = platformFee,
                    lineTotal = platformFee,
                    invoiceLineItemType = PlatformFee,
                    isExternalCharge = False
                  },
                InvoiceLineItem
                  { description = ridePaymentRefOfferDiscount,
                    quantity = 1,
                    unitPrice = negate offerDiscountAmount,
                    lineTotal = negate offerDiscountAmount,
                    invoiceLineItemType = OfferDiscount,
                    isExternalCharge = False
                  },
                InvoiceLineItem
                  { description = ridePaymentRefCashbackPayout,
                    quantity = 1,
                    unitPrice = cashbackPayoutAmount,
                    lineTotal = cashbackPayoutAmount,
                    invoiceLineItemType = CashbackPayout,
                    isExternalCharge = False
                  }
              ],
          gstBreakdown = Nothing,
          isVat = False,
          issuedToTaxNo = Nothing,
          issuedByTaxNo = Nothing
        }
  case result of
    Left err -> do
      logError $ "Failed to create ride payment ledger: " <> show err
      pure $ Left err
    Right (mbInvoiceId, entryIds) ->
      pure $ Right RidePaymentLedgerResult {invoiceId = mbInvoiceId, entryIds}

-- ---------------------------------------------------------------------------
-- 1a. Upsert core ride payment ledger on amount change
-- ---------------------------------------------------------------------------

-- | Outcome of 'upsertCoreRidePaymentLedger'. 'coreEntryIds' is the set of
--   PENDING core entries after the upsert — the caller can pass these
--   directly to 'settleRidePaymentLedger' / debt-settle flows.
data UpsertCoreLedgerResult = UpsertCoreLedgerResult
  { coreEntryIds :: [Id LE.LedgerEntry],
    invoiceId :: Maybe (Id FInvoice.Invoice),
    -- | Whether a fresh 'createRidePaymentLedger' was issued on this call.
    didCreate :: Bool,
    -- | Whether stale PENDING core entries were voided (amount-change recreate).
    didVoidStale :: Bool
  }
  deriving (Show)

-- | Idempotent create-or-recreate of the core rider-obligation ledger entries
--   (RideFare / GST / PlatformFee / OfferDiscount / CashbackPayout) for a ride.
--
--     * No prior core entries → create fresh.
--     * Prior PENDING core entries whose total differs from
--       @rideFare + gstAmount + platformFee@ → void the stale PENDING set
--       (and their invoice, via 'voidRidePaymentLedger') and recreate at the
--       new amounts.
--     * Prior core entries but totals match → no-op.
--
upsertCoreRidePaymentLedger ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  FinanceCtx ->
  HighPrecMoney -> -- rideFare (without GST)
  HighPrecMoney -> -- gstAmount
  HighPrecMoney -> -- platformFee
  HighPrecMoney -> -- offerDiscountAmount
  HighPrecMoney -> -- cashbackPayoutAmount
  m UpsertCoreLedgerResult
upsertCoreRidePaymentLedger ctx rideFare gstAmount platformFee offerDiscountAmount cashbackPayoutAmount = do
  let rideId = ctx.referenceId
  existingEntries <- findRidePaymentEntries rideId
  let coreEntries = filter (\e -> e.referenceType `elem` coreRidePaymentRefTypes) existingEntries
      pendingCoreEntries = filter (\e -> e.status == LE.PENDING) coreEntries
      newTotal = rideFare + gstAmount + platformFee
      oldTotal = sum $ map (.amount) pendingCoreEntries
      doCreate = do
        result <- createRidePaymentLedger ctx rideFare gstAmount platformFee offerDiscountAmount cashbackPayoutAmount
        case result of
          Right res -> pure (res.entryIds, res.invoiceId)
          Left err -> do
            logError $ "Failed to create core ride payment ledger for " <> rideId <> ": " <> show err
            pure ([], Nothing)
  if null coreEntries
    then do
      (newIds, mbInv) <- doCreate
      logInfo $ "Created PENDING core ride payment ledger entries for ride: " <> rideId
      pure UpsertCoreLedgerResult {coreEntryIds = newIds, invoiceId = mbInv, didCreate = True, didVoidStale = False}
    else
      if not (null pendingCoreEntries) && newTotal /= oldTotal
        then do
          let staleIds = map (.id) pendingCoreEntries
          voidRidePaymentLedger staleIds
          logInfo $
            "Voided " <> show (length staleIds) <> " stale PENDING core entries (old="
              <> show oldTotal
              <> " new="
              <> show newTotal
              <> ") for ride: "
              <> rideId
          (newIds, mbInv) <- doCreate
          pure UpsertCoreLedgerResult {coreEntryIds = newIds, invoiceId = mbInv, didCreate = True, didVoidStale = True}
        else do
          logInfo $ "Core ride payment ledger already up to date for ride: " <> rideId
          pure
            UpsertCoreLedgerResult
              { coreEntryIds = map (.id) pendingCoreEntries,
                invoiceId = Nothing,
                didCreate = False,
                didVoidStale = False
              }

-- ---------------------------------------------------------------------------
-- 1b. Create SETTLED ledger entries for fully discounted rides (amount = 0)
-- ---------------------------------------------------------------------------

-- | Create ledger entries for a fully discounted ride (100% offer covers fare).
--   Thin wrapper over create + settle: PENDING entries are created as usual,
--   then immediately settled so the ledger ends in the captured state with
--   no outstanding rider obligation.
createFullyDiscountedRidePaymentLedger ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  FinanceCtx ->
  HighPrecMoney -> -- rideFare (original, before discount)
  HighPrecMoney -> -- gstAmount
  HighPrecMoney -> -- platformFee
  HighPrecMoney -> -- offerDiscountAmount (should equal rideFare + gstAmount + platformFee)
  m (Either FinanceError RidePaymentLedgerResult)
createFullyDiscountedRidePaymentLedger ctx rideFare gstAmount platformFee offerDiscountAmount = do
  createResult <-
    createRidePaymentLedger
      ctx
      rideFare
      gstAmount
      platformFee
      offerDiscountAmount
      0 -- cashback doesn't apply to fully-discounted flow
  case createResult of
    Left err -> pure $ Left err
    Right res -> do
      settleResult <- settleRidePaymentLedger ctx res.entryIds settledReasonRidePayment
      case settleResult of
        Left err -> do
          logError $ "Failed to settle fully discounted ride payment ledger: " <> show err
          pure $ Left err
        Right _ -> do
          logInfo "Created + settled ledger for fully discounted ride (offer covers 100%)"
          pure $ Right res

-- ---------------------------------------------------------------------------
-- 2. Settle ledger entries after payment capture success
-- ---------------------------------------------------------------------------

-- | Settle PENDING entries after payment capture success.
--   Flips each entry to SETTLED (posting its accrual balance delta via the
--   updated 'settleEntry'), then for online rider-obligation entries posts
--   the 2-leg capture pattern:
--     BuyerExternal  → BuyerAsset    (cash-in clears A/R)
--     OwnerLiability → BuyerExternal (cash-out drains suspense to BPP)
--   Cash rider-obligation entries use Control accounts; settle is mark-only.
--   Amounts come from the entries themselves.
--
--   Invoice is NOT created here — it was created (and linked) inside
--   'createRidePaymentLedger' and voided together with the ledger entries in
--   'voidRidePaymentLedger'.
settleRidePaymentLedger ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  FinanceCtx ->
  [Id LE.LedgerEntry] -> -- entry IDs from createRidePaymentLedger
  Text -> -- settlement reason
  m (Either FinanceError ())
settleRidePaymentLedger ctx entryIds settledReason = do
  entryDetails <- forM entryIds $ \entryId -> do
    mbEntry <- Lib.Finance.Ledger.Service.getEntry entryId
    settleEntry entryId
    pure mbEntry
  let settledEntries = catMaybes entryDetails
      isOnline = ctx.isOnline
  logInfo $
    "Settled " <> show (length settledEntries) <> " ride payment ledger entries, reason: "
      <> settledReason
      <> ", isOnline="
      <> show isOnline
  result <- runFinance ctx $
    forM_ settledEntries $ \entry ->
      if entry.referenceType `elem` riderObligationRefTypes
        then when isOnline $ do
          transfer_ BuyerExternal BuyerAsset entry.amount entry.referenceType
          transfer_ OwnerLiability BuyerExternal entry.amount entry.referenceType
        else
          when (entry.referenceType == ridePaymentRefOfferDiscount && isOnline) $
            transfer_ BuyerAsset BuyerExternal entry.amount entry.referenceType
  case result of
    Left err -> do
      logError $ "Failed to post settlement capture legs: " <> show err
      pure $ Left err
    Right _ -> pure $ Right ()

-- | Cashback ledger entries (refType=CashbackPayout) for a set of rideIds
--   that are still UNSETTLED (not yet flagged PAID_OUT). Used by the
--   payout job to build the list that gets stashed on PayoutRequest.
findUnsettledCashbackEntriesForRefs ::
  (BeamFlow.BeamFlow m r) =>
  [Text] ->
  m [LE.LedgerEntry]
findUnsettledCashbackEntriesForRefs refIds = do
  entries <- concat <$> mapM (getEntriesByReference ridePaymentRefCashbackPayout) refIds
  pure $ filter (\e -> e.settlementStatus /= Just LE.PAID_OUT) entries

-- | Called on successful Juspay payout webhook for a RIDE_OFFER_CASHBACK
--   payout. Drains the OwnerLiability that accrued for the rider's
--   cashback (OwnerLiability → BuyerExternal for the sum of the still-
--   unsettled cashback entries), then flags those entries PAID_OUT with
--   the PayoutRequest id.
--
--   Semantics mirror the driver-side pair
--   'createWalletEntryDelta (negate amount)' + 'settleWalletEntries'.
--   Idempotent: replaying the webhook finds nothing to drain and no
--   entries to flag.
markCashbackEntriesAsPaidOut ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  FinanceCtx ->
  [Id LE.LedgerEntry] -> -- entry IDs (from PayoutRequest.ledgerEntryIds)
  Text ->                -- PayoutRequest id → settlementId on the row
  m (Either FinanceError ())
markCashbackEntriesAsPaidOut ctx entryIds payoutRequestId = do
  mbEntries <- forM entryIds getEntry
  let entries = catMaybes mbEntries
      eligible = filter (\e -> e.settlementStatus /= Just LE.PAID_OUT) entries
      totalAmount = sum (map (.amount) eligible)
  if null eligible
    then do
      logInfo $ "markCashbackEntriesAsPaidOut: nothing eligible (payoutRequestId=" <> payoutRequestId <> ")"
      pure $ Right ()
    else do
      drainResult <- runFinance ctx $
        when (totalAmount > 0) $
          transfer_ OwnerLiability BuyerExternal totalAmount ridePaymentRefCashbackPayout
      case drainResult of
        Left err -> do
          logError $ "Cashback payout drain leg failed: " <> show err
          pure $ Left err
        Right _ -> do
          Lib.Finance.Ledger.Service.markEntriesAsPaidOut (map (.id) eligible) payoutRequestId
          logInfo $
            "Cashback payout settled — " <> show (length eligible)
              <> " entries → PAID_OUT (payoutRequestId=" <> payoutRequestId
              <> ", drainAmount=" <> show totalAmount <> ")"
          pure $ Right ()

-- ---------------------------------------------------------------------------
-- 3. Void ledger entries when payment intent is cancelled
-- ---------------------------------------------------------------------------

-- | Void all PENDING ledger entries and any invoice linked to them.
--   Used when payment is cancelled or when the fare is recomputed and we
--   recreate the ledger — stale invoices from the previous attempt must
--   also be marked VOIDED so dashboards / lookups don't surface them.
voidRidePaymentLedger ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  [Id LE.LedgerEntry] ->
  m ()
voidRidePaymentLedger entryIds = do
  -- Collect linked invoice IDs BEFORE voiding the entries, in case
  -- voiding cascades or otherwise disrupts the link lookup.
  mbInvoices <- forM entryIds $ \entryId -> InvoiceSvc.getInvoiceForEntry entryId
  let uniqueInvoiceIds = List.nub [inv.id | Just inv <- mbInvoices]
  forM_ entryIds $ \entryId ->
    voidEntry entryId "PaymentIntentCancelled"
  forM_ uniqueInvoiceIds $ \invId ->
    QInvoice.updateStatus FInvoice.Voided invId
  logInfo $
    "Voided " <> show (length entryIds) <> " ride payment ledger entries and "
      <> show (length uniqueInvoiceIds)
      <> " linked invoice(s)"

-- ---------------------------------------------------------------------------
-- 4. Tip ledger entries
-- ---------------------------------------------------------------------------

-- | Create PENDING tip ledger entries.
--   Accrual only: BuyerAsset → OwnerLiability in PENDING state.
--   Capture-side legs happen in settleRidePaymentLedger when chargePaymentIntent
--   succeeds. Tip is a pure pass-through to the driver — no BAP commission.
createTipLedger ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  FinanceCtx ->
  HighPrecMoney -> -- tipAmount
  m (Either FinanceError [Id LE.LedgerEntry])
createTipLedger ctx tipAmount = do
  result <- runFinance ctx $ do
    transferPending BuyerAsset OwnerLiability tipAmount ridePaymentRefTip
  case result of
    Left err -> do
      logError $ "Failed to create tip ledger: " <> show err
      pure $ Left err
    Right (_, entryIds) -> pure $ Right entryIds

-- ---------------------------------------------------------------------------
-- 5. Cancellation fee ledger entries
-- ---------------------------------------------------------------------------

-- | Create ledger entries for cancellation fee. 3-leg pass-through, SETTLED
--   immediately (cash already captured via payment intent). Same online
--   rider-obligation shape as ride-fare.
createCancellationFeeLedger ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  FinanceCtx ->
  HighPrecMoney -> -- cancellationFee (without GST)
  HighPrecMoney -> -- cancellationGST
  m (Either FinanceError (Maybe (Id FInvoice.Invoice), [Id LE.LedgerEntry]))
createCancellationFeeLedger ctx cancellationFee cancellationGST = do
  result <- runFinance ctx $ do
    transfer_ BuyerAsset OwnerLiability cancellationFee ridePaymentRefCancellationFee
    transfer_ BuyerAsset OwnerLiability cancellationGST ridePaymentRefCancellationGST
    transfer_ BuyerExternal BuyerAsset cancellationFee ridePaymentRefCancellationFee
    transfer_ BuyerExternal BuyerAsset cancellationGST ridePaymentRefCancellationGST
    transfer_ OwnerLiability BuyerExternal cancellationFee ridePaymentRefCancellationFee
    transfer_ OwnerLiability BuyerExternal cancellationGST ridePaymentRefCancellationGST
    -- Invoice for cancellation
    invoice
      InvoiceConfig
        { invoiceType = FInvoice.RideCancellation,
          issuedToType = "RIDER",
          issuedToId = ctx.counterpartyId,
          issuedToName = Nothing,
          issuedToAddress = Nothing,
          lineItems =
            filter
              (\li -> li.lineTotal > 0)
              [ InvoiceLineItem
                  { description = ridePaymentRefCancellationFee,
                    quantity = 1,
                    unitPrice = cancellationFee,
                    lineTotal = cancellationFee,
                    invoiceLineItemType = CancellationFee,
                    isExternalCharge = False
                  },
                InvoiceLineItem
                  { description = ridePaymentRefCancellationGST,
                    quantity = 1,
                    unitPrice = cancellationGST,
                    lineTotal = cancellationGST,
                    invoiceLineItemType = CancellationGST,
                    isExternalCharge = False
                  }
              ],
          gstBreakdown = Nothing,
          isVat = False,
          issuedToTaxNo = Nothing,
          issuedByTaxNo = Nothing
        }
  case result of
    Left err -> do
      logError $ "Failed to create cancellation fee ledger: " <> show err
      pure $ Left err
    Right (mbInvoiceId, entryIds) -> pure $ Right (mbInvoiceId, entryIds)

-- ---------------------------------------------------------------------------
-- 6. Query helpers (replaces PaymentInvoice reads)
-- ---------------------------------------------------------------------------

-- | Core ride fare reference types (excludes tip and cancellation).
coreRidePaymentRefTypes :: [Text]
coreRidePaymentRefTypes =
  [ ridePaymentRefRideFare,
    ridePaymentRefGST,
    ridePaymentRefPlatformFee,
    ridePaymentRefOfferDiscount,
    ridePaymentRefCashbackPayout
  ]

-- | All ride payment reference types.
allRidePaymentRefTypes :: [Text]
allRidePaymentRefTypes =
  coreRidePaymentRefTypes
    <>  [ ridePaymentRefTip,
          ridePaymentRefCancellationFee,
          ridePaymentRefCancellationGST
        ]

-- | Find all ledger entries for a ride (all reference types).
--   Replaces: QPaymentInvoice.findAllByRideId
findRidePaymentEntries ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- rideId
  m [LE.LedgerEntry]
findRidePaymentEntries rideId = do
  entries <- concat <$> mapM (\refType -> getEntriesByReference refType rideId) allRidePaymentRefTypes
  pure entries

-- | Find PENDING ledger entries for a ride (unpaid).
--   Replaces: QPaymentInvoiceExtra.findByRideIdAndTypeAndPurpose rideId PAYMENT RIDE
--             where paymentStatus == PENDING
findPendingRidePaymentEntries ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- rideId
  m [LE.LedgerEntry]
findPendingRidePaymentEntries rideId = do
  entries <- findRidePaymentEntries rideId
  pure $ filter (\e -> e.status == LE.PENDING) entries

-- | Find unsettled ledger entries for a ride (PENDING or DUE — anything not yet settled).
findUnsettledRidePaymentEntries ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- rideId
  m [LE.LedgerEntry]
findUnsettledRidePaymentEntries rideId = do
  entries <- findRidePaymentEntries rideId
  pure $ filter (\e -> e.status == LE.PENDING || e.status == LE.DUE) entries

-- | Find DUE ledger entries for a ride (capture was attempted and failed).
findDueRidePaymentEntries ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- rideId
  m [LE.LedgerEntry]
findDueRidePaymentEntries rideId = do
  entries <- findRidePaymentEntries rideId
  pure $ filter (\e -> e.status == LE.DUE) entries

-- | Mark ledger entries as DUE (capture was attempted and failed).
markEntriesAsDue ::
  (BeamFlow.BeamFlow m r) =>
  [Id LE.LedgerEntry] ->
  m ()
markEntriesAsDue entryIds =
  forM_ entryIds $ \entryId -> Lib.Finance.Ledger.Service.updateEntryStatus entryId LE.DUE

-- | Check if ride payment is settled (captured).
--   Replaces: checking PaymentInvoice.paymentStatus == CAPTURED
isRidePaymentSettled ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- rideId
  m Bool
isRidePaymentSettled rideId = do
  rideEntries <- getEntriesByReference ridePaymentRefRideFare rideId
  let settledEntries = filter (\e -> e.status == LE.SETTLED) rideEntries
  pure $ not (null settledEntries)
