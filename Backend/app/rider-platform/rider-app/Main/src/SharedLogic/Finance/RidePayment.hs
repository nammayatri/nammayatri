{-
  Finance integration for rider-side (BAP) ride payments.

  ┌────────────────────────────────────────────────────────────────────────────┐
  │ BAP ledger model.                                                          │
  │ - Online rider-obligation: BuyerAsset → OwnerLiability at create; 2-leg   │
  │   settle clears A/R via BuyerExternal and drains OwnerLiability. Standard │
  │   pass-through flow rider → BAP → BPP.                                    │
  │ - Cash rider-obligation: BuyerControl → OwnerControl (memo only). Settle  │
  │   is mark-only; Control balances persist as the record of cash that       │
  │   flowed rider → driver outside BAP's books.                              │
  │ - OwnerLiability doubles as rider-suspense (online ride) and real platform │
  │   payable (cashback). OwnerControl is strictly the cash-memo tracker on   │
  │   BAP (per-rider, never drains).                                          │
  ├──────────────────────────────────┬─────────────────────┬───────────────────┤
  │ Ref type                         │ Create              │ Settle            │
  ├──────────────────────────────────┼─────────────────────┼───────────────────┤
  │ Rider-obligation (online)        │ BuyerAsset →        │ 2-leg capture:    │
  │   RideFare, GST, TollFare,       │ OwnerLiability      │   BuyerExternal → │
  │   TollVAT, PlatformFee, Tip      │ (Dr A/R ↑,          │     BuyerAsset    │
  │                                  │  Cr suspense ↑)     │   OwnerLiability →│
  │                                  │                     │     BuyerExternal │
  │                                  │                     │ Net: A/R=0,       │
  │                                  │                     │ Suspense=0, Ext=0 │
  ├──────────────────────────────────┼─────────────────────┼───────────────────┤
  │ Rider-obligation (cash)          │ BuyerControl →      │ mark SETTLED      │
  │   (same refs)                    │ OwnerControl        │ (no legs — no     │
  │                                  │                     │  cash flowed      │
  │                                  │                     │  through BAP)     │
  ├──────────────────────────────────┼─────────────────────┼───────────────────┤
  │ OfferDiscount (online)           │ BuyerExpense →      │ BuyerAsset →      │
  │                                  │ BuyerAsset          │ BuyerExternal     │
  │                                  │ (Dr Exp ↑,          │ (undo A/R cr +    │
  │                                  │  Cr A/R ↓)          │  cash out to BPP) │
  ├──────────────────────────────────┼─────────────────────┼───────────────────┤
  │ OfferDiscount (cash)             │ BuyerExpense →      │ mark SETTLED      │
  │                                  │ BuyerExternal       │ (create already   │
  │                                  │ (Dr Exp ↑,          │  landed the       │
  │                                  │  Cr Ext ↑ payable)  │  payable)         │
  ├──────────────────────────────────┼─────────────────────┼───────────────────┤
  │ CashbackPayout                   │ BuyerExpense →      │ mark SETTLED      │
  │ (BAP actually owes rider)        │ OwnerLiability      │ (actual payout    │
  │                                  │                     │  is a separate    │
  │                                  │                     │  wallet flow)     │
  ├──────────────────────────────────┼─────────────────────┼───────────────────┤
  │ CancellationFee, CancellationGST │ BuyerAsset →        │ n/a (created      │
  │ (paid via payment intent)        │ OwnerControl        │ SETTLED directly) │
  │                                  │ (SETTLED)           │                   │
  └──────────────────────────────────┴─────────────────────┴───────────────────┘

  Account-role glossary:
    * 'BuyerAsset'     — Real rider A/R (Asset).
    * 'BuyerControl'   — Asset-class memo tracker for rider-side cash flow.
    * 'OwnerControl'   — Asset-class memo tracker for driver-pool obligations.
                         Credited at create, so its balance goes negative;
                         magnitude = amount tracked toward driver-pool.
                         Per-booking, Cr to OwnerControl cancels Dr on
                         BuyerAsset/BuyerControl so each booking is balance-
                         sheet neutral.
    * 'OwnerLiability' — Real "platform owes rider" liability (cashback,
                         refunds). Never used for ride-fare suspense.

  Semantics legend:
    from → to  = Dr from, Cr to (standard double-entry).
    Asset/Expense as `from` raises its balance; as `to` lowers it.
    Liability/External as `from` lowers; as `to` raises.

  Invoice creation lives in 'createRidePaymentLedger' (and
  'createFullyDiscountedRidePaymentLedger') — it's built from the same
  amounts used for the ledger entries (see 'buildRidePaymentInvoiceConfig')
  and linked to those entries via invoice_ledger_link. 'settleRidePaymentLedger'
  only posts capture-side ledger legs; it does NOT touch invoices.

  On cancel / recreate: 'voidRidePaymentLedger' voids the pending ledger
  entries AND the invoice(s) linked to them, so stale invoices don't persist
  on retry flows.
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
    ridePaymentRefCashbackPayoutTransfer,
    ridePaymentRefTollFare,
    ridePaymentRefTollVAT,
    ridePaymentRefRideVatOnDiscount,

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
    createPendingCashbackPayoutTransfer,
    markCashbackPayoutTransferAsDue,
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
    sumByRefType,
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

ridePaymentRefCashbackPayoutTransfer :: Text
ridePaymentRefCashbackPayoutTransfer = "CashbackPayoutTransfer"

ridePaymentRefTollFare :: Text
ridePaymentRefTollFare = "TollFare"

ridePaymentRefTollVAT :: Text
ridePaymentRefTollVAT = "TollVAT"

-- | Platform-absorbed VAT on the discount portion: BAP funds this via
--   BuyerExpense → BuyerAsset, and the amount is paid across to the BPP
--   via the buyer-external path where it settles to the driver under the
--   BaseRide line.
ridePaymentRefRideVatOnDiscount :: Text
ridePaymentRefRideVatOnDiscount = "RideVatOnDiscount"

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
      tdsRateReason = Nothing,
      emitLedgerEntries = True
    }

-- ---------------------------------------------------------------------------
-- 1. Create PENDING ledger entries after payment order creation
-- ---------------------------------------------------------------------------

-- | Create PENDING ledger entries after successful payment order creation.
--   Rider-obligation entries accrue as Asset(BUYER) → Liability(RIDER),
--   i.e. Dr A/R, Cr suspense — both balances rise.
--   BAP-absorbed entries (offer discount, absorbed VAT) use
--   Expense(BUYER) → Asset(BUYER) since they represent real BAP cost, not pass-through.
--   Returns entry IDs for later settlement + invoice ID.
createRidePaymentLedger ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  FinanceCtx ->
  HighPrecMoney -> -- rideFare (base, without tax)
  HighPrecMoney -> -- gstAmount (GST/VAT on ride fare)
  HighPrecMoney -> -- tollFare (toll charges, without tax)
  HighPrecMoney -> -- tollVatAmount (VAT on toll)
  HighPrecMoney -> -- platformFee (application fee / commission)
  HighPrecMoney -> -- offerDiscountAmount (charge reduction, 0 for CASHBACK)
  HighPrecMoney -> -- cashbackPayoutAmount (amount to pay back to rider, 0 for DISCOUNT)
  HighPrecMoney -> -- rideVatAbsorbedOnDiscount (platform-absorbed VAT on the discount portion)
  m (Either FinanceError RidePaymentLedgerResult)
createRidePaymentLedger ctx rideFare gstAmount tollFare tollVatAmount platformFee offerDiscountAmount cashbackPayoutAmount _rideVatAbsorbedOnDiscount = do
  result <- runFinance ctx $ do
    let (riderSrc, riderDst) =
          if ctx.isOnline
            then (BuyerAsset, OwnerLiability)
            else (BuyerControl, OwnerControl)
        postRiderObligation amt ref = void $ transferPending riderSrc riderDst amt ref
    postRiderObligation rideFare ridePaymentRefRideFare
    postRiderObligation gstAmount ridePaymentRefGST
    postRiderObligation tollFare ridePaymentRefTollFare
    postRiderObligation tollVatAmount ridePaymentRefTollVAT
    postRiderObligation platformFee ridePaymentRefPlatformFee

    when (offerDiscountAmount > 0) $
      void $ transferPending BuyerExpense BuyerAsset offerDiscountAmount ridePaymentRefOfferDiscount

    when (cashbackPayoutAmount > 0) $
      void $ transferPending BuyerExpense OwnerLiability cashbackPayoutAmount ridePaymentRefCashbackPayout

    invoice $
      buildRidePaymentInvoiceConfig
        ctx
        rideFare
        gstAmount
        tollFare
        tollVatAmount
        platformFee
        offerDiscountAmount
        cashbackPayoutAmount
  case result of
    Left err -> do
      logError $ "Failed to create ride payment ledger: " <> show err
      pure $ Left err
    Right (mbInvoiceId, entryIds) ->
      pure $ Right RidePaymentLedgerResult {invoiceId = mbInvoiceId, entryIds}

-- Invoice line item helpers
mkLineItem :: Text -> HighPrecMoney -> Bool -> Maybe InvoiceLineItem
mkLineItem desc amt isExt
  | amt > 0 = Just InvoiceLineItem {description = desc, quantity = 1, unitPrice = amt, lineTotal = amt, isExternalCharge = isExt}
  | otherwise = Nothing

-- | Deduction line: positive input amount is rendered as a negative lineTotal
--   so the invoice shows it as a credit reducing the rider's total.
mkDeductionLineItem :: Text -> HighPrecMoney -> Bool -> Maybe InvoiceLineItem
mkDeductionLineItem desc amt isExt
  | amt > 0 = Just InvoiceLineItem {description = desc, quantity = 1, unitPrice = - amt, lineTotal = - amt, isExternalCharge = isExt}
  | otherwise = Nothing

-- | Ride-fare invoice line. When a discount is applied, the description is
--   appended with @Post Discount (<currency> <discountAmount>)@ so the
--   rider invoice shows the post-discount fare with a human-readable
--   discount summary, without a separate deduction line.
mkRideFareLineItem :: HighPrecMoney -> Currency -> HighPrecMoney -> Maybe InvoiceLineItem
mkRideFareLineItem amt currency discountAmount
  | amt <= 0 = Nothing
  | discountAmount > 0 =
    let desc = "Ride Fare Post Discount (" <> show currency <> " " <> show discountAmount <> ")"
     in Just InvoiceLineItem {description = desc, quantity = 1, unitPrice = amt, lineTotal = amt, isExternalCharge = False}
  | otherwise = Just InvoiceLineItem {description = "Ride Fare", quantity = 1, unitPrice = amt, lineTotal = amt, isExternalCharge = False}

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
--   (RideFare / GST / TollFare / TollVAT / PlatformFee / OfferDiscount /
--   CashbackPayout / RideVatOnDiscount) for a ride.
--
--     * No prior core entries → create fresh.
--     * Prior PENDING core entries whose total differs from the new total
--       (rideFare + gstAmount + tollFare + tollVatAmount + platformFee) →
--       void the stale PENDING set (and their invoice, via
--       'voidRidePaymentLedger') and recreate at the new amounts.
--     * Prior core entries but totals match → no-op.
upsertCoreRidePaymentLedger ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  FinanceCtx ->
  HighPrecMoney -> -- rideFare (without GST, post-discount)
  HighPrecMoney -> -- gstAmount  (post-discount)
  HighPrecMoney -> -- tollFare   (without VAT)
  HighPrecMoney -> -- tollVatAmount
  HighPrecMoney -> -- platformFee
  HighPrecMoney -> -- offerDiscountAmount
  HighPrecMoney -> -- cashbackPayoutAmount
  HighPrecMoney -> -- rideVatAbsorbedOnDiscount
  m UpsertCoreLedgerResult
upsertCoreRidePaymentLedger ctx rideFare gstAmount tollFare tollVatAmount platformFee offerDiscountAmount cashbackPayoutAmount rideVatAbsorbedOnDiscount = do
  let rideId = ctx.referenceId
  existingEntries <- findRidePaymentEntries rideId
  let coreEntries = filter (\e -> e.referenceType `elem` coreRidePaymentRefTypes) existingEntries
      pendingCoreEntries = filter (\e -> e.status == LE.PENDING) coreEntries
      newTotal = rideFare + gstAmount + tollFare + tollVatAmount + platformFee
      -- Exclude subsidy-absorbed entries (OfferDiscount / RideVatOnDiscount /
      -- CashbackPayout) from the staleness total comparison: they don't
      -- participate in the rider-obligation suspense and change independently.
      pendingRiderObligationEntries =
        filter
          (\e -> e.referenceType `notElem` [ridePaymentRefOfferDiscount, ridePaymentRefRideVatOnDiscount, ridePaymentRefCashbackPayout])
          pendingCoreEntries
      oldTotal = sum $ map (.amount) pendingRiderObligationEntries
      doCreate = do
        result <-
          createRidePaymentLedger
            ctx
            rideFare
            gstAmount
            tollFare
            tollVatAmount
            platformFee
            offerDiscountAmount
            cashbackPayoutAmount
            rideVatAbsorbedOnDiscount
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
--   Thin wrapper over 'createRidePaymentLedger' + 'settleRidePaymentLedger':
--   create PENDING entries as usual, then immediately settle them so the
--   ledger ends in the captured state with no outstanding rider obligation.
createFullyDiscountedRidePaymentLedger ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  FinanceCtx ->
  HighPrecMoney -> -- rideFare (original, before discount)
  HighPrecMoney -> -- gstAmount
  HighPrecMoney -> -- tollFare
  HighPrecMoney -> -- tollVatAmount
  HighPrecMoney -> -- platformFee
  HighPrecMoney -> -- offerDiscountAmount (should equal rideFare + gstAmount + platformFee)
  HighPrecMoney -> -- rideVatAbsorbedOnDiscount (platform-absorbed VAT on the discount portion)
  m (Either FinanceError RidePaymentLedgerResult)
createFullyDiscountedRidePaymentLedger ctx rideFare gstAmount tollFare tollVatAmount platformFee offerDiscountAmount rideVatAbsorbedOnDiscount = do
  createResult <-
    createRidePaymentLedger
      ctx
      rideFare
      gstAmount
      tollFare
      tollVatAmount
      platformFee
      offerDiscountAmount
      0 -- cashback doesn't apply to fully-discounted flow
      rideVatAbsorbedOnDiscount
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

-- | Reference types that carry real rider → BPP cash movement.
--   On online settle they take a 2-leg capture pattern (clear A/R + drain
--   Control to External). On cash settle they are mark-only.
riderObligationRefTypes :: [Text]
riderObligationRefTypes =
  [ ridePaymentRefRideFare,
    ridePaymentRefGST,
    ridePaymentRefTollFare,
    ridePaymentRefTollVAT,
    ridePaymentRefPlatformFee,
    ridePaymentRefTip,
    ridePaymentRefCancellationFee,
    ridePaymentRefCancellationGST
  ]

-- ---------------------------------------------------------------------------
-- 2. Settle ledger entries after payment capture success
-- ---------------------------------------------------------------------------

-- | Settle PENDING entries after payment capture success.
--   Flips each entry to SETTLED (posting its accrual balance delta), then
--   posts the capture-side legs. For rider-obligation entries
--   ('riderObligationRefTypes') the three-leg pattern applies: cash-in clears
--   A/R, then a pass-through leg clears the rider suspense to external.
--   BAP-absorbed entries (offer discount, absorbed VAT) just post the
--   single Asset→External cash-out leg, leaving BuyerExpense on the books as
--   the real P&L impact. Amounts come from the entries themselves.
--
--   Invoice is NOT created here — it is created (and linked) inside
--   'createRidePaymentLedger' and voided together with the ledger entries
--   in 'voidRidePaymentLedger'.
settleRidePaymentLedger ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  FinanceCtx ->
  [Id LE.LedgerEntry] -> -- entry IDs from createRidePaymentLedger
  Text -> -- settlement reason
  m (Either FinanceError ())
settleRidePaymentLedger ctx entryIds settledReason = do
  entryDetails <- forM entryIds $ \entryId -> do
    mbEntry <- getEntry entryId
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

-- | Create a PENDING drain transfer entry (OwnerLiability → BuyerExternal)
--   for a cashback payout, before calling the external payout service.
--   The entry stays PENDING while the payout is in flight; it is settled
--   by 'markCashbackEntriesAsPaidOut' on webhook success, or flipped to
--   DUE by 'markCashbackPayoutTransferAsDue' if the submission fails.
createPendingCashbackPayoutTransfer ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  FinanceCtx ->
  HighPrecMoney -> -- payout amount
  m (Either FinanceError [Id LE.LedgerEntry])
createPendingCashbackPayoutTransfer ctx amount = do
  result <-
    runFinance ctx $
      void $ transferPending OwnerLiability BuyerExternal amount ridePaymentRefCashbackPayoutTransfer
  case result of
    Left err -> do
      logError $ "Failed to create PENDING cashback payout transfer entry: " <> show err
      pure $ Left err
    Right (_, entryIds) -> pure $ Right entryIds

-- | Mark cashback payout transfer entries as DUE after a failed payout
--   submission. Only flips PENDING/DUE entries; SETTLED entries are
--   untouched (guarded by 'updateEntryStatus' callers).
markCashbackPayoutTransferAsDue ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  [Id LE.LedgerEntry] ->
  m ()
markCashbackPayoutTransferAsDue entryIds = do
  mbEntries <- forM entryIds getEntry
  let duable = [e | Just e <- mbEntries, e.status == LE.PENDING]
  forM_ duable $ \e -> Lib.Finance.Ledger.Service.updateEntryStatus e.id LE.DUE
  logInfo $ "Marked " <> show (length duable) <> " cashback payout transfer entries as DUE"

-- | Called on successful Juspay payout webhook for a RIDE_OFFER_CASHBACK
--   payout. Settles any PENDING/DUE payout transfer entries (which posts
--   the OwnerLiability → BuyerExternal drain via 'settleEntry'), then
--   flags all supplied entries PAID_OUT with the PayoutRequest id.
--
--   Entry IDs are the union of (a) the original SETTLED cashback accrual
--   entries (refType=CashbackPayout) and (b) the PENDING/DUE drain
--   transfer entries (refType=CashbackPayoutTransfer) created by the
--   scheduler job before 'submitPayoutRequest'.
--   Idempotent: replaying the webhook finds nothing PENDING/DUE and no
--   entries to flag.
markCashbackEntriesAsPaidOut ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  FinanceCtx ->
  [Id LE.LedgerEntry] -> -- entry IDs (from PayoutRequest.ledgerEntryIds)
  Text -> -- PayoutRequest id → settlementId on the row
  m (Either FinanceError ())
markCashbackEntriesAsPaidOut _ctx entryIds payoutRequestId = do
  mbEntries <- forM entryIds getEntry
  let entries = catMaybes mbEntries
      eligible = filter (\e -> e.settlementStatus /= Just LE.PAID_OUT) entries
  if null eligible
    then do
      logInfo $ "markCashbackEntriesAsPaidOut: nothing eligible (payoutRequestId=" <> payoutRequestId <> ")"
      pure $ Right ()
    else do
      let pendingOrDue = filter (\e -> e.status == LE.PENDING || e.status == LE.DUE) eligible
      forM_ pendingOrDue $ \e -> Lib.Finance.Ledger.Service.settleEntry e.id
      Lib.Finance.Ledger.Service.markEntriesAsPaidOut (map (.id) eligible) payoutRequestId
      logInfo $
        "Cashback payout settled — " <> show (length eligible)
          <> " entries → PAID_OUT (payoutRequestId="
          <> payoutRequestId
          <> ", settledDrainEntries="
          <> show (length pendingOrDue)
          <> ")"
      pure $ Right ()

-- | Build the canonical 'InvoiceConfig' for a rider ride-payment invoice.
--   Reuses the same line-item layout that createRidePaymentLedger used when
--   it owned invoice creation, so callers at settle time get identical output.
buildRidePaymentInvoiceConfig ::
  FinanceCtx ->
  HighPrecMoney -> -- rideFare (post-discount)
  HighPrecMoney -> -- gstAmount
  HighPrecMoney -> -- tollFare
  HighPrecMoney -> -- tollVatAmount
  HighPrecMoney -> -- platformFee
  HighPrecMoney -> -- offerDiscountAmount (for ride-fare description suffix)
  HighPrecMoney -> -- cashbackPayoutAmount (rendered as deduction line)
  InvoiceConfig
buildRidePaymentInvoiceConfig ctx rideFare gstAmount tollFare tollVatAmount platformFee offerDiscountAmount cashbackPayoutAmount =
  InvoiceConfig
    { invoiceType = FInvoice.Ride,
      issuedToType = "RIDER",
      issuedToId = ctx.counterpartyId,
      issuedToName = Nothing,
      issuedToAddress = Nothing,
      lineItems =
        catMaybes
          [ mkRideFareLineItem (rideFare + platformFee) ctx.currency offerDiscountAmount,
            mkLineItem "Ride Tax" gstAmount False,
            mkLineItem "Toll Fare" tollFare True,
            mkLineItem "Toll Tax" tollVatAmount True,
            mkDeductionLineItem "Cashback Offer" cashbackPayoutAmount False
          ],
      gstBreakdown = Nothing,
      isVat = gstAmount > 0 || tollVatAmount > 0,
      issuedToTaxNo = Nothing,
      issuedByTaxNo = Nothing
    }

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
--   Accrual only: Asset(BUYER) → Liability(RIDER) in PENDING state.
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
                    isExternalCharge = False
                  },
                InvoiceLineItem
                  { description = ridePaymentRefCancellationGST,
                    quantity = 1,
                    unitPrice = cancellationGST,
                    lineTotal = cancellationGST,
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
    ridePaymentRefTollFare,
    ridePaymentRefTollVAT,
    ridePaymentRefPlatformFee,
    ridePaymentRefOfferDiscount,
    ridePaymentRefCashbackPayout,
    ridePaymentRefRideVatOnDiscount
  ]

-- | All ride payment reference types.
allRidePaymentRefTypes :: [Text]
allRidePaymentRefTypes =
  coreRidePaymentRefTypes
    <> [ ridePaymentRefTip,
         ridePaymentRefCancellationFee,
         ridePaymentRefCancellationGST
       ]

-- | Sum ledger entry amounts for a given reference type.
sumByRefType :: Text -> [LE.LedgerEntry] -> HighPrecMoney
sumByRefType refType entries = sum [e.amount | e <- entries, e.referenceType == refType]

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
  pure $ filter (\e -> (e.status == LE.PENDING || e.status == LE.DUE)) entries

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
  forM_ entryIds $ \entryId -> updateEntryStatus entryId LE.DUE

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
