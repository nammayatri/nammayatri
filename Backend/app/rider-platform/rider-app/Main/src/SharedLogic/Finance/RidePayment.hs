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
    ridePaymentRefParkingCharge,
    ridePaymentRefParkingVAT,
    ridePaymentRefRideVatOnDiscount,
    ridePaymentRefRideFareRefund,
    ridePaymentRefRideFareRefundVAT,
    ridePaymentRefTollRefund,
    ridePaymentRefTollRefundVAT,
    ridePaymentRefParkingRefund,
    ridePaymentRefParkingRefundVAT,

    -- * Settlement reason constants
    settledReasonRidePayment,
    settledReasonDebtSettlement,
    settledReasonTipPayment,

    -- * Context builder
    buildRiderFinanceCtx,
    applyBookingProviderFieldsToCtx,

    -- * Ledger operations
    createRidePaymentLedger,
    createFullyDiscountedRidePaymentLedger,
    upsertCoreRidePaymentLedger,
    UpsertCoreLedgerResult (..),
    settleRidePaymentLedger,
    createSettledCashbackPayoutLeg,
    getWalletAccountByOwner,
    getWalletBalanceByOwner,
    getPayoutEligibilityData,
    reserveCashbackEntriesForPayout,
    releaseCashbackEntriesReservation,
    markCashbackEntriesAsPaidOut,
    voidRidePaymentLedger,
    voidRidePaymentEntriesAndInvoice,
    createTipLedger,
    regenerateRideTipInvoice,
    createRefundInvoice,
    createRefundRaisedLedger,
    createRefundSucceededLedger,
    refundSucceededAlreadyRecorded,
    voidRefundRaisedLedger,
    RefundComponentSplit (..),
    refundLegRefTypes,
    getRefundLegEntries,
    getSettledRefundByComponent,
    settledRefundByComponent,
    createPendingCancellationFeeLedger,
    markCancellationFeeInvoicePaid,
    voidRideInvoice,
    markRideInvoicePaid,
    markRideInvoiceIssued,

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

import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import Data.Foldable.Extra (findM)
import qualified Data.List as List
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.FareBreakup as DFareBreakup
import Domain.Types.Invoice (InvoiceType (Ride, RideCancellation))
import qualified Domain.Types.Invoice as DInvType
import qualified Domain.Types.Person
import qualified "this" Domain.Types.RefundRequest as DRefundRequest
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (MonadFlow, getCurrentTime, logDebug, logError, logInfo, throwError)
import Lib.Finance
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Finance.Invoice.Service as FInvoiceService
import qualified Lib.Finance.Ledger.Service
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.Invoice as QInvoice
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QInvoiceExtra

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

ridePaymentRefParkingCharge :: Text
ridePaymentRefParkingCharge = "ParkingCharge"

ridePaymentRefParkingVAT :: Text
ridePaymentRefParkingVAT = "ParkingVAT"

-- | Platform-absorbed VAT on the discount portion: BAP funds this via
--   BuyerExpense → BuyerAsset, and the amount is paid across to the BPP
--   via the buyer-external path where it settles to the driver under the
--   BaseRide line.
ridePaymentRefRideVatOnDiscount :: Text
ridePaymentRefRideVatOnDiscount = "RideVatOnDiscount"

-- Per-component refund reference types. One base + one VAT leg per refunded
-- component; all-caps VAT matches the ride-side 'TollVAT'.
ridePaymentRefRideFareRefund :: Text
ridePaymentRefRideFareRefund = "RideFareRefund"

ridePaymentRefRideFareRefundVAT :: Text
ridePaymentRefRideFareRefundVAT = "RideFareRefundVAT"

ridePaymentRefTollRefund :: Text
ridePaymentRefTollRefund = "TollRefund"

ridePaymentRefTollRefundVAT :: Text
ridePaymentRefTollRefundVAT = "TollRefundVAT"

ridePaymentRefParkingRefund :: Text
ridePaymentRefParkingRefund = "ParkingRefund"

ridePaymentRefParkingRefundVAT :: Text
ridePaymentRefParkingRefundVAT = "ParkingRefundVAT"

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
  Maybe Text -> -- fromLocationAddress
  FinanceCtx
buildRiderFinanceCtx merchantId merchantOpCityId currency isOnline riderId referenceId merchantName merchantShortId fromLocationAddress =
  FinanceCtx
    { merchantId = merchantId,
      merchantOpCityId = merchantOpCityId,
      currency = currency,
      isOnline = isOnline,
      counterpartyType = RIDER,
      counterpartyId = riderId,
      concernedIndividualId = Nothing,
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
      emitLedgerEntries = True,
      fromLocationAddress = fromLocationAddress,
      issuedToName = Nothing
    }

applyBookingProviderFieldsToCtx :: DRB.Booking -> FinanceCtx -> FinanceCtx
applyBookingProviderFieldsToCtx booking ctx =
  ctx{merchantId = fromMaybe ctx.merchantId booking.issuedById,
      merchantName = booking.issuedByName <|> ctx.merchantName,
      issuedByAddress = booking.issuedByAddress <|> ctx.issuedByAddress,
      supplierName = booking.supplierName <|> ctx.supplierName,
      supplierAddress = booking.supplierAddress <|> ctx.supplierAddress,
      supplierGSTIN = booking.supplierGSTIN <|> ctx.supplierGSTIN,
      supplierVatNumber = booking.supplierTaxNo <|> ctx.supplierVatNumber,
      supplierId = booking.supplierId <|> ctx.supplierId
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
  (BeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  FinanceCtx ->
  HighPrecMoney -> -- rideFare (base, without tax)
  HighPrecMoney -> -- gstAmount (GST/VAT on ride fare)
  HighPrecMoney -> -- tollFare (toll charges, without tax)
  HighPrecMoney -> -- tollVatAmount (VAT on toll)
  HighPrecMoney -> -- parkingCharge (parking charges, without tax)
  HighPrecMoney -> -- parkingChargeVat (VAT on parking)
  HighPrecMoney -> -- platformFee (application fee / commission)
  HighPrecMoney -> -- offerDiscountAmount (charge reduction, 0 for CASHBACK)
  HighPrecMoney -> -- cashbackPayoutAmount (amount to pay back to rider, 0 for DISCOUNT)
  HighPrecMoney -> -- rideVatAbsorbedOnDiscount (platform-absorbed VAT on the discount portion)
  m (Either FinanceError RidePaymentLedgerResult)
createRidePaymentLedger ctx rideFare gstAmount tollFare tollVatAmount parkingCharge parkingChargeVat platformFee offerDiscountAmount cashbackPayoutAmount rideVatAbsorbedOnDiscount = do
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
    postRiderObligation parkingCharge ridePaymentRefParkingCharge
    postRiderObligation parkingChargeVat ridePaymentRefParkingVAT
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
        parkingCharge
        parkingChargeVat
        platformFee
        offerDiscountAmount
        cashbackPayoutAmount
        rideVatAbsorbedOnDiscount
  case result of
    Left err -> do
      logError $ "Failed to create ride payment ledger: " <> show err
      pure $ Left err
    Right (mbInvoiceId, entryIds) ->
      pure $ Right RidePaymentLedgerResult {invoiceId = mbInvoiceId, entryIds}

-- Invoice line item helpers. Callers pass both legacy 'description :: Text'
-- (DB back-compat, hardcoded just like before our refactor) and the new typed
-- 'descriptionType' (used by the new PDF renderer for localization).
mkLineItem :: Text -> LineItemDescription -> HighPrecMoney -> Bool -> ItemType -> Maybe Text -> Maybe InvoiceLineItem
mkLineItem desc descType amt isExt typ groupIdStr
  | amt > 0 = Just InvoiceLineItem {description = desc, descriptionType = Just descType, quantity = 1, unitPrice = amt, lineTotal = amt, isExternalCharge = isExt, groupId = groupIdStr, itemType = Just typ}
  | otherwise = Nothing

-- | Deduction line: positive input amount is rendered as a negative lineTotal
--   so the invoice shows it as a credit reducing the rider's total.
mkDeductionLineItem :: Text -> LineItemDescription -> HighPrecMoney -> Bool -> Maybe InvoiceLineItem
mkDeductionLineItem desc descType amt isExt
  | amt > 0 = Just InvoiceLineItem {description = desc, descriptionType = Just descType, quantity = 1, unitPrice = - amt, lineTotal = - amt, isExternalCharge = isExt, groupId = Nothing, itemType = Just Adjustment}
  | otherwise = Nothing

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
--   (RideFare / GST / TollFare / TollVAT / ParkingCharge / ParkingVAT / PlatformFee / OfferDiscount /
--   CashbackPayout / RideVatOnDiscount) for a ride.
--
--     * No prior core entries → create fresh.
--     * Prior PENDING core entries whose total differs from the new total
--       (rideFare + gstAmount + tollFare + tollVatAmount + platformFee) →
--       void the stale PENDING set (and their invoice, via
--       'voidRidePaymentLedger') and recreate at the new amounts.
--     * Prior core entries but totals match → no-op.
upsertCoreRidePaymentLedger ::
  (BeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  FinanceCtx ->
  HighPrecMoney -> -- rideFare (without GST, post-discount)
  HighPrecMoney -> -- gstAmount  (post-discount)
  HighPrecMoney -> -- tollFare   (without VAT)
  HighPrecMoney -> -- tollVatAmount
  HighPrecMoney -> -- parkingCharge (without VAT)
  HighPrecMoney -> -- parkingChargeVat
  HighPrecMoney -> -- platformFee
  HighPrecMoney -> -- offerDiscountAmount
  HighPrecMoney -> -- cashbackPayoutAmount
  HighPrecMoney -> -- rideVatAbsorbedOnDiscount
  HighPrecMoney -> -- cancellationCharge (0 for normal ride)
  HighPrecMoney -> -- cancellationTax (0 for normal ride)
  m UpsertCoreLedgerResult
upsertCoreRidePaymentLedger ctx rideFare gstAmount tollFare tollVatAmount parkingCharge parkingChargeVat platformFee offerDiscountAmount cashbackPayoutAmount rideVatAbsorbedOnDiscount cancellationCharge cancellationTax = do
  let rideId = ctx.referenceId
  existingEntries <- findRidePaymentEntries rideId
  let coreEntries = filter (\e -> e.referenceType `elem` coreRidePaymentRefTypes) existingEntries
      pendingCoreEntries = filter (\e -> e.status == LE.PENDING) coreEntries
      newTotal = rideFare + gstAmount + tollFare + tollVatAmount + parkingCharge + parkingChargeVat + platformFee
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
            parkingCharge
            parkingChargeVat
            platformFee
            offerDiscountAmount
            cashbackPayoutAmount
            rideVatAbsorbedOnDiscount
        case result of
          Right res -> pure (res.entryIds, res.invoiceId)
          Left err -> do
            logError $ "Failed to create core ride payment ledger for " <> rideId <> ": " <> show err
            pure ([], Nothing)
  coreResult <-
    if null coreEntries
      then do
        if newTotal > 0
          then do
            (newIds, mbInv) <- doCreate
            logInfo $ "Created PENDING core ride payment ledger entries for ride: " <> rideId
            pure UpsertCoreLedgerResult {coreEntryIds = newIds, invoiceId = mbInv, didCreate = True, didVoidStale = False}
          else do
            logInfo $ "Core ride payment total is zero, skipping create for ride: " <> rideId
            pure UpsertCoreLedgerResult {coreEntryIds = [], invoiceId = Nothing, didCreate = False, didVoidStale = False}
      else
        if not (null pendingCoreEntries) && newTotal /= oldTotal
          then do
            let staleIds = map (.id) pendingCoreEntries
            voidRidePaymentLedger staleIds
            voidRideInvoice rideId
            logInfo $
              "Voided " <> show (length staleIds) <> " stale PENDING core entries (old="
                <> show oldTotal
                <> " new="
                <> show newTotal
                <> ") for ride: "
                <> rideId
            if newTotal > 0
              then do
                (newIds, mbInv) <- doCreate
                pure UpsertCoreLedgerResult {coreEntryIds = newIds, invoiceId = mbInv, didCreate = True, didVoidStale = True}
              else do
                logInfo $ "Core ride payment total is zero, skipping create after void for ride: " <> rideId
                pure UpsertCoreLedgerResult {coreEntryIds = [], invoiceId = Nothing, didCreate = False, didVoidStale = True}
          else do
            logInfo $ "Core ride payment ledger already up to date for ride: " <> rideId
            pure
              UpsertCoreLedgerResult
                { coreEntryIds = map (.id) pendingCoreEntries,
                  invoiceId = Nothing,
                  didCreate = False,
                  didVoidStale = False
                }
  -- Upsert cancellation fee entries when cancellationCharge is non-zero.
  -- Idempotent: if entries were already created (e.g. by createPendingCancellationFeeLedger
  -- before this call) the existing-entries check skips re-creation.
  when (cancellationCharge > 0) $ do
    let cancelEntries = filter (\e -> e.referenceType `elem` [ridePaymentRefCancellationFee, ridePaymentRefCancellationGST]) existingEntries
    when (null cancelEntries) $ do
      void $ createPendingCancellationFeeLedger ctx cancellationCharge cancellationTax
      logInfo $ "Created PENDING cancellation fee ledger entries for ride: " <> rideId
  pure coreResult

-- ---------------------------------------------------------------------------
-- 1b. Create SETTLED ledger entries for fully discounted rides (amount = 0)
-- ---------------------------------------------------------------------------

-- | Create ledger entries for a fully discounted ride (100% offer covers fare).
--   Thin wrapper over 'createRidePaymentLedger' + 'settleRidePaymentLedger':
--   create PENDING entries as usual, then immediately settle them so the
--   ledger ends in the captured state with no outstanding rider obligation.
createFullyDiscountedRidePaymentLedger ::
  (BeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  FinanceCtx ->
  HighPrecMoney -> -- rideFare (post-discount, from the ledger info — 0 for the fully-discounted portion)
  HighPrecMoney -> -- gstAmount (post-discount)
  HighPrecMoney -> -- tollFare
  HighPrecMoney -> -- tollVatAmount
  HighPrecMoney -> -- parkingCharge
  HighPrecMoney -> -- parkingChargeVat
  HighPrecMoney -> -- platformFee
  HighPrecMoney -> -- offerDiscountAmount (the full clamped gross discount)
  HighPrecMoney -> -- rideVatAbsorbedOnDiscount (platform-absorbed VAT on the discount portion)
  m (Either FinanceError RidePaymentLedgerResult)
createFullyDiscountedRidePaymentLedger ctx rideFare gstAmount tollFare tollVatAmount parkingCharge parkingChargeVat platformFee offerDiscountAmount rideVatAbsorbedOnDiscount = do
  createResult <-
    createRidePaymentLedger
      ctx
      rideFare
      gstAmount
      tollFare
      tollVatAmount
      parkingCharge
      parkingChargeVat
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
    ridePaymentRefParkingCharge,
    ridePaymentRefParkingVAT,
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
  (BeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
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

-- | Post and settle ONLY the cashback-payout accrual leg
--   (BuyerExpense → OwnerLiability, ref 'ridePaymentRefCashbackPayout'),
--   leaving it in the same SETTLED / settlementStatus=NULL state that ride
--   completion produces. This is the state the ExecuteCashRideCashbackPayout
--   job looks for (settled + unsettled-settlementStatus). Used by the
--   dashboard payout-offer sync to backfill the cashback entry so the payout
--   job has something to drain. No-op when the amount is non-positive.
createSettledCashbackPayoutLeg ::
  (BeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  FinanceCtx ->
  HighPrecMoney ->
  m ()
createSettledCashbackPayoutLeg ctx cashbackPayoutAmount =
  when (cashbackPayoutAmount > 0) $ do
    result <- runFinance ctx $ void $ transferPending BuyerExpense OwnerLiability cashbackPayoutAmount ridePaymentRefCashbackPayout
    case result of
      Left err -> logError $ "Failed to create cashback payout ledger leg for " <> ctx.referenceId <> ": " <> show err
      Right (_, entryIds) -> do
        settleResult <- settleRidePaymentLedger ctx entryIds settledReasonRidePayment
        case settleResult of
          Right () -> logInfo $ "Settled cashback payout ledger leg for ride: " <> ctx.referenceId
          Left err -> logError $ "Cashback payout ledger leg settle failed for " <> ctx.referenceId <> ": " <> show err

-- ---------------------------------------------------------------------------
-- Wallet account / payout-eligibility helpers
-- ---------------------------------------------------------------------------

getWalletAccountByOwner ::
  (BeamFlow.BeamFlow m r) =>
  CounterpartyType ->
  Text ->
  m (Maybe Account)
getWalletAccountByOwner counterpartyType ownerId =
  findAccountsByCounterparty (Just counterpartyType) (Just ownerId) Liability

getWalletBalanceByOwner ::
  (BeamFlow.BeamFlow m r) =>
  CounterpartyType ->
  Text ->
  m (Maybe HighPrecMoney)
getWalletBalanceByOwner counterpartyType ownerId = do
  mbAcc <- getWalletAccountByOwner counterpartyType ownerId
  pure $ mbAcc <&> (.balance)

netAmountForAccount :: Id Account -> LE.LedgerEntry -> HighPrecMoney
netAmountForAccount accountId e =
  if e.fromAccountId == accountId then negate e.amount else e.amount

-- | Returns the wallet balance and the list of unsettled (status DUE or
--   SETTLED) ledger entries paired with their net effect on this account's
--   balance:
--   * `-amount` if this account is on the `from` side of the entry
--   * `+amount` if this account is on the `to` side
--   The sum of the net amounts must equal the wallet balance, otherwise
--   the wallet/ledger has drifted and we throw.
getPayoutEligibilityData ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  CounterpartyType ->
  Id Domain.Types.Person.Person ->
  m (HighPrecMoney, [(LE.LedgerEntry, HighPrecMoney)])
getPayoutEligibilityData counterparty personId = do
  now <- getCurrentTime
  mbAccount <- getWalletAccountByOwner counterparty personId.getId
  case mbAccount of
    Nothing -> pure (0, [])
    Just Account {id = accountId, balance = walletBalance} -> do
      unsettledEntries <- Lib.Finance.Ledger.Service.findUnsettledByAccountBeforeTime accountId now
      let entriesWithNet = map (\e -> (e, netAmountForAccount accountId e)) unsettledEntries
          totalNet = sum (map snd entriesWithNet)
      when (walletBalance < totalNet) $ do
        logError $
          "Wallet balance less than net amount for person: " <> personId.getId
            <> " wallet balance: "
            <> show walletBalance
            <> " unsettled entries net amount: "
            <> show totalNet
            <> " unsettled entries: "
            <> show (map (\e -> (e.id, e.amount)) unsettledEntries)
        throwError $ InvalidRequest "Wallet balance less than net amount"
      pure (walletBalance, entriesWithNet)

-- | Reserve a batch of cashback ledger entries for an in-flight payout
--   by flipping their settlementStatus from UNSETTLED → PROCESSING. This
--   is the DB-level guard that keeps subsequent eligibility queries
--   (`findUnsettledByAccountBeforeTime*`) from returning the same entries
--   while the Juspay payout is in flight (which can outlive any Redis
--   lock TTL). Idempotent — entries already PROCESSING/PAID_OUT are
--   left alone.
reserveCashbackEntriesForPayout ::
  (BeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  [Id LE.LedgerEntry] ->
  Maybe Text -> -- optional PayoutRequest id (settlementId)
  m ()
reserveCashbackEntriesForPayout entryIds mbSettlementId = do
  Lib.Finance.Ledger.Service.markEntriesAsProcessing entryIds mbSettlementId
  logInfo $
    "Reserved " <> show (length entryIds) <> " cashback entries as PROCESSING"
      <> maybe "" (\s -> " (settlementId=" <> s <> ")") mbSettlementId

-- | Release a previously-reserved batch of cashback ledger entries by
--   flipping their settlementStatus from PROCESSING → UNSETTLED so they
--   become eligible again. Used when the payout submission fails (sync
--   PayoutFailed) or the webhook reports a terminal failure.
--   Only flips PROCESSING entries — PAID_OUT entries are untouched.
releaseCashbackEntriesReservation ::
  (BeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  [Id LE.LedgerEntry] ->
  m ()
releaseCashbackEntriesReservation entryIds = do
  Lib.Finance.Ledger.Service.markEntriesAsUnsettled entryIds
  logInfo $ "Released cashback entries reservation (" <> show (length entryIds) <> " entries reverted to UNSETTLED)"

-- | Called after a successful payout submission (and on the Juspay webhook
--   replay) for a RIDE_OFFER_CASHBACK payout. Posts the drain transfer
--   (OwnerLiability → BuyerExternal) for the payout amount with refType
--   `ridePaymentRefCashbackPayoutTransfer` and refId = personId (carried
--   on `ctx.referenceId`), settles that drain entry, and flags both the
--   original cashback accrual entries and the new drain entry as
--   PAID_OUT against the PayoutRequest id.
--
--   Idempotent: if all supplied entries are already PAID_OUT we no-op
--   (and skip creating a duplicate drain transfer).
markCashbackEntriesAsPaidOut ::
  (BeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  FinanceCtx ->
  [Id LE.LedgerEntry] -> -- original cashback entry IDs (from PayoutRequest.ledgerEntryIds)
  HighPrecMoney -> -- payout amount (drives the OwnerLiability → BuyerExternal drain)
  Text -> -- PayoutRequest id → settlementId on the row
  m (Either FinanceError ())
markCashbackEntriesAsPaidOut ctx entryIds amount payoutRequestId = do
  mbEntries <- forM entryIds getEntry
  let entries = catMaybes mbEntries
      eligible = filter (\e -> e.settlementStatus /= Just LE.PAID_OUT) entries
  if null eligible
    then do
      logInfo $ "markCashbackEntriesAsPaidOut: nothing eligible (payoutRequestId=" <> payoutRequestId <> ")"
      pure $ Right ()
    else do
      transferRes <-
        runFinance ctx $
          void $ transferPending OwnerLiability BuyerExternal amount ridePaymentRefCashbackPayoutTransfer
      case transferRes of
        Left err -> do
          logError $ "Failed to create cashback payout drain transfer: " <> show err
          pure $ Left err
        Right (_, transferEntryIds) -> do
          forM_ transferEntryIds $ \tid -> Lib.Finance.Ledger.Service.settleEntry tid
          let allPaidOutIds = map (.id) eligible <> transferEntryIds
          Lib.Finance.Ledger.Service.markEntriesAsPaidOut allPaidOutIds payoutRequestId
          logInfo $
            "Cashback payout settled — " <> show (length eligible)
              <> " original entries → PAID_OUT (payoutRequestId="
              <> payoutRequestId
              <> ", drainTransferEntries="
              <> show (length transferEntryIds)
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
  HighPrecMoney -> -- parkingCharge
  HighPrecMoney -> -- parkingChargeVat
  HighPrecMoney -> -- platformFee
  HighPrecMoney -> -- offerDiscountAmount (rendered as its own deduction line)
  HighPrecMoney -> -- cashbackPayoutAmount (rendered as deduction line)
  HighPrecMoney -> -- rideVatAbsorbedOnDiscount (reconstructs the pre-discount fare/tax)
  InvoiceConfig
buildRidePaymentInvoiceConfig ctx rideFare gstAmount tollFare tollVatAmount parkingCharge parkingChargeVat platformFee offerDiscountAmount cashbackPayoutAmount rideVatAbsorbedOnDiscount =
  -- The main table shows the FULL pre-discount fare and its full VAT; the discount is a
  -- separate negative Adjustment below the total.
  -- Reconstruction is universal: every caller passes post-discount amounts + the clamped
  -- gross discount + the VAT absorbed inside it (fully-discounted path included).
  let preDiscountTax = gstAmount + rideVatAbsorbedOnDiscount
      preDiscountFareLine = rideFare + platformFee + (offerDiscountAmount - rideVatAbsorbedOnDiscount)
   in InvoiceConfig
        { invoiceType = Ride,
          issuedToType = DInvType.RIDER,
          issuedToId = ctx.counterpartyId,
          issuedToName = ctx.issuedToName,
          issuedToAddress = ctx.fromLocationAddress,
          referenceId = Just ctx.referenceId,
          lineItems =
            catMaybes
              [ mkLineItem "Ride Fare" RideFare preDiscountFareLine False Fare (Just "g-ride"),
                mkLineItem "Ride Tax" RideTax preDiscountTax False Tax (Just "g-ride"),
                mkLineItem "Toll Fare" TollFare tollFare True Fare (Just "g-toll"),
                mkLineItem "Toll Tax" TollTax tollVatAmount True Tax (Just "g-toll"),
                mkLineItem "Parking Charge" ParkingCharges parkingCharge True Fare (Just "g-parking"),
                mkLineItem "Parking Tax" ParkingChargesTax parkingChargeVat True Tax (Just "g-parking"),
                mkDeductionLineItem "Discount" OfferDiscount offerDiscountAmount False,
                mkDeductionLineItem "Cashback Offer" CashbackOffer cashbackPayoutAmount False
              ],
          gstBreakdown = Nothing,
          isVat = preDiscountTax > 0 || tollVatAmount > 0 || parkingChargeVat > 0,
          issuedToTaxNo = Nothing,
          issuedByTaxNo = Nothing,
          paymentMode = Just $ if ctx.isOnline then "ONLINE" else "CASH",
          periodStart = Nothing,
          periodEnd = Nothing
        }

-- ---------------------------------------------------------------------------
-- 3. Void ledger entries when payment intent is cancelled
-- ---------------------------------------------------------------------------

-- | Void all PENDING ledger entries and any invoice linked to them.
--   Used when payment is cancelled or when the fare is recomputed and we
--   recreate the ledger — stale invoices from the previous attempt must
--   also be marked VOIDED so dashboards / lookups don't surface them.
voidRidePaymentLedger ::
  (BeamFlow.BeamFlow m r, MonadFlow m, Finance.HasActorInfo m r) =>
  [Id LE.LedgerEntry] ->
  m ()
voidRidePaymentLedger entryIds = do
  -- Collect linked invoice IDs BEFORE voiding the entries, in case
  -- voiding cascades or otherwise disrupts the link lookup.
  actorInfo <- asks (.actorInfo)
  mbInvoices <- forM entryIds $ \entryId -> FInvoiceService.getInvoiceForEntry entryId
  let uniqueInvoiceIds = List.nub [inv.id | Just inv <- mbInvoices]
  forM_ entryIds $ \entryId ->
    voidEntry entryId "PaymentIntentCancelled"
  forM_ uniqueInvoiceIds $ \invId ->
    QInvoice.updateStatus FInvoice.Voided (Just actorInfo.actorType) actorInfo.actorId invId
  logInfo $
    "Voided " <> show (length entryIds) <> " ride payment ledger entries and "
      <> show (length uniqueInvoiceIds)
      <> " linked invoice(s)"

-- | Void cancellation fee ledger entries + mark invoice Voided.
--   Use only when there is genuinely no debt (e.g. zero effective amount).

-- | Mark cancellation fee invoice as Paid after successful Stripe capture.
markCancellationFeeInvoicePaid ::
  (BeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  Maybe (Id FInvoice.Invoice) ->
  m ()
markCancellationFeeInvoicePaid mbInvoiceId =
  whenJust mbInvoiceId $ \invoiceId -> do
    FInvoiceService.updateInvoiceStatus invoiceId FInvoice.Paid
    logInfo $ "Marked cancellation fee invoice as Paid: " <> invoiceId.getId

-- | Find and cancel the Ride invoice linked to a rideId (called when ride intent is cancelled).
voidRideInvoice ::
  (BeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  Text -> -- rideId
  m ()
voidRideInvoice rideId = do
  rideEntries <- findRidePaymentEntries rideId
  logDebug $ "[voidRideInvoice] rideId=" <> rideId <> " found " <> show (length rideEntries) <> " entries"
  let rideFareEntry = find (\e -> e.referenceType == ridePaymentRefRideFare) rideEntries
  case rideFareEntry of
    Nothing -> logDebug $ "[voidRideInvoice] No RideFare entry found for rideId=" <> rideId
    Just entry -> do
      logDebug $ "[voidRideInvoice] Found RideFare entry id=" <> entry.id.getId <> " status=" <> show entry.status
      mbInvoice <- FInvoiceService.getInvoiceForEntry entry.id
      case mbInvoice of
        Nothing -> logDebug $ "[voidRideInvoice] No invoice link found for entry id=" <> entry.id.getId
        Just inv -> do
          FInvoiceService.updateInvoiceStatus inv.id FInvoice.Cancelled
          logInfo $ "[voidRideInvoice] Cancelled Ride invoice " <> inv.id.getId <> " for rideId=" <> rideId

-- | Void all unsettled ledger entries for a ride then cancel the ride invoice.
--   Single call-site for any cancellation path (cash or online, with or without fee).
voidRidePaymentEntriesAndInvoice ::
  (BeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  Text -> -- rideId
  m ()
voidRidePaymentEntriesAndInvoice rideId = do
  pendingEntries <- findUnsettledRidePaymentEntries rideId
  unless (null pendingEntries) $ do
    voidRidePaymentLedger (map (.id) pendingEntries)
    logInfo $ "Voided " <> show (length pendingEntries) <> " pending ledger entries for cancelled ride: " <> rideId
  voidRideInvoice rideId

-- | Mark the Ride invoice as Paid after successful payment capture.
markRideInvoicePaid ::
  (BeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  Text -> -- rideId
  m ()
markRideInvoicePaid rideId = do
  rideEntries <- findRidePaymentEntries rideId
  let settledEntry = find (\e -> e.referenceType == ridePaymentRefRideFare && e.status == LE.SETTLED) rideEntries
  whenJust settledEntry $ \entry -> do
    mbInvoice <- FInvoiceService.getInvoiceForEntry entry.id
    whenJust mbInvoice $ \inv -> do
      FInvoiceService.updateInvoiceStatus inv.id FInvoice.Paid
      logInfo $ "Marked Ride invoice as Paid for rideId: " <> rideId

-- | Mark the Ride invoice as Issued when capture fails (entries now DUE for collection).
markRideInvoiceIssued ::
  (BeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  Text -> -- rideId
  m ()
markRideInvoiceIssued rideId = do
  rideEntries <- findRidePaymentEntries rideId
  let dueEntry = find (\e -> e.referenceType == ridePaymentRefRideFare && e.status == LE.DUE) rideEntries
  whenJust dueEntry $ \entry -> do
    mbInvoice <- FInvoiceService.getInvoiceForEntry entry.id
    whenJust mbInvoice $ \inv -> do
      FInvoiceService.updateInvoiceStatus inv.id FInvoice.Issued
      logInfo $ "Marked Ride invoice as Issued (DUE) for rideId: " <> rideId

-- ---------------------------------------------------------------------------
-- 4. Tip ledger entries
-- ---------------------------------------------------------------------------

-- | Create PENDING tip ledger entries.
--   Accrual only: Asset(BUYER) → Liability(RIDER) in PENDING state.
--   Capture-side legs happen in settleRidePaymentLedger when chargePaymentIntent
--   succeeds. Tip is a pure pass-through to the driver — no BAP commission.
createTipLedger ::
  (BeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
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
-- 5. Refund ledger entries
-- ---------------------------------------------------------------------------
--
-- Per refund_request (per-component refTypes RideFareRefund/TollRefund/ParkingRefund + VAT,
-- ref = rideId, {refundRequestId} metadata):
-- raise → pending OwnerAsset→BuyerAsset legs; success → settle them; failure → void them.
-- VOIDED legs are ignored by dedup/guards (so a retry re-raises); "done" = a SETTLED leg.

-- | Links a ledger leg to its refund request. Legs carry only the id; request state lives
--   solely in refund_request, and checks pair the id with the leg's own status column.
newtype RefundLedgerMetadata = RefundLedgerMetadata {refundRequestId :: Text}
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

mkRefundMetadata :: Id DRefundRequest.RefundRequest -> Aeson.Value
mkRefundMetadata refundRequestId = Aeson.toJSON (RefundLedgerMetadata refundRequestId.getId)

entryMatchesRefundRequest :: Id DRefundRequest.RefundRequest -> LE.LedgerEntry -> Bool
entryMatchesRefundRequest refundRequestId entry =
  (metadataRefundRequestId =<< entry.metadata) == Just refundRequestId.getId
  where
    metadataRefundRequestId v = case Aeson.fromJSON v of
      Aeson.Success (RefundLedgerMetadata rid) -> Just rid
      _ -> Nothing

-- | Per-component refund split (BAP). One base + one VAT leg per refunded component.
data RefundComponentSplit = RefundComponentSplit
  { component :: DFareBreakup.FareComponent,
    fareAmount :: HighPrecMoney,
    vatAmount :: HighPrecMoney
  }
  deriving (Show)

-- | (base refType, VAT refType) for a refunded component.
refundRefTypesForComponent :: DFareBreakup.FareComponent -> (Text, Text)
refundRefTypesForComponent = \case
  DFareBreakup.RIDE_FARE -> (ridePaymentRefRideFareRefund, ridePaymentRefRideFareRefundVAT)
  DFareBreakup.TOLL -> (ridePaymentRefTollRefund, ridePaymentRefTollRefundVAT)
  DFareBreakup.PARKING -> (ridePaymentRefParkingRefund, ridePaymentRefParkingRefundVAT)

-- | All refund leg refTypes: the 6 per-component legs (base + VAT for ride-fare/toll/parking).
--   Used to find every refund leg for a ride (dedup / settle / void / cap-check).
refundLegRefTypes :: [Text]
refundLegRefTypes =
  [ ridePaymentRefRideFareRefund,
    ridePaymentRefRideFareRefundVAT,
    ridePaymentRefTollRefund,
    ridePaymentRefTollRefundVAT,
    ridePaymentRefParkingRefund,
    ridePaymentRefParkingRefundVAT
  ]

getRefundLegEntries :: (BeamFlow.BeamFlow m r) => Text -> m [LE.LedgerEntry]
getRefundLegEntries rideId = concat <$> mapM (`getEntriesByReference` rideId) refundLegRefTypes

-- | Total SETTLED refund amount for a ride for one component (base + VAT legs) — the
--   ledger-driven "already refunded" that the per-component cap check compares against.
getSettledRefundByComponent :: (BeamFlow.BeamFlow m r) => Text -> DFareBreakup.FareComponent -> m HighPrecMoney
getSettledRefundByComponent rideId component = do
  entries <- getRefundLegEntries rideId
  pure $ settledRefundByComponent entries component

-- | Pure part of 'getSettledRefundByComponent' — sums the SETTLED base+VAT legs for one
--   component from pre-fetched entries, so per-component checks can share a single fetch.
settledRefundByComponent :: [LE.LedgerEntry] -> DFareBreakup.FareComponent -> HighPrecMoney
settledRefundByComponent entries component =
  let (baseRef, vatRef) = refundRefTypesForComponent component
   in sum [e.amount | e <- entries, e.status == LE.SETTLED, e.referenceType `elem` [baseRef, vatRef]]

createRefundRaisedLedger ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  FinanceCtx ->
  Id DRefundRequest.RefundRequest ->
  [RefundComponentSplit] -> -- per-component split (always present; caller throws on no breakup)
  m (Either FinanceError [Id LE.LedgerEntry])
createRefundRaisedLedger ctx refundRequestId splits = do
  existing <- getRefundLegEntries ctx.referenceId
  -- Skip VOIDED legs so a retry re-raises a fresh pending leg; a live leg de-dups a re-fire.
  case List.find (\e -> e.status /= LE.VOIDED && entryMatchesRefundRequest refundRequestId e) existing of
    Just e -> do
      logInfo $ "Refund APPROVED ledger already written for refundRequest " <> refundRequestId.getId
      pure $ Right [e.id]
    Nothing -> do
      let meta = Just (mkRefundMetadata refundRequestId)
      result <-
        runFinance ctx $
          forM_ splits $ \s -> do
            let (baseRef, vatRef) = refundRefTypesForComponent s.component
            void $ transferPendingWithMetadata OwnerAsset BuyerAsset s.fareAmount baseRef meta
            void $ transferPendingWithMetadata OwnerAsset BuyerAsset s.vatAmount vatRef meta
      case result of
        Left err -> do
          logError $ "Failed to create refund APPROVED ledger: " <> show err
          pure $ Left err
        Right (_, entryIds) -> pure $ Right entryIds

createRefundSucceededLedger ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  FinanceCtx ->
  Id DRefundRequest.RefundRequest ->
  m (Either FinanceError [Id LE.LedgerEntry])
createRefundSucceededLedger ctx refundRequestId = do
  existing <- getRefundLegEntries ctx.referenceId
  let approvedEntries = filter (entryMatchesRefundRequest refundRequestId) existing
      pendingApproved = filter (\e -> e.status == LE.PENDING) approvedEntries
  -- Settle the pending APPROVED leg in place — no reverse entry. Idempotent (settleEntry no-ops if settled).
  forM_ pendingApproved $ \e -> Lib.Finance.Ledger.Service.settleEntry e.id
  pure $ Right (map (\e -> e.id) approvedEntries)

-- | True once this refund's APPROVED leg is SETTLED — the "success already done" signal that
--   gates the one-shot REFUNDED side-effects (settle, invoice, BPP call) against hook re-fires.
refundSucceededAlreadyRecorded ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- rideId
  Id DRefundRequest.RefundRequest ->
  m Bool
refundSucceededAlreadyRecorded rideId refundRequestId = do
  existing <- getRefundLegEntries rideId
  pure $ any (\e -> e.status == LE.SETTLED && entryMatchesRefundRequest refundRequestId e) existing

-- | On FAILED, void this refund's pending APPROVED leg(s) — the raised obligation never
--   materialized. Only PENDING legs → idempotent.
voidRefundRaisedLedger ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- rideId
  Id DRefundRequest.RefundRequest ->
  m ()
voidRefundRaisedLedger rideId refundRequestId = do
  existing <- getRefundLegEntries rideId
  let pendingApproved =
        filter (\e -> e.status == LE.PENDING && entryMatchesRefundRequest refundRequestId e) existing
  forM_ pendingApproved $ \e -> Lib.Finance.Ledger.Service.voidEntry e.id "RefundFailed"

-- | Create PENDING Leg-1 ledger entries + RideCancellation invoice for a
--   cancellation fee. Call 'markCancellationFeeInvoicePaid' on success or
--   'voidCancellationFeeLedger' / 'markDueCancellationFeeLedger' on failure.
createPendingCancellationFeeLedger ::
  (BeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  FinanceCtx ->
  HighPrecMoney -> -- cancellationFee (without GST)
  HighPrecMoney -> -- cancellationGST
  m (Either FinanceError (Maybe (Id FInvoice.Invoice), [Id LE.LedgerEntry]))
createPendingCancellationFeeLedger ctx cancellationFee cancellationGST = do
  result <- runFinance ctx $ do
    -- Leg 1 only (PENDING): Asset(BUYER) → Liability(OWNER) — matches createRidePaymentLedger direction
    _ <- transferPending BuyerAsset OwnerLiability cancellationFee ridePaymentRefCancellationFee
    _ <- transferPending BuyerAsset OwnerLiability cancellationGST ridePaymentRefCancellationGST
    -- Invoice (same config as createCancellationFeeLedger)
    invoice
      InvoiceConfig
        { invoiceType = RideCancellation,
          issuedToType = DInvType.RIDER,
          issuedToId = ctx.counterpartyId,
          issuedToName = Nothing,
          issuedToAddress = Nothing,
          lineItems =
            filter
              (\li -> li.lineTotal > 0)
              [ InvoiceLineItem
                  { description = "Cancellation Fee",
                    descriptionType = Just CancellationFee,
                    quantity = 1,
                    unitPrice = cancellationFee,
                    lineTotal = cancellationFee,
                    isExternalCharge = False,
                    groupId = Just "g-cancel",
                    itemType = Just Fare
                  },
                InvoiceLineItem
                  { description = "Cancellation Fee VAT",
                    descriptionType = Just CancellationFeeVat,
                    quantity = 1,
                    unitPrice = cancellationGST,
                    lineTotal = cancellationGST,
                    isExternalCharge = False,
                    groupId = Just "g-cancel",
                    itemType = Just Tax
                  }
              ],
          referenceId = Just ctx.referenceId,
          gstBreakdown = Nothing,
          isVat = False,
          issuedToTaxNo = Nothing,
          issuedByTaxNo = Nothing,
          paymentMode = Nothing,
          periodStart = Nothing,
          periodEnd = Nothing
        }
  case result of
    Left err -> do
      logError $ "Failed to create pending cancellation fee ledger: " <> show err
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
    ridePaymentRefParkingCharge,
    ridePaymentRefParkingVAT,
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
  (BeamFlow.BeamFlow m r, Finance.HasActorInfo m r) =>
  [Id LE.LedgerEntry] ->
  m ()
markEntriesAsDue entryIds = do
  forM_ entryIds $ \entryId -> updateEntryStatus entryId LE.DUE
  -- Mark linked invoices as Issued
  mbInvoices <- forM entryIds $ \entryId -> FInvoiceService.getInvoiceForEntry entryId
  let uniqueInvoiceIds = List.nub [inv.id | Just inv <- mbInvoices]
  forM_ uniqueInvoiceIds $ \invId ->
    FInvoiceService.updateInvoiceStatus invId FInvoice.Issued

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

-- ---------------------------------------------------------------------------
-- 6. Ride+Tip invoice regeneration (both tip cases)
-- ---------------------------------------------------------------------------

-- | Void the existing Ride invoice and mint a new RideTip invoice that
--   includes all original line items plus a Tip line item.
--   Called after chargePaymentIntent succeeds when tip was folded into the
--   existing PI (paymentStatus was not Completed when tip was added).
regenerateRideTipInvoice ::
  (BeamFlow.BeamFlow m r, MonadFlow m, Finance.HasActorInfo m r) =>
  Text -> -- rideId
  HighPrecMoney -> -- tipAmount
  m ()
regenerateRideTipInvoice rideId tipAmount = do
  rideEntries <- findRidePaymentEntries rideId
  let rideFareEntries = List.filter (\e -> e.referenceType == ridePaymentRefRideFare) rideEntries
  -- Find the RideFare entry linked to an active (Paid/Issued/Draft) invoice.
  -- There may be stale entries from earlier recreate attempts linked to Voided invoices.
  mbActiveEntry <-
    findM
      ( \entry -> do
          mbInv <- FInvoiceService.getInvoiceForEntry entry.id
          pure $ case mbInv of
            Nothing -> False
            Just inv -> inv.status `elem` [FInvoice.Paid, FInvoice.Issued, FInvoice.Draft]
      )
      rideFareEntries
  case mbActiveEntry of
    Nothing -> logInfo $ "regenerateRideTipInvoice: no active RideFare invoice found for ride " <> rideId
    Just entry -> do
      mbInv <- FInvoiceService.getInvoiceForEntry entry.id
      case mbInv of
        Nothing -> logInfo $ "regenerateRideTipInvoice: no invoice linked to RideFare entry for ride " <> rideId
        Just priorInv -> do
          -- Step 1: collect all entries linked to the prior invoice
          allEntries <- FInvoiceService.getEntriesForInvoice priorInv.id
          -- Step 2: find tip entry IDs
          tipEntries <- getEntriesByReference ridePaymentRefTip rideId
          let priorEntryIds = map (.id) $ List.filter (\e -> e.referenceType /= ridePaymentRefTip) allEntries
              tipEntryIds = map (.id) tipEntries
          -- Step 3: parse existing line items, drop any prior Tip rows, append new one
          let priorLineItems = case Aeson.fromJSON priorInv.lineItems of
                Aeson.Success xs -> List.filter (\li -> li.descriptionType /= Just Tip) xs
                Aeson.Error _ -> []
              tipLineItem =
                InvoiceLineItem
                  { description = "Tip",
                    descriptionType = Just Tip,
                    quantity = 1,
                    unitPrice = tipAmount,
                    lineTotal = tipAmount,
                    isExternalCharge = False,
                    groupId = Nothing,
                    itemType = Just Adjustment
                  }
              newLineItems = priorLineItems <> [tipLineItem]
          -- Step 4: create the new invoice first — only void old one after success
          createRes <-
            FInvoiceService.createInvoice
              InvoiceInput
                { invoiceType = priorInv.invoiceType,
                  entityReferenceId = priorInv.entityReferenceId,
                  referenceInvoiceNumber = Nothing,
                  issuedToType = priorInv.issuedToType,
                  issuedToId = priorInv.issuedToId,
                  issuedToName = priorInv.issuedToName,
                  issuedToAddress = priorInv.issuedToAddress,
                  issuedByType = priorInv.issuedByType,
                  issuedById = priorInv.issuedById,
                  issuedByName = priorInv.issuedByName,
                  issuedByAddress = priorInv.issuedByAddress,
                  supplierName = priorInv.supplierName,
                  supplierAddress = priorInv.supplierAddress,
                  supplierGSTIN = priorInv.supplierGSTIN,
                  supplierTaxNo = priorInv.supplierTaxNo,
                  supplierId = priorInv.supplierId,
                  merchantGstin = priorInv.merchantGstin,
                  referenceId = priorInv.referenceId,
                  gstinOfParty = Nothing,
                  panOfParty = Nothing,
                  panType = Nothing,
                  counterpartyId = priorInv.issuedToId,
                  tdsRateReason = Nothing,
                  tanOfDeductee = Nothing,
                  lineItems = newLineItems,
                  gstBreakdown = Nothing,
                  currency = priorInv.currency,
                  dueAt = priorInv.dueAt,
                  merchantId = priorInv.merchantId,
                  merchantOperatingCityId = priorInv.merchantOperatingCityId,
                  merchantShortId = priorInv.merchantId,
                  isVat = False,
                  issuedToTaxNo = Nothing,
                  issuedByTaxNo = Nothing,
                  paymentMode = priorInv.paymentMode,
                  periodStart = priorInv.periodStart,
                  periodEnd = priorInv.periodEnd
                }
              (priorEntryIds <> tipEntryIds)
          case createRes of
            Left err -> logError $ "regenerateRideTipInvoice: failed to create RideTip invoice for ride " <> rideId <> ": " <> show err
            Right newInv -> do
              -- Step 5: void old invoice only after new one is persisted
              FInvoiceService.updateInvoiceStatus priorInv.id FInvoice.Voided
              FInvoiceService.updateInvoiceStatus newInv.id FInvoice.Paid
              logInfo $ "regenerateRideTipInvoice: created RideTip invoice " <> newInv.id.getId <> " (replacing " <> priorInv.id.getId <> ") for ride " <> rideId

-- | One Refund invoice per refund request — no void-prior, no cumulation. Per-component
--   negative line items (Fare + inline Tax via a shared groupId so 'attachTaxToFares' renders
--   the VAT inline); referenceInvoiceNumber = the parent ride invoice number.
createRefundInvoice ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  Text -> -- rideId
  Text -> -- refundsRequestId
  [RefundComponentSplit] -> -- per-component split (always present; caller throws on no breakup)
  m ()
createRefundInvoice rideId refundsRequestId splits = do
  let activeStatuses = [FInvoice.Draft, FInvoice.Issued, FInvoice.Paid]
  -- Parent ride invoice: header source (customer/merchant/currency) + referenceInvoiceNumber.
  mbSource <- listToMaybe <$> QInvoiceExtra.findByReferenceIdWithOptions rideId (Just DInvType.Ride) activeStatuses (Just 1) (Just 0)
  case mbSource of
    Nothing -> logInfo $ "createRefundInvoice: no ride invoice for ride " <> rideId <> "; skipping refund invoice"
    Just srcInv -> do
      let lineItems = concatMap mkRefundLineItems splits
          anyVat = any (\s -> s.vatAmount > 0) splits
      createRes <-
        FInvoiceService.createInvoice
          InvoiceInput
            { invoiceType = DInvType.Refund,
              entityReferenceId = Just refundsRequestId,
              referenceInvoiceNumber = Just srcInv.invoiceNumber,
              issuedToType = srcInv.issuedToType,
              issuedToId = srcInv.issuedToId,
              issuedToName = srcInv.issuedToName,
              issuedToAddress = srcInv.issuedToAddress,
              issuedByType = srcInv.issuedByType,
              issuedById = srcInv.issuedById,
              issuedByName = srcInv.issuedByName,
              issuedByAddress = srcInv.issuedByAddress,
              supplierName = srcInv.supplierName,
              supplierAddress = srcInv.supplierAddress,
              supplierGSTIN = srcInv.supplierGSTIN,
              supplierTaxNo = srcInv.supplierTaxNo,
              supplierId = srcInv.supplierId,
              merchantGstin = srcInv.merchantGstin,
              referenceId = Just rideId,
              gstinOfParty = Nothing,
              panOfParty = Nothing,
              panType = Nothing,
              counterpartyId = srcInv.issuedToId,
              tdsRateReason = Nothing,
              tanOfDeductee = Nothing,
              lineItems = lineItems,
              gstBreakdown = Nothing,
              currency = srcInv.currency,
              dueAt = srcInv.dueAt,
              merchantId = srcInv.merchantId,
              merchantOperatingCityId = srcInv.merchantOperatingCityId,
              merchantShortId = srcInv.merchantId,
              isVat = anyVat,
              issuedToTaxNo = Nothing,
              issuedByTaxNo = Nothing,
              paymentMode = srcInv.paymentMode,
              periodStart = srcInv.periodStart,
              periodEnd = srcInv.periodEnd
            }
          []
      case createRes of
        Left err -> logError $ "createRefundInvoice: failed for ride " <> rideId <> ": " <> show err
        Right newInv -> do
          FInvoiceService.updateInvoiceStatus newInv.id FInvoice.Paid
          logInfo $ "createRefundInvoice: created refund invoice " <> newInv.id.getId <> " for ride " <> rideId <> " refundRequest " <> refundsRequestId

-- | Per-component refund invoice lines: a negative Fare + a negative inline Tax (shared
--   groupId). Toll/Parking are flagged external (mirrors the ride invoice) so they render
--   as their own lines; only Ride Fare is the taxable base.
mkRefundLineItems :: RefundComponentSplit -> [InvoiceLineItem]
mkRefundLineItems s =
  let (fareDesc, fareType, taxDesc, taxType, gId) = refundLineItemMeta s.component
      isExt = case s.component of DFareBreakup.RIDE_FARE -> False; _ -> True
      mkNeg desc dtype amt typ =
        if amt > 0
          then Just InvoiceLineItem {description = desc, descriptionType = Just dtype, quantity = 1, unitPrice = negate amt, lineTotal = negate amt, isExternalCharge = isExt, groupId = Just gId, itemType = Just typ}
          else Nothing
   in catMaybes [mkNeg fareDesc fareType s.fareAmount Fare, mkNeg taxDesc taxType s.vatAmount Tax]

refundLineItemMeta :: DFareBreakup.FareComponent -> (Text, LineItemDescription, Text, LineItemDescription, Text)
refundLineItemMeta = \case
  DFareBreakup.RIDE_FARE -> ("Ride Fare Refund", RideFareRefund, "Ride Fare Refund VAT", RideFareRefundTax, "g-refund-ridefare")
  DFareBreakup.TOLL -> ("Toll Refund", TollRefund, "Toll Refund VAT", TollRefundTax, "g-refund-toll")
  DFareBreakup.PARKING -> ("Parking Refund", ParkingRefund, "Parking Refund VAT", ParkingRefundTax, "g-refund-parking")
