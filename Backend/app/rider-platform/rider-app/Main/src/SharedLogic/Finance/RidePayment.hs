{-
  Finance integration for rider-side Stripe ride payments.

  Ledger flow:
    1. After payment order creation (PENDING):
       Liability(RIDER) → Asset(BUYER) for each line item

    2. After payment capture success (SETTLED):
       Mark pending entries as SETTLED
       Asset(BUYER) → External(BUYER) for each line item

    3. On tip / cancellation fee: same two-leg pattern

    4. On cancel intent: mark ledger + invoice VOIDED
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
    settleRidePaymentLedger,
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

import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (MonadFlow, logError, logInfo)
import Lib.Finance
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import Lib.Finance.Ledger.Service (getEntry)
import qualified Lib.Finance.Ledger.Service
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow

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
  Text -> -- riderId (person ID)
  Text -> -- referenceId (ride ID)
  Maybe Text -> -- merchantName
  Maybe Text -> -- merchantShortId
  FinanceCtx
buildRiderFinanceCtx merchantId merchantOpCityId currency riderId referenceId merchantName merchantShortId =
  FinanceCtx
    { merchantId = merchantId,
      merchantOpCityId = merchantOpCityId,
      currency = currency,
      counterpartyType = RIDER,
      counterpartyId = riderId,
      referenceId = referenceId,
      merchantName = merchantName,
      merchantShortId = merchantShortId,
      issuedByAddress = Nothing,
      supplierName = Nothing,
      supplierGSTIN = Nothing,
      supplierId = Nothing,
      panOfParty = Nothing,
      panType = Nothing,
      tdsRateReason = Nothing
    }

-- ---------------------------------------------------------------------------
-- 1. Create PENDING ledger entries after payment order creation
-- ---------------------------------------------------------------------------

-- | Create PENDING ledger entries after successful payment order creation.
--   Flow: Liability(RIDER) → Asset(BUYER) for each line item.
--   Returns entry IDs for later settlement + invoice ID.
createRidePaymentLedger ::
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
    -- PENDING entries: Liability(RIDER) → Asset(BUYER) for full fare
    _ <- transferPending OwnerLiability BuyerAsset rideFare ridePaymentRefRideFare
    _ <- transferPending OwnerLiability BuyerAsset gstAmount ridePaymentRefGST
    _ <- transferPending OwnerLiability BuyerAsset platformFee ridePaymentRefPlatformFee

    -- Offer discount: Asset(BUYER) → Liability(BUYER) — marketplace absorbs the charge reduction
    when (offerDiscountAmount > 0) $ do
      _ <- transferPending BuyerAsset OwnerLiability offerDiscountAmount ridePaymentRefOfferDiscount
      pure ()

    -- Cashback payout: Liability(BUYER) → Asset(RIDER) — marketplace owes rider a cashback
    when (cashbackPayoutAmount > 0) $ do
      _ <- transferPending OwnerLiability BuyerAsset cashbackPayoutAmount ridePaymentRefCashbackPayout
      pure ()

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
                    isExternalCharge = False
                  },
                InvoiceLineItem
                  { description = ridePaymentRefGST,
                    quantity = 1,
                    unitPrice = gstAmount,
                    lineTotal = gstAmount,
                    isExternalCharge = False
                  },
                InvoiceLineItem
                  { description = ridePaymentRefPlatformFee,
                    quantity = 1,
                    unitPrice = platformFee,
                    lineTotal = platformFee,
                    isExternalCharge = False
                  },
                InvoiceLineItem
                  { description = ridePaymentRefOfferDiscount,
                    quantity = 1,
                    unitPrice = negate offerDiscountAmount,
                    lineTotal = negate offerDiscountAmount,
                    isExternalCharge = False
                  },
                InvoiceLineItem
                  { description = ridePaymentRefCashbackPayout,
                    quantity = 1,
                    unitPrice = cashbackPayoutAmount,
                    lineTotal = cashbackPayoutAmount,
                    isExternalCharge = False
                  }
              ],
          gstBreakdown = Nothing
        }
  case result of
    Left err -> do
      logError $ "Failed to create ride payment ledger: " <> show err
      pure $ Left err
    Right (mbInvoiceId, entryIds) ->
      pure $ Right RidePaymentLedgerResult {invoiceId = mbInvoiceId, entryIds}

-- ---------------------------------------------------------------------------
-- 1b. Create SETTLED ledger entries for fully discounted rides (amount = 0)
-- ---------------------------------------------------------------------------

-- | Create ledger entries for a fully discounted ride (100% offer covers fare).
--   Core entries (RideFare, GST, PlatformFee) are created as SETTLED immediately
--   since no payment is needed. The OfferDiscount entry stays PENDING as it
--   represents BuyerLiability (marketplace cost) to be cleared separately.
--   This does NOT show in getPaymentDues and does NOT block rides.
createFullyDiscountedRidePaymentLedger ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  FinanceCtx ->
  HighPrecMoney -> -- rideFare (original, before discount)
  HighPrecMoney -> -- gstAmount
  HighPrecMoney -> -- platformFee
  HighPrecMoney -> -- offerDiscountAmount (should equal rideFare + gstAmount + platformFee)
  m (Either FinanceError RidePaymentLedgerResult)
createFullyDiscountedRidePaymentLedger ctx rideFare gstAmount platformFee offerDiscountAmount = do
  result <- runFinance ctx $ do
    -- Core entries as SETTLED: no payment needed from rider
    _ <- transfer OwnerLiability BuyerAsset rideFare ridePaymentRefRideFare
    _ <- transfer OwnerLiability BuyerAsset gstAmount ridePaymentRefGST
    _ <- transfer OwnerLiability BuyerAsset platformFee ridePaymentRefPlatformFee

    -- Settlement second leg: Asset(BUYER) → External(BUYER)
    _ <- transfer BuyerAsset BuyerExternal rideFare ridePaymentRefRideFare
    _ <- transfer BuyerAsset BuyerExternal gstAmount ridePaymentRefGST
    _ <- transfer BuyerAsset BuyerExternal platformFee ridePaymentRefPlatformFee

    -- Offer discount: PENDING — BuyerLiability, cleared separately by marketplace
    when (offerDiscountAmount > 0) $ do
      _ <- transferPending BuyerAsset OwnerLiability offerDiscountAmount ridePaymentRefOfferDiscount
      pure ()

    -- Invoice
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
                    isExternalCharge = False
                  },
                InvoiceLineItem
                  { description = ridePaymentRefGST,
                    quantity = 1,
                    unitPrice = gstAmount,
                    lineTotal = gstAmount,
                    isExternalCharge = False
                  },
                InvoiceLineItem
                  { description = ridePaymentRefPlatformFee,
                    quantity = 1,
                    unitPrice = platformFee,
                    lineTotal = platformFee,
                    isExternalCharge = False
                  },
                InvoiceLineItem
                  { description = ridePaymentRefOfferDiscount,
                    quantity = 1,
                    unitPrice = negate offerDiscountAmount,
                    lineTotal = negate offerDiscountAmount,
                    isExternalCharge = False
                  }
              ],
          gstBreakdown = Nothing
        }
  case result of
    Left err -> do
      logError $ "Failed to create fully discounted ride payment ledger: " <> show err
      pure $ Left err
    Right (mbInvoiceId, entryIds) -> do
      logInfo $ "Created SETTLED ledger for fully discounted ride (offer covers 100%)"
      pure $ Right RidePaymentLedgerResult {invoiceId = mbInvoiceId, entryIds}

-- ---------------------------------------------------------------------------
-- 2. Settle ledger entries after payment capture success
-- ---------------------------------------------------------------------------

-- | Settle PENDING entries after payment capture success.
--   Marks existing entries as SETTLED and creates the second leg:
--   Asset(BUYER) → External(BUYER) for each settled entry amount.
--   Reads amounts from the entries themselves — no need to pass amounts separately.
settleRidePaymentLedger ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  FinanceCtx ->
  [Id LE.LedgerEntry] -> -- entry IDs from createRidePaymentLedger
  Text -> -- settlement reason
  m (Either FinanceError ())
settleRidePaymentLedger ctx entryIds settledReason = do
  -- First, settle all pending entries and collect their amounts/refTypes
  entryDetails <- forM entryIds $ \entryId -> do
    mbEntry <- Lib.Finance.Ledger.Service.getEntry entryId
    settleEntry entryId
    pure mbEntry
  let settledEntries = catMaybes entryDetails
  logInfo $ "Settled " <> show (length settledEntries) <> " ride payment ledger entries, reason: " <> settledReason
  -- Then create the second leg: Asset(BUYER) → External(BUYER) for each settled entry
  result <- runFinance ctx $ do
    forM_ settledEntries $ \entry ->
      transfer_ BuyerAsset BuyerExternal entry.amount entry.referenceType
  case result of
    Left err -> do
      logError $ "Failed to create settlement second leg: " <> show err
      pure $ Left err
    Right _ -> pure $ Right ()

-- ---------------------------------------------------------------------------
-- 3. Void ledger entries when payment intent is cancelled
-- ---------------------------------------------------------------------------

-- | Void all PENDING ledger entries and invoice when payment is cancelled.
voidRidePaymentLedger ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  [Id LE.LedgerEntry] ->
  m ()
voidRidePaymentLedger entryIds = do
  forM_ entryIds $ \entryId ->
    voidEntry entryId "PaymentIntentCancelled"
  logInfo $ "Voided " <> show (length entryIds) <> " ride payment ledger entries"

-- ---------------------------------------------------------------------------
-- 4. Tip ledger entries (both legs created and settled immediately)
-- ---------------------------------------------------------------------------

-- | Create PENDING tip ledger entries.
--   Leg 1 only: Liability(RIDER) → Asset(BUYER) in PENDING state.
--   Settlement (leg 2) happens when chargePaymentIntent succeeds.
--   No card fee or platform commission for tips.
createTipLedger ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  FinanceCtx ->
  HighPrecMoney -> -- tipAmount
  m (Either FinanceError [Id LE.LedgerEntry])
createTipLedger ctx tipAmount = do
  result <- runFinance ctx $ do
    transferPending OwnerLiability BuyerAsset tipAmount ridePaymentRefTip
  case result of
    Left err -> do
      logError $ "Failed to create tip ledger: " <> show err
      pure $ Left err
    Right (_, entryIds) -> pure $ Right entryIds

-- ---------------------------------------------------------------------------
-- 5. Cancellation fee ledger entries
-- ---------------------------------------------------------------------------

-- | Create ledger entries for cancellation fee. Both legs settled immediately.
createCancellationFeeLedger ::
  (BeamFlow.BeamFlow m r, MonadFlow m) =>
  FinanceCtx ->
  HighPrecMoney -> -- cancellationFee (without GST)
  HighPrecMoney -> -- cancellationGST
  m (Either FinanceError (Maybe (Id FInvoice.Invoice), [Id LE.LedgerEntry]))
createCancellationFeeLedger ctx cancellationFee cancellationGST = do
  result <- runFinance ctx $ do
    -- Leg 1: Liability(RIDER) → Asset(BUYER)
    _ <- transfer OwnerLiability BuyerAsset cancellationFee ridePaymentRefCancellationFee
    _ <- transfer OwnerLiability BuyerAsset cancellationGST ridePaymentRefCancellationGST
    -- Leg 2: Asset(BUYER) → External(BUYER)
    transfer_ BuyerAsset BuyerExternal cancellationFee ridePaymentRefCancellationFee
    transfer_ BuyerAsset BuyerExternal cancellationGST ridePaymentRefCancellationGST
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
          gstBreakdown = Nothing
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
  [ ridePaymentRefRideFare,
    ridePaymentRefGST,
    ridePaymentRefPlatformFee,
    ridePaymentRefOfferDiscount,
    ridePaymentRefCashbackPayout,
    ridePaymentRefTip,
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
