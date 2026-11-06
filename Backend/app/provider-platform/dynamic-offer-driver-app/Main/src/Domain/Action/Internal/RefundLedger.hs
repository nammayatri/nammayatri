{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.RefundLedger where

import qualified BecknV2.OnDemand.FareBreakup.Tax as Tax
import qualified Domain.Types.Booking as DBooking
import Domain.Types.FareParameters (FareParameters)
import "beckn-spec" Domain.Types.Invoice (IssuedToType (..))
import qualified "beckn-spec" Domain.Types.Invoice as BeckInvoice
import Domain.Types.Ride (Ride)
import qualified Domain.Types.TransporterConfig as DTC
import Environment
import Kernel.Beam.Functions (runInReplica)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance (AccountRole (..), FinanceCtx (..), transfer, transferPending)
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Finance.Invoice.Interface as InvoiceI
import qualified Lib.Finance.Invoice.Service as InvoiceSvc
import qualified Lib.Finance.Ledger.Service as LedgerSvc
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QFinanceInvoiceExtra
import qualified SharedLogic.FareCalculator as FC
import SharedLogic.Finance.PostActions (runFinance)
import qualified SharedLogic.Finance.Wallet as Wallet
import qualified SharedLogic.RideTax as RideTax
import qualified Storage.CachedQueries.Merchant as QM
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FareParameters as QFP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error

-- | Defined here because rider-app's RefundRequestStatus can't be cross-imported.
data RefundLedgerStatus = APPROVED | REFUNDED | FAILED
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

-- | Mirrors rider-app's Domain.Types.FareBreakup.RefundFareComponent (can't be cross-imported) with
--   identical constructors, so the Generic JSON wire is byte-compatible and FromJSON rejects any
--   unknown tag at decode.
data RefundFareComponent = RIDE_FARE | TOLL | PARKING | CANCELLATION_FEE
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

-- | Per-component refund breakdown (BAP-split). 'component' is the RefundFareComponent enum
--   (Generic JSON: "RIDE_FARE"/"TOLL"/"PARKING"/"CANCELLATION_FEE"). Field names byte-identical to BAP.
data RefundLedgerComponent = RefundLedgerComponent
  { component :: RefundFareComponent,
    fareAmount :: HighPrecMoney,
    vatAmount :: HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Field names must match the BAP-side request verbatim (Generic JSON wire).
data RefundLedgerReq = RefundLedgerReq
  { refundRequestId :: Text,
    refundsAmount :: HighPrecMoney,
    deductFromDriver :: Maybe Bool,
    refundRequestStatus :: RefundLedgerStatus,
    refundComponents :: Maybe [RefundLedgerComponent] -- per-component split (always present for APPROVED/REFUNDED; Nothing only on FAILED, which just voids)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | APPROVED → pending leg(s); REFUNDED → settle them; FAILED → void them.
--   Idempotent via a per-request lock + leg-status dedup; throws on write failure.
refundLedger :: Id Ride -> RefundLedgerReq -> Maybe Text -> Flow APISuccess
refundLedger rideId req apiKey = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let merchantId = fromMaybe booking.providerId ride.merchantId
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  Redis.withWaitAndLockRedis (refundLedgerLockKey req.refundRequestId) 60 50000 $ do
    transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = ride.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
    mbDriver <- QPerson.findById ride.driverId
    mbDriverInfo <- QDI.findById (cast ride.driverId)
    -- online cab only; driver-deducted clawback legs debit OwnerLiability (the driver wallet), reducing payout.
    ctx <- Wallet.buildFinanceCtx booking ride mbDriver Nothing mbDriverInfo transporterConfig True Nothing
    -- When the approval doesn't specify who bears the refund, the merchant config decides;
    -- with no config either, the platform absorbs.
    cancellationCommissionGross <- cancellationCommissionFromLedger (fromMaybe False booking.fareParams.isVatTaxType) ride
    overdueBenefit <- cancellationOverdueBenefitFromLedger ride
    fareParams <- QFP.rideFareParams ride booking
    let totals = refundTotals ride fareParams
    let deductFromDriver = fromMaybe (fromMaybe False transporterConfig.defaultRefundDeductFromDriver) req.deductFromDriver
        isVatMarket = fromMaybe False booking.fareParams.isVatTaxType
        legs = refundLegs deductFromDriver (RideTax.commissionTaxPct (RideTax.bookingRideTax booking transporterConfig)) cancellationCommissionGross overdueBenefit isVatMarket totals req ride
    existing <- getRefundRequestLegs req.refundRequestId
    case req.refundRequestStatus of
      APPROVED ->
        -- VOIDED legs are skipped so a retry re-raises fresh; ANY live leg de-dups a re-fire.
        if any (\e -> e.status /= LE.VOIDED) existing
          then logInfo $ "refundLedger: APPROVED call dedup — live legs already exist for refundRequestId " <> req.refundRequestId <> "; skipping"
          else postLegs ctx Pending legs req.refundRequestId
      REFUNDED -> do
        -- alreadyDone = this refund's live legs exist and are ALL settled (skip the invoice).
        -- Strictly none-PENDING, so a crash mid-settle still completes invoices on redelivery.
        let idLegs = filter (\e -> e.status /= LE.VOIDED) existing
            alreadyDone = not (null idLegs) && not (any (\e -> e.status == LE.PENDING) idLegs)
        if not (null idLegs)
          then forM_ (filter (\e -> e.status == LE.PENDING) idLegs) $ \e -> LedgerSvc.settleEntry e.id
          else do
            -- APPROVED was missed; post directly as SETTLED (reachable only with no live legs — fires at most once).
            logInfo $ "refundLedger: REFUNDED call with no live legs (APPROVED missed) — posting directly as SETTLED for refundRequestId " <> req.refundRequestId
            postLegs ctx Settled legs req.refundRequestId
        -- the fleet/driver-visible refund invoice + (if driver-deducted) the negative Commission invoice, once per refund
        unless alreadyDone $ do
          createRefundInvoice ctx booking req.refundRequestId (fromMaybe [] req.refundComponents)
          when deductFromDriver $ createCommissionRefundInvoice ctx req totals ride booking transporterConfig cancellationCommissionGross
      FAILED ->
        -- Failed: void the pending APPROVED leg(s). Only PENDING → idempotent.
        forM_ (filter (\e -> e.status == LE.PENDING) existing) $ \e ->
          LedgerSvc.voidEntry e.id "RefundFailed"
  pure Success

data PostMode = Pending | Settled

-- | Post the legs in one finance transaction; throws on failure so the caller redelivers.
postLegs :: FinanceCtx -> PostMode -> [(AccountRole, AccountRole, HighPrecMoney, Text)] -> Text -> Flow ()
postLegs ctx mode legs refundRequestId = do
  res <- runFinance ctx {entityReferenceId = Just refundRequestId, entityReferenceType = Just LE.RefundRequest} $
    forM_ legs $ \(fromRole, toRole, amount, refType) ->
      void $ case mode of
        Pending -> transferPending fromRole toRole amount refType
        Settled -> transfer fromRole toRole amount refType Nothing
  case res of
    Left err -> throwError $ InternalError $ "Refund ledger write failed for " <> refundRequestId <> ": " <> show err
    Right _ -> pure ()

-- | The cancellation commission (incl. VAT) actually charged for this ride's booking: the sum of
--   its SETTLED CancellationCommission(+VAT) ledger legs. Nothing ever reverses these legs
--   (refund clawbacks post their own separate legs), so no reversal filtering is needed. The
--   ledger is the source of truth here — the cancellation_dues_details table stores two candidate
--   amounts (normal and overdue) without recording which one was actually charged.
cancellationCommissionFromLedger :: Bool -> Ride -> Flow (HighPrecMoney, HighPrecMoney)
cancellationCommissionFromLedger isVat ride = do
  baseLegs <- LedgerSvc.getEntriesByReference Wallet.walletReferenceCancellationCommission ride.bookingId.getId
  vatLegs <- LedgerSvc.getEntriesByReference (if isVat then Wallet.walletReferenceCancellationCommissionVAT else Wallet.walletReferenceCancellationCommissionGST) ride.bookingId.getId
  let settledGross es = sum [e.amount | e <- es, e.status == LE.SETTLED]
  pure (settledGross baseLegs + settledGross vatLegs, settledGross vatLegs)

splitByPostedTax :: (HighPrecMoney, HighPrecMoney) -> HighPrecMoney -> (HighPrecMoney, HighPrecMoney)
splitByPostedTax (postedGross, postedTax) slice =
  let sliceTax = if postedGross > 0 then slice * postedTax / postedGross else 0
   in (slice - sliceTax, sliceTax)

-- | The (base, tax) overdue benefit the platform kept for this ride's booking and still holds.
--   Each is written as a pass-through PAIR of equal ledger legs under one reference, so ONE
--   un-reversed leg carries the full amount; reversal entries and the originals they point at
--   are skipped.
cancellationOverdueBenefitFromLedger :: Ride -> Flow (HighPrecMoney, HighPrecMoney)
cancellationOverdueBenefitFromLedger ride = do
  let unreversedAmount ref = do
        entries <- LedgerSvc.getEntriesByReference ref ride.bookingId.getId
        let reversedIds = mapMaybe (.reversalOf) entries
        pure $ maybe 0 (.amount) $ listToMaybe [e | e <- entries, e.status == LE.SETTLED, isNothing e.reversalOf, e.id `notElem` reversedIds]
  (,) <$> unreversedAmount Wallet.walletReferenceCancellationOverdueBenefit <*> unreversedAmount Wallet.walletReferenceCancellationOverdueBenefitTax

-- | Per-component refund legs. Platform-absorbed (deductFromDriver = False): every component posts
--   from SellerExpense. Driver-deducted: the driver refunds every component gross, and the platform
--   rebates to the driver the commission slice and the overdue benefit it kept on the refunded part.
refundLegs :: Bool -> Maybe Double -> (HighPrecMoney, HighPrecMoney) -> (HighPrecMoney, HighPrecMoney) -> Bool -> RefundTotals -> RefundLedgerReq -> Ride -> [(AccountRole, AccountRole, HighPrecMoney, Text)]
refundLegs deductFromDriver mbCommissionVatPct cancellationCommissionGross (overdueBenefit, overdueBenefitTax) isVatMarket totals req ride =
  concatMap componentLegs (fromMaybe [] req.refundComponents)
  where
    commission = fromMaybe 0 ride.commission
    rideFareTotal = totals.rideFareBase
    cancellationTotal = totals.cancellationGross
    -- Taxes return to where they sat forward: driver in VAT markets, government in GST.
    remittedTaxSource = if isVatMarket then OwnerLiability else GovtIndirect
    -- Full precision, no rounding: legs must sum to the component exactly and a full refund must
    -- claw back exactly the charged commission (the commission aggregate nets to zero).
    byTaxRegime vatRef gstRef = if isVatMarket then vatRef else gstRef
    driverOnlyLegs taxSource base tax baseRef vatRef =
      [ (OwnerLiability, BuyerExternal, base, baseRef),
        (taxSource, BuyerExternal, tax, vatRef)
      ]
    platformLegs legs = [(source, BuyerExternal, amount, ref) | (source, amount, ref) <- legs, amount > 0]
    platformToDriverRebateLegs legs = [(SellerExpense, OwnerLiability, amount, ref) | (amount, ref) <- legs, amount > 0]
    -- ride-fare prorates by BASE (base/base); cancellation below prorates by GROSS (gross/gross)
    rideFareLegs comp baseRef vatRef =
      let slice = if rideFareTotal > 0 then commission * comp.fareAmount / rideFareTotal else 0
          (sliceBase, sliceVat) = Tax.splitGrossByTaxPct mbCommissionVatPct slice
       in driverOnlyLegs remittedTaxSource comp.fareAmount comp.vatAmount baseRef vatRef
            <> platformToDriverRebateLegs
              [ (sliceBase, Wallet.walletReferenceRideFareRefundCommission),
                (sliceVat, byTaxRegime Wallet.walletReferenceRideFareRefundCommissionVAT Wallet.walletReferenceRideFareRefundCommissionGST)
              ]
    -- Three buckets funded the fee (driver net share, charged commission, overdue benefit); each
    -- returns the same fraction of the refunded gross, so a full refund zeroes every bucket.
    cancellationFeeLegs comp baseRef vatRef =
      let prorate amt = if cancellationTotal > 0 then amt * (comp.fareAmount + comp.vatAmount) / cancellationTotal else 0
          commissionSlice = prorate (fst cancellationCommissionGross)
          benefitSlice = prorate overdueBenefit
          benefitTaxSlice = if isVatMarket then prorate overdueBenefitTax else 0
          (sliceBase, sliceVat) = splitByPostedTax cancellationCommissionGross commissionSlice
       in driverOnlyLegs remittedTaxSource comp.fareAmount comp.vatAmount baseRef vatRef
            <> platformToDriverRebateLegs
              [ (sliceBase, Wallet.walletReferenceCancellationRefundCommission),
                (sliceVat, byTaxRegime Wallet.walletReferenceCancellationRefundCommissionVAT Wallet.walletReferenceCancellationRefundCommissionGST),
                (benefitSlice, Wallet.walletReferenceCancellationOverdueBenefitRefund),
                (benefitTaxSlice, Wallet.walletReferenceCancellationOverdueBenefitRefundTax)
              ]
    componentLegs comp =
      let (baseRef, vatRef) = refundRefTypesForComponent isVatMarket comp.component
       in if not deductFromDriver
            then platformLegs [(SellerExpense, comp.fareAmount, baseRef), (SellerExpense, comp.vatAmount, vatRef)]
            else case comp.component of
              RIDE_FARE -> rideFareLegs comp baseRef vatRef
              CANCELLATION_FEE -> cancellationFeeLegs comp baseRef vatRef
              _ -> driverOnlyLegs OwnerLiability comp.fareAmount comp.vatAmount baseRef vatRef

data RefundTotals = RefundTotals
  { rideFareBase :: HighPrecMoney,
    cancellationGross :: HighPrecMoney
  }

refundTotals :: Ride -> FareParameters -> RefundTotals
refundTotals ride fareParams =
  RefundTotals
    { rideFareBase = if gross > 0 then amounts.rideBase * remaining / gross else amounts.rideBase,
      cancellationGross = amounts.cancellationDues
    }
  where
    amounts = FC.fareParamsAmounts ride.fare fareParams
    gross = amounts.rideBase + amounts.rideTax
    remaining = gross - max 0 (min (fromMaybe 0 ride.discountAmount) gross)

-- | (base refType, VAT refType) for a refunded component — exhaustive over the enum.
refundRefTypesForComponent :: Bool -> RefundFareComponent -> (Text, Text)
refundRefTypesForComponent isVat fc = case fc of
  RIDE_FARE -> (Wallet.walletReferenceRideFareRefund, byRegime Wallet.walletReferenceRideFareRefundVAT Wallet.walletReferenceRideFareRefundGST)
  TOLL -> (Wallet.walletReferenceTollRefund, byRegime Wallet.walletReferenceTollRefundVAT Wallet.walletReferenceTollRefundGST)
  PARKING -> (Wallet.walletReferenceParkingRefund, byRegime Wallet.walletReferenceParkingRefundVAT Wallet.walletReferenceParkingRefundGST)
  CANCELLATION_FEE -> (Wallet.walletReferenceCancellationFeeRefund, byRegime Wallet.walletReferenceCancellationFeeRefundVAT Wallet.walletReferenceCancellationFeeRefundGST)
  where
    byRegime vatRef gstRef = if isVat then vatRef else gstRef

-- | The legs of one refund request. Every entry here is a leg: BPP refund legs are real debits
--   against the driver or the platform, so nothing reverses them (the zero-out is BAP-only).
getRefundRequestLegs :: Text -> Flow [LE.LedgerEntry]
getRefundRequestLegs = LedgerSvc.getEntriesByEntityReference LE.RefundRequest

-- | One lock per refund request: APPROVED/REFUNDED/FAILED deliveries serialize.
refundLedgerLockKey :: Text -> Text
refundLedgerLockKey refundRequestId = "refundLedger:" <> refundRequestId

-- | One BPP (fleet/driver-visible) refund invoice per refund request — no void-prior, no cumulation.
--   Per-component negative line items (Fare + inline Tax via a shared groupId); issuedTo follows the
--   ride's CUSTOMER copy so it surfaces on the dashboard Refunds tab. Caller gates against re-fires.
createRefundInvoice ::
  FinanceCtx ->
  DBooking.Booking ->
  Text -> -- refundsRequestId
  [RefundLedgerComponent] -> -- per-component split (always present; caller guarantees it)
  Flow ()
createRefundInvoice ctx booking refundsRequestId comps = do
  let referenceId = ctx.referenceId -- booking.id
      lineItems = concatMap mkRefundInvoiceLineItems comps
      anyVat = any (\c -> c.vatAmount > 0) comps
  -- Parent = the ride's CUSTOMER invoice (keyed by bookingId); Nothing if none found.
  mbParentRideInvoice <-
    listToMaybe
      <$> QFinanceInvoiceExtra.findByReferenceIdWithOptions referenceId (Just BeckInvoice.Ride) (Just CUSTOMER) [FInvoice.Draft, FInvoice.Issued, FInvoice.Paid] (Just 1) (Just 0)
  createRes <-
    InvoiceSvc.createInvoice
      InvoiceI.InvoiceInput
        { invoiceType = BeckInvoice.Refund,
          entityReferenceId = Just refundsRequestId,
          referenceInvoiceNumber = (\inv -> inv.invoiceNumber) <$> mbParentRideInvoice,
          issuedToType = CUSTOMER,
          issuedToId = maybe "" (.getId) booking.riderId,
          issuedToName = booking.riderName,
          issuedToAddress = booking.fromLocation.address.fullAddress,
          issuedByType = "BUYER",
          issuedById = ctx.merchantId,
          issuedByName = ctx.merchantName,
          issuedByAddress = ctx.issuedByAddress,
          supplierName = ctx.supplierName,
          supplierAddress = ctx.issuedByAddress,
          supplierGSTIN = ctx.supplierGSTIN,
          supplierTaxNo = Nothing,
          supplierId = ctx.supplierId,
          merchantGstin = ctx.merchantGstin,
          referenceId = Just referenceId,
          gstinOfParty = Nothing,
          panOfParty = Nothing,
          panType = Nothing,
          counterpartyId = ctx.counterpartyId,
          tdsRateReason = Nothing,
          tanOfDeductee = Nothing,
          lineItems = lineItems,
          gstBreakdown = Nothing,
          currency = ctx.currency,
          dueAt = Nothing,
          periodStart = Nothing,
          periodEnd = Nothing,
          merchantId = ctx.merchantId,
          merchantOperatingCityId = ctx.merchantOpCityId,
          merchantShortId = fromMaybe ctx.merchantId ctx.merchantShortId,
          isVat = anyVat,
          issuedToTaxNo = Nothing,
          issuedByTaxNo = Nothing,
          paymentMode = Nothing
        }
      []
  case createRes of
    Left err -> logError $ "createRefundInvoice: failed to create BPP refund invoice for booking " <> referenceId <> ": " <> show err
    Right newInv -> do
      InvoiceSvc.updateInvoiceStatus newInv.id FInvoice.Paid
      logInfo $ "createRefundInvoice: created BPP refund invoice " <> newInv.id.getId <> " for booking " <> referenceId <> " refundRequest " <> refundsRequestId

-- | Per-component BPP refund invoice lines (negative Fare + inline Tax, shared groupId).
--   Ride fare and cancellation fee are regular taxable lines; toll/parking are flagged
--   external, mirroring the ride invoice.
mkRefundInvoiceLineItems :: RefundLedgerComponent -> [InvoiceI.InvoiceLineItem]
mkRefundInvoiceLineItems c =
  let (fareDesc, fareType, taxDesc, taxType, gId) = refundInvoiceLineMeta c.component
      isExt = c.component `elem` [TOLL, PARKING]
      mkNeg desc dtype amt typ =
        if amt > 0
          then Just InvoiceI.InvoiceLineItem {description = desc, descriptionType = Just dtype, quantity = 1, unitPrice = negate amt, lineTotal = negate amt, isExternalCharge = isExt, groupId = Just gId, itemType = Just typ}
          else Nothing
   in catMaybes [mkNeg fareDesc fareType c.fareAmount InvoiceI.Fare, mkNeg taxDesc taxType c.vatAmount InvoiceI.Tax]

refundInvoiceLineMeta :: RefundFareComponent -> (Text, InvoiceI.LineItemDescription, Text, InvoiceI.LineItemDescription, Text)
refundInvoiceLineMeta fc = case fc of
  RIDE_FARE -> ("Ride Fare Refund", InvoiceI.RideFareRefund, "Ride Fare Refund VAT", InvoiceI.RideFareRefundTax, "g-refund-ridefare")
  TOLL -> ("Toll Refund", InvoiceI.TollRefund, "Toll Refund VAT", InvoiceI.TollRefundTax, "g-refund-toll")
  PARKING -> ("Parking Refund", InvoiceI.ParkingRefund, "Parking Refund VAT", InvoiceI.ParkingRefundTax, "g-refund-parking")
  CANCELLATION_FEE -> ("Cancellation Fee Refund", InvoiceI.CancellationFeeRefund, "Cancellation Fee Refund VAT", InvoiceI.CancellationFeeRefundTax, "g-refund-cancellation")

-- | On a driver-deducted refund, record the returned commission slice(s) — ride fare and/or
--   cancellation fee — as a NEGATIVE Commission invoice, left Draft so the commission-aggregation
--   scheduler nets it in. A record, not a second cash movement (the cash is the *RefundCommission
--   ledger legs). Same operands as refundLegs' slices, so invoice and legs agree to the last digit.
createCommissionRefundInvoice :: FinanceCtx -> RefundLedgerReq -> RefundTotals -> Ride -> DBooking.Booking -> DTC.TransporterConfig -> (HighPrecMoney, HighPrecMoney) -> Flow ()
createCommissionRefundInvoice ctx req totals ride booking transporterConfig cancellationCommissionGross = do
  let commission = fromMaybe 0 ride.commission
      rideFareTotal = totals.rideFareBase
      cancellationTotal = totals.cancellationGross
      refundedFor p f = maybe 0 (sum . map f . filter (\c -> c.component == p)) req.refundComponents
      commissionSlice = if rideFareTotal > 0 then commission * refundedFor RIDE_FARE (.fareAmount) / rideFareTotal else 0
      cancellationSlice = if cancellationTotal > 0 then fst cancellationCommissionGross * refundedFor CANCELLATION_FEE (\c -> c.fareAmount + c.vatAmount) / cancellationTotal else 0
      bookingTax = RideTax.bookingRideTax booking transporterConfig
      (commissionSliceBase, commissionSliceVat) = RideTax.splitCommissionInclusive bookingTax commissionSlice
      (cancellationSliceBase, cancellationSliceVat) = splitByPostedTax cancellationCommissionGross cancellationSlice
  when (commissionSlice > 0 || cancellationSlice > 0) $ do
    let issuedToType' = if isJust ride.fleetOwnerId then FLEET_OWNER else DRIVER
        issuedToId' = maybe ride.driverId.getId (.getId) ride.fleetOwnerId
        mkNegLine gId desc dtype amt typ =
          if amt > 0
            then Just InvoiceI.InvoiceLineItem {description = desc, descriptionType = Just dtype, quantity = 1, unitPrice = negate amt, lineTotal = negate amt, isExternalCharge = False, groupId = Just gId, itemType = Just typ}
            else Nothing
        commissionRefundLines =
          catMaybes
            [ mkNegLine "g-commission-refund" "Commission Refund" InvoiceI.CommissionRefund commissionSliceBase InvoiceI.Fare,
              mkNegLine "g-commission-refund" "Commission VAT Refund" InvoiceI.CommissionRefundTax commissionSliceVat InvoiceI.Tax,
              mkNegLine "g-commission-refund-cancellation" "Cancellation Commission Refund" InvoiceI.CancellationCommissionRefund cancellationSliceBase InvoiceI.Fare,
              mkNegLine "g-commission-refund-cancellation" "Cancellation Commission VAT Refund" InvoiceI.CancellationCommissionRefundTax cancellationSliceVat InvoiceI.Tax
            ]
    -- Parent = the per-ride Commission invoice (keyed by bookingId; refund-commissions are keyed
    -- by refundRequestId, so they don't collide here); Nothing if none found.
    mbParentCommissionInvoice <-
      listToMaybe <$> QFinanceInvoiceExtra.findByReferenceIdWithOptions ride.bookingId.getId (Just BeckInvoice.Commission) Nothing [FInvoice.Draft, FInvoice.Issued, FInvoice.Paid] (Just 1) (Just 0)
    createRes <-
      InvoiceSvc.createInvoice
        InvoiceI.InvoiceInput
          { invoiceType = BeckInvoice.Commission,
            entityReferenceId = Nothing,
            referenceInvoiceNumber = (\inv -> inv.invoiceNumber) <$> mbParentCommissionInvoice,
            issuedToType = issuedToType',
            issuedToId = issuedToId',
            issuedToName = Nothing,
            issuedToAddress = Nothing,
            issuedByType = "BUYER",
            issuedById = ctx.merchantId,
            issuedByName = ctx.merchantName,
            issuedByAddress = ctx.issuedByAddress,
            supplierName = ctx.supplierName,
            supplierAddress = ctx.issuedByAddress,
            supplierGSTIN = ctx.supplierGSTIN,
            supplierTaxNo = Nothing,
            supplierId = ctx.supplierId,
            merchantGstin = ctx.merchantGstin,
            referenceId = Just req.refundRequestId,
            gstinOfParty = Nothing,
            panOfParty = Nothing,
            panType = Nothing,
            counterpartyId = issuedToId',
            tdsRateReason = Nothing,
            tanOfDeductee = Nothing,
            lineItems = commissionRefundLines,
            gstBreakdown = Nothing,
            currency = ctx.currency,
            dueAt = Nothing,
            periodStart = Nothing,
            periodEnd = Nothing,
            merchantId = ctx.merchantId,
            merchantOperatingCityId = ctx.merchantOpCityId,
            merchantShortId = fromMaybe ctx.merchantId ctx.merchantShortId,
            isVat = fromMaybe False booking.fareParams.isVatTaxType,
            issuedToTaxNo = Nothing,
            issuedByTaxNo = Nothing,
            paymentMode = Nothing
          }
        []
    case createRes of
      Left err -> logError $ "createCommissionRefundInvoice: failed for refundRequest " <> req.refundRequestId <> ": " <> show err
      Right newInv -> logInfo $ "createCommissionRefundInvoice: created negative Commission invoice " <> newInv.id.getId <> " (ride-fare slice " <> show commissionSlice <> ", cancellation slice " <> show cancellationSlice <> ") for refundRequest " <> req.refundRequestId
