{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.RefundLedger where

import qualified Domain.Types.Booking as DBooking
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
import Lib.Finance (AccountRole (..), FinanceCtx (..), runFinance, transfer, transferPending)
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Finance.Invoice.Interface as InvoiceI
import qualified Lib.Finance.Invoice.Service as InvoiceSvc
import qualified Lib.Finance.Ledger.Service as LedgerSvc
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QFinanceInvoiceExtra
import qualified SharedLogic.Finance.Wallet as Wallet
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as QM
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error

-- | Defined here because rider-app's RefundRequestStatus can't be cross-imported.
data RefundLedgerStatus = APPROVED | REFUNDED | FAILED
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

-- | Per-component refund breakdown (BAP-split). 'component' is the FareComponent enum
--   (Generic JSON: "RIDE_FARE"/"TOLL"/"PARKING"/"CANCELLATION_FEE"). Field names byte-identical to BAP.
data RefundLedgerComponent = RefundLedgerComponent
  { component :: RefundFareComponent,
    fareAmount :: HighPrecMoney,
    vatAmount :: HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Mirrors rider-app's Domain.Types.FareBreakup.FareComponent (can't be cross-imported) with
--   identical constructors, so the Generic JSON wire is byte-compatible and FromJSON rejects any
--   unknown tag at decode.
data RefundFareComponent = RIDE_FARE | TOLL | PARKING | CANCELLATION_FEE deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Field names must match the BAP-side request verbatim (Generic JSON wire).
data RefundLedgerReq = RefundLedgerReq
  { refundRequestId :: Text,
    refundsAmount :: HighPrecMoney,
    deductFromDriver :: Maybe Bool,
    refundRequestStatus :: RefundLedgerStatus,
    refundComponents :: Maybe [RefundLedgerComponent], -- per-component split (always present for APPROVED/REFUNDED; Nothing only on FAILED, which just voids)
    rideFareComponentTotal :: Maybe HighPrecMoney, -- ride-fare commission-slice denominator (BAP-supplied)
    cancellationComponentTotal :: Maybe HighPrecMoney -- cancellation-fee commission-slice denominator (BAP-supplied): fee base+tax
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
  Redis.withLockRedis (refundLedgerLockKey req.refundRequestId) 60 $ do
    transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = ride.merchantOperatingCityId.getId}) (Just (SCTC.findByMerchantOpCityId ride.merchantOperatingCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
    mbDriver <- QPerson.findById ride.driverId
    mbDriverInfo <- QDI.findById (cast ride.driverId)
    -- online cab only; driver-deducted clawback legs debit OwnerLiability (the driver wallet), reducing payout.
    ctx <- Wallet.buildFinanceCtx booking ride mbDriver Nothing mbDriverInfo transporterConfig True
    -- When the approval doesn't specify who bears the refund, the merchant config decides;
    -- with no config either, the platform absorbs.
    cancellationCommissionGross <- cancellationCommissionFromLedger ride
    overdueBenefit <- cancellationOverdueBenefitFromLedger ride
    let deductFromDriver = fromMaybe (fromMaybe False transporterConfig.defaultRefundDeductFromDriver) req.deductFromDriver
        isVatMarket = fromMaybe False booking.fareParams.isVatTaxType
        legs = refundLegs deductFromDriver transporterConfig.taxConfig.commissionVatPercentage cancellationCommissionGross overdueBenefit isVatMarket req ride
    existing <- getRefundRequestLegs req.refundRequestId
    case req.refundRequestStatus of
      APPROVED ->
        -- VOIDED legs are skipped so a retry re-raises fresh; ANY live leg de-dups a re-fire.
        unless (any (\e -> e.status /= LE.VOIDED) existing) $
          postLegs ctx Pending legs req.refundRequestId
      REFUNDED -> do
        -- alreadyDone = this refund's live legs exist and are ALL settled (skip the invoice).
        -- Strictly none-PENDING, so a crash mid-settle still completes invoices on redelivery.
        let idLegs = filter (\e -> e.status /= LE.VOIDED) existing
            alreadyDone = not (null idLegs) && not (any (\e -> e.status == LE.PENDING) idLegs)
        if not (null idLegs)
          then forM_ (filter (\e -> e.status == LE.PENDING) idLegs) $ \e -> LedgerSvc.settleEntry e.id
          else -- APPROVED was missed; post directly as SETTLED (reachable only with no live legs — fires at most once).
            postLegs ctx Settled legs req.refundRequestId
        -- the fleet/driver-visible refund invoice + (if driver-deducted) the negative Commission invoice, once per refund
        unless alreadyDone $ do
          createRefundInvoice ctx booking req.refundRequestId (fromMaybe [] req.refundComponents)
          when deductFromDriver $ createCommissionRefundInvoice ctx req ride booking transporterConfig cancellationCommissionGross
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
        Settled -> transfer fromRole toRole amount refType
  case res of
    Left err -> throwError $ InternalError $ "Refund ledger write failed for " <> refundRequestId <> ": " <> show err
    Right _ -> pure ()

-- | The cancellation commission (incl. VAT) actually charged for this ride's booking: the sum of
--   its SETTLED CancellationCommission(+VAT) ledger legs. Nothing ever reverses these legs
--   (refund clawbacks post their own separate legs), so no reversal filtering is needed. The
--   ledger is the source of truth here — the cancellation_dues_details table stores two candidate
--   amounts (normal and overdue) without recording which one was actually charged.
cancellationCommissionFromLedger :: Ride -> Flow HighPrecMoney
cancellationCommissionFromLedger ride = do
  baseLegs <- LedgerSvc.getEntriesByReference Wallet.walletReferenceCancellationCommission ride.bookingId.getId
  vatLegs <- LedgerSvc.getEntriesByReference Wallet.walletReferenceCancellationCommissionVAT ride.bookingId.getId
  let settledGross es = sum [e.amount | e <- es, e.status == LE.SETTLED]
  pure (settledGross baseLegs + settledGross vatLegs)

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
--   from SellerExpense. Driver-deducted: each party gives back a prorated share of what it actually
--   received — the driver his net share, the platform its commission slice and (if the fee went
--   overdue) its kept benefit; toll/parking are fully driver-borne.
refundLegs :: Bool -> Maybe Double -> HighPrecMoney -> (HighPrecMoney, HighPrecMoney) -> Bool -> RefundLedgerReq -> Ride -> [(AccountRole, AccountRole, HighPrecMoney, Text)]
refundLegs deductFromDriver mbCommissionVatPct cancellationCommissionGross (overdueBenefit, overdueBenefitTax) isVatMarket req ride =
  concatMap componentLegs (fromMaybe [] req.refundComponents)
  where
    commission = fromMaybe 0 ride.commission
    rideFareTotal = fromMaybe 0 req.rideFareComponentTotal
    cancellationTotal = fromMaybe 0 req.cancellationComponentTotal
    -- Taxes return to where they sat forward: driver in VAT markets, government in GST.
    cancelTaxDest = if isVatMarket then OwnerLiability else GovtIndirect
    benefitTaxSource = if isVatMarket then SellerExpense else GovtIndirect
    -- Full precision, no rounding: legs must sum to the component exactly and a full refund must
    -- claw back exactly the charged commission (the commission aggregate nets to zero).
    rideFareLegs comp baseRef vatRef =
      -- ride-fare prorates by BASE (base/base); cancellation below prorates by GROSS (gross/gross)
      let slice = if rideFareTotal > 0 then commission * comp.fareAmount / rideFareTotal else 0
          (sliceBase, sliceVat) = Wallet.splitGrossByVatPct mbCommissionVatPct slice
       in [ (OwnerLiability, BuyerExternal, comp.fareAmount - slice, baseRef),
            (OwnerLiability, BuyerExternal, comp.vatAmount, vatRef),
            (SellerExpense, BuyerExternal, sliceBase, Wallet.walletReferenceRideFareRefundCommission)
          ]
            <> [(SellerExpense, BuyerExternal, sliceVat, Wallet.walletReferenceRideFareRefundCommissionVAT) | sliceVat > 0]
    -- Three buckets funded the fee (driver net share, charged commission, overdue benefit); each
    -- returns the same fraction of the refunded gross, so a full refund zeroes every bucket.
    cancellationFeeLegs comp baseRef vatRef =
      let prorate amt = if cancellationTotal > 0 then amt * (comp.fareAmount + comp.vatAmount) / cancellationTotal else 0
          commissionSlice = prorate cancellationCommissionGross
          benefitSlice = prorate overdueBenefit
          benefitTaxSlice = prorate overdueBenefitTax
          (sliceBase, sliceVat) = Wallet.splitGrossByVatPct mbCommissionVatPct commissionSlice
       in [ (OwnerLiability, BuyerExternal, comp.fareAmount - benefitSlice - commissionSlice, baseRef),
            (cancelTaxDest, BuyerExternal, comp.vatAmount - benefitTaxSlice, vatRef)
          ]
            <> [(SellerExpense, BuyerExternal, sliceBase, Wallet.walletReferenceCancellationRefundCommission) | sliceBase > 0]
            <> [(SellerExpense, BuyerExternal, sliceVat, Wallet.walletReferenceCancellationRefundCommissionVAT) | sliceVat > 0]
            <> [(SellerExpense, BuyerExternal, benefitSlice, Wallet.walletReferenceCancellationOverdueBenefitRefund) | benefitSlice > 0]
            <> [(benefitTaxSource, BuyerExternal, benefitTaxSlice, Wallet.walletReferenceCancellationOverdueBenefitRefundTax) | benefitTaxSlice > 0]
    driverOnlyLegs comp baseRef vatRef =
      [ (OwnerLiability, BuyerExternal, comp.fareAmount, baseRef),
        (OwnerLiability, BuyerExternal, comp.vatAmount, vatRef)
      ]
    componentLegs comp =
      let (baseRef, vatRef) = refundRefTypesForComponent comp.component
       in if not deductFromDriver
            then -- platform absorbs the whole component

              [ (SellerExpense, BuyerExternal, comp.fareAmount, baseRef),
                (SellerExpense, BuyerExternal, comp.vatAmount, vatRef)
              ]
            else case comp.component of
              RIDE_FARE -> rideFareLegs comp baseRef vatRef
              CANCELLATION_FEE -> cancellationFeeLegs comp baseRef vatRef
              TOLL -> driverOnlyLegs comp baseRef vatRef
              PARKING -> driverOnlyLegs comp baseRef vatRef

-- | (base refType, VAT refType) for a refunded component — exhaustive over the enum.
refundRefTypesForComponent :: RefundFareComponent -> (Text, Text)
refundRefTypesForComponent fc = case fc of
  RIDE_FARE -> (Wallet.walletReferenceRideFareRefund, Wallet.walletReferenceRideFareRefundVAT)
  TOLL -> (Wallet.walletReferenceTollRefund, Wallet.walletReferenceTollRefundVAT)
  PARKING -> (Wallet.walletReferenceParkingRefund, Wallet.walletReferenceParkingRefundVAT)
  CANCELLATION_FEE -> (Wallet.walletReferenceCancellationFeeRefund, Wallet.walletReferenceCancellationFeeRefundVAT)

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
      isExt = case c.component of RIDE_FARE -> False; CANCELLATION_FEE -> False; _ -> True
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
createCommissionRefundInvoice :: FinanceCtx -> RefundLedgerReq -> Ride -> DBooking.Booking -> DTC.TransporterConfig -> HighPrecMoney -> Flow ()
createCommissionRefundInvoice ctx req ride booking transporterConfig cancellationCommissionGross = do
  let commission = fromMaybe 0 ride.commission
      rideFareTotal = fromMaybe 0 req.rideFareComponentTotal
      cancellationTotal = fromMaybe 0 req.cancellationComponentTotal
      refundedFor p f = maybe 0 (sum . map f . filter (\c -> c.component == p)) req.refundComponents
      commissionSlice = if rideFareTotal > 0 then commission * refundedFor RIDE_FARE (.fareAmount) / rideFareTotal else 0
      cancellationSlice = if cancellationTotal > 0 then cancellationCommissionGross * refundedFor CANCELLATION_FEE (\c -> c.fareAmount + c.vatAmount) / cancellationTotal else 0
      splitPct = Wallet.splitGrossByVatPct transporterConfig.taxConfig.commissionVatPercentage
      (commissionSliceBase, commissionSliceVat) = splitPct commissionSlice
      (cancellationSliceBase, cancellationSliceVat) = splitPct cancellationSlice
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
