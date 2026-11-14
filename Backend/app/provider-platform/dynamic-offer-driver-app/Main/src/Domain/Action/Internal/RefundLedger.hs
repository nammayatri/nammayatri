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
import Control.Applicative ((<|>))
import qualified Domain.Types.Booking as DBooking
import "beckn-spec" Domain.Types.Invoice (IssuedToType (..))
import qualified "beckn-spec" Domain.Types.Invoice as BeckInvoice
import qualified Domain.Types.Person as DP
import Domain.Types.Ride (Ride)
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
import SharedLogic.Finance.PostActions (runFinance)
import qualified SharedLogic.Finance.Wallet as Wallet
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

data RefundLedgerComponent = RefundLedgerComponent
  { component :: RefundFareComponent,
    fareAmount :: HighPrecMoney,
    taxAmount :: HighPrecMoney,
    refundShare :: HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RefundFareComponent = RIDE_FARE | TOLL | PARKING | CANCELLATION_FEE deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

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
    let isVatMarket = fromMaybe False booking.fareParams.isVatTaxType
    cancellationCommissionGross <- cancellationCommissionFromLedger isVatMarket ride req.refundRequestId
    driverCancellationCredit <- cancellationCreditFromLedger isVatMarket ride req.refundRequestId
    discountSubsidy <- discountSubsidyFromLedger ride
    rideCommission <- rideCommissionFromLedger isVatMarket ride
    let totals = RefundTotals {discountSubsidy, cancellationCredit = driverCancellationCredit}
    let deductFromDriver = fromMaybe (fromMaybe False transporterConfig.defaultRefundDeductFromDriver) req.deductFromDriver
        legs = refundLegs deductFromDriver rideCommission cancellationCommissionGross isVatMarket totals req
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
          createRefundInvoice ctx (customerRefundParty booking) req.refundRequestId (fromMaybe [] req.refundComponents)
          when deductFromDriver $ do
            createRefundInvoice ctx (driverRefundParty ride mbDriver) req.refundRequestId (map (driverCancellationShare totals) (fromMaybe [] req.refundComponents))
            createCommissionRefundInvoice ctx req totals ride booking rideCommission cancellationCommissionGross
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

cancellationCommissionFromLedger :: Bool -> Ride -> Text -> Flow (HighPrecMoney, HighPrecMoney)
cancellationCommissionFromLedger isVat ride refundRequestId = do
  base <- remaining Wallet.walletReferenceCancellationCommission Wallet.walletReferenceCancellationRefundCommission
  vat <- remaining (Wallet.walletReferenceCancellationCommissionTax (marketTaxRegime isVat)) (Wallet.walletReferenceCancellationRefundCommissionTax (marketTaxRegime isVat))
  pure (base + vat, vat)
  where
    remaining postedRef rebateRef = do
      posted <- entriesTotal <$> postedEntries ride [postedRef]
      rebated <- entriesTotal <$> priorRefundLegs ride refundRequestId rebateRef
      pure $ max 0 (posted - rebated)

splitByPostedTax :: (HighPrecMoney, HighPrecMoney) -> HighPrecMoney -> (HighPrecMoney, HighPrecMoney)
splitByPostedTax (postedGross, postedTax) slice =
  let sliceTax = if postedGross > 0 then slice * postedTax / postedGross else 0
   in (slice - sliceTax, sliceTax)

rideCommissionFromLedger :: Bool -> Ride -> Flow (HighPrecMoney, HighPrecMoney)
rideCommissionFromLedger isVat ride = do
  base <- entriesTotal <$> postedEntries ride [Wallet.walletReferenceCommission]
  vat <- entriesTotal <$> postedEntries ride [Wallet.walletReferenceCommissionTax (marketTaxRegime isVat)]
  pure (base, vat)

discountSubsidyFromLedger :: Ride -> Flow HighPrecMoney
discountSubsidyFromLedger ride = do
  entries <- postedEntries ride [Wallet.walletReferenceDiscounts]
  let passThroughAccounts = (.fromAccountId) <$> entries
  pure $ entriesTotal [e | e <- entries, e.toAccountId `notElem` passThroughAccounts]

cancellationCreditFromLedger :: Bool -> Ride -> Text -> Flow (HighPrecMoney, HighPrecMoney)
cancellationCreditFromLedger isVat ride refundRequestId = do
  base <- remaining [Wallet.walletReferenceCustomerCancellationCharges] Wallet.walletReferenceCancellationFeeRefund
  tax <- remaining [Wallet.walletReferenceCustomerCancellationTax (marketTaxRegime isVat)] (Wallet.walletReferenceCancellationFeeRefundTax (marketTaxRegime isVat))
  pure (base, tax)
  where
    remaining creditRefs refundRef = do
      credits <- postedEntries ride creditRefs
      refunds <- priorRefundLegs ride refundRequestId refundRef
      let creditedAccounts = (.toAccountId) <$> credits
      pure $ max 0 (entriesTotal credits - entriesTotal [e | e <- refunds, e.fromAccountId `elem` creditedAccounts])

postedEntries :: Ride -> [Text] -> Flow [LE.LedgerEntry]
postedEntries ride refs = do
  entries <- concat <$> mapM (`LedgerSvc.getEntriesByReference` ride.bookingId.getId) refs
  let reversedIds = mapMaybe (.reversalOf) entries
  pure [e | e <- entries, e.status == LE.SETTLED, isNothing e.reversalOf, e.id `notElem` reversedIds]

priorRefundLegs :: Ride -> Text -> Text -> Flow [LE.LedgerEntry]
priorRefundLegs ride refundRequestId ref =
  filter (\e -> e.status /= LE.VOIDED && e.entityReferenceId /= Just refundRequestId) <$> LedgerSvc.getEntriesByReference ref ride.bookingId.getId

entriesTotal :: [LE.LedgerEntry] -> HighPrecMoney
entriesTotal = sum . map (.amount)

marketTaxRegime :: Bool -> Tax.Regime
marketTaxRegime isVat = if isVat then Tax.VAT else Tax.GST

driverCancellationShare :: RefundTotals -> RefundLedgerComponent -> RefundLedgerComponent
driverCancellationShare totals comp
  | comp.component == CANCELLATION_FEE = comp {fareAmount = min comp.fareAmount (fst totals.cancellationCredit), taxAmount = min comp.taxAmount (snd totals.cancellationCredit)}
  | otherwise = comp

cancellationCommissionSlice :: RefundTotals -> HighPrecMoney -> RefundLedgerComponent -> HighPrecMoney
cancellationCommissionSlice totals commissionGross driverComp =
  let creditBase = fst totals.cancellationCredit
   in if creditBase > 0 then commissionGross * driverComp.fareAmount / creditBase else 0

refundLegs :: Bool -> (HighPrecMoney, HighPrecMoney) -> (HighPrecMoney, HighPrecMoney) -> Bool -> RefundTotals -> RefundLedgerReq -> [(AccountRole, AccountRole, HighPrecMoney, Text)]
refundLegs deductFromDriver rideCommission cancellationCommissionGross isVatMarket totals req =
  concatMap componentLegs (fromMaybe [] req.refundComponents)
  where
    -- Taxes return to where they sat forward: driver in VAT markets, government in GST.
    remittedTaxSource = if isVatMarket then OwnerLiability else GovtIndirect
    -- Full precision, no rounding: legs must sum to the component exactly and a full refund must
    -- claw back exactly the charged commission (the commission aggregate nets to zero).
    taxRegime = marketTaxRegime isVatMarket
    driverOnlyLegs taxSource base tax baseRef vatRef =
      [ (OwnerLiability, BuyerExternal, base, baseRef),
        (taxSource, BuyerExternal, tax, vatRef)
      ]
    platformLegs legs = [(source, BuyerExternal, amount, ref) | (source, amount, ref) <- legs, amount > 0]
    platformToDriverRebateLegs legs = [(SellerExpense, OwnerLiability, amount, ref) | (amount, ref) <- legs, amount > 0]
    rideFareLegs comp baseRef vatRef =
      let sliceBase = fst rideCommission * comp.refundShare
          sliceVat = snd rideCommission * comp.refundShare
       in driverOnlyLegs remittedTaxSource comp.fareAmount comp.taxAmount baseRef vatRef
            <> filter (\(_, _, amount, _) -> amount > 0) [(OwnerLiability, BuyerExternal, totals.discountSubsidy * comp.refundShare, Wallet.walletReferenceDiscountsRefund)]
            <> platformToDriverRebateLegs
              [ (sliceBase, Wallet.walletReferenceRideFareRefundCommission),
                (sliceVat, Wallet.walletReferenceRideFareRefundCommissionTax taxRegime)
              ]
    cancellationFeeLegs comp baseRef vatRef =
      let driverComp = driverCancellationShare totals comp
          (sliceBase, sliceVat) = splitByPostedTax cancellationCommissionGross (cancellationCommissionSlice totals (fst cancellationCommissionGross) driverComp)
       in filter (\(_, _, amount, _) -> amount > 0) (driverOnlyLegs remittedTaxSource driverComp.fareAmount driverComp.taxAmount baseRef vatRef)
            <> platformLegs [(SellerExpense, comp.fareAmount - driverComp.fareAmount, baseRef), (SellerExpense, comp.taxAmount - driverComp.taxAmount, vatRef)]
            <> platformToDriverRebateLegs
              [ (sliceBase, Wallet.walletReferenceCancellationRefundCommission),
                (sliceVat, Wallet.walletReferenceCancellationRefundCommissionTax taxRegime)
              ]
    componentLegs comp =
      let (baseRef, vatRef) = refundRefTypesForComponent isVatMarket comp.component
       in if not deductFromDriver
            then platformLegs [(SellerExpense, comp.fareAmount, baseRef), (SellerExpense, comp.taxAmount, vatRef)]
            else case comp.component of
              RIDE_FARE -> rideFareLegs comp baseRef vatRef
              CANCELLATION_FEE -> cancellationFeeLegs comp baseRef vatRef
              _ -> driverOnlyLegs OwnerLiability comp.fareAmount comp.taxAmount baseRef vatRef

data RefundTotals = RefundTotals
  { discountSubsidy :: HighPrecMoney,
    cancellationCredit :: (HighPrecMoney, HighPrecMoney)
  }

-- | (base refType, VAT refType) for a refunded component — exhaustive over the enum.
refundRefTypesForComponent :: Bool -> RefundFareComponent -> (Text, Text)
refundRefTypesForComponent isVat fc = case fc of
  RIDE_FARE -> (Wallet.walletReferenceRideFareRefund, Wallet.walletReferenceRideFareRefundTax taxRegime)
  TOLL -> (Wallet.walletReferenceTollRefund, Wallet.walletReferenceTollRefundTax taxRegime)
  PARKING -> (Wallet.walletReferenceParkingRefund, Wallet.walletReferenceParkingRefundTax taxRegime)
  CANCELLATION_FEE -> (Wallet.walletReferenceCancellationFeeRefund, Wallet.walletReferenceCancellationFeeRefundTax taxRegime)
  where
    taxRegime = marketTaxRegime isVat

-- | The legs of one refund request. Every entry here is a leg: BPP refund legs are real debits
--   against the driver or the platform, so nothing reverses them (the zero-out is BAP-only).
getRefundRequestLegs :: Text -> Flow [LE.LedgerEntry]
getRefundRequestLegs = LedgerSvc.getEntriesByEntityReference LE.RefundRequest

-- | One lock per refund request: APPROVED/REFUNDED/FAILED deliveries serialize.
refundLedgerLockKey :: Text -> Text
refundLedgerLockKey refundRequestId = "refundLedger:" <> refundRequestId

data RefundInvoiceParty = RefundInvoiceParty
  { issuedToType :: IssuedToType,
    issuedToId :: Text,
    issuedToName :: Maybe Text,
    issuedToAddress :: Maybe Text
  }

customerRefundParty :: DBooking.Booking -> RefundInvoiceParty
customerRefundParty booking =
  RefundInvoiceParty
    { issuedToType = CUSTOMER,
      issuedToId = maybe "" (.getId) booking.riderId,
      issuedToName = booking.riderName,
      issuedToAddress = booking.fromLocation.address.fullAddress
    }

driverRefundParty :: Ride -> Maybe DP.Person -> RefundInvoiceParty
driverRefundParty ride mbDriver =
  RefundInvoiceParty
    { issuedToType = if isJust ride.fleetOwnerId then FLEET_OWNER else DRIVER,
      issuedToId = maybe ride.driverId.getId (.getId) ride.fleetOwnerId,
      issuedToName = if isJust ride.fleetOwnerId then Nothing else (.firstName) <$> mbDriver,
      issuedToAddress = Nothing
    }

createRefundInvoice ::
  FinanceCtx ->
  RefundInvoiceParty ->
  Text ->
  [RefundLedgerComponent] ->
  Flow ()
createRefundInvoice ctx party refundsRequestId comps = do
  let referenceId = ctx.referenceId -- booking.id
      lineItems = concatMap mkRefundInvoiceLineItems comps
      anyVat = any (\c -> c.taxAmount > 0) comps
  mbParentRideInvoice <-
    listToMaybe
      <$> QFinanceInvoiceExtra.findByReferenceIdWithOptions referenceId (Just BeckInvoice.Ride) (Just party.issuedToType) [FInvoice.Draft, FInvoice.Issued, FInvoice.Paid] (Just 1) (Just 0)
  createRes <-
    InvoiceSvc.createInvoice
      InvoiceI.InvoiceInput
        { invoiceType = BeckInvoice.Refund,
          entityReferenceId = Just refundsRequestId,
          referenceInvoiceNumber = (\inv -> inv.invoiceNumber) <$> mbParentRideInvoice,
          issuedToType = party.issuedToType,
          issuedToId = party.issuedToId,
          issuedToName = party.issuedToName,
          issuedToAddress = party.issuedToAddress,
          issuedByType = "BUYER",
          issuedById = ctx.merchantId,
          issuedByName = ctx.merchantName,
          issuedByAddress = ctx.issuedByAddress,
          supplierName = ctx.supplierName,
          supplierAddress =
            (mbParentRideInvoice >>= (.supplierAddress))
              <|> (if anyVat then ctx.supplierAddress <|> ctx.issuedByAddress else ctx.supplierAddress),
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
    Left err -> logError $ "createRefundInvoice: failed to create BPP " <> show party.issuedToType <> " refund invoice for booking " <> referenceId <> ": " <> show err
    Right newInv -> do
      InvoiceSvc.updateInvoiceStatus newInv.id FInvoice.Paid
      logInfo $ "createRefundInvoice: created BPP " <> show party.issuedToType <> " refund invoice " <> newInv.id.getId <> " for booking " <> referenceId <> " refundRequest " <> refundsRequestId

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
   in catMaybes [mkNeg fareDesc fareType c.fareAmount InvoiceI.Fare, mkNeg taxDesc taxType c.taxAmount InvoiceI.Tax]

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
createCommissionRefundInvoice :: FinanceCtx -> RefundLedgerReq -> RefundTotals -> Ride -> DBooking.Booking -> (HighPrecMoney, HighPrecMoney) -> (HighPrecMoney, HighPrecMoney) -> Flow ()
createCommissionRefundInvoice ctx req totals ride booking rideCommission cancellationCommissionGross = do
  let refundedFor p f = maybe 0 (sum . map f . filter (\c -> c.component == p)) req.refundComponents
      rideFareShare = refundedFor RIDE_FARE (.refundShare)
      commissionSliceBase = fst rideCommission * rideFareShare
      commissionSliceVat = snd rideCommission * rideFareShare
      commissionSlice = commissionSliceBase + commissionSliceVat
      cancellationSlice = refundedFor CANCELLATION_FEE (cancellationCommissionSlice totals (fst cancellationCommissionGross) . driverCancellationShare totals)
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
            -- Mirror the parent commission invoice so both documents name the same supplier address.
            supplierAddress = (mbParentCommissionInvoice >>= (.supplierAddress)) <|> ctx.issuedByAddress,
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
