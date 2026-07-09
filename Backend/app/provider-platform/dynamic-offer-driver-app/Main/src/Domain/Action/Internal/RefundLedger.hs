{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.RefundLedger where

import qualified Data.Aeson as Aeson
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
import Lib.Finance (AccountRole (..), FinanceCtx, runFinance, transferPendingWithMetadata, transferWithMetadata)
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Finance.Invoice.Interface as InvoiceI
import qualified Lib.Finance.Invoice.Service as InvoiceSvc
import qualified Lib.Finance.Ledger.Service as LedgerSvc
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QFinanceInvoiceExtra
import qualified SharedLogic.Finance.Wallet as Wallet
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error

-- | Defined here because rider-app's RefundRequestStatus can't be cross-imported.
data RefundLedgerStatus = APPROVED | REFUNDED | FAILED
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

-- | Per-component refund breakdown (BAP-split). 'component' is the FareComponent enum
--   (Generic JSON: "RIDE_FARE"/"TOLL"/"PARKING"). Field names byte-identical to BAP.
data RefundLedgerComponent = RefundLedgerComponent
  { component :: RefundFareComponent,
    fareAmount :: HighPrecMoney,
    vatAmount :: HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Mirrors rider-app's Domain.Types.FareBreakup.FareComponent (can't be cross-imported) with
--   identical constructors, so the Generic JSON wire is byte-compatible and FromJSON rejects any
--   unknown tag at decode.
data RefundFareComponent = RIDE_FARE | TOLL | PARKING deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Field names must match the BAP-side request verbatim (Generic JSON wire).
data RefundLedgerReq = RefundLedgerReq
  { refundRequestId :: Text,
    refundsAmount :: HighPrecMoney,
    deductFromDriver :: Maybe Bool,
    refundRequestStatus :: RefundLedgerStatus,
    refundComponents :: Maybe [RefundLedgerComponent], -- per-component split (always present for APPROVED/REFUNDED; Nothing only on FAILED, which just voids)
    rideFareComponentTotal :: Maybe HighPrecMoney -- commission-slice denominator (BAP-supplied)
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
    transporterConfig <- SCTC.findByMerchantOpCityId ride.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
    mbDriver <- QPerson.findById ride.driverId
    mbDriverInfo <- QDI.findById (cast ride.driverId)
    -- online cab only; Case-2 clawback legs debit OwnerLiability (the driver wallet), reducing payout.
    ctx <- Wallet.buildFinanceCtx booking ride mbDriver Nothing mbDriverInfo transporterConfig True
    let referenceId = booking.id.getId
        -- When the approval doesn't specify who bears the refund, the merchant config decides;
        -- with no config either, the platform absorbs.
        deductFromDriver = fromMaybe (fromMaybe False transporterConfig.defaultRefundDeductFromDriver) req.deductFromDriver
        legs = refundLegs deductFromDriver transporterConfig.taxConfig.commissionVatPercentage req ride
    existing <- getRefundEntries referenceId
    case req.refundRequestStatus of
      APPROVED ->
        -- VOIDED legs are skipped so a retry re-raises fresh; ANY live leg de-dups a re-fire.
        unless (any (\e -> e.status /= LE.VOIDED && entryMatchesRefundRequest req.refundRequestId e) existing) $
          postLegs ctx Pending (mkRefundMetadata req.refundRequestId) legs req.refundRequestId
      REFUNDED -> do
        -- alreadyDone = this refund's live legs exist and are ALL settled (skip the invoice).
        -- Strictly none-PENDING, so a crash mid-settle still completes invoices on redelivery.
        let idLegs = filter (\e -> e.status /= LE.VOIDED && entryMatchesRefundRequest req.refundRequestId e) existing
            alreadyDone = not (null idLegs) && not (any (\e -> e.status == LE.PENDING) idLegs)
        if not (null idLegs)
          then forM_ (filter (\e -> e.status == LE.PENDING) idLegs) $ \e -> LedgerSvc.settleEntry e.id
          else -- APPROVED was missed; post directly as SETTLED (reachable only with no live legs — fires at most once).
            postLegs ctx Settled (mkRefundMetadata req.refundRequestId) legs req.refundRequestId
        -- the fleet/driver-visible refund invoice + (Case-2) the negative Commission invoice, once per refund
        unless alreadyDone $ do
          createRefundInvoice ctx booking req.refundRequestId (fromMaybe [] req.refundComponents)
          when deductFromDriver $ createCommissionRefundInvoice ctx req ride booking transporterConfig
      FAILED ->
        -- Failed: void the pending APPROVED leg(s). Only PENDING → idempotent.
        forM_ (filter (\e -> e.status == LE.PENDING && entryMatchesRefundRequest req.refundRequestId e) existing) $ \e ->
          LedgerSvc.voidEntry e.id "RefundFailed"
  pure Success

data PostMode = Pending | Settled

-- | Post the legs in one finance transaction; throws on failure so the caller redelivers.
postLegs :: FinanceCtx -> PostMode -> Aeson.Value -> [(AccountRole, AccountRole, HighPrecMoney, Text)] -> Text -> Flow ()
postLegs ctx mode meta legs refundRequestId = do
  res <- runFinance ctx $
    forM_ legs $ \(fromRole, toRole, amount, refType) ->
      void $ case mode of
        Pending -> transferPendingWithMetadata fromRole toRole amount refType (Just meta)
        Settled -> transferWithMetadata fromRole toRole amount refType (Just meta)
  case res of
    Left err -> throwError $ InternalError $ "Refund ledger write failed for " <> refundRequestId <> ": " <> show err
    Right _ -> pure ()

-- | Per-component refund legs. Case 1 (platform absorbs) posts every component on
--   SellerExpense→BuyerExternal. Case 2 (clawback) posts driver legs on OwnerLiability→BuyerExternal
--   (reducing the next payout) plus, for ride-fare only, a SellerExpense→BuyerExternal commission
--   slice the platform bears (base + ALV split when commissionVatPercentage is set; the driver
--   clawback uses the GROSS slice, so payout is rate-independent); toll/parking are 100% driver.
refundLegs :: Bool -> Maybe Double -> RefundLedgerReq -> Ride -> [(AccountRole, AccountRole, HighPrecMoney, Text)]
refundLegs deductFromDriver mbCommissionVatPct req ride =
  concatMap componentLegs (fromMaybe [] req.refundComponents)
  where
    commission = fromMaybe 0 ride.commission
    rideFareTotal = fromMaybe 0 req.rideFareComponentTotal
    componentLegs comp =
      let (baseRef, vatRef) = refundRefTypesForComponent comp.component
       in if not deductFromDriver
            then -- Case 1: platform absorbs the whole component

              [ (SellerExpense, BuyerExternal, comp.fareAmount, baseRef),
                (SellerExpense, BuyerExternal, comp.vatAmount, vatRef)
              ]
            else
              if comp.component == RIDE_FARE
                then -- Case 2 ride-fare: driver bears all but the prorated commission slice

                -- Full precision, no rounding: legs must sum to the component exactly, and a
                -- full refund must claw back exactly the forward commission (agg nets to zero).
                -- Rounding the complement over-debits the driver by the rounding residue.

                  let commissionSlice =
                        if rideFareTotal > 0
                          then commission * comp.fareAmount / rideFareTotal
                          else 0
                      driverBase = comp.fareAmount - commissionSlice
                      (commissionSliceBase, commissionSliceVat) = Wallet.splitGrossByVatPct mbCommissionVatPct commissionSlice
                   in [ (OwnerLiability, BuyerExternal, driverBase, baseRef),
                        (OwnerLiability, BuyerExternal, comp.vatAmount, vatRef),
                        (SellerExpense, BuyerExternal, commissionSliceBase, Wallet.walletReferenceRideFareRefundCommission)
                      ]
                        <> [(SellerExpense, BuyerExternal, commissionSliceVat, Wallet.walletReferenceRideFareRefundCommissionVAT) | commissionSliceVat > 0]
                else -- Case 2 toll/parking: 100% driver

                  [ (OwnerLiability, BuyerExternal, comp.fareAmount, baseRef),
                    (OwnerLiability, BuyerExternal, comp.vatAmount, vatRef)
                  ]

-- | (base refType, VAT refType) for a refunded component — exhaustive over the enum.
refundRefTypesForComponent :: RefundFareComponent -> (Text, Text)
refundRefTypesForComponent fc = case fc of
  RIDE_FARE -> (Wallet.walletReferenceRideFareRefund, Wallet.walletReferenceRideFareRefundVAT)
  TOLL -> (Wallet.walletReferenceTollRefund, Wallet.walletReferenceTollRefundVAT)
  PARKING -> (Wallet.walletReferenceParkingRefund, Wallet.walletReferenceParkingRefundVAT)

-- | All refund ledger entries for a booking, across the 8 refund refTypes.
getRefundEntries :: Text -> Flow [LE.LedgerEntry]
getRefundEntries referenceId =
  concat
    <$> mapM
      (`LedgerSvc.getEntriesByReference` referenceId)
      [ Wallet.walletReferenceRideFareRefund,
        Wallet.walletReferenceRideFareRefundVAT,
        Wallet.walletReferenceTollRefund,
        Wallet.walletReferenceTollRefundVAT,
        Wallet.walletReferenceParkingRefund,
        Wallet.walletReferenceParkingRefundVAT,
        Wallet.walletReferenceRideFareRefundCommission,
        Wallet.walletReferenceRideFareRefundCommissionVAT
      ]

-- | Links a ledger leg to its refund request. Legs carry only the id; request state lives
--   solely in refund_request, and checks pair the id with the leg's own status column.
newtype RefundLedgerMetadata = RefundLedgerMetadata {refundRequestId :: Text}
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

mkRefundMetadata :: Text -> Aeson.Value
mkRefundMetadata refundRequestId = Aeson.toJSON (RefundLedgerMetadata refundRequestId)

entryMatchesRefundRequest :: Text -> LE.LedgerEntry -> Bool
entryMatchesRefundRequest refundRequestId entry =
  (metadataRefundRequestId =<< entry.metadata) == Just refundRequestId
  where
    metadataRefundRequestId v = case Aeson.fromJSON v of
      Aeson.Success (RefundLedgerMetadata rid) -> Just rid
      _ -> Nothing

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
    listToMaybe . filter (\inv -> inv.issuedToType == CUSTOMER)
      <$> QFinanceInvoiceExtra.findByReferenceIdWithOptions referenceId (Just BeckInvoice.Ride) [FInvoice.Draft, FInvoice.Issued, FInvoice.Paid] (Just 10) (Just 0)
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
--   Toll/Parking are flagged external (mirrors the ride invoice); only Ride Fare is taxable.
mkRefundInvoiceLineItems :: RefundLedgerComponent -> [InvoiceI.InvoiceLineItem]
mkRefundInvoiceLineItems c =
  let (fareDesc, fareType, taxDesc, taxType, gId) = refundInvoiceLineMeta c.component
      isExt = case c.component of RIDE_FARE -> False; _ -> True
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

-- | On a Case-2 ride-fare refund, record the returned commission slice as a NEGATIVE Commission
--   invoice, left Draft so the agg-commission scheduler nets it in. A record, not a second cash
--   movement (the cash is the RideFareRefundCommission ledger leg); issuedTo mirrors the per-ride
--   Commission invoice so it aggregates against the same recipient. Mirrors the forward
--   base + ALV split as a negative Fare + Tax pair.
createCommissionRefundInvoice :: FinanceCtx -> RefundLedgerReq -> Ride -> DBooking.Booking -> DTC.TransporterConfig -> Flow ()
createCommissionRefundInvoice ctx req ride booking transporterConfig = do
  let commission = fromMaybe 0 ride.commission
      rideFareTotal = fromMaybe 0 req.rideFareComponentTotal
      rideFareRefundedFare = maybe 0 (sum . map (.fareAmount) . filter (\c -> c.component == RIDE_FARE)) req.refundComponents
      -- Full precision (same operands as refundLegs' slice — the ledger legs and this
      -- invoice must agree to the last digit).
      commissionSlice = if rideFareTotal > 0 then commission * rideFareRefundedFare / rideFareTotal else 0
      (commissionSliceBase, commissionSliceVat) = Wallet.splitGrossByVatPct transporterConfig.taxConfig.commissionVatPercentage commissionSlice
  when (commissionSlice > 0) $ do
    let issuedToType' = if isJust ride.fleetOwnerId then FLEET_OWNER else DRIVER
        issuedToId' = maybe ride.driverId.getId (.getId) ride.fleetOwnerId
        mkNegLine desc dtype amt typ =
          if amt > 0
            then Just InvoiceI.InvoiceLineItem {description = desc, descriptionType = Just dtype, quantity = 1, unitPrice = negate amt, lineTotal = negate amt, isExternalCharge = False, groupId = Just "g-commission-refund", itemType = Just typ}
            else Nothing
        commissionRefundLines =
          catMaybes
            [ mkNegLine "Commission Refund" InvoiceI.CommissionRefund commissionSliceBase InvoiceI.Fare,
              mkNegLine "Commission VAT Refund" InvoiceI.CommissionRefundTax commissionSliceVat InvoiceI.Tax
            ]
    -- Parent = the per-ride Commission invoice (keyed by bookingId; refund-commissions are keyed
    -- by refundRequestId, so they don't collide here); Nothing if none found.
    mbParentCommissionInvoice <-
      listToMaybe <$> QFinanceInvoiceExtra.findByReferenceIdWithOptions ride.bookingId.getId (Just BeckInvoice.Commission) [FInvoice.Draft, FInvoice.Issued, FInvoice.Paid] (Just 10) (Just 0)
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
      Right newInv -> logInfo $ "createCommissionRefundInvoice: created negative Commission invoice " <> newInv.id.getId <> " (slice " <> show commissionSlice <> ") for refundRequest " <> req.refundRequestId
