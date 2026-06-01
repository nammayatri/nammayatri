{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.RefundLedger where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Domain.Types.Booking as DBooking
import "beckn-spec" Domain.Types.Invoice (IssuedToType (..))
import qualified "beckn-spec" Domain.Types.Invoice as BeckInvoice
import Domain.Types.Ride (Ride)
import Environment
import Kernel.Beam.Functions (runInReplica)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance (AccountRole (..), FinanceCtx, roundAmount, runFinance, transferPendingWithMetadata, transferWithMetadata)
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
data RefundLedgerStatus = APPROVED | REFUNDED
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

-- | Field names must match the BAP-side request verbatim (Generic JSON wire).
data RefundLedgerReq = RefundLedgerReq
  { refundRequestId :: Text,
    refundsAmount :: HighPrecMoney,
    transactionAmount :: HighPrecMoney, -- original charge; the clawback-split denominator
    deductFromDriver :: Maybe Bool,
    refundRequestStatus :: RefundLedgerStatus
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | APPROVED writes the PENDING leg(s); REFUNDED settles them (or writes fresh
--   SETTLED if missing). Idempotent via per-transition lock + metadata dedup; a
--   ledger-write failure throws so the caller redelivers.
refundLedger :: Id Ride -> RefundLedgerReq -> Maybe Text -> Flow APISuccess
refundLedger rideId req apiKey = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let merchantId = fromMaybe booking.providerId ride.merchantId
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  Redis.withLockRedis (refundLedgerLockKey req.refundRequestId req.refundRequestStatus) 60 $ do
    transporterConfig <- SCTC.findByMerchantOpCityId ride.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
    mbDriver <- QPerson.findById ride.driverId
    mbDriverInfo <- QDI.findById (cast ride.driverId)
    -- online cab only; the clawback leg reverses OwnerLiability, credited only on online rides
    ctx <- Wallet.buildFinanceCtx booking ride mbDriver Nothing mbDriverInfo transporterConfig True
    let referenceId = booking.id.getId
        deductFromDriver = fromMaybe True req.deductFromDriver -- default: clawback
        legs = refundLegs deductFromDriver req ride
    existing <- getRefundEntries referenceId
    case req.refundRequestStatus of
      APPROVED ->
        unless (any (entryMatchesRefundStatus req.refundRequestId APPROVED) existing) $
          postLegs ctx Pending (mkRefundMetadata req.refundRequestId APPROVED) legs req.refundRequestId
      REFUNDED -> do
        -- alreadyDone is True when there's already evidence of a prior successful REFUNDED processing for this refund
        -- either via a REFUNDED-tagged entry (recovery path was done) or via APPROVED-tagged entries that are no longer PENDING (normal path was done).
        -- On a genuine first run, neither is true, so the invoice gets written. On any re-fire, one is true, so the invoice is skipped.
        let approvedEntries = filter (entryMatchesRefundStatus req.refundRequestId APPROVED) existing
            alreadyDone =
              any (entryMatchesRefundStatus req.refundRequestId REFUNDED) existing
                || (not (null approvedEntries) && not (any (\e -> e.status == LE.PENDING) approvedEntries))
        if not (null approvedEntries)
          then forM_ (filter (\e -> e.status == LE.PENDING) approvedEntries) $ \e -> LedgerSvc.settleEntry e.id
          else
            unless (any (entryMatchesRefundStatus req.refundRequestId REFUNDED) existing) $
              postLegs ctx Settled (mkRefundMetadata req.refundRequestId REFUNDED) legs req.refundRequestId
        -- regenerate the fleet/driver-visible cumulative refund invoice, once per refund
        unless alreadyDone $ regenerateRefundInvoice ctx booking req.refundsAmount
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

-- | Case 1 (platform absorbs) = one leg; Case 2 (clawback) = driver + commission shares.
--   driverShare derived by subtraction so the two shares sum exactly to the refund.
refundLegs :: Bool -> RefundLedgerReq -> Ride -> [(AccountRole, AccountRole, HighPrecMoney, Text)]
refundLegs deductFromDriver req ride
  | deductFromDriver =
    let commission = fromMaybe 0 ride.commission
        platformShare =
          if req.transactionAmount > 0
            then roundAmount (commission * req.refundsAmount / req.transactionAmount)
            else 0
        driverShare = roundAmount (req.refundsAmount - platformShare)
     in [ (OwnerLiability, BuyerExternal, driverShare, Wallet.walletReferenceRideRefundDriverShare),
          (SellerRevenue, BuyerExternal, platformShare, Wallet.walletReferenceRideRefundCommissionShare)
        ]
  | otherwise = [(SellerExpense, BuyerExternal, req.refundsAmount, Wallet.walletReferenceRideRefund)]

-- | All refund ledger entries for a booking, across the single + the two split refTypes.
getRefundEntries :: Text -> Flow [LE.LedgerEntry]
getRefundEntries referenceId =
  concat
    <$> mapM
      (`LedgerSvc.getEntriesByReference` referenceId)
      [ Wallet.walletReferenceRideRefund,
        Wallet.walletReferenceRideRefundDriverShare,
        Wallet.walletReferenceRideRefundCommissionShare
      ]

mkRefundMetadata :: Text -> RefundLedgerStatus -> Aeson.Value
mkRefundMetadata refundRequestId status =
  Aeson.object ["refundRequestId" .= refundRequestId, "refundRequestStatus" .= status]

entryMatchesRefundStatus :: Text -> RefundLedgerStatus -> LE.LedgerEntry -> Bool
entryMatchesRefundStatus refundRequestId status entry =
  entry.metadata == Just (mkRefundMetadata refundRequestId status)

refundLedgerLockKey :: Text -> RefundLedgerStatus -> Text
refundLedgerLockKey refundRequestId status = "refundLedger:" <> refundRequestId <> ":" <> show status

-- | Running-cumulative or first-time BPP refund invoice — the fleet/driver-visible
--   copy (the rider's copy is the BAP one). Reads the latest non-voided Refund
--   invoice for the booking, adds this refund to the running total, mints a new
--   single-line Paid invoice, voids the prior. issuedTo follows the ride's
--   CUSTOMER copy (issued to the rider, mirroring the customer ride invoice) so it
--   surfaces on the dashboard Refunds tab. Header from the caller's ctx; no linked entries
--   (no tax). Caller gates against re-fires.
regenerateRefundInvoice ::
  FinanceCtx ->
  DBooking.Booking ->
  HighPrecMoney -> -- this refund's GROSS amount (rider-POV; NOT the ledger split)
  Flow ()
regenerateRefundInvoice ctx booking refundAmount = do
  let referenceId = ctx.referenceId -- booking.id
      activeStatuses = [FInvoice.Draft, FInvoice.Issued, FInvoice.Paid]
  mbPriorRefund <- listToMaybe <$> QFinanceInvoiceExtra.findByReferenceIdWithOptions referenceId (Just BeckInvoice.Refund) activeStatuses (Just 1) (Just 0)
  let priorCumulative = maybe 0 (.totalAmount) mbPriorRefund
      newCumulative = priorCumulative + refundAmount
      refundLineItem =
        InvoiceI.InvoiceLineItem
          { description = "Refund",
            descriptionType = Just InvoiceI.RideRefund,
            quantity = 1,
            unitPrice = newCumulative,
            lineTotal = newCumulative,
            isExternalCharge = False,
            groupId = Just "g-refund",
            itemType = Just InvoiceI.Fare
          }
  createRes <-
    InvoiceSvc.createInvoice
      InvoiceI.InvoiceInput
        { invoiceType = BeckInvoice.Refund,
          paymentOrderId = Nothing,
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
          lineItems = [refundLineItem],
          gstBreakdown = Nothing,
          currency = ctx.currency,
          dueAt = Nothing,
          periodStart = Nothing,
          periodEnd = Nothing,
          merchantId = ctx.merchantId,
          merchantOperatingCityId = ctx.merchantOpCityId,
          merchantShortId = fromMaybe ctx.merchantId ctx.merchantShortId,
          isVat = False,
          issuedToTaxNo = Nothing,
          issuedByTaxNo = Nothing,
          paymentMode = Nothing
        }
      []
  case createRes of
    Left err -> logError $ "regenerateRefundInvoice: failed to create BPP refund invoice for booking " <> referenceId <> ": " <> show err
    Right newInv -> do
      whenJust mbPriorRefund $ \priorRefund -> InvoiceSvc.updateInvoiceStatus priorRefund.id FInvoice.Voided
      InvoiceSvc.updateInvoiceStatus newInv.id FInvoice.Paid
      logInfo $ "regenerateRefundInvoice: created BPP cumulative refund invoice " <> newInv.id.getId <> " (total " <> show newCumulative <> ") for booking " <> referenceId
