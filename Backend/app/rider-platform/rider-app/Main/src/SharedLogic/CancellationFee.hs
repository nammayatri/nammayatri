{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CancellationFee
  ( settleCancellationFeeViaStripe,
  )
where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.Finance.RidePayment as RidePaymentFinance
import SharedLogic.Payment (MakePaymentIntentConstraints)
import qualified SharedLogic.Payment as SPayment
import qualified Storage.Queries.Ride as QRide
import Tools.Error

-- | Charge a customer's cancellation fee via Stripe and reconcile the finance ledger.
--   Shared by the immediate-charge path (Beckn.Common.cancellationTransaction, Card)
--   and the deferred CancelExecutePaymentIntent job so both behave identically:
--     1. cancel the existing ride payment intent (voids unsettled ride-fare entries)
--     2. create the pending cancellation-fee ledger (base + tax)
--     3. create a NEW payment intent for the cancellation fee and charge it
--     4. on capture: mark the cancellation invoice paid, mark ride payment Completed,
--        and sync SettleCancellationLedger to the BPP
--        on failure: mark the pending entries DUE and sync OverdueCancellationLedger
settleCancellationFeeViaStripe ::
  (MakePaymentIntentConstraints m r c, EsqDBReplicaFlow m r) =>
  DRB.Booking ->
  DRide.Ride ->
  DPerson.Person ->
  HighPrecMoney ->
  HighPrecMoney ->
  Currency ->
  (CallBPPInternal.CancellationLedgerAction -> m ()) ->
  m ()
settleCancellationFeeViaStripe booking ride personD cancellationBase cancellationTax currency syncCancellationLedger = do
  let pickupAddress = listToMaybe $ catMaybes [booking.fromLocation.address.area, booking.fromLocation.address.street, booking.fromLocation.address.city]
      ledgerCtx = RidePaymentFinance.applyBookingProviderFieldsToCtx booking $ RidePaymentFinance.buildRiderFinanceCtx booking.merchantId.getId booking.merchantOperatingCityId.getId currency True booking.riderId.getId ride.id.getId Nothing Nothing pickupAddress
      cancellationTotal = cancellationBase + cancellationTax
  (customerPaymentId, paymentMethodId) <- SPayment.getCustomerAndPaymentMethod booking personD
  driverAccountId <- ride.driverAccountId & fromMaybeM (RideFieldNotPresent "driverAccountId")
  let createPaymentIntentServiceReq =
        DPayment.CreatePaymentIntentServiceReq
          { amount = cancellationTotal,
            applicationFeeAmount = 0,
            discountAmount = 0,
            offerId = Nothing,
            currency = currency,
            customer = customerPaymentId,
            paymentMethod = paymentMethodId,
            receiptEmail = Nothing,
            driverAccountId
          }
  -- Step 1: Cancel existing ride payment intent FIRST (voids RideFare entries only)
  logDebug $ "[CancellationSettlement] Cancelling existing ride payment intent for rideId=" <> ride.id.getId
  void $ SPayment.cancelPaymentIntent booking.merchantId booking.merchantOperatingCityId booking.paymentMode ride.id
  -- Step 2: Create pending cancellation ledger AFTER cancel so it isn't voided
  logDebug $ "[CancellationSettlement] Creating pending cancellation ledger for rideId=" <> ride.id.getId
  ledgerResp <- RidePaymentFinance.createPendingCancellationFeeLedger ledgerCtx cancellationBase cancellationTax
  let mbCancelInvoiceId = case ledgerResp of
        Right (inv, _) -> inv
        _ -> Nothing
  -- Step 3: Create new payment intent for cancellation fee (wrapped to catch Stripe errors)
  logDebug $ "[CancellationSettlement] Creating cancellation payment intent amount=" <> show cancellationTotal <> " currency=" <> show currency
  let cancellationLedgerInfo =
        SPayment.RidePaymentLedgerInfo
          { rideFare = 0,
            gstAmount = 0,
            tollFare = 0,
            tollVatAmount = 0,
            parkingCharge = 0,
            parkingChargeVat = 0,
            platformFee = 0,
            offerDiscountAmount = 0,
            cashbackPayoutAmount = 0,
            rideVatAbsorbedOnDiscount = 0,
            cancellationCharge = cancellationBase,
            cancellationTax = cancellationTax,
            financeCtx = ledgerCtx
          }
  eitherMbIntent <-
    withTryCatch "[CancellationSettlement] makePaymentIntent" $
      SPayment.makePaymentIntent personD.merchantId personD.merchantOperatingCityId booking.paymentMode personD.id (Just ride.id) Nothing DOrder.RideHailing createPaymentIntentServiceReq (Just cancellationLedgerInfo)
  let markDueOnFailure = do
        case ledgerResp of
          Right (_inv, entryIds) -> RidePaymentFinance.markEntriesAsDue entryIds
          _ -> return ()
        syncCancellationLedger CallBPPInternal.OverdueCancellationLedger
  case eitherMbIntent of
    Left err -> do
      logError $ "[CancellationSettlement] Stripe error creating cancellation intent, marking DUE: " <> show err
      markDueOnFailure
    Right mbCancellationPaymentIntentResp ->
      case mbCancellationPaymentIntentResp of
        Nothing -> logDebug $ "[CancellationSettlement] Cancellation payment intent skipped (zero effective amount) for booking: " <> show booking.id
        Just cancellationPaymentIntentResp -> do
          logDebug $ "[CancellationSettlement] Charging cancellation payment intent: " <> cancellationPaymentIntentResp.paymentIntentId
          offerStatsInput <- SPayment.buildOfferStatsInput personD
          -- Step 4: chargePaymentIntent internally settles ledger entries
          eitherCaptured <-
            withTryCatch "[CancellationSettlement] chargePaymentIntent" $
              SPayment.chargePaymentIntent booking.merchantId booking.merchantOperatingCityId booking.paymentMode DOrder.RideHailing cancellationPaymentIntentResp.paymentIntentId ride.id RidePaymentFinance.settledReasonRidePayment booking.riderId offerStatsInput
          case eitherCaptured of
            Right True -> do
              RidePaymentFinance.markCancellationFeeInvoicePaid mbCancelInvoiceId
              QRide.markPaymentStatus DRide.Completed ride.id
              syncCancellationLedger CallBPPInternal.SettleCancellationLedger
              logDebug "[CancellationSettlement] Captured cancellation fee, invoice marked Paid, payment status Completed"
            _ -> do
              logError $ "[CancellationSettlement] Charge failed for PI: " <> cancellationPaymentIntentResp.paymentIntentId
              markDueOnFailure
