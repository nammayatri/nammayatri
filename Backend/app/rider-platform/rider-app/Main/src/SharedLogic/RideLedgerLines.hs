module SharedLogic.RideLedgerLines
  ( breakupLedgerLines,
    rideRefundableLines,
    customerBornePaymentCharge,
  )
where

import BecknV2.OnDemand.FareBreakup.Tax (TaxBreakup)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.FareBreakup as DFareBreakup
import qualified Domain.Types.OfferEntity as DOfferEntity
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideStatus as DRideStatus
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified SharedLogic.FareBreakupInfo as SFareBreakupInfo
import qualified SharedLogic.FareBreakupLines as FBL
import qualified SharedLogic.Finance.RidePayment as RidePaymentFinance
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.OfferEntity as QOfferEntity

breakupLedgerLines :: HighPrecMoney -> [DFareBreakup.FareBreakup] -> HighPrecMoney -> (HighPrecMoney, Maybe HighPrecMoney) -> [RidePaymentFinance.LedgerLine]
breakupLedgerLines fareAmount breakups discount paymentCharge =
  map (netOfDiscount appliedDiscount) (RidePaymentFinance.rideLedgerLines fareAmount (maybe [] (.breakupLines) mbFareLines) paymentCharge)
  where
    mbFareLines = FBL.fareLinesFromBreakups fareAmount breakups
    appliedDiscount = maybe discount (\fareLines -> (FBL.applyOffer discount fareLines).appliedDiscount) mbFareLines

netOfDiscount :: HighPrecMoney -> RidePaymentFinance.LedgerLine -> RidePaymentFinance.LedgerLine
netOfDiscount discount line
  | line.chargeRef /= RidePaymentFinance.ridePaymentRefRideFare || gross <= 0 = line
  | otherwise = line {RidePaymentFinance.chargeBase = scaled line.chargeBase, RidePaymentFinance.chargeTax = scaled line.chargeTax}
  where
    gross = RidePaymentFinance.ledgerLineTotal line
    remaining = gross - max 0 (min discount gross)
    scaled amount = amount * remaining / gross

rideRefundableLines :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, BeamFlow.BeamFlow m r) => DRide.Ride -> DBooking.Booking -> m [RidePaymentFinance.LedgerLine]
rideRefundableLines ride booking
  | ride.status == DRideStatus.CANCELLED = RidePaymentFinance.settledCancellationLedgerLines <$> RidePaymentFinance.findRidePaymentEntries ride.id.getId
  | otherwise = completedRideRefundableLines ride booking

completedRideRefundableLines :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DRide.Ride -> DBooking.Booking -> m [RidePaymentFinance.LedgerLine]
completedRideRefundableLines ride booking = do
  mbOfferEntity <- QOfferEntity.findByEntityIdAndEntityType ride.id.getId DOfferEntity.RIDE
  breakups <- SFareBreakupInfo.getFareBreakupsWithFallback ride.id.getId DFareBreakup.RIDE (QFareBreakup.findAllByEntityIdAndEntityType ride.id.getId DFareBreakup.RIDE)
  pure $
    breakupLedgerLines
      (maybe booking.estimatedFare.amount (.amount) ride.totalFare)
      breakups
      (maybe 0 (.discountAmount) mbOfferEntity)
      (customerBornePaymentCharge ride.paymentCharge ride.paymentChargeTaxBreakup ride.paymentChargeBearer)

customerBornePaymentCharge :: Maybe HighPrecMoney -> Maybe TaxBreakup -> Maybe Text -> (HighPrecMoney, Maybe HighPrecMoney)
customerBornePaymentCharge mbCharge mbChargeTaxBreakup mbBearer
  | mbBearer == Just "PAYMENT_CUSTOMER" = (fromMaybe 0 mbCharge, (.total) <$> mbChargeTaxBreakup)
  | otherwise = (0, Nothing)
