{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The only writers of a rider's cancellation-dues balance. Every path that adds a
-- charge (customer cancel, driver no-show) or settles one (next-ride fare collection,
-- Stripe immediate capture) goes through here, so balance and per-ride rows cannot
-- drift apart.
module SharedLogic.CancellationDues where

import Data.List (sortOn)
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.CancellationDuesDetails as DCDD
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderDetails as DRD
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.CancellationDuesDetails as QCDD
import qualified Storage.Queries.RiderDetails as QRD
import Tools.Error

data ApplyCancellationChargeReq = ApplyCancellationChargeReq
  { ride :: DRide.Ride,
    riderId :: Id DRD.RiderDetails,
    -- rider's dues balance as already fetched by the caller; the new balance is
    -- currentDues + totalCharges
    currentDues :: HighPrecMoney,
    -- the full amount added to the balance (base + tax)
    totalCharges :: HighPrecMoney,
    currency :: Currency,
    cancellationFee :: Maybe HighPrecMoney,
    cancellationFeeTax :: Maybe HighPrecMoney,
    overdueCancellationCharge :: Maybe HighPrecMoney,
    overdueCancellationTax :: Maybe HighPrecMoney,
    cancellationCommission :: Maybe HighPrecMoney,
    overdueCancellationCommission :: Maybe HighPrecMoney
  }

-- | Add a cancellation charge to the rider's running balance and record the per-ride
-- breakdown row that waive-off and settlement key on. Flow-specific counters
-- (valid-cancellation counts, due-ride counts) stay with the callers.
applyCancellationCharge :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => ApplyCancellationChargeReq -> m ()
applyCancellationCharge req = do
  void $ QRD.updateCancellationDues (req.totalCharges + req.currentDues) req.riderId
  when (req.totalCharges > 0) $ do
    duesDetailsId <- generateGUID
    now <- getCurrentTime
    QCDD.create
      DCDD.CancellationDuesDetails
        { id = duesDetailsId,
          rideId = req.ride.id,
          riderId = req.riderId,
          cancellationAmount = req.totalCharges,
          cancellationFee = req.cancellationFee,
          cancellationFeeTax = req.cancellationFeeTax,
          overdueCancellationCharge = req.overdueCancellationCharge,
          overdueCancellationTax = req.overdueCancellationTax,
          cancellationCommission = req.cancellationCommission,
          overdueCancellationCommission = req.overdueCancellationCommission,
          currency = req.currency,
          paymentStatus = DCDD.PENDING,
          createdAt = now,
          updatedAt = now,
          merchantId = req.ride.merchantId,
          merchantOperatingCityId = Just req.ride.merchantOperatingCityId
        }

-- | Settle up to @amountCollected@ of the rider's PENDING dues rows, oldest first,
-- decrementing the balance by exactly the collected amount rather than zeroing it —
-- dues that accrued after the amount was quoted (e.g. a cancellation mid-ride) stay
-- PENDING and keep riding on the next fare. Returns the rideIds of the rows marked
-- PAID, for the BAP fee-status callback.
settleCancellationDuesUpTo :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DRD.RiderDetails -> HighPrecMoney -> m [Id DRide.Ride]
settleCancellationDuesUpTo riderDetails amountCollected = do
  pendingDues <- QCDD.findAllPendingByRiderId riderDetails.id
  let coveredRows = takeCovered 0 (sortOn (.createdAt) pendingDues)
  unless (null coveredRows) $
    QCDD.updateStatusByIds DCDD.PAID ((.id) <$> coveredRows)
  void $ QRD.updateCancellationDues (max 0 (riderDetails.cancellationDues - amountCollected)) riderDetails.id
  pure ((.rideId) <$> coveredRows)
  where
    takeCovered _ [] = []
    takeCovered coveredSoFar (dues : rest)
      | coveredSoFar + dues.cancellationAmount <= amountCollected = dues : takeCovered (coveredSoFar + dues.cancellationAmount) rest
      | otherwise = []

-- | Settle exactly one ride's pending dues row (Stripe immediate-capture path):
-- decrement the balance by that row's amount and mark it PAID.
settleCustomerCancellationDues ::
  ( EsqDBFlow m r,
    CacheFlow m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  m ()
settleCustomerCancellationDues booking ride =
  case booking.riderId of
    Nothing -> logError $ "settleCustomerCancellationDues: no riderId in booking " <> booking.id.getId
    Just rid -> do
      mbCancellationDuesDetails <- QCDD.findByRideId ride.id
      case mbCancellationDuesDetails of
        Just cancellationDuesDetails | cancellationDuesDetails.paymentStatus == DCDD.PENDING -> do
          riderDetails <- QRD.findById rid >>= fromMaybeM (RiderDetailsNotFound rid.getId)
          QRD.updateCancellationDues (max 0 (riderDetails.cancellationDues - cancellationDuesDetails.cancellationAmount)) rid
          QRD.updateCancellationDuesPaymentInfo cancellationDuesDetails.cancellationAmount riderDetails
          QCDD.updatePaymentStatusByRideId DCDD.PAID ride.id
          logInfo $ "Cleared customer cancellation dues for rideId: " <> ride.id.getId <> " amount=" <> show cancellationDuesDetails.cancellationAmount
        _ -> logInfo $ "No pending cancellation dues to settle for rideId: " <> ride.id.getId
