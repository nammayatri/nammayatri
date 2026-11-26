{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FindAnotherDriver
  ( isFindAnotherDriverRequest,
    recordSwap,
    swapCancelConsequences,
    swapCancelQuote,
    clearSwap,
  )
where

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Merchant as DMerc
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTC
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CancellationFault as CancellationFault
import qualified SharedLogic.CancellationOrchestrator as Orchestrator
import qualified Storage.Queries.Person as QPers
import qualified Storage.Queries.Ride as QRide
import Tools.Error

findAnotherDriverReasonCode :: Text
findAnotherDriverReasonCode = "FIND_ANOTHER_DRIVER"

isFindAnotherDriverRequest :: Maybe Bool -> Maybe Text -> Bool
isFindAnotherDriverRequest userReallocationEnabled cancellationReason =
  userReallocationEnabled == Just True && cancellationReason == Just findAnotherDriverReasonCode

-- | Call once the trip has been reallocated, with the ride the rider swapped away and what cancelling
-- would have cost at the swap. Marks the trip with that ride when it was a fee, and unmarks it when it
-- wasn't: only the latest swap counts.
recordSwap :: SRB.Booking -> DRide.Ride -> Maybe HighPrecMoney -> Flow ()
recordSwap booking ride mbSwapFee =
  if maybe False (> 0) mbSwapFee
    then do
      now <- getCurrentTime
      let ttlSeconds = max 0 (ceiling (diffUTCTime booking.startTime now)) + 86400 :: Int
      Redis.setExp (swapKey booking.transactionId) ride.id ttlSeconds
    else clearSwap booking

-- | Cancelling a marked trip before a new driver accepts gets round the fee the swap avoided, so it
-- counts as the rider cancelling the swapped-away ride, at fault. The charge is the usual one for that:
-- the deposit on a deposit booking, otherwise the fee set in the city's consequence matrix.
swapCancelVerdict :: CancellationFault.FaultVerdict
swapCancelVerdict = CancellationFault.FaultVerdict {atFault = CancellationFault.CustomerAtFault, rule = findAnotherDriverReasonCode}

-- | For a cancel with no driver assigned: the consequences that charge it, when the trip is marked.
-- The verdict is recorded on the swapped-away ride first, so deciding the consequences takes it
-- instead of running the city's fault rules.
swapCancelConsequences :: DMerc.Merchant -> SRB.Booking -> DTC.TransporterConfig -> SBCR.BookingCancellationReason -> Flow (Maybe Orchestrator.ConsequenceCtx)
swapCancelConsequences merchant booking transporterConfig bookingCR = do
  mbSwappedRide <- swappedRide booking
  forM mbSwappedRide $ \ride -> do
    CancellationFault.recordFaultVerdict ride swapCancelVerdict
    driver <- QPers.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    decision <- Orchestrator.decideCancellationConsequences booking ride transporterConfig bookingCR.source bookingCR.reasonCode Nothing
    pure Orchestrator.ConsequenceCtx {merchant = merchant, booking = booking, ride = ride, transporterConfig = transporterConfig, driver = driver, source = bookingCR.source, decision = decision}

-- | The soft-cancel quote for a marked trip with no driver assigned. Cancelling it is at fault
-- ('swapCancelConsequences'), which forfeits the deposit on a deposit booking.
swapCancelQuote :: SRB.Booking -> Maybe DRide.Ride -> Flow (Maybe PriceAPIEntity)
swapCancelQuote booking mbRide = case mfilter (> 0) booking.bookingDeposit of
  Just deposit | isNothing mbRide -> do
    marked <- isJust <$> swappedRideId booking
    pure $ if marked then Just PriceAPIEntity {amount = deposit, currency = booking.currency} else Nothing
  _ -> pure Nothing

clearSwap :: CacheFlow m r => SRB.Booking -> m ()
clearSwap booking = Redis.del (swapKey booking.transactionId)

swappedRide :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => SRB.Booking -> m (Maybe DRide.Ride)
swappedRide booking = swappedRideId booking >>= maybe (pure Nothing) QRide.findById

swappedRideId :: CacheFlow m r => SRB.Booking -> m (Maybe (Id DRide.Ride))
swappedRideId booking = Redis.safeGet (swapKey booking.transactionId)

swapKey :: Text -> Text
swapKey transactionId = "FindAnotherDriver:SwappedWithFee:TransactionId-" <> transactionId
