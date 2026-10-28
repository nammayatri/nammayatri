-- | Boarding hardening: attempt rate limit and the per-vehicle no-location boarding count, in the cross-app master cell
-- (Booking.shared) so the scheduler and ops cells can see them.
module SharedLogic.SharedCab.RateLimit
  ( enforceBoardingAttemptLimit,
    enforceChatMessageLimit,
    enforceStickerCodeLimit,
    recordNoLocationBoarding,
  )
where

import qualified Domain.Types.FRFSTicketBooking as DBooking
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (slidingWindowLimiter)
import SharedLogic.SharedCab.Booking (shared)
import SharedLogic.SharedCab.Keys (boardAttemptsKey, chatAttemptsKey, noLocationKey, stickerCodeAttemptsKey)
import Tools.Error

-- | <= boardAttemptsPer10Min. Over the limit is the SAME generic BoardingFailed: the client must not be able to tell a
-- rate-limited probe from a wrong code.
enforceBoardingAttemptLimit :: (Redis.HedisFlow m r, MonadFlow m) => Int -> Id DBooking.FRFSTicketBooking -> m ()
enforceBoardingAttemptLimit boardAttemptsPer10Min bookingId =
  unlessM (shared $ slidingWindowLimiter (boardAttemptsKey bookingId) boardAttemptsPer10Min (10 * 60)) $
    throwError BoardingFailed

chatMessagesPerWindow :: Int
chatMessagesPerWindow = 10

chatWindowSec :: Int
chatWindowSec = 60

-- | <= chatMessagesPerWindow chat pushes per chatWindowSec from one driver to one booking.
enforceChatMessageLimit :: (Redis.HedisFlow m r, MonadFlow m) => Id DBooking.FRFSTicketBooking -> Text -> m ()
enforceChatMessageLimit bookingId driverId =
  unlessM (shared $ slidingWindowLimiter (chatAttemptsKey bookingId driverId) chatMessagesPerWindow chatWindowSec) $
    throwError (HitsLimitError chatWindowSec)

stickerCodesPerWindow :: Int
stickerCodesPerWindow = 10

stickerCodeWindowSec :: Int
stickerCodeWindowSec = 10 * 60

-- | <= stickerCodesPerWindow walk-up sticker-code resolutions per stickerCodeWindowSec for one rider.
enforceStickerCodeLimit :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m ()
enforceStickerCodeLimit personId =
  unlessM (shared $ slidingWindowLimiter (stickerCodeAttemptsKey personId) stickerCodesPerWindow stickerCodeWindowSec) $
    throwError (HitsLimitError stickerCodeWindowSec)

-- | One no-location boarding against the vehicle's count over the last day (a driver feeding codes to non-app riders shows up per
-- vehicle, not per rider). Logs the ops flag from the noLocationSpotBookingsPerVehiclePerDay-th one on.
recordNoLocationBoarding :: (Redis.HedisFlow m r, MonadFlow m) => Int -> Text -> m ()
recordNoLocationBoarding noLocationSpotBookingsPerVehiclePerDay plate = do
  belowThreshold <- shared $ slidingWindowLimiter (noLocationKey plate) (noLocationSpotBookingsPerVehiclePerDay - 1) (24 * 60 * 60)
  unless belowThreshold $
    logWarning $
      "sharedcab:ops-flag: vehicle " <> plate <> " has at least " <> show noLocationSpotBookingsPerVehiclePerDay <> " no-location boardings in the last day"
