-- | Boarding hardening: attempt rate limit and the per-vehicle no-location boarding count, in the cross-app master cell
-- (Booking.shared) so the scheduler and ops cells can see them.
module SharedLogic.SharedCab.RateLimit
  ( enforceBoardingAttemptLimit,
    recordNoLocationBoarding,
  )
where

import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Domain.Types.FRFSTicketBooking as DBooking
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.SharedCab.Booking (shared)
import SharedLogic.SharedCab.Keys (boardAttemptsKey, noLocationKey)
import Tools.Error

boardAttemptWindowSec :: Int
boardAttemptWindowSec = 10 * 60

istDayStamp :: UTCTime -> Text
istDayStamp = T.pack . formatTime defaultTimeLocale "%Y%m%d" . addUTCTime 19800

-- | <= boardAttemptsPer10Min. Over the limit is the SAME generic BoardingFailed: the client must not be able to tell a
-- rate-limited probe from a wrong code.
enforceBoardingAttemptLimit :: (Redis.HedisFlow m r, MonadFlow m) => Int -> Id DBooking.FRFSTicketBooking -> m ()
enforceBoardingAttemptLimit boardAttemptsPer10Min bookingId =
  shared $ do
    -- the TTL is set with the key, so an INCR can never leave a counter that lives forever
    void $ Redis.setNxExpire key boardAttemptWindowSec (0 :: Integer)
    attempts <- Redis.incr key
    when (attempts > fromIntegral boardAttemptsPer10Min) $
      throwError BoardingFailed
  where
    key = boardAttemptsKey bookingId

-- | One no-location boarding against the vehicle's day count (a driver feeding codes to non-app riders shows up per vehicle,
-- not per rider). Logs the ops flag once the count crosses noLocationSpotBookingsPerVehiclePerDay.
recordNoLocationBoarding :: (Redis.HedisFlow m r, MonadFlow m) => Int -> Text -> m Integer
recordNoLocationBoarding noLocationSpotBookingsPerVehiclePerDay plate = do
  now <- getCurrentTime
  let key = noLocationKey plate (istDayStamp now)
  count <-
    shared $ do
      void $ Redis.setNxExpire key (2 * 24 * 60 * 60) (0 :: Integer)
      Redis.incrby key 1
  when (count >= fromIntegral noLocationSpotBookingsPerVehiclePerDay) $
    logWarning $
      "sharedcab:ops-flag: vehicle " <> plate <> " at " <> show count
        <> " no-location boardings today (threshold "
        <> show noLocationSpotBookingsPerVehiclePerDay
        <> ")"
  pure count
