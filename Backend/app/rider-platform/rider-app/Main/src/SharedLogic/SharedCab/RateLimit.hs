{-
  Shared-cab boarding hardening — M8.6.
  Design sources (Plans/Shared-Cab-Plans):
    05-allocation-plan.md §8.1 — code proves presence, not knowledge:
      ≤ boardAttemptsPer10Min boarding attempts; one generic error for every failure;
      no location -> allocated-cab or spot booking only, never re-bind; the
      per-vehicle daily count of no-location spot bookings feeds an ops flag.
    05-allocation-plan.md §8.12 — all day-part logic in IST (the daily bucket below).
    08-build-tasks.md M8 8.6   — "rate limits + per-vehicle no-location spot-booking
      count; limits trip"

  REDIS: every key here lives in the cross-app master cell (Booking.shared) so the
  scheduler / ops cells can see them; a plain-prefix write would be invisible there.
-}
module SharedLogic.SharedCab.RateLimit
  ( enforceBoardingAttemptLimit,
    recordNoLocationBoarding,
    noLocationSpotBookingsToday,
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
import Tools.Error

boardAttemptWindowSec :: Int
boardAttemptWindowSec = 10 * 60

-- | Counts are per booking: a booking belongs to exactly one rider (R11), so this bounds the
-- rider's probe rate without letting a stalker ride someone else's counter.
boardAttemptsKey :: Id DBooking.FRFSTicketBooking -> Text
boardAttemptsKey bookingId = "sharedcab:rate:board:" <> bookingId.getId

-- | Per vehicle per IST day (05 §8.12). The day suffix is in the key, so yesterday's count can't
-- leak into today; stale keys flush after 2 days.
noLocationKey :: Text -> Text
noLocationKey plate = "sharedcab:noloc:" <> plate

-- | The IST day bucket for the counter key (05 §8.12: all day-part logic in IST).
istDayStamp :: UTCTime -> Text
istDayStamp = T.pack . formatTime defaultTimeLocale "%Y%m%d" . addUTCTime 19800

-- | 05 §8.1: ≤ boardAttemptsPer10Min (Config.getTunables). Over the limit is the SAME generic BoardingFailed — the
-- client must not be able to tell a rate-limited probe from a wrong code.
enforceBoardingAttemptLimit :: (Redis.HedisFlow m r, MonadFlow m) => Int -> Id DBooking.FRFSTicketBooking -> m ()
enforceBoardingAttemptLimit boardAttemptsPer10Min bookingId =
  shared $ do
    attempts <- Redis.incr key
    when (attempts == 1) $ Redis.expire key boardAttemptWindowSec
    when (attempts > fromIntegral boardAttemptsPer10Min) $
      throwError BoardingFailed
  where
    key = boardAttemptsKey bookingId

-- | One no-location boarding (allocated cab or spot) against the vehicle's day count
-- (05 §8.1: a driver feeding codes to non-app riders to harvest bookings shows up per vehicle,
-- not per rider). Returns the vehicle's count for the IST day; logs the ops flag once the count
-- crosses noLocationSpotBookingsPerVehiclePerDay (Config.getTunables).
recordNoLocationBoarding :: (Redis.HedisFlow m r, MonadFlow m) => Int -> Text -> m Integer
recordNoLocationBoarding noLocationSpotBookingsPerVehiclePerDay plate = do
  now <- getCurrentTime
  let key = noLocationKey plate <> ":" <> istDayStamp now
  count <-
    shared $ do
      c <- Redis.incrby key 1
      when (c == 1) $ Redis.expire key (2 * 24 * 60 * 60)
      pure c
  when (count >= fromIntegral noLocationSpotBookingsPerVehiclePerDay) $
    logWarning $
      "sharedcab:ops-flag: vehicle " <> plate <> " at " <> show count
        <> " no-location boardings today (05 §8.1, threshold "
        <> show noLocationSpotBookingsPerVehiclePerDay
        <> ")"
  pure count

-- | Read side of the per-vehicle day count (ops tooling / the M8.7 scenario script).
noLocationSpotBookingsToday :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m Int
noLocationSpotBookingsToday plate = do
  now <- getCurrentTime
  mbCount <- shared $ Redis.get @Integer (noLocationKey plate <> ":" <> istDayStamp now)
  pure $ maybe 0 fromIntegral mbCount
