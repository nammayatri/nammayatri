{-
  Silent reallocation window.

  When a driver cancels and the BPP reallocates, the rider is normally pushed to the
  pooling screen at once. With a per-city window configured, the BAP instead keeps the
  old booking visible (flagged) for that window so a new driver can pick the trip up
  without the rider ever seeing the "finding another driver" moment. The window is a
  single Redis key per rider; whichever comes first (new ride assigned, rider cancel,
  or the expiry job) deletes it.
-}
module SharedLogic.SilentReallocation
  ( SilentReallocationCtx (..),
    setSilentReallocation,
    getSilentReallocation,
    takeSilentReallocationForExpiry,
    clearSilentReallocation,
    includeSilentReallocationBooking,
    maskSilentReallocationBooking,
  )
where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.API as DRBAPI
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.BookingStatus as DRB
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Extra.Ride as DRideAPI
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideStatus as DRide
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB

data SilentReallocationCtx = SilentReallocationCtx
  { bookingId :: Id DRB.Booking,
    rideId :: Id DRide.Ride,
    estimateId :: Id DEstimate.Estimate,
    cancellationSource :: DBCR.CancellationSource,
    expiresAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-- The key is written by rider-app and read by rider-app-scheduler, which use different
-- per-app Redis key prefixes. Every access therefore goes through 'withCrossAppRedis',
-- which drops the prefix, so both processes address the same key.
silentReallocationKey :: Id DP.Person -> Text
silentReallocationKey personId = "app-shared:silentRealloc:rider:" <> personId.getId

type SilentReallocFlow m r = (CacheFlow m r, TryException m)

-- | Grace over the configured window so a late expiry job (scheduler poll interval,
-- KV lag, a busy pod) still finds the key. The presentation window itself ends at
-- 'expiresAt' regardless of this TTL; the grace only protects the held push.
keyGraceSeconds :: Int
keyGraceSeconds = 120

setSilentReallocation :: SilentReallocFlow m r => Id DP.Person -> SilentReallocationCtx -> Seconds -> m ()
setSilentReallocation personId ctx windowSeconds =
  Redis.withCrossAppRedis $ Redis.setExp (silentReallocationKey personId) ctx (windowSeconds.getSeconds + keyGraceSeconds)

getSilentReallocation :: SilentReallocFlow m r => Id DP.Person -> m (Maybe SilentReallocationCtx)
getSilentReallocation personId = do
  mbCtx <- Redis.withCrossAppRedis $ Redis.safeGet (silentReallocationKey personId)
  now <- getCurrentTime
  pure $ case mbCtx of
    Just ctx | ctx.expiresAt > now -> Just ctx
    _ -> Nothing

clearSilentReallocation :: SilentReallocFlow m r => Id DP.Person -> m ()
clearSilentReallocation personId = Redis.withCrossAppRedis $ Redis.del (silentReallocationKey personId)

-- | For the expiry job only: read the window context even though 'expiresAt' has
-- (by design) just passed, and close it. 'getSilentReallocation' hides an elapsed
-- window from the API, which is exactly what the job must not rely on.
takeSilentReallocationForExpiry :: SilentReallocFlow m r => Id DP.Person -> m (Maybe SilentReallocationCtx)
takeSilentReallocationForExpiry personId = do
  mbCtx <- Redis.withCrossAppRedis $ Redis.safeGet (silentReallocationKey personId)
  whenJust mbCtx $ \_ -> clearSilentReallocation personId
  pure mbCtx

-- | For an active-bookings query: if the rider is inside a silent window and no newer
-- active booking exists, return the reallocated booking so the app keeps its tracking
-- screen. Returns the context so the caller can mask the API entity.
includeSilentReallocationBooking :: (SilentReallocFlow m r, EsqDBFlow m r) => Id DP.Person -> [DRB.Booking] -> m ([DRB.Booking], Maybe SilentReallocationCtx)
includeSilentReallocationBooking personId bookings = do
  mbCtx <- getSilentReallocation personId
  case mbCtx of
    Nothing -> pure (bookings, Nothing)
    Just ctx
      | any (\b -> b.id == ctx.bookingId) bookings -> pure (bookings, Just ctx)
      | not (null bookings) -> pure (bookings, Nothing) -- a newer booking already replaced it
      | otherwise -> do
        mbBooking <- QRB.findById ctx.bookingId
        case mbBooking of
          Just booking | booking.status == DRB.REALLOCATED -> pure ([booking], Just ctx)
          _ -> pure (bookings, Nothing)

-- | Present the reallocated booking as still assigned, with the flag the app keys on,
-- the old driver's number hidden and the ride status echoed as NEW. Name, vehicle and
-- photo stay so the app can draw the "driver approaching" state itself.
maskSilentReallocationBooking :: Maybe SilentReallocationCtx -> DRBAPI.BookingAPIEntity -> DRBAPI.BookingAPIEntity
maskSilentReallocationBooking mbCtx entity =
  case mbCtx of
    Just ctx
      | ctx.bookingId == entity.id ->
        entity
          { DRBAPI.status = DRB.TRIP_ASSIGNED,
            DRBAPI.isSilentReallocation = Just True,
            DRBAPI.rideList = map maskRide entity.rideList
          }
    _ -> entity
  where
    maskRide :: DRideAPI.RideAPIEntity -> DRideAPI.RideAPIEntity
    maskRide ride =
      ride
        { DRideAPI.driverNumber = Nothing,
          DRideAPI.status = DRide.NEW
        }
