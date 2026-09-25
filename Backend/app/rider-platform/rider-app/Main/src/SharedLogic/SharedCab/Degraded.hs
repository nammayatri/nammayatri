{-
  Shared-cab degraded boarding — M8.5.
  Design sources (Plans/Shared-Cab-Plans):
    05-allocation-plan.md §5 — degraded boarding (marker sharedcab:degraded:{bookingId},
      timeout end when the marker expires; never blocked, PRD §11.4)
    08-build-tasks.md M8 8.5  — "degraded boarding marker + timeout end;
      unknown code still boards, flagged"

  A degraded ride has no session, so no LTS stream and no geofence auto-end (prime 23:53).
  The two ends: rider confirm ("I got down" -> markDropped) or the marker's TTL expiring —
  detected lazily on the rider's status poll by expireDegradedBoardingIfNeeded.

  REDIS: the marker lives in the cross-app master cell (Booking.shared) so the scheduler /
  engine cells can see it; a plain-prefix write would be invisible there.
-}
module SharedLogic.SharedCab.Degraded
  ( DegradedBoarding (..),
    degradedKey,
    planDegrade,
    shouldExpireDegraded,
    markDegradedBoarding,
    clearDegradedMarker,
    isMarkerAlive,
    expireDegradedBoardingIfNeeded,
  )
where

import qualified Domain.Types.FRFSTicketBooking as DBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DBookingStatus
import qualified Domain.Types.FRFSTicketStatus as TicketStatus
-- Field `at` (the spec names the marker payload `{typedCode, at}`, 05 §5) shadows Safe.at.
import Kernel.Prelude hiding (at)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.SharedCab.Booking (shared, tryWithBookingLock)
import SharedLogic.SharedCab.LegState (isDroppable)
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QBooking

-- | 05 §5: the marker's payload — what the rider typed and when they were let through.
data DegradedBoarding = DegradedBoarding
  { typedCode :: Text,
    at :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)

degradedKey :: Id DBooking.FRFSTicketBooking -> Text
degradedKey bookingId = "sharedcab:degraded:" <> bookingId.getId

-- | Whether an unknown code may degrade this booking, and which allocated plate it gives back first.
-- Nothing: refuse (not CONFIRMED, no ticket still held, already riding a real cab, or already degraded: a retry
-- must not refresh the marker's TTL). Just mbPlate: degrade,
-- releasing the allocation to that plate if there is one — a degraded ride has no cab and no seat (05 §5).
planDegrade :: DBookingStatus.FRFSTicketBookingStatus -> Maybe Text -> [TicketStatus.FRFSTicketStatus] -> Bool -> Maybe (Maybe Text)
planDegrade bookingStatus mbPlate statuses markerAlive
  | bookingStatus /= DBookingStatus.CONFIRMED = Nothing
  | markerAlive = Nothing
  | not (any isDroppable statuses) = Nothing
  | isJust mbPlate && TicketStatus.INPROGRESS `elem` statuses = Nothing
  | otherwise = Just mbPlate

-- | The timeout end: no cab, still INPROGRESS, marker expired. A booking that has since boarded a real
-- cab (plate set) ends with that cab, never here.
shouldExpireDegraded :: Maybe Text -> [TicketStatus.FRFSTicketStatus] -> Bool -> Bool
shouldExpireDegraded mbPlate statuses markerAlive =
  isNothing mbPlate && TicketStatus.INPROGRESS `elem` statuses && not markerAlive

-- | Set the degrade marker (05 §5: `{typedCode, at}` with TTL degradedTimeoutSec).
markDegradedBoarding :: (Redis.HedisFlow m r, MonadFlow m) => Int -> Id DBooking.FRFSTicketBooking -> Text -> m ()
markDegradedBoarding ttlSec bookingId typedCode = do
  now <- getCurrentTime
  shared $ Redis.setExp (degradedKey bookingId) DegradedBoarding {typedCode, at = now} ttlSec

-- | A real boarding supersedes a degraded one; a leftover marker would mask the booking in the invariants.
clearDegradedMarker :: (Redis.HedisFlow m r, MonadFlow m) => Id DBooking.FRFSTicketBooking -> m ()
clearDegradedMarker bookingId = shared . void $ Redis.del (degradedKey bookingId)

isMarkerAlive :: (Redis.HedisFlow m r, MonadFlow m) => Id DBooking.FRFSTicketBooking -> m Bool
isMarkerAlive bookingId = isJust <$> shared (Redis.get @DegradedBoarding (degradedKey bookingId))

-- | The timeout end of a degraded boarding (05 §5). No session means no tick watches this ride,
-- so the rider's poll is the clock: marker gone (TTL ran out) + tickets still INPROGRESS =>
-- the ride ends here (USED), which the leg state then derives as DROPPED.
-- Returns True when this call did the flip, so the caller can adjust the status it just computed.
-- Re-decided on a fresh read under the booking lock, so a boarding that lands after the caller's read is
-- never ended; a poll that finds the lock taken leaves it to the next poll.
-- TODO(review MED 3, after merge): a sweep for degraded rides whose rider never polls again; they stay INPROGRESS.
expireDegradedBoardingIfNeeded ::
  (CacheFlow m r, EsqDBFlow m r, Redis.HedisFlow m r, MonadFlow m) =>
  DBooking.FRFSTicketBooking ->
  m Bool
expireDegradedBoardingIfNeeded booking
  | isJust booking.vehicleNumber = pure False -- a real cab owns this ride's end (geofence / driver action)
  | otherwise = do
    tickets <- QTicket.findAllByTicketBookingId booking.id
    alive <- isMarkerAlive booking.id
    if not (shouldExpireDegraded booking.vehicleNumber (map (.status) tickets) alive)
      then pure False
      else fmap (fromMaybe False) . tryWithBookingLock booking.id $ do
        mbFresh <- QBooking.findById booking.id
        freshTickets <- QTicket.findAllByTicketBookingId booking.id
        freshAlive <- isMarkerAlive booking.id
        if maybe False (\b -> shouldExpireDegraded b.vehicleNumber (map (.status) freshTickets) freshAlive) mbFresh
          then do
            forM_ (filter ((== TicketStatus.INPROGRESS) . (.status)) freshTickets) $ \t ->
              QTicket.updateStatusByTBookingIdAndTicketNumber TicketStatus.USED t.scannedByVehicleNumber booking.id t.ticketNumber
            -- TODO(7.6): Events.forBooking booking.id "dropped" [("by", "degraded_timeout")] — task 7.6 not merged yet.
            logInfo $ "sharedcab:event:dropped " <> show ([("booking", booking.id.getId), ("by", "degraded_timeout")] :: [(Text, Text)])
            pure True
          else pure False
