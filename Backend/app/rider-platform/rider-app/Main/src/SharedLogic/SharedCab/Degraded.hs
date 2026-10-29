-- | Degraded boarding: an unknown code still boards, flagged by a marker in the cross-app master cell (Booking.shared) so the
-- scheduler can see it. The ride has no session, so it ends on the rider's confirm or when the marker's TTL expires.
module SharedLogic.SharedCab.Degraded
  ( DegradedBoarding (..),
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
import Kernel.Prelude hiding (at)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.SharedCab.Booking (shared, tryWithBookingLock)
import SharedLogic.SharedCab.Keys (degradedKey)
import SharedLogic.SharedCab.LegState (isDroppable)
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QBooking

data DegradedBoarding = DegradedBoarding
  { typedCode :: Text,
    at :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Whether an unknown code may degrade this booking and which allocated plate it gives back. Nothing refuses (not CONFIRMED, no ticket
-- held, riding a real cab, or already degraded: a retry must not refresh the marker's TTL).
planDegrade :: DBookingStatus.FRFSTicketBookingStatus -> Maybe Text -> [TicketStatus.FRFSTicketStatus] -> Bool -> Maybe (Maybe Text)
planDegrade bookingStatus mbPlate statuses markerAlive
  | bookingStatus /= DBookingStatus.CONFIRMED = Nothing
  | markerAlive = Nothing
  | not (any isDroppable statuses) = Nothing
  | isJust mbPlate && TicketStatus.INPROGRESS `elem` statuses = Nothing
  | otherwise = Just mbPlate

shouldExpireDegraded :: Maybe Text -> [TicketStatus.FRFSTicketStatus] -> Bool -> Bool
shouldExpireDegraded mbPlate statuses markerAlive =
  isNothing mbPlate && TicketStatus.INPROGRESS `elem` statuses && not markerAlive

markDegradedBoarding :: (Redis.HedisFlow m r, MonadFlow m) => Int -> Id DBooking.FRFSTicketBooking -> Text -> m ()
markDegradedBoarding ttlSec bookingId typedCode = do
  now <- getCurrentTime
  shared $ Redis.setExp (degradedKey bookingId) DegradedBoarding {typedCode, at = now} ttlSec

clearDegradedMarker :: (Redis.HedisFlow m r, MonadFlow m) => Id DBooking.FRFSTicketBooking -> m ()
clearDegradedMarker bookingId = shared . void $ Redis.del (degradedKey bookingId)

isMarkerAlive :: (Redis.HedisFlow m r, MonadFlow m) => Id DBooking.FRFSTicketBooking -> m Bool
isMarkerAlive bookingId = isJust <$> shared (Redis.get @DegradedBoarding (degradedKey bookingId))

-- | The timeout end: marker gone + tickets still INPROGRESS => USED. Re-decided on a fresh read under the booking lock, so a boarding that
-- landed after the caller's read is never ended; the poll (and the degraded sweep) is the clock.
expireDegradedBoardingIfNeeded ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m, Redis.HedisFlow m r) =>
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
            pure True
          else pure False
