-- | With the city's driverAcceptEnabled a claim is an offer: the seat is held, the rider hears nothing, and the driver accepts or declines
-- within the offer timer. Accepting arms the timer the claim would have armed and sends the rider the pushes that were held back;
-- declining (or letting it lapse, in Allocation) closes the allocation as DriverDeclined / OfferTimeout.
module SharedLogic.SharedCab.Offer
  ( AcceptDecision (..),
    DeclineDecision (..),
    decideAccept,
    decideDecline,
    acceptOffer,
    declineOffer,
  )
where

import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.RouteStopMapping as Fields
import qualified Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.SharedCab.Allocation as Allocation
import SharedLogic.SharedCab.Allocation.Select (CabStance (..), ClaimPush (..), cabStance, isFreshPosition, pushForStance, timerForStance)
import SharedLogic.SharedCab.Allocation.Types
import SharedLogic.SharedCab.Booking (withBookingLock)
import SharedLogic.SharedCab.BookingState (readAlloc, writeAlloc)
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Lts as Lts
import qualified SharedLogic.SharedCab.Notify as Notify
import qualified SharedLogic.SharedCab.Seats as Seats
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (Session (..), ownedSession)
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import Tools.Error (SharedCabDriverActionError (..), SharedCabSessionError (SessionNotFound))

data AcceptDecision = AcceptNow AllocationState | AlreadyAccepted
  deriving (Show, Eq)

data DeclineDecision = DeclineNow | AlreadyClosed
  deriving (Show, Eq)

-- | The offer on this plate is still open (or already answered, which is a no-op). An offer that lapsed, or an allocation that is gone, cannot be accepted.
decideAccept :: UTCTime -> Text -> Maybe Text -> Maybe AllocationState -> Either SharedCabDriverActionError AcceptDecision
decideAccept now plate bookingPlate mbAlloc
  | isNothing bookingPlate = Left OfferExpired
  | bookingPlate /= Just plate = Left BookingNotOnThisCab
  | otherwise = case mbAlloc of
    Nothing -> Left OfferExpired
    Just st -> case st.timer of
      OfferTimer deadline
        | now <= deadline -> Right (AcceptNow st)
        | otherwise -> Left OfferExpired
      _ -> Right AlreadyAccepted

-- | Declining an open (even a lapsed) offer closes it; one already closed is a no-op; one the driver accepted is not an offer any more.
decideDecline :: Text -> Maybe Text -> Maybe AllocationState -> Either SharedCabDriverActionError DeclineDecision
decideDecline plate bookingPlate mbAlloc
  | isNothing bookingPlate = Right AlreadyClosed
  | bookingPlate /= Just plate = Left BookingNotOnThisCab
  | otherwise = case mbAlloc of
    Nothing -> Right AlreadyClosed
    Just st
      | isOffered st -> Right DeclineNow
      | otherwise -> Left BookingNotOffered

acceptOffer :: Text -> Text -> Id DFTB.FRFSTicketBooking -> Environment.Flow Session
acceptOffer driver plate bookingId = do
  (merchantId, accepted) <- Session.withPlateLock plate $ do
    s <- Session.readSession plate >>= either throwError pure . ownedSession driver
    fmap (s.merchantId,) . withBookingLock bookingId $ do
      booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM OfferExpired
      mbAlloc <- readAlloc bookingId
      now <- getCurrentTime
      cfg <- Config.getTunables booking.merchantOperatingCityId
      decision <- either throwError pure $ decideAccept now plate booking.vehicleNumber mbAlloc
      case decision of
        AlreadyAccepted -> pure Nothing
        AcceptNow st -> do
          stance <- cabStanceNow cfg now s booking.fromStationCode
          writeAlloc (allocKeyTtl cfg) bookingId st {timer = timerForStance cfg now stance}
          pure (Just (booking, stance, cfg))
  forM_ accepted $ \(booking, stance, cfg) -> case pushForStance stance of
    PushArriving -> Notify.notifyArriving plate cfg.standTimerSec booking
    PushAssigned -> Notify.notifyAssigned plate booking
  Session.getSession merchantId plate >>= fromMaybeM SessionNotFound
  where
    -- where the cab stands now decides the timer; without a fresh fix the wait is bounded like an away cab's
    cabStanceNow cfg now s boardStopCode = do
      mbTrack <- Lts.readRoutePosition s.routeCode plate
      stops <- Seats.routeStopRows s.integratedBppConfigId s.routeCode
      pure . fromMaybe CabAway $ do
        cab <- mbTrack
        guard (isFreshPosition now cfg.ltsMaxAgeSec cab)
        guard (isJust (Lts.etaAt boardStopCode cab)) -- the board stop is still ahead of it
        boardPoint <- (.stopPoint) <$> find ((== boardStopCode) . (.stopCode)) stops
        pure (cabStance cfg cab boardStopCode boardPoint)

declineOffer :: Text -> Text -> Id DFTB.FRFSTicketBooking -> Environment.Flow Session
declineOffer driver plate bookingId = do
  booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM BookingNotOnThisCab
  cfg <- Config.getTunables booking.merchantOperatingCityId
  Session.withPlateLock plate $ do
    void $ Session.readSession plate >>= either throwError pure . ownedSession driver
    fresh <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM BookingNotOnThisCab
    mbAlloc <- readAlloc bookingId
    decision <- either throwError pure $ decideDecline plate fresh.vehicleNumber mbAlloc
    -- closed under the plate lock acceptOffer also takes (plate -> booking order; the close takes no plate lock), so an accept
    -- cannot land between the check and the close
    when (decision == DeclineNow) . void $ Allocation.releaseSharedCabAllocation cfg bookingId plate DriverDeclined
  Session.getSession booking.merchantId plate >>= fromMaybeM SessionNotFound
  where
