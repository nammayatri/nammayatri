{-# LANGUAGE TypeApplications #-}

-- | The allocation tick and its entry points: read the city's bookings and cab positions, expire timers, claim.
module SharedLogic.SharedCab.Allocation
  ( InternalEndpointFlow,
    runSharedCabAllocationTick,
    triggerSharedCabAllocation,
    releaseSharedCabAllocation,
    skipSharedCabAllocation,
    releaseUnboarded,
    allocationPass,
  )
where

import qualified BecknV2.FRFS.Enums as Spec
import Data.List (nub)
import qualified Domain.Types.FRFSTicketBooking as DFTB
import Domain.Types.FRFSTicketBookingStatus (FRFSTicketBookingStatus (..))
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.SharedCab.Allocation.Claim (AllocFlow, InternalEndpointFlow, claimFirst, liveBookingsOf)
import SharedLogic.SharedCab.Allocation.Reallocate (closeAllocation)
import SharedLogic.SharedCab.Allocation.Select (FindingBooking (..), LiveBooking (..), allocatedPlate, eligibleCandidates, findingOf, groupAllOn, isFinding, movingCab, silentCab, skippedWhileFinding, withoutSkipped)
import SharedLogic.SharedCab.Allocation.Timers (expireTimers, pushTimeFallbacks)
import SharedLogic.SharedCab.Allocation.Types
import SharedLogic.SharedCab.Booking (partyOf, partySizes)
import SharedLogic.SharedCab.BookingState
import qualified SharedLogic.SharedCab.Config as Config
import SharedLogic.SharedCab.Enabled (sharedCabEnabled)
import SharedLogic.SharedCab.Lts (readRoutePositions)
import SharedLogic.SharedCab.Schedule (withCityTickLease)
import qualified SharedLogic.SharedCab.Session as Session
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking

cityLiveBookings ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m [LiveBooking]
cityLiveBookings cityId =
  QFRFSTicketBooking.findAllByMerchantOperatingCityIdAndServiceTierTypeAndStatus cityId (Just Spec.SHARED_CAB) [CONFIRMED] >>= liveBookingsOf

-- | Idempotent close: whoever loses the booking CAS emits nothing.
releaseSharedCabAllocation ::
  AllocFlow m r =>
  Config.SharedCabTunables ->
  Id DFTB.FRFSTicketBooking ->
  Text -> -- expected plate -- the cab we believe holds the booking
  AllocationOutcome ->
  m Bool
releaseSharedCabAllocation cfg bookingId expectedPlate outcome = do
  closed <- closeAllocation cfg bookingId expectedPlate outcome (pure ())
  whenJust closed (triggerSharedCabAllocation . (.closedCity))
  pure (isJust closed)

-- | Rider skip: the plate joins the skipped set in the same booking-lock hold, so no claim can re-offer it first.
skipSharedCabAllocation :: AllocFlow m r => Config.SharedCabTunables -> Id DFTB.FRFSTicketBooking -> Text -> SkipReason -> m Bool
skipSharedCabAllocation cfg bookingId plate reason = do
  let outcome = RiderSkipped reason
  closed <- closeAllocation cfg bookingId plate outcome (addSkipped (allocKeyTtl cfg) bookingId plate)
  whenJust closed (triggerSharedCabAllocation . (.closedCity))
  pure (isJust closed)

-- | Releases a leaving cab's unboarded allocations without penalty. Call after the session write, outside the plate lock.
releaseUnboarded :: AllocFlow m r => Text -> AllocationOutcome -> m ()
releaseUnboarded plate outcome = do
  live <- QFRFSTicketBooking.findAllByVehicleNumberAndServiceTierTypeAndStatus (Just plate) (Just Spec.SHARED_CAB) [CONFIRMED] >>= liveBookingsOf
  let unboarded = [lb.booking | lb <- live, isJust (allocatedPlate lb)]
  cities <- forM unboarded $ \b -> do
    cfg <- Config.getTunables b.merchantOperatingCityId
    closeAllocation cfg b.id plate outcome (pure ())
  mapM_ triggerSharedCabAllocation (nub (map (.closedCity) (catMaybes cities)))

triggerSharedCabAllocation ::
  AllocFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  m ()
triggerSharedCabAllocation cityId =
  void $ fork "shared-cab-allocation-trigger" $ runSharedCabAllocationTick cityId

runSharedCabAllocationTick ::
  AllocFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  m ()
runSharedCabAllocationTick cityId = withCityTickLease cityId . void $ allocationPass cityId

type RoutePositions = [(Text, [LT.VehicleTrackingOnRouteResp])]

-- | One pass of the tick: read the city, close what has run out, then (if the city is on) claim cabs for the bookings still finding one.
-- A suspended city drains (timers, and the job's timeouts and refunds) but allocates nothing new.
allocationPass ::
  AllocFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  m ([LiveBooking], RoutePositions)
allocationPass cityId = do
  cfg <- Config.getTunables cityId
  enabled <- sharedCabEnabled cityId
  now <- getCurrentTime
  (live, positionsByRoute) <- readCity cityId
  closeExpired cfg now live positionsByRoute
  when enabled $ claimFindings cfg now live positionsByRoute
  pure (live, positionsByRoute)

-- | The city's live bookings and the LTS positions of every route that has one waiting for, or holding, a cab.
readCity :: AllocFlow m r => Id DMOC.MerchantOperatingCity -> m ([LiveBooking], RoutePositions)
readCity cityId = do
  live <- cityLiveBookings cityId
  let routes = nub [route | lb <- live, isFinding lb || isJust (allocatedPlate lb), Just route <- [lb.booking.routeCode]]
  positionsByRoute <- fmap catMaybes . forM routes $ \routeCode ->
    try (readRoutePositions routeCode) >>= \case
      -- Known limitation: an LTS outage is not told apart from every cab being silent. A failed read skips the route, which is
      -- fail-safe: no allocation from stale data, no stand timer cleared.
      Left (e :: SomeException) -> Nothing <$ logError ("shared-cab tick: LTS read failed for route " <> routeCode <> ": " <> show e)
      Right positions -> pure (Just (routeCode, positions))
  pure (live, positionsByRoute)

closeExpired :: AllocFlow m r => Config.SharedCabTunables -> UTCTime -> [LiveBooking] -> RoutePositions -> m ()
closeExpired cfg now live positionsByRoute = expireTimers cfg now movingOn silentOn live
  where
    onRoute route = fromMaybe [] (lookup route positionsByRoute)
    movingOn plate route = movingCab now cfg.ltsMaxAgeSec plate (onRoute route)
    silentOn plate route = silentCab now cfg.ltsMaxAgeSec plate (onRoute route)

claimFindings :: AllocFlow m r => Config.SharedCabTunables -> UTCTime -> [LiveBooking] -> RoutePositions -> m ()
claimFindings cfg now live positionsByRoute = do
  parties <- partySizes (map (.booking) (filter isFinding live)) -- one query for the tick; a party needs all its seats
  findings <- forM (mapMaybe (\lb -> (lb.booking,) <$> findingOf (partyOf parties) lb) live) $ \(b, fb) -> do
    since <- readFindingSince fb.bookingId fb.findingSince
    pure (b, fb {findingSince = since})
  pushTimeFallbacks cfg now findings
  forM_ (groupAllOn (.routeCode) (map snd findings)) $ \(routeCode, bookings) ->
    whenJust (lookup routeCode positionsByRoute) $ \positions -> do
      sessions <- Session.activeSessionsOnRoute routeCode
      forM_ bookings $ \booking -> do
        attempts <- readAttempts booking.bookingId
        skipped <- skippedWhileFinding cfg now attempts booking.findingSince <$> readSkipped booking.bookingId
        void $ claimFirst cfg booking (withoutSkipped skipped (eligibleCandidates now cfg positions sessions booking))
