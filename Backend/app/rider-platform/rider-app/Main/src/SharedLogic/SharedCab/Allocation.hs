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
    readRoutePositions,
    readRoutePosition,
  )
where

import qualified BecknV2.FRFS.Enums as Spec
import qualified Data.Aeson as A
import Data.List (nub)
import qualified Data.Text as T
import qualified Domain.Types.FRFSTicketBooking as DFTB
import Domain.Types.FRFSTicketBookingStatus (FRFSTicketBookingStatus (..))
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.SharedCab.Allocation.Claim (AllocFlow, InternalEndpointFlow, claimFirst, readAttempts, readFindingSince)
import SharedLogic.SharedCab.Allocation.Reallocate (afterClose, closeLocked)
import SharedLogic.SharedCab.Allocation.Select (FindingBooking (..), allocatedPlate, findingOf, groupAllOn, movingCab, planRouteAllocation, silentCab, skippedWhileFinding, withoutSkipped)
import SharedLogic.SharedCab.Allocation.Timers (expireTimers, pushTimeFallbacks)
import SharedLogic.SharedCab.Allocation.Types
import SharedLogic.SharedCab.Booking (partyOf, partySizes, shared, withBookingLock)
import qualified SharedLogic.SharedCab.Config as Config
import SharedLogic.SharedCab.Enabled (sharedCabEnabled)
import SharedLogic.SharedCab.Keys (attemptsKey, skippedKey)
import SharedLogic.SharedCab.Schedule (withCityTickLease)
import qualified SharedLogic.SharedCab.Session as Session
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking

cityLiveBookings ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m [(DFTB.FRFSTicketBooking, [DFRFSTicket.FRFSTicketStatus])]
cityLiveBookings cityId = do
  bookings <- QFRFSTicketBooking.findAllByMerchantOperatingCityIdAndServiceTierTypeAndStatus cityId (Just Spec.SHARED_CAB) [CONFIRMED]
  tickets <- if null bookings then pure [] else QFRFSTicket.findAllByTicketBookingIds (map (.id) bookings)
  pure [(b, [t.status | t <- tickets, t.frfsTicketBookingId == b.id]) | b <- bookings]

-- | The scheduler env has no ltsCfg for the HTTP client, so the tick reads the LTS hash directly (FRFSUtils.trackVehicles uses
-- bus next-stop logic, wrong for cabs). Fields decode one by one: a bad one is dropped, not the route.
readRoutePositions ::
  (MonadFlow m, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, Log m) =>
  Text ->
  m [LT.VehicleTrackingOnRouteResp]
readRoutePositions routeCode = do
  pairs <- Redis.runInMultiCloudLTSRedisForListFromReplica $ Redis.hGetAll @A.Value ("route:" <> routeCode)
  fmap catMaybes . forM pairs $ \(plate, raw) -> case A.fromJSON raw of
    A.Success info -> pure (Just (LT.VehicleTrackingOnRouteResp plate info))
    A.Error err -> Nothing <$ logWarning ("shared-cab tick: dropping LTS field " <> plate <> " on route " <> routeCode <> ": " <> T.pack err)

readRoutePosition ::
  (MonadFlow m, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Text ->
  Text ->
  m (Maybe LT.VehicleTrackingOnRouteResp)
readRoutePosition routeCode plate = do
  fields <- Redis.runInMultiCloudLTSRedisForListFromReplica $ maybeToList <$> Redis.hGet @A.Value ("route:" <> routeCode) plate
  pure $ case listToMaybe fields of
    Just raw | A.Success info <- A.fromJSON raw -> Just (LT.VehicleTrackingOnRouteResp plate info)
    _ -> Nothing

-- | Idempotent close: whoever loses the booking CAS emits nothing.
releaseSharedCabAllocation ::
  AllocFlow m r =>
  Config.SharedCabTunables ->
  Id DFTB.FRFSTicketBooking ->
  Text -> -- expected plate -- the cab we believe holds the booking
  AllocationOutcome ->
  m Bool
releaseSharedCabAllocation cfg bookingId expectedPlate outcome = do
  closed <- withBookingLock bookingId $ closeLocked cfg bookingId expectedPlate outcome
  afterClose cfg bookingId expectedPlate outcome closed
  whenJust closed (triggerSharedCabAllocation . (.closedCity))
  pure (isJust closed)

-- | Rider skip: the plate joins the skipped set in the same booking-lock hold, so no claim can re-offer it first.
skipSharedCabAllocation :: AllocFlow m r => Config.SharedCabTunables -> Id DFTB.FRFSTicketBooking -> Text -> SkipReason -> m Bool
skipSharedCabAllocation cfg bookingId plate reason = do
  let outcome = RiderSkipped reason
  closed <- withBookingLock bookingId $ do
    closedInfo <- closeLocked cfg bookingId plate outcome
    when (isJust closedInfo) $ shared $ Redis.sAddExp (skippedKey bookingId) [plate] cfg.findingTimeoutSec
    pure closedInfo
  afterClose cfg bookingId plate outcome closed
  whenJust closed (triggerSharedCabAllocation . (.closedCity))
  pure (isJust closed)

-- | Releases a leaving cab's unboarded allocations without penalty. Call after the session write, outside the plate lock.
releaseUnboarded :: AllocFlow m r => Text -> AllocationOutcome -> m ()
releaseUnboarded plate outcome = do
  bookings <- QFRFSTicketBooking.findAllByVehicleNumberAndServiceTierTypeAndStatus (Just plate) (Just Spec.SHARED_CAB) [CONFIRMED]
  tickets <- if null bookings then pure [] else QFRFSTicket.findAllByTicketBookingIds (map (.id) bookings)
  let unboarded = [b | b <- bookings, isJust (allocatedPlate (b, [t.status | t <- tickets, t.frfsTicketBookingId == b.id]))]
  cities <- forM unboarded $ \b -> do
    cfg <- Config.getTunables b.merchantOperatingCityId
    closed <- withBookingLock b.id $ closeLocked cfg b.id plate outcome
    afterClose cfg b.id plate outcome closed
    pure closed
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

allocationPass ::
  AllocFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  m ([(DFTB.FRFSTicketBooking, [DFRFSTicket.FRFSTicketStatus])], [(Text, [LT.VehicleTrackingOnRouteResp])])
allocationPass cityId = do
  cfg <- Config.getTunables cityId
  enabled <- sharedCabEnabled cityId
  live <- cityLiveBookings cityId
  now <- getCurrentTime
  let routes = nub [route | entry@(b, _) <- live, isJust (findingOf entry) || isJust (allocatedPlate entry), Just route <- [b.routeCode]]
  positionsByRoute <- fmap catMaybes . forM routes $ \routeCode ->
    try (readRoutePositions routeCode) >>= \case
      -- TODO: "LTS outage != everyone silent": most sessions of a city stale in the same minute must suspend the freshness
      -- gate. Skipping the route is fail-safe: no allocation from stale data, no stand timer cleared.
      Left (e :: SomeException) -> Nothing <$ logError ("shared-cab tick: LTS read failed for route " <> routeCode <> ": " <> show e)
      Right positions -> pure (Just (routeCode, positions))
  let onRoute route = fromMaybe [] (lookup route positionsByRoute)
      movingOn plate route = movingCab now cfg.ltsMaxAgeSec plate (onRoute route)
      silentOn plate route = silentCab now cfg.ltsMaxAgeSec plate (onRoute route)
  expireTimers cfg now movingOn silentOn live
  let findingEntries = mapMaybe (\entry -> (fst entry,) <$> findingOf entry) live
  parties <- partySizes (map fst findingEntries) -- one query for the tick; a party needs all its seats
  findings <- forM findingEntries $ \(b, fb) -> do
    since <- readFindingSince fb.bookingId fb.findingSince
    pure (b, fb {findingSince = since, seats = partyOf parties b})
  -- a suspended city drains (timers above, timeouts and refunds in the job) but allocates nothing new
  when enabled $ do
    pushTimeFallbacks cfg now findings
    forM_ (groupAllOn (.routeCode) (map snd findings)) $ \(routeCode, bookings) ->
      whenJust (lookup routeCode positionsByRoute) $ \positions -> do
        sessions <- Session.activeSessionsOnRoute routeCode
        forM_ bookings $ \booking -> do
          attempts <- readAttempts (attemptsKey booking.bookingId)
          skipped <- skippedWhileFinding cfg now attempts booking.findingSince <$> shared (Redis.sMembers (skippedKey booking.bookingId))
          void $ claimFirst cfg booking (withoutSkipped skipped (planRouteAllocation now cfg positions sessions booking))
  pure (live, positionsByRoute)
