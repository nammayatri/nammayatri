{-
  Shared-cab boarding engine — M8.1..M8.3 SKELETON (build logic follows in the boarding tasks).
  Design sources (Plans/Shared-Cab-Plans):
    05-allocation-plan.md §4   — boarding code & re-bind (endpoint, INPROGRESS-not-USED, CAS, events)
    05-allocation-plan.md §8   — hardening (proximity ladder, one generic error, CAS, corridor re-bind)
    08-build-tasks.md M8       — 8.1 code = plate last-4 -> ACTIVE cab; ticket INPROGRESS;
                                  vehicleTripId set; leg finalBoardedBusNumber
                               — 8.2 proximity ladder: stream -> request lat/lon -> allocated/spot only
                               — 8.3 re-bind: another ACTIVE cab on a serving route, target lock,
                                  old cab freed, no driver penalty

  REUSE points (backend/feat/shared-cab-booking landed — canonical helpers imported):
    * SharedLogic.SharedCab.Booking.withBookingLock (key sharedcab:lock:booking:{bookingId}, 10s;
      NON-REENTRANT — never call release / re-bind / cancelLeg under it). IMPORTED there.
    * SharedLogic.SharedCab.Booking.isSharedCabBooking — IMPORTED there.
    * SharedLogic.SharedCab.Session.activeSessionsOnRoute — the canonical copy lives in Session.
    * SharedLogic.SharedCab.LegState (deriveSharedCabState) + LegStatus.sharedCab{state, vehicleNumber,
      vehicleModel, driverName, driverPhotoUrl, etaToBoardStopSec, etaToDropStopSec, cabsComing} —
      read-side only; the caller's final `getAllLegsInfo` picks these up.
-}
module SharedLogic.SharedCab.Boarding
  ( SharedCabBoardOutcome (..),
    BoardingSummary (..),
    boardingCodeMatches,
    UnknownCodeStep (..),
    unknownCodeStep,
    forceHonoured,
    tryBoardSharedCab,
    seatCheck,
    canBoard,
  )
where

import qualified API.Types.UI.MultimodalConfirm as API
import Control.Applicative ((<|>))
import Data.List (nub, sortOn)
import qualified Data.Text as T
import qualified Domain.Types.FRFSTicketBooking as DBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DBookingStatus
import qualified Domain.Types.FRFSTicketStatus as TicketStatus
import qualified Domain.Types.Journey as DJourney
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Person as DPerson
import qualified Environment
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Location as JMLocation
import qualified SharedLogic.External.LocationTrackingService.Flow as LTSFlow
import SharedLogic.SharedCab.Booking (isSharedCabBooking, liveSeatsOnVehicle, shared, withBookingLock)
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Degraded as Degraded
import qualified SharedLogic.SharedCab.Events as Events
import SharedLogic.SharedCab.Plate (canonicalisePlate)
import qualified SharedLogic.SharedCab.RateLimit as RateLimit
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState
import qualified Storage.CachedQueries.IntegratedBPPConfig as CQIBC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QBooking
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import Tools.Error

-- | What the caller (postMultimodalOrderSublegSetOnboardedVehicleDetails) renders.
data SharedCabBoardOutcome
  = -- | Boarding committed: ticket INPROGRESS, booking pointed at the cab, event emitted.
    SharedCabBoarded BoardingSummary
  | -- | 8.5 (05 §5): the typed code matched no ACTIVE cab and the rider confirmed (forceCheckIn) —
    -- boarded in degraded mode: ticket INPROGRESS, degrade marker set, no session / driver card / seat effect.
    SharedCabDegraded
  | -- | Soft proximity miss: caller turns this into `boardingConfirmationRequired = True`
    -- (a forceCheckIn retry is honoured for the *allocated* cab or a booking with no cab — 05 §4 item 3).
    SharedCabProximityHold (Maybe Double) Text
  deriving (Show, Generic)

data BoardingSummary = BoardingSummary
  { boardedSession :: Session,
    -- | 8.3: the cab the seat was released from, for the `rebound` event / audit. Never penalised.
    reboundFrom :: Maybe Session,
    -- | Boarded without any rider location (allowed for the allocated cab / direct board only;
    -- feeds the per-vehicle ops flag — 05 §8.1; the rate limit is task 8.6).
    boardedWithoutLocation :: Bool
  }
  deriving (Show, Generic)

-- | Code = plate last-4 (05 §4). A full plate also matches (retry path after an ambiguous code).
boardingCodeMatches :: Text -> Text -> Bool
boardingCodeMatches code plate
  | T.length code <= 4 = canonicalisePlate code `T.isSuffixOf` canonicalisePlate plate
  | otherwise = canonicalisePlate code == canonicalisePlate plate

-- | An unknown code never boards on its own: a typo would otherwise consume the ticket (INPROGRESS blocks cancel).
-- The rider is asked to confirm an unlisted cab; the retry with forceCheckIn is the explicit confirm.
data UnknownCodeStep = AskToConfirmUnlisted | BoardUnlisted
  deriving (Show, Eq)

unknownCodeStep :: Maybe Bool -> UnknownCodeStep
unknownCodeStep forceCheckIn
  | forceCheckIn == Just True = BoardUnlisted
  | otherwise = AskToConfirmUnlisted

-- | forceCheckIn skips the proximity check for the allocated cab, and for a booking with no cab yet (FINDING/FALLBACK:
-- "board any cab"). Never for a re-bind: that can't be forced from home.
forceHonoured :: Maybe Bool -> Maybe Text -> Text -> Bool
forceHonoured forceCheckIn bookingPlate targetPlate =
  forceCheckIn == Just True && maybe True (== targetPlate) bookingPlate

-- ---------------- code resolution (8.1) ----------------

-- | Routes that may host this rider's cab: the booking's route plus its corridor sibling
-- (05 §8.8 corridor re-bind; pairing rule from SessionState.returnRouteOf).
candidateRoutes :: DBooking.FRFSTicketBooking -> [Text]
candidateRoutes booking =
  nub $
    maybeToList booking.routeCode
      ++ foldMap (\c -> either (const []) (: []) (returnRouteOf c)) booking.routeCode

-- | ACTIVE sessions on the candidate routes whose plate matches the code AND whose route's CURRENT
-- stop order still serves board->drop, in order (05 §8.10 stale-feed rule).
resolveCabByCode :: DBooking.FRFSTicketBooking -> Text -> Environment.Flow [Session]
resolveCabByCode booking code = do
  sessions <- concat <$> mapM Session.activeSessionsOnRoute (candidateRoutes booking)
  let candidates = filter (\s -> boardingCodeMatches code s.vehicleNumber) sessions
  catMaybes <$> mapM (serveCheck) candidates
  where
    serveCheck s = do
      ok <- sessionServesBoardDrop booking s
      pure $ if ok then Just s else Nothing

sessionServesBoardDrop :: DBooking.FRFSTicketBooking -> Session -> Environment.Flow Bool
sessionServesBoardDrop booking session = do
  integratedBppConfig <- CQIBC.findById session.integratedBppConfigId >>= fromMaybeM BoardingFailed
  stops <- OTPRest.getRouteStopMappingByRouteCode session.routeCode integratedBppConfig
  let seqOf c = (\s -> s.sequenceNum) <$> find (\s -> s.stopCode == c) stops
  pure $ case (seqOf booking.fromStationCode, seqOf booking.toStationCode) of
    (Just fromSeq, Just toSeq) -> fromSeq < toSeq
    _ -> False

-- ---------------- proximity ladder (8.2) ----------------

-- | Cab position from the raw LTS client (05 decision 8: this engine never goes through the bus
-- in-memory tracker / getBusLiveInfo — cabs exist only in LTS route:{code}).
lookupCabPosition :: Session -> Environment.Flow (Maybe LatLong)
lookupCabPosition session = do
  outcome <-
    withTryCatch "lookupCabPosition" $
      LTSFlow.vehicleTrackingOnRoute (LTSFlow.ByRoute session.routeCode)
  pure $ case outcome of
    Left _ -> Nothing -- LTS outage behaves identically to NO_LIVE_CAB_DATA (8.2 ladder floor)
    Right tracked ->
      find (\v -> canonicalisePlate v.vehicleNumber == session.vehicleNumber) tracked
        <&> (\v -> LatLong v.vehicleInfo.latitude v.vehicleInfo.longitude)

-- TODO(8.2 completion): freshness gate on VehicleInfo.timestamp vs ltsMaxAgeSec (05 §3) BEFORE the
-- position is trusted; a stale cab position must behave like NO_LIVE_CAB_DATA.

-- | Left (distance, holdReason) -> proximity hold; Right (distance, boardedWithoutLocation) -> pass.
-- Ladder (agreed, shared_session 2026-09-23 18:35; 05 §4):
--   1. journey location stream (`locations:{journeyId}`, Location.getAllPoints)
--   2. request lat/lon — OnboardedVehicleDetailsReq fields PENDING a MultiModal.yaml addition
--      (deliberately NOT in this commit; spot/allocated flows until then fall to step 3)
--   3. no location: allocated cab / direct board pass flagged; re-bind is a hard fail (M8.2 done-when).
-- `boardProximityM` is Config's (150 m default); boardingMatchRadiusInMeters (bus, 30 m) is deliberately NOT reused.
checkRiderNearSharedCab ::
  Int -> -- boardProximityM
  Session ->
  DJourney.Journey ->
  DBooking.FRFSTicketBooking ->
  Bool -> -- isRebind
  Environment.Flow (Either (Maybe Double, Text) (Maybe Double, Bool))
checkRiderNearSharedCab boardProximityM session journey booking isRebind = do
  riderHistory <- JMLocation.getAllPoints journey.id
  mbAnchor <- (<|>) <$> lookupCabPosition session <*> pure booking.fromStationPoint
  now <- getCurrentTime
  let freshRiderPoints = filter (\p -> abs (diffUTCTime now p.currTime) < 300) riderHistory
      newestRiderPoint = listToMaybe (reverse (sortOn (\p -> p.currTime) freshRiderPoints))
  case mbAnchor of
    Nothing -> pure $ Left (Nothing, "NO_CAB_POSITION")
    Just anchor ->
      case newestRiderPoint of
        Just riderPoint -> do
          let d :: Double = realToFrac (highPrecMetersToMeters (distanceBetweenInMeters riderPoint.latLong anchor))
          pure $
            if d <= fromIntegral boardProximityM
              then Right (Just d, False)
              else Left (Just d, "TOO_FAR_FROM_CAB")
        Nothing
          | isRebind -> throwError BoardingLocationRequired -- M8.2 done-when: hard fail
          | otherwise -> pure $ Right (Nothing, True)

-- ---------------- degraded boarding (8.5) ----------------

-- | 05 §5: the rider typed a code that matches no ACTIVE shared cab on their route. NEVER blocked
-- (PRD §11.4): ticket -> INPROGRESS, degrade marker with the degradedTimeoutMin TTL, ops event.
-- No driver card and no seat effect — the driver records the rider as a walk-up. The ride ends on
-- rider confirm ("I got down") or when the marker expires (Boarding.Degraded.expireDegradedIfNeeded).
-- Decided on a fresh read under the booking lock (Degraded.planDegrade): a booking riding a real cab, or with no
-- ticket still held, is refused; an allocated one gives its cab back first (plate CAS -> null, alloc key cleared),
-- so a degraded ride never counts as a seat on the cab it was allocated to.
degradedBoarding :: Int -> DBooking.FRFSTicketBooking -> Text -> Environment.Flow ()
degradedBoarding degradedTimeoutSec booking typedCode =
  withBookingLock booking.id $ do
    fresh <- QBooking.findById booking.id >>= fromMaybeM BoardingFailed
    tickets <- QTicket.findAllByTicketBookingId booking.id
    markerAlive <- Degraded.isMarkerAlive booking.id
    mbAllocatedPlate <- Degraded.planDegrade fresh.status fresh.vehicleNumber (map (.status) tickets) markerAlive & fromMaybeM BoardingFailed
    whenJust mbAllocatedPlate $ \plate -> do
      -- TODO(7.6): Events allocation_closed {outcome: degraded, blame: none} for the released allocation.
      QBooking.updateAllocatedVehicle Nothing booking.id (Just plate)
      shared . void $ Redis.del (allocKey booking.id)
    forM_ (filter ((== TicketStatus.ACTIVE) . (.status)) tickets) $ \t ->
      QTicket.updateStatusByTBookingIdAndTicketNumber TicketStatus.INPROGRESS t.scannedByVehicleNumber booking.id t.ticketNumber
    Degraded.markDegradedBoarding degradedTimeoutSec booking.id typedCode
    Events.forBooking Events.DegradedBoarding booking

-- ---------------- commit (8.1 + 8.3) ----------------

-- Lock order when two locks are needed (05 §2): target-cab plate lock first, then the booking lock.

-- | 05 §2: the allocation engine's per-booking record. Cleared on boarding — a boarded booking is
-- terminal for allocation; `attempts` is NOT bumped on re-bind (old cab gets no penalty, PRD §11.3).
allocKey :: Id DBooking.FRFSTicketBooking -> Text
allocKey bookingId = "sharedcab:alloc:" <> bookingId.getId

-- | One atomic board/re-bind. Either everything below was observed or the boarding never happened.
commitBoarding ::
  DJourneyLeg.JourneyLeg ->
  DBooking.FRFSTicketBooking ->
  Session -> -- target cab
  Maybe Session -> -- old cab, iff re-bind
  Environment.Flow ()
commitBoarding journeyLeg booking target mbOld =
  let plate = canonicalisePlate target.vehicleNumber
   in Session.withPlateLock plate $ do
        -- target-cab lock FIRST (05 §2 lock order)
        withBookingLock booking.id $ do
          -- Re-validate under locks: the cab may have switched route / ended between resolve and here.
          -- readSession, not getSession: getSession may rebuild the session under this same (non-re-entrant) plate lock.
          fresh <- Redis.withMasterRedis (Session.readSession plate) >>= fromMaybeM BoardingFailed
          unless (fresh.status == ACTIVE) $ throwError BoardingFailed
          -- SEAT GUARD (05 §4 option A): capacity − walkups − app-held seats ≥ this booking's
          -- seats (one FRFSTicket row = one seat), recomputed UNDER the plate lock. Refuses
          -- clearly as CabFull; the rider moves to the next cab or re-books.
          held <- liveSeatsOnVehicle plate
          myTickets <- QTicket.findAllByTicketBookingId booking.id
          -- settle on ONE eligibility set (also the flip list): ACTIVE or INPROGRESS only.
          let eligible = [t | t <- myTickets, t.status `elem` [TicketStatus.ACTIVE, TicketStatus.INPROGRESS]]
          when (null eligible) $ throwError BoardingFailed
          -- fresh read under the booking lock: the R11 owner was verified pre-lock; re-verify state + our own occupancy.
          freshBooking <- QBooking.findById booking.id >>= fromMaybeM BoardingFailed
          unless (freshBooking.status == DBookingStatus.CONFIRMED) $ throwError BoardingFailed
          unless (canBoard fresh.capacity fresh.walkupCount held (freshBooking.vehicleNumber == Just plate) (length eligible)) $
            throwError CabFull
          -- Ticket FIRST: INPROGRESS — never postFrfsTicketVerify, which marks USED and the journey
          -- layer reads USED as leg completed (05 §2 "Why not USED at boarding").
          -- FLIP ONLY the eligible set (BLOCKER-3): CANCELLED/USED tickets stay put.
          forM_ eligible $ \t ->
            QTicket.updateStatusByTBookingIdAndTicketNumber TicketStatus.INPROGRESS (Just fresh.vehicleNumber) booking.id t.ticketNumber
          -- CAS vehicleNumber (spec/Storage/FrfsTicket.yaml:656-666). Expected = what we read at
          -- request time; a racing allocate/close makes this write a no-op.
          -- TODO(M8.1 completion): KV read-back + "who won" check per the yaml comment —
          -- frfs_ticket_booking is KV-enabled in prod (05 §2, review R8).
          QBooking.updateAllocatedVehicle (Just fresh.vehicleNumber) booking.id booking.vehicleNumber
          -- Ride the driver's ACTIVE run row: driver trips/history joins on this (04 §3a).
          QBooking.updateVehicleTripId (Just fresh.vehicleTripId) booking.id
          -- Old cab's seat release = derived (05 decision 6): the booking row now counts against the new
          -- cab, so the old cab's seat frees itself. Explicitly do NOT: bump session.consecutiveMisses,
          -- bump alloc attempts, emit allocation_closed{blame: driver} — no driver penalty (8.3).
          shared . void $ Redis.del (allocKey booking.id)
          Degraded.clearDegradedMarker booking.id
          -- Ledger entries on the booking row (bus-branch parity, MultimodalConfirm.hs ~3059 analytics sync).
          fork "SharedCab: sync boarded vehicle data to ticket booking" $
            QBooking.updateFRFSTicketBookingVehicleDataById
              (Just fresh.vehicleNumber)
              (Just DJourneyLeg.UserActivated)
              Nothing -- waybill: bus-fleet concept, cabs have none
              Nothing -- scheduleNo
              Nothing -- depot
              booking.serviceTierType
              Nothing -- conductorId
              (Just fresh.driverId)
              Nothing -- driverName: join driver-app later
              Nothing -- driverMobileNumber
              booking.id
          -- Leg fields mean "boarded": written at boarding / re-bind only, never at allocation (05 §2).
          QJourneyLeg.updateByPrimaryKey $
            journeyLeg
              { DJourneyLeg.finalBoardedBusNumber = Just fresh.vehicleNumber,
                DJourneyLeg.finalBoardedBusNumberSource = Just DJourneyLeg.UserActivated,
                DJourneyLeg.finalBoardedBusServiceTierType = booking.serviceTierType
              }
          -- NOTE(spec, not migrated): BusBoardingMethod has no SHARED_CAB value on this base;
          -- UserActivated is the truthful closest fit. A dedicated value needs spec/Storage/MultiModal.yaml.
          now <- getCurrentTime
          let event k = Events.withTrip (Just fresh.vehicleTripId.getId) $ Events.bookingEvent k booking.id.getId (Just fresh.vehicleNumber) (Just fresh.routeCode) now
          Events.emit fresh.merchantOperatingCityId . event $ case mbOld of
            Nothing -> Events.Boarded Events.ByCode
            Just old -> Events.Rebound old.vehicleNumber (old.routeCode /= fresh.routeCode)

-- ---------------- entry point ----------------

-- | The SHARED_CAB branch of postMultimodalOrderSublegSetOnboardedVehicleDetails.
-- `Nothing` means "not our tier": the caller falls through to the bus/metro body unchanged.
tryBoardSharedCab ::
  DJourney.Journey ->
  DJourneyLeg.JourneyLeg ->
  DBooking.FRFSTicketBooking ->
  Maybe (Id DPerson.Person) ->
  API.OnboardedVehicleDetailsReq ->
  Environment.Flow (Maybe SharedCabBoardOutcome)
tryBoardSharedCab _ _ booking _ _
  | not (isSharedCabBooking booking) = pure Nothing
tryBoardSharedCab journey journeyLeg booking mbPersonId req = do
  -- R11 (05 §8.1): the caller must own the booking.
  personId <- mbPersonId & fromMaybeM BoardingFailed
  unless (booking.riderId == personId) $ throwError BoardingFailed
  -- BLOCKER-3: CONFIRMED-only boarding; terminal bookings refuse, never resurrect.
  unless (booking.status == DBookingStatus.CONFIRMED) $ throwError BoardingFailed
  -- 8.6 (05 §8.1): ≤ boardAttemptsPer10Min; the limit trips the same generic error as a wrong code.
  tunables <- Config.getTunables booking.merchantOperatingCityId
  RateLimit.enforceBoardingAttemptLimit tunables.boardAttemptsPer10Min booking.id
  code <- req.vehicleNumber & fromMaybeM BoardingFailed
  matches <- resolveCabByCode booking code
  case matches of
    -- 05 §5 degraded boarding, only on an explicit confirm; otherwise the same hold response as the proximity miss.
    [] -> case unknownCodeStep req.forceCheckIn of
      AskToConfirmUnlisted -> pure . Just $ SharedCabProximityHold Nothing "UNLISTED_CAB"
      BoardUnlisted -> degradedBoarding tunables.degradedTimeoutSec booking code >> pure (Just SharedCabDegraded)
    [target] -> Just <$> boardMatched tunables target
    _ -> throwError BoardingCodeAmbiguous -- two match: ask for the full plate (05 §4)
  where
    boardMatched tunables target = do
      let isAllocatedCab = booking.vehicleNumber == Just target.vehicleNumber
          isRebind = isJust booking.vehicleNumber && not isAllocatedCab
          -- 8.2: forceCheckIn reopens re-bind-from-home without this; honour it only for the allocated cab or no cab yet.
          forced = forceHonoured req.forceCheckIn booking.vehicleNumber target.vehicleNumber
      mbCase <-
        if forced
          then pure $ Right (Nothing, False)
          else checkRiderNearSharedCab tunables.boardProximityM target journey booking isRebind
      case mbCase of
        Left (mbDist, reason) -> pure $ SharedCabProximityHold mbDist reason
        Right (_mbDist, noLocation) -> do
          -- 8.3: old cab (if any) — for the `rebound` event; may already be ENDED, which is fine here.
          mbOld <-
            if isRebind
              then join <$> traverse Session.getSession booking.vehicleNumber
              else pure Nothing
          commitBoarding journeyLeg booking target mbOld
          -- 8.6 (05 §8.1): a boarding with no rider location counts against the VEHICLE's IST-day
          -- counter — a driver feeding codes to non-app riders shows up per vehicle, not per rider.
          when noLocation $
            void $ RateLimit.recordNoLocationBoarding tunables.noLocationSpotBookingsPerVehiclePerDay (canonicalisePlate target.vehicleNumber)
          -- TODO(M8 completion): re-fare if the re-bound route differs (05 §4 corridor re-bind row).
          pure $
            SharedCabBoarded
              BoardingSummary
                { boardedSession = target,
                  reboundFrom = mbOld,
                  boardedWithoutLocation = noLocation
                }

-- | 05 §4 option A: seats left after walk-ups and other bookings' held seats cover this booking's.
seatCheck :: Int -> Int -> Int -> Int -> Bool
seatCheck capacity walkups heldNet need = capacity - walkups - heldNet >= need

-- | `held` is liveSeatsOnVehicle, which already counts this booking's seats when it is on this plate
-- (allocated here, or re-entering the cab it boarded); those are credited back, not asked for twice.
-- e.g. capacity 4, 3 walk-ups, 1 held seat that is ours: 4 - 3 - (1 - 1) >= 1.
canBoard :: Int -> Int -> Int -> Bool -> Int -> Bool
canBoard capacity walkups held onThisPlate need = seatCheck capacity walkups (held - own) need
  where
    own = if onThisPlate then need else 0
