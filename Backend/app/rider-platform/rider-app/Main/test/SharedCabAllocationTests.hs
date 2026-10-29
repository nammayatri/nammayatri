{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabAllocationTests (tests) where

import qualified "rider-app" API.Types.UI.MultimodalConfirm as ApiTypes
import qualified "rider-app" API.Types.UI.SharedCab as API
import "beckn-spec" BecknV2.FRFS.Enums (ServiceTierType (AC))
import Control.Exception (IOException, throwIO, try)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Data.Time.Format.ISO8601 (iso8601Show)
import "rider-app" Domain.Action.UI.SharedCab (skipReason)
import qualified "beckn-spec" Domain.Types.FRFSTicketBookingStatus as BS
import qualified "beckn-spec" Domain.Types.FRFSTicketStatus as TS
import "mobility-core" Kernel.External.Maps.Types (LatLong (..))
import "mobility-core" Kernel.Types.Id (Id (..))
import qualified "rider-app" SharedLogic.External.LocationTrackingService.Types as LT
import "rider-app" SharedLogic.SharedCab.Allocation (eachIsolated, mergeLive)
import "rider-app" SharedLogic.SharedCab.Allocation.Claim (ClaimMiss (..), claimGate)
import "rider-app" SharedLogic.SharedCab.Allocation.Select (CabStance (..), ClaimPush (..), FindingBooking (..), Phase (..), PreferenceLoss (..), PreferenceVerdict (..), RankedCandidate (..), armedTimer, cabForId, claimPush, claimTimer, claimable, closable, crossedMaxAttempts, eligibleCandidates, etaPlausible, isFreshPosition, isRescheduledChild, movingCab, onlyPreferred, phase, preferenceVerdict, silentCab, silentReleaseMult, skippedWhileFinding, withRoomFor, withoutSkipped)
import "rider-app" SharedLogic.SharedCab.Allocation.Types
import "rider-app" SharedLogic.SharedCab.Approach (cabStopEta)
import "rider-app" SharedLogic.SharedCab.Boarding (boardingGate)
import "rider-app" SharedLogic.SharedCab.Booking (alignByKey, boardedTouchDue, cityLiveWindowSec, liveSince, plateLiveWindowSec)
import "rider-app" SharedLogic.SharedCab.Config (SharedCabTunables (allocationWindowSec, driverAcceptEnabled, fallbackAfterSec, findingTimeoutSec, movingTimerSec, offerRetryTimeoutSec, offerTimeoutSec, standTimerSec), defaultTunables)
import "rider-app" SharedLogic.SharedCab.Demand (StopDemand (..), demandAheadRows, searchingRiders, waitingRiders)
import "rider-app" SharedLogic.SharedCab.DriverAction (DriverAction (..), offerBlocks)
import "rider-app" SharedLogic.SharedCab.FindingTimeout (findingTimeoutRefund)
import "rider-app" SharedLogic.SharedCab.Interest (bestEffort, windowStart)
import "rider-app" SharedLogic.SharedCab.Offer (AcceptDecision (..), DeclineDecision (..), decideAccept, decideDecline)
import "rider-app" SharedLogic.SharedCab.RefundDecision (Refund (..))
import "rider-app" SharedLogic.SharedCab.RefundPolicy (cancellableStatus)
import "rider-app" SharedLogic.SharedCab.RouteSnapshot (SnapshotRead (..), snapshotRead)
import "rider-app" SharedLogic.SharedCab.Schedule (chainsAliveDue, claimedTick, nextTickAfter)
import "rider-app" SharedLogic.SharedCab.SessionState (Session (..), SessionStatus (ACTIVE, PAUSED))
import "rider-app" SharedLogic.SharedCab.WalkUp (cabVehicle)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import "rider-app" Tools.Error (SharedCabBoardingError (..), SharedCabDriverActionError (..))
import Prelude

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 9 25) 36000

standing :: AllocationState
standing = AllocationState {vehicleNumber = "ML05A1234", timer = StandTimer (addUTCTime 180 t0)}

-- a board stop, a point ~30 m from it and one ~1.1 km away
stop, nearStop, farAway :: LatLong
stop = LatLong 25.5690 91.8930
nearStop = LatLong 25.5692 91.8932
farAway = LatLong 25.5790 91.8930

-- atStopRadiusM 100, fixes fresh for 60 s
blameAt :: Maybe RiderFix -> Blame
blameAt = passedStopBlame 100 60 t0 stop

candidate :: Text -> RankedCandidate
candidate plate =
  RankedCandidate
    { rcSession =
        Session
          { driverId = "d1",
            vehicleNumber = plate,
            merchantId = Id "m",
            merchantOperatingCityId = Id "moc",
            integratedBppConfigId = Id "ibc",
            serviceTierType = AC,
            routeCode = "R1",
            queuedRouteCode = Nothing,
            capacity = 4,
            walkupCount = 0,
            status = ACTIVE,
            pauseReason = Nothing,
            version = 0,
            startedAt = t0,
            vehicleTripId = Id "trip1"
          },
      rcEtaToBoardStopSec = 60,
      rcUpcomingStop = LT.UpcomingStop (LT.Stop "S1" stop "S1" 1 0 0) t0 LT.Upcoming Nothing,
      rcStance = CabMoving
    }

-- a cab at `pos` doing `speed` m/s whose last fix was `agoSec` before t0, with S1 as its upcoming board stop
cabFix :: Text -> LatLong -> Maybe Double -> Int -> LT.VehicleTrackingOnRouteResp
cabFix = cabFixEta 120

-- as cabFix, reaching the board stop `etaSec` after t0
cabFixEta :: Int -> Text -> LatLong -> Maybe Double -> Int -> LT.VehicleTrackingOnRouteResp
cabFixEta etaSec plate pos speed agoSec =
  LT.VehicleTrackingOnRouteResp
    plate
    LT.VehicleInfo
      { startTime = Nothing,
        scheduleRelationship = Nothing,
        tripId = Nothing,
        latitude = pos.lat,
        longitude = pos.lon,
        speed,
        timestamp = Just (T.pack (iso8601Show (addUTCTime (fromIntegral (negate agoSec)) t0))),
        upcomingStops = Just [LT.UpcomingStop (LT.Stop "S1" stop "S1" 1 0 0) (addUTCTime (fromIntegral etaSec) t0) LT.Upcoming Nothing]
      }

finding :: FindingBooking
finding = FindingBooking {bookingId = Id "b", riderId = Id "r", routeCode = "R1", boardStopCode = "S1", dropStopCode = "S9", seats = 1, findingSince = t0}

claimedAs :: LT.VehicleTrackingOnRouteResp -> [CabStance]
claimedAs veh = [c.rcStance | c <- eligibleCandidates t0 defaultTunables [veh] [(candidate "P1").rcSession] finding]

tests :: TestTree
tests =
  testGroup
    "SharedCab allocation timers and LTS timestamps"
    [ testCase "city scans read the last 6 h, a plate's queries the last 17 h (a session's 16 h + 1 h)" $
        (liveSince cityLiveWindowSec t0, liveSince plateLiveWindowSec t0) @?= (addUTCTime (-6 * 3600) t0, addUTCTime (-17 * 3600) t0),
      testCase "the tick's live set is every plated booking plus the waiting ones, the plated row winning for one claimed in between" $
        mergeLive fst [(1 :: Int, Nothing), (2, Nothing)] [(2, Just "P2" :: Maybe Text), (3, Just "P3")] @?= [(2, Just "P2"), (3, Just "P3"), (1, Nothing)],
      testCase "a cab without room for the party over its ride is no candidate; a party that fits one cab only gets that one" $
        let free = Map.fromList [("FULL", 0), ("ONE", 1), ("TWO", 2)]
            freeFor sess = Map.findWithDefault 0 sess.vehicleNumber free
         in (map (.rcSession.vehicleNumber) (withRoomFor freeFor 1 (map candidate ["FULL", "ONE", "TWO"])), map (.rcSession.vehicleNumber) (withRoomFor freeFor 2 (map candidate ["FULL", "ONE", "TWO"])))
              @?= (["ONE", "TWO"], ["TWO"]),
      testCase "a tick that found the city lease held comes back in 1 s, three times at most, then on the normal schedule" $
        map (uncurry (nextTickAfter 3)) [(True, Nothing), (True, Just 2), (False, Nothing), (False, Just 1), (False, Just 2), (False, Just 3)]
          @?= [(3, Nothing), (3, Nothing), (1, Just 1), (1, Just 2), (1, Just 3), (3, Nothing)],
      testCase "a rider poll reseeds the city's chains only when it opened the 30 s window" $
        (chainsAliveDue t0 t0, chainsAliveDue (addUTCTime 5 t0) t0) @?= (True, False),
      testCase "route snapshot: nothing built yet is rebuilt inline" $
        snapshotRead t0 Nothing False @?= Rebuild,
      testCase "route snapshot: under 2 s old is served, rebuild running or not" $
        (snapshotRead (addUTCTime 1 t0) (Just t0) False, snapshotRead (addUTCTime 1 t0) (Just t0) True) @?= (Serve, Serve),
      testCase "route snapshot: stale with nobody rebuilding is rebuilt by this request" $
        snapshotRead (addUTCTime 5 t0) (Just t0) False @?= Rebuild,
      testCase "route snapshot: stale while another request rebuilds is served as is" $
        snapshotRead (addUTCTime 5 t0) (Just t0) True @?= ServeStaleNoRebuild,
      testCase "route snapshot: 10 s old is never served, even while another request rebuilds" $
        snapshotRead (addUTCTime 10 t0) (Just t0) True @?= Rebuild,
      testCase "the driver view's batched reads come back in its bookings' order, a missing key as Nothing" $
        alignByKey ["k1", "k2", "k3"] [("k3", 'c'), ("k1", 'a')] @?= [Just 'a', Nothing, Just 'c'],
      testCase "a tick item that throws is reported and the next one still runs" $ do
        ran <- newIORef []
        failed <- newIORef []
        eachIsolated
          (try :: IO () -> IO (Either IOException ()))
          (\x _ -> modifyIORef failed (x :))
          (\x -> if x == 2 then throwIO (userError "boom") else modifyIORef ran (x :))
          [1, 2, 3 :: Int]
        (,) <$> readIORef ran <*> readIORef failed >>= (@?= ([3, 1], [2])),
      testCase "a boarded booking is touched only once it is over an hour old" $
        (boardedTouchDue (addUTCTime 3600 t0) t0, boardedTouchDue (addUTCTime 3601 t0) t0) @?= (False, True),
      testCase "a running timer keeps the allocation" $
        timerExpiry (addUTCTime 179 t0) (Just standing) @?= Nothing,
      testCase "an expired stand timer releases as StandTimeout, a moving one as MovingTimeout" $
        (timerExpiry (addUTCTime 181 t0) (Just standing), timerExpiry (addUTCTime 181 t0) (Just standing {timer = MovingTimer (addUTCTime 180 t0)}))
          @?= (Just StandTimeout, Just MovingTimeout),
      testCase "a cab claimed while moving has no timer to lapse" $
        timerExpiry (addUTCTime 3600 t0) (Just standing {timer = NoTimer}) @?= Nothing,
      testCase "timer mode follows the cab: moving above 1 m/s, no speed reads as stationary" $
        map isMovingSpeed [Just 5.0, Just 0.2, Nothing] @?= [True, False, False],
      testCase "a vanished alloc key releases without blame" $
        (timerExpiry t0 Nothing, blameFor TimerLost, countsTowardAttempts TimerLost) @?= (Just TimerLost, BlameNone, False),
      testCase "LTS chrono RFC 3339 with nanoseconds and Z" $
        parseLtsTimestamp "2026-09-25T10:00:00.123456789Z" @?= Just (addUTCTime 0.123456789 t0),
      testCase "offsets, a space separator and epoch seconds also parse" $
        map parseLtsTimestamp ["2026-09-25T15:30:00+05:30", "2026-09-25 10:00:00Z", "1790330400"]
          @?= [Just t0, Just t0, Just t0],
      testCase "rider fresh and at the stop when the cab passed -> the driver's miss" $
        blameAt (Just (RiderFix nearStop (addUTCTime (-30) t0))) @?= BlameDriver,
      testCase "rider fresh but away from the stop -> the rider's no-show" $
        blameAt (Just (RiderFix farAway (addUTCTime (-30) t0))) @?= BlameRider,
      testCase "rider position unknown -> nobody" $
        blameAt Nothing @?= BlameNone,
      testCase "rider position stale, even at the stop -> nobody" $
        blameAt (Just (RiderFix nearStop (addUTCTime (-61) t0))) @?= BlameNone,
      testCase "PASSED_STOP carries its blame and always counts as an attempt" $
        map (\b -> (blameFor (PassedStop b), countsTowardAttempts (PassedStop b))) [BlameDriver, BlameRider, BlameNone]
          @?= [(BlameDriver, True), (BlameRider, True), (BlameNone, True)],
      testCase "skipping a full cab is nobody's fault and not an attempt" $
        (blameFor (RiderSkipped SkipFull), countsTowardAttempts (RiderSkipped SkipFull)) @?= (BlameNone, False),
      testCase "skipping for any other reason is also nobody's fault and not an attempt: the one allowed skip costs the rider nothing" $
        (blameFor (RiderSkipped SkipOther), countsTowardAttempts (RiderSkipped SkipOther)) @?= (BlameNone, False),
      testCase "the API reasons map onto the skip reasons" $
        map skipReason [API.FULL, API.OTHER] @?= [SkipFull, SkipOther],
      testCase "a skipped cab is never offered to that booking again" $
        map (.rcSession.vehicleNumber) (withoutSkipped ["ML05B2222"] (map candidate ["ML05A1111", "ML05B2222", "ML05C3333"]))
          @?= ["ML05A1111", "ML05C3333"],
      testCase "a close needs a live booking still holding the plate with nobody boarded" $
        [ closable "P1" BS.CONFIRMED (Just "P1") [TS.ACTIVE, TS.ACTIVE],
          closable "P1" BS.CONFIRMED (Just "P1") [TS.ACTIVE, TS.INPROGRESS],
          closable "P1" BS.CONFIRMED (Just "P1") [TS.INPROGRESS],
          closable "P1" BS.CANCELLED (Just "P1") [TS.ACTIVE],
          closable "P1" BS.CONFIRMED (Just "P2") [TS.ACTIVE],
          closable "P1" BS.CONFIRMED Nothing [TS.ACTIVE]
        ]
          @?= [True, False, False, False, False, False],
      testCase "H1: a claim needs a live cab-less booking, every ticket unboarded, and no degrade marker" $
        [ claimable BS.CONFIRMED Nothing [TS.ACTIVE, TS.ACTIVE] False,
          claimable BS.CONFIRMED Nothing [TS.INPROGRESS] False,
          claimable BS.CONFIRMED Nothing [TS.ACTIVE, TS.INPROGRESS] False,
          claimable BS.CONFIRMED Nothing [TS.ACTIVE] True,
          claimable BS.CONFIRMED (Just "P1") [TS.ACTIVE] False,
          claimable BS.CANCELLED Nothing [TS.ACTIVE] False,
          claimable BS.CONFIRMED Nothing [] False
        ]
          @?= [True, False, False, False, False, False, False],
      testCase "the fallback push fires on the close that crosses maxAttempts, not before or after" $
        map (uncurry (crossedMaxAttempts 2)) [(0, 1), (1, 2), (2, 3), (0, 0)] @?= [False, True, False, False],
      testCase "only the two timer outcomes push the missed-cab copy" $
        map skipsPlateOnClose [StandTimeout, MovingTimeout, DriverCancelled, PassedStop BlameRider, SeatLost, RouteChanged, SessionClosed, TimerLost, RiderSkipped SkipOther]
          @?= [True, True, False, False, False, False, False, False, False],
      testCase "skips bind while FINDING, and stop binding in FALLBACK (attempts or clock)" $
        [ skippedWhileFinding defaultTunables t0 0 t0 ["A"],
          skippedWhileFinding defaultTunables t0 2 t0 ["A"],
          skippedWhileFinding defaultTunables (addUTCTime 599 t0) 1 t0 ["A"],
          skippedWhileFinding defaultTunables (addUTCTime 600 t0) 1 t0 ["A"]
        ]
          @?= [["A"], [], ["A"], []],
      testCase "only a stationary cab within atStopRadiusM of the board stop gets the stand timer" $
        map claimedAs [cabFix "P1" nearStop (Just 0) 5, cabFix "P1" farAway (Just 0) 5, cabFix "P1" nearStop (Just 8) 5, cabFix "P1" farAway Nothing 5]
          @?= [[CabAtStop], [CabAway], [CabMoving], [CabAway]],
      testCase "a stationary cab gets a bounded wait, at the stop the stand timer and away the allocation window; a moving cab none" $
        map (\veh -> [claimTimer defaultTunables t0 c | c <- eligibleCandidates t0 defaultTunables [veh] [(candidate "P1").rcSession] finding]) [cabFix "P1" nearStop (Just 0) 5, cabFix "P1" farAway (Just 0) 5, cabFix "P1" nearStop (Just 8) 5]
          @?= [[StandTimer (addUTCTime (fromIntegral defaultTunables.standTimerSec) t0)], [AwayTimer (addUTCTime (fromIntegral defaultTunables.allocationWindowSec) t0)], [NoTimer]],
      testCase "a missed or never-arriving cab is skipped for the booking, no other close is" $
        map skipsPlateOnClose [StandTimeout, MovingTimeout, AwayTimeout, DriverCancelled, PassedStop BlameRider, SeatLost, RouteChanged, SessionClosed, TimerLost, CabSilent, RiderSkipped SkipOther]
          @?= [True, True, True, False, False, False, False, False, False, False, False],
      testCase "a silent cab is released only past silentReleaseMult x ltsMaxAgeSec, and only while the route is otherwise reporting" $
        let quiet ago = silentCab t0 60 "P1" [cabFix "P1" nearStop (Just 5) ago, cabFix "P2" nearStop (Just 5) 3]
            limit = silentReleaseMult * 60
         in [ quiet 10,
              quiet limit,
              quiet (limit + 1),
              silentCab t0 60 "P1" [cabFix "P2" nearStop (Just 5) 3],
              silentCab t0 60 "P1" [cabFix "P1" nearStop (Just 5) (limit + 1), cabFix "P2" nearStop (Just 5) (limit + 1)],
              silentCab t0 60 "P1" []
            ]
              @?= [False, False, True, True, False, False],
      testCase "a booking is Finding with no cab and a ticket to serve, AllocatedTo while every ticket is unboarded, NotWaiting otherwise" $
        [ phase BS.CONFIRMED Nothing [TS.ACTIVE],
          phase BS.CONFIRMED Nothing [TS.ACTIVE, TS.INPROGRESS],
          phase BS.CONFIRMED Nothing [TS.INPROGRESS],
          phase BS.CONFIRMED (Just "P1") [TS.ACTIVE, TS.ACTIVE],
          phase BS.CONFIRMED (Just "P1") [TS.ACTIVE, TS.INPROGRESS],
          phase BS.CONFIRMED (Just "P1") [],
          phase BS.CANCELLED Nothing [TS.ACTIVE],
          phase BS.CANCELLED (Just "P1") [TS.ACTIVE]
        ]
          @?= [Finding, Finding, NotWaiting, AllocatedTo "P1", NotWaiting, NotWaiting, NotWaiting, NotWaiting],
      testCase "the claim gate refuses a skipped plate first, then a booking no longer waiting" $
        let s = (candidate "ML05A1234").rcSession
         in [claimGate s ["ML05A1234"] Nothing False, claimGate s [] Nothing False] @?= [Left ClaimSkipped, Left ClaimCasLost],
      testCase "a boarding is refused when the cab is no longer running, and when the booking has no ticket left to board" $
        let s = (candidate "ML05A1234").rcSession
            unused = error "the later guards are not reached"
         in map
              (either Just (const Nothing))
              [ boardingGate "ML05A1234" s {status = PAUSED} [TS.ACTIVE] Nothing unused,
                boardingGate "ML05A1234" s [] Nothing unused
              ]
              @?= [Just BoardingFailed, Just BoardingFailed],
      testCase "eligible cabs come nearest first" $
        map (.rcSession.vehicleNumber) (eligibleCandidates t0 defaultTunables [cabFixEta 300 "P2" nearStop (Just 0) 5, cabFixEta 60 "P1" nearStop (Just 0) 5] [(candidate "P2").rcSession, (candidate "P1").rcSession] finding)
          @?= ["P1", "P2"],
      testCase "a cab counts as moving only on a fresh report above the speed threshold" $
        [ movingCab t0 60 "P1" [cabFix "P1" nearStop (Just 8) 5],
          movingCab t0 60 "P1" [cabFix "P1" nearStop (Just 0) 5],
          movingCab t0 60 "P1" [cabFix "P1" nearStop (Just 8) 500],
          movingCab t0 60 "P1" [cabFix "P2" nearStop (Just 8) 5]
        ]
          @?= [True, False, False, False],
      testCase "a silent-cab release is nobody's fault and not an attempt" $
        (blameFor CabSilent, countsTowardAttempts CabSilent) @?= (BlameNone, False),
      testCase "the away timer lapses as AwayTimeout, which blames nobody and skips the cab that never came" $
        ( timerExpiry (addUTCTime 181 t0) (Just standing {timer = AwayTimer (addUTCTime 180 t0)}),
          blameFor AwayTimeout,
          skipsPlateOnClose AwayTimeout
        )
          @?= (Just AwayTimeout, BlameNone, True),
      testCase "(user decision 2026-09-30): a stand timeout is the rider's no-show, not a driver miss; a driver cancel still is" $
        map blameFor [StandTimeout, MovingTimeout, DriverCancelled, AwayTimeout]
          @?= [BlameRider, BlameRider, BlameDriver, BlameNone],
      testCase "only a stand or moving timer gives the rider a board deadline: an away timer reads as an assigned cab still on its way" $
        map (allocationDeadline . (\timer -> standing {timer})) [StandTimer t0, MovingTimer t0, AwayTimer t0, NoTimer] @?= [Just t0, Just t0, Nothing, Nothing],
      testCase "stand and away timers are cleared once the cab moves, the moving timer is not" $
        map clearOnMove [StandTimer t0, AwayTimer t0, MovingTimer t0, NoTimer] @?= [NoTimer, NoTimer, MovingTimer t0, NoTimer],
      testCase "a cab whose ETA cannot cover its straight-line distance (a parked cab snapped to the first stop) is not a candidate" $
        let ghost = LatLong 25.6600 91.9500
            names veh = map (.rcSession.vehicleNumber) (eligibleCandidates t0 defaultTunables [veh] [(candidate "P1").rcSession] finding)
         in ( names (cabFixEta 0 "P1" ghost (Just 0) 5),
              names (cabFixEta 0 "P1" nearStop (Just 0) 5),
              names (cabFixEta 120 "P1" farAway (Just 8) 5),
              names (cabFixEta 60 "P1" ghost (Just 8) 5)
            )
              @?= ([], ["P1"], ["P1"], []),
      testCase "etaPlausible allows the at-stop radius plus eta times the max speed" $
        [etaPlausible defaultTunables e nearStop stop | e <- [0, 10]] ++ [etaPlausible defaultTunables 80 farAway stop, etaPlausible defaultTunables 5 farAway stop]
          @?= [True, True, True, False],
      testCase "a claim on a stationary cab at the stop pushes ARRIVING, any other claim pushes ASSIGNED" $
        map (\veh -> [claimPush c | c <- eligibleCandidates t0 defaultTunables [veh] [(candidate "P1").rcSession] finding]) [cabFix "P1" nearStop (Just 0) 5, cabFix "P1" farAway (Just 0) 5, cabFix "P1" nearStop (Just 8) 5]
          @?= [[PushArriving], [PushAssigned], [PushAssigned]],
      testCase "a FINDING booking is cancelled once its current stint is older than findingTimeoutSec, so a late release still gets its reallocation" $
        -- (age of the stint, age of the booking)
        map (\(stint, age) -> findingTimeoutAction (addUTCTime age t0) 1200 (addUTCTime (age - stint) t0) t0) [(0, 1300), (1200, 1300), (1201, 1300), (60, 3000), (10, 3600), (10, 3601), (10, 7200)]
          @?= [KeepFinding, KeepFinding, CancelNoCab, KeepFinding, KeepFinding, CancelNoCab, CancelNoCab],
      testCase "a booking already cancelled is not cancelled (or refunded) again" $
        map cancellableStatus [BS.CONFIRMED, BS.CANCELLED, BS.COUNTER_CANCELLED, BS.CANCEL_INITIATED, BS.FAILED, BS.RESCHEDULED] @?= [True, False, False, False, False, False],
      testCase "a booking with a parent (rescheduled) never counted its tickets at confirm, so none are reversed" $
        (isRescheduledChild (Nothing :: Maybe ()), isRescheduledChild (Just ())) @?= (False, True),
      testCase "the next tick is scheduled even when the run claim or the tick body throws, but not when the run was already claimed" $ do
        let runWith claim body = do
              next <- newIORef False
              r <- try (claimedTick claim body (writeIORef next True)) :: IO (Either IOException ())
              (,) (either (const True) (const False) r) <$> readIORef next
        claimDown <- runWith (throwIO (userError "redis down")) (pure ())
        bodyDown <- runWith (pure True) (throwIO (userError "db down"))
        fine <- runWith (pure True) (pure ())
        duplicate <- runWith (pure False) (throwIO (userError "never runs"))
        [claimDown, bodyDown, fine, duplicate] @?= [(True, True), (True, True), (False, True), (False, False)],
      testCase "a finding-timeout cancel refunds in full, unless a no-show is already booked against the booking" $
        map (\noShows -> findingTimeoutRefund noShows [TS.ACTIVE]) [0, 1, 2]
          @?= [FullRefund, NoRefund, NoRefund],
      testCase "garbage is not a timestamp" $
        parseLtsTimestamp "yesterday" @?= Nothing,
      testCase "a clock-skewed fix from the future is not fresh" $ do
        let fromTheFutureAgo = (-10) -- cabFix puts `agoSec` before t0
        ( isFreshPosition t0 60 (cabFix "P1" nearStop (Just 5) fromTheFutureAgo).vehicleInfo,
          isFreshPosition t0 60 (cabFix "P1" nearStop (Just 5) 10).vehicleInfo,
          silentCab t0 60 "P1" [cabFix "P1" nearStop (Just 5) fromTheFutureAgo]
          )
          @?= (False, True, False),
      testCase "the alloc key lives at least findingTimeoutSec and always covers the stacked timers plus the margin" $
        let stacked = defaultTunables.standTimerSec + defaultTunables.movingTimerSec + allocKeyTtlMarginSec
            short = defaultTunables {findingTimeoutSec = 25, standTimerSec = 12, movingTimerSec = 8}
         in ( allocKeyTtl defaultTunables,
              allocKeyTtl short,
              allocKeyTtl defaultTunables {findingTimeoutSec = stacked},
              allocKeyTtl defaultTunables {findingTimeoutSec = stacked - 1}
            )
              @?= (1200, 12 + 8 + allocKeyTtlMarginSec, stacked, stacked),
      testCase "a cab reaches a stop with the stop's own entry, only while the stop is still ahead of it" $
        ( (.stop.stopCode) <$> cabStopEta t0 defaultTunables "S1" stop (cabFixEta 125 "A" stop (Just 5) 10),
          (.stop.stopCode) <$> cabStopEta t0 defaultTunables "S2" stop (cabFixEta 125 "A" stop (Just 5) 10)
        )
          @?= (Just "S1", Nothing),
      testCase "a stop the cab has already reached is not ahead of it" $
        let reached = (cabFixEta 60 "A" stop (Just 5) 10) {LT.vehicleInfo = (cabFixEta 60 "A" stop (Just 5) 10).vehicleInfo {LT.upcomingStops = Just [LT.UpcomingStop (LT.Stop "S1" stop "S1" 1 0 0) t0 LT.Reached Nothing]}}
         in (.stop.stopCode) <$> cabStopEta t0 defaultTunables "S1" stop reached @?= Nothing,
      testCase "no ETA from a stale fix, a stop the cab is past, or an ETA the cab's distance cannot back" $
        map
          (fmap (.stop.stopCode))
          [ cabStopEta t0 defaultTunables "S1" stop (cabFixEta 60 "A" stop (Just 5) 3600),
            cabStopEta t0 defaultTunables "S2" stop (cabFixEta 60 "A" stop (Just 5) 10),
            cabStopEta t0 defaultTunables "S1" stop (cabFixEta 30 "A" (LatLong 25.6690 91.8930) (Just 5) 10)
          ]
          @?= [Nothing, Nothing, Nothing],
      testCase "a picked cab holds while it is unskipped, within the wait bound, active on the route and has room" $
        ( verdict [] 0 [pickable] 4 2 "ML05A1234",
          verdict ["ML05A9999"] 0 [pickable] 4 2 "ML05A1234"
        )
          @?= (Honour "ML05A1234", Honour "ML05A1234"),
      testCase "a picked cab that was skipped is lost without a word, even with everything else in order" $
        verdict ["ML05A1234"] 0 [pickable] 4 2 "ML05A1234" @?= Lost PrefSkipped,
      testCase "a picked cab is lost once the rider has waited fallbackAfterSec, and not a second sooner" $
        ( verdict [] (defaultTunables.fallbackAfterSec - 1) [pickable] 4 2 "ML05A1234",
          verdict [] defaultTunables.fallbackAfterSec [pickable] 4 2 "ML05A1234"
        )
          @?= (Honour "ML05A1234", Lost PrefTimedOut),
      testCase "a picked cab is lost when it has no active session on the route" $
        ( verdict [] 0 [] 4 2 "ML05A1234",
          verdict [] 0 [pickable {status = PAUSED}] 4 2 "ML05A1234",
          verdict [] 0 [pickable {vehicleNumber = "ML05A7777"}] 4 2 "ML05A1234"
        )
          @?= (Lost PrefOffRoute, Lost PrefOffRoute, Lost PrefOffRoute),
      testCase "a picked cab is lost when the seat engine no longer has the party's seats free for its ride" $
        (verdict [] 0 [pickable] 2 2 "ML05A1234", verdict [] 0 [pickable] 1 2 "ML05A1234")
          @?= (Honour "ML05A1234", Lost PrefFull),
      testCase "a cabId names the active cab with that trip, and nothing else" $
        ( cabForId "trip1" [pickable],
          cabForId "trip2" [pickable],
          cabForId "trip1" [pickable {status = PAUSED}],
          cabForId "trip1" []
        )
          @?= (Just "ML05A1234", Nothing, Nothing, Nothing),
      testCase "the route view never carries a cab's full plate, live or listed without a fix; a plate the rider names comes back whole" $ do
        let shown named = BL.unpack . encode . cabVehicle t0 defaultTunables named (Just ("S1", stop)) (3, 1) pickable
            fresh = Just (cabFixEta 120 "ML05A1234" stop (Just 5) 10)
        ( map (("05A1234" `isInfixOf`) . shown False) [fresh, Nothing],
          map (("\"1234\"" `isInfixOf`) . shown False) [fresh, Nothing],
          map (("\"ML05A1234\"" `isInfixOf`) . shown True) [fresh, Nothing]
          )
          @?= ([False, False], [True, True], [True, True]),
      testCase "a cab with a fresh fix is live with its trip and seats; one with a stale fix or none stays listed, pickable by its trip" $
        let entry = cabVehicle t0 defaultTunables False (Just ("S1", stop)) (3, 1) pickable
            summary = either (\v -> ("schedule", v.tripId, v.availableSeats, Nothing, isJust v.position)) (\v -> ("live", v.currentTripId, v.availableSeats, v.occupiedSeats, True))
         in map (summary . entry) [Just (cabFixEta 120 "ML05A1234" stop (Just 5) 10), Just (cabFixEta 120 "ML05A1234" stop (Just 5) 3600), Nothing]
              @?= [ ("live", Just "trip1", Just 3, Just 1, True),
                    ("schedule", Just "trip1", Just 3, Nothing, True),
                    ("schedule", Just "trip1", Just 3, Nothing, False)
                  ],
      testCase "a live cab's board-stop ETA is dropped when its distance can't back it; its other stops keep theirs" $
        let twoStops pos = (cabFixEta 30 "ML05A1234" pos (Just 5) 10) {LT.vehicleInfo = (cabFixEta 30 "ML05A1234" pos (Just 5) 10).vehicleInfo {LT.upcomingStops = Just [LT.UpcomingStop (LT.Stop "S1" stop "S1" 1 0 0) (addUTCTime 30 t0) LT.Upcoming Nothing, LT.UpcomingStop (LT.Stop "S2" stop "S2" 2 0 0) (addUTCTime 600 t0) LT.Upcoming Nothing]}}
            etaStops board pos = either (const Nothing) (fmap (map (.stopCode)) . (.eta)) (cabVehicle t0 defaultTunables False board (3, 1) pickable (Just (twoStops pos)))
            tooFar = LatLong 25.6690 91.8930
         in (etaStops (Just ("S1", stop)) tooFar, etaStops (Just ("S1", stop)) stop, etaStops Nothing tooFar)
              @?= (Just ["S2"], Just ["S1", "S2"], Just ["S1", "S2"]),
      testCase "riders waiting on a route are the distinct riders with a recent finding booking" $
        ( waitingRiders 1200 t0 [(addUTCTime (-60) t0, "r1"), (addUTCTime (-120) t0, "r2"), (addUTCTime (-30) t0, "r1")] [],
          waitingRiders 1200 t0 [(addUTCTime (-1199) t0, "r1"), (addUTCTime (-1200) t0, "r2")] [],
          waitingRiders 1200 t0 [] []
        )
          @?= (2, 1, 0),
      testCase "a rider who opened a stand and also booked counts once; each person who only opened it counts" $
        ( waitingRiders 1200 t0 [(addUTCTime (-60) t0, "r1")] ["r1"],
          waitingRiders 1200 t0 [(addUTCTime (-60) t0, "r1")] ["r1", "r2", "r3"],
          waitingRiders 1200 t0 [] ["r2", "r2"]
        )
          @?= (1, 3, 1),
      testCase "a finding booking counts as waiting for the city's finding timeout, not a fixed 20 minutes" $
        ( waitingRiders 300 t0 [(addUTCTime (-299) t0, "r1"), (addUTCTime (-301) t0, "r2")] [],
          waitingRiders 3600 t0 [(addUTCTime (-1800) t0, "r1")] []
        )
          @?= (1, 1),
      testCase "demandAhead rows carry the stop's code and name, in route order, and only for stops where someone waits or searches" $
        map
          (\r -> (r.stopCode, r.stopName, r.waiting, r.searching))
          (demandAheadRows [("A", "Alpha"), ("B", "Bravo"), ("C", "Charlie")] (Map.fromList [("A", StopDemand 2 1), ("B", StopDemand 0 0), ("C", StopDemand 0 3)]))
          @?= [("A", "Alpha", 2, 1), ("C", "Charlie", 0, 3)],
      testCase "riders searching on a route are those who looked at any of its stops and have no recent booking, each counted once" $
        ( searchingRiders [Set.fromList ["r1", "r2"], Set.fromList ["r2", "r3"]] Set.empty,
          searchingRiders [Set.fromList ["r1", "r2"], Set.fromList ["r2", "r3"]] (Set.fromList ["r2"]),
          searchingRiders [] (Set.fromList ["r1"]),
          searchingRiders [Set.fromList ["r1"]] (Set.fromList ["r1"])
        )
          @?= (3, 2, 0, 0),
      testCase "an interest entry ages out at the window's start, to the second" $
        ( windowStart t0 600 - windowStart (addUTCTime (-600) t0) 0,
          windowStart t0 600 + 600 - windowStart t0 0,
          windowStart (addUTCTime 60 t0) 600 - windowStart t0 600
        )
          @?= (0, 0, 60),
      testCase "a failing action is logged and answered with nothing, a working one with its result" $ do
        logged <- newIORef ([] :: [Text])
        failed <- bestEffort (\msg -> modifyIORef logged (msg :)) (throwIO (userError "redis down") :: IO ())
        worked <- bestEffort (\_ -> pure ()) (pure (7 :: Int))
        messages <- readIORef logged
        (failed, worked, length messages) @?= (Nothing, Just 7, 1),
      testCase "without the driver-accept switch a claim arms the stance's own timer, as before" $
        map (armedTimer defaultTunables t0 0) [CabMoving, CabAtStop, CabAway]
          @?= [NoTimer, StandTimer (addUTCTime (fromIntegral defaultTunables.standTimerSec) t0), AwayTimer (addUTCTime (fromIntegral defaultTunables.allocationWindowSec) t0)],
      testCase "with the switch on a claim is an offer, 30 s the first time and 15 s each later one, whatever the stance" $
        let on = defaultTunables {driverAcceptEnabled = True}
         in [armedTimer on t0 0 CabMoving, armedTimer on t0 1 CabAtStop, armedTimer on t0 5 CabAway, armedTimer on t0 0 CabAway]
              @?= [OfferTimer (addUTCTime 30 t0), OfferTimer (addUTCTime 15 t0), OfferTimer (addUTCTime 15 t0), OfferTimer (addUTCTime 30 t0)],
      testCase "an offer has a deadline but no countdown for the rider, and survives the cab moving" $
        let st = standing {timer = OfferTimer (addUTCTime 30 t0)}
         in (timerDeadline st.timer, allocationDeadline st, clearOnMove st.timer, isOffered st, isOffered standing)
              @?= (Just (addUTCTime 30 t0), Nothing, OfferTimer (addUTCTime 30 t0), True, False),
      testCase "an offer lapses as OfferTimeout only after its deadline" $
        let st = Just standing {timer = OfferTimer (addUTCTime 30 t0)}
         in (timerExpiry (addUTCTime 30 t0) st, timerExpiry (addUTCTime 31 t0) st) @?= (Nothing, Just OfferTimeout),
      testCase "a decline or a lapse is nobody's miss and no allocation attempt, and skips the plate for the booking" $
        [ (blameFor o, countsTowardAttempts o, skipsPlateOnClose o)
          | o <- [DriverDeclined, OfferTimeout]
        ]
          @?= [(BlameNone, False, True), (BlameNone, False, True)],
      testCase "an offer is accepted on its own plate before it lapses; later it is gone, and an answered one is a no-op" $
        let offer d = Just standing {timer = OfferTimer (addUTCTime d t0)}
         in ( decideAccept t0 "ML05A1234" (Just "ML05A1234") (offer 30),
              decideAccept (addUTCTime 31 t0) "ML05A1234" (Just "ML05A1234") (offer 30),
              decideAccept t0 "ML05A1234" (Just "ML05A1234") Nothing,
              decideAccept t0 "ML05A1234" (Just "ML05A1234") (Just standing),
              decideAccept t0 "ML05A1234" (Just "ML05A9999") (offer 30),
              decideAccept t0 "ML05A1234" Nothing (offer 30)
            )
              @?= (Right (AcceptNow standing {timer = OfferTimer (addUTCTime 30 t0)}), Left OfferExpired, Left OfferExpired, Right AlreadyAccepted, Left BookingNotOnThisCab, Left OfferExpired),
      testCase "an offer is declined while it is open, even lapsed; one already closed is a no-op; an accepted one is not an offer" $
        let offer = Just standing {timer = OfferTimer (addUTCTime 30 t0)}
         in ( decideDecline "ML05A1234" (Just "ML05A1234") offer,
              decideDecline "ML05A1234" Nothing offer,
              decideDecline "ML05A1234" (Just "ML05A1234") Nothing,
              decideDecline "ML05A1234" (Just "ML05A1234") (Just standing),
              decideDecline "ML05A1234" (Just "ML05A9999") offer
            )
              @?= (Right DeclineNow, Right AlreadyClosed, Right AlreadyClosed, Left BookingNotOffered, Left BookingNotOnThisCab),
      testCase "the rider sees no cab while the offer is open, and the cab once it is accepted" $
        ( visiblePlate (Just standing {timer = OfferTimer (addUTCTime 30 t0)}) (Just "ML05A1234"),
          visiblePlate (Just standing) (Just "ML05A1234"),
          visiblePlate Nothing (Just "ML05A1234"),
          visiblePlate Nothing Nothing
        )
          @?= (Nothing, Just "ML05A1234", Just "ML05A1234", Nothing),
      testCase "an unanswered offer is not a seat the rider can miss" $
        (seatIsRiders (Just standing {timer = OfferTimer (addUTCTime 30 t0)}), seatIsRiders (Just standing), seatIsRiders Nothing) @?= (False, True, True),
      testCase "no call or chat while the offer waits on the driver" $
        (offerPending (Just standing {timer = OfferTimer (addUTCTime 30 t0)}), offerPending (Just standing), offerPending Nothing) @?= (True, False, False),
      testCase "only the rider's own choice uses the booking's one skip; a full cab never does" $
        map usesRiderSkip [SkipFull, SkipOther] @?= [False, True],
      testCase "a driver cannot cancel or board-without-code an offer, but a drop is not blocked and an accepted booking is not either" $
        let offer = Just standing {timer = OfferTimer (addUTCTime 30 t0)}
         in ([offerBlocks a offer | a <- [DriverCancel, DriverBoarded, DriverDropped]], [offerBlocks a (Just standing) | a <- [DriverCancel, DriverBoarded]], offerBlocks DriverCancel Nothing)
              @?= ([True, True, False], [False, False], False),
      testCase "only the picked cab stays a candidate" $
        map (.rcSession.vehicleNumber) (onlyPreferred "ML05A1234" [candidate "ML05A9999", candidate "ML05A1234", candidate "ML05A5555"]) @?= ["ML05A1234"]
    ]
  where
    pickable = (candidate "ML05A1234").rcSession
    -- skipped plates, seconds waited, the route's sessions, seats taken on the cab by bookings, party size, picked plate
    verdict skipped waited sessions freeSeats seats =
      preferenceVerdict defaultTunables t0 (addUTCTime (negate (fromIntegral waited)) t0) skipped sessions freeSeats seats
