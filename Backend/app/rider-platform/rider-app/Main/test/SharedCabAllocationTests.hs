{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabAllocationTests (tests) where

import qualified "rider-app" API.Types.UI.SharedCab as API
import "beckn-spec" BecknV2.FRFS.Enums (ServiceTierType (AC))
import Control.Exception (IOException, throwIO, try)
import Data.IORef (newIORef, readIORef, writeIORef)
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
import "rider-app" SharedLogic.SharedCab.Allocation.Claim (ClaimMiss (..), claimGate)
import "rider-app" SharedLogic.SharedCab.Allocation.Select (CabStance (..), ClaimPush (..), FindingBooking (..), Phase (..), RankedCandidate (..), claimPush, claimTimer, claimable, closable, crossedMaxAttempts, eligibleCandidates, isFreshPosition, isRescheduledChild, movingCab, phase, silentCab, silentReleaseMult, skippedWhileFinding, withoutSkipped)
import "rider-app" SharedLogic.SharedCab.Allocation.Types
import "rider-app" SharedLogic.SharedCab.Boarding (boardingGate)
import "rider-app" SharedLogic.SharedCab.Config (SharedCabTunables (findingTimeoutSec, movingTimerSec, standTimerSec), defaultTunables)
import "rider-app" SharedLogic.SharedCab.FindingTimeout (findingTimeoutRefund)
import "rider-app" SharedLogic.SharedCab.RefundDecision (Refund (..))
import "rider-app" SharedLogic.SharedCab.RefundPolicy (cancellableStatus)
import "rider-app" SharedLogic.SharedCab.Schedule (thenReschedule)
import "rider-app" SharedLogic.SharedCab.SessionState (Session (..), SessionStatus (ACTIVE, PAUSED))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import "rider-app" Tools.Error (SharedCabBoardingError (..))
import Prelude

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 9 25) 36000

standing :: AllocationState
standing = AllocationState {vehicleNumber = "ML05A1234", driverId = Just "d1", vehicleTripId = Just "t1", allocatedAt = t0, timer = StandTimer (addUTCTime 180 t0), attempts = 0}

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
            consecutiveMisses = 0,
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
    [ testCase "a running timer keeps the allocation" $
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
      testCase "allocation_closed outcomes use the wire spellings" $
        map outcomeText [StandTimeout, MovingTimeout, DriverCancelled, PassedStop BlameDriver, SeatLost, RouteChanged, SessionClosed, TimerLost, RiderSkipped SkipFull]
          @?= ["STAND_TIMEOUT", "MOVING_TIMEOUT", "DRIVER_CANCELLED", "PASSED_STOP", "SEAT_LOST", "ROUTE_CHANGED", "SESSION_CLOSED", "TIMER_LOST", "RIDER_SKIPPED"],
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
      testCase "skipping for any other reason is the rider's and counts" $
        (blameFor (RiderSkipped SkipOther), countsTowardAttempts (RiderSkipped SkipOther)) @?= (BlameRider, True),
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
      testCase "a missed cab is skipped for the booking, no other close is" $
        map skipsPlateOnClose [StandTimeout, MovingTimeout, DriverCancelled, PassedStop BlameRider, SeatLost, RouteChanged, SessionClosed, TimerLost, CabSilent, RiderSkipped SkipOther]
          @?= [True, True, False, False, False, False, False, False, False, False],
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
      testCase "a cab whose LTS position spells the plate differently is still eligible and still silent-checked" $
        ( map (\c -> c.rcSession.vehicleNumber) (eligibleCandidates t0 defaultTunables [cabFix "p 1" nearStop (Just 0) 5] [(candidate "P1").rcSession] finding),
          movingCab t0 60 "P1" [cabFix "p 1" nearStop (Just 8) 5]
        )
          @?= (["P1"], True),
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
      testCase "the claim gate refuses a skipped plate first, then a cab without the seats, then a booking no longer waiting" $
        let s = (candidate "ML05A1234").rcSession
            gate skipped liveSeats = claimGate s finding skipped liveSeats Nothing False
         in [gate ["ML05A1234"] 0, gate [] (4 - finding.seats + 1), gate [] 0] @?= [Left ClaimSkipped, Left ClaimSeatsGone, Left ClaimCasLost],
      testCase "a boarding is refused when the cab is no longer running, and when the booking has no ticket left to board" $
        let s = (candidate "ML05A1234").rcSession
            unused = error "the later guards are not reached"
         in map
              (either Just (const Nothing))
              [ boardingGate "ML05A1234" s {status = PAUSED} 0 [TS.ACTIVE] Nothing unused 1,
                boardingGate "ML05A1234" s 0 [] Nothing unused 1
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
        (outcomeText CabSilent, blameFor CabSilent, countsTowardAttempts CabSilent, countsTowardDriverMisses CabSilent) @?= ("CAB_SILENT", BlameNone, False, False),
      testCase "the away timer lapses as AwayTimeout, which blames nobody, skips no cab and is not a driver miss" $
        ( timerExpiry (addUTCTime 181 t0) (Just standing {timer = AwayTimer (addUTCTime 180 t0)}),
          blameFor AwayTimeout,
          skipsPlateOnClose AwayTimeout,
          countsTowardDriverMisses AwayTimeout,
          outcomeText AwayTimeout
        )
          @?= (Just AwayTimeout, BlameNone, False, False, "AWAY_TIMEOUT"),
      testCase "(user decision 2026-09-30): a stand timeout is the rider's no-show, not a driver miss; a driver cancel still is" $
        ( map blameFor [StandTimeout, MovingTimeout, DriverCancelled, AwayTimeout],
          map countsTowardDriverMisses [StandTimeout, MovingTimeout, DriverCancelled, AwayTimeout]
        )
          @?= ([BlameRider, BlameRider, BlameDriver, BlameNone], [False, False, True, False]),
      testCase "stand and away timers are cleared once the cab moves, the moving timer is not" $
        map clearOnMove [StandTimer t0, AwayTimer t0, MovingTimer t0, NoTimer] @?= [NoTimer, NoTimer, MovingTimer t0, NoTimer],
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
      testCase "the next tick is scheduled even when the tick body throws" $ do
        ran <- newIORef False
        r <- try (thenReschedule (throwIO (userError "db down")) (writeIORef ran True)) :: IO (Either IOException ())
        wasRescheduled <- readIORef ran
        (either (const True) (const False) r, wasRescheduled) @?= (True, True)
        ran' <- newIORef False
        thenReschedule (pure ()) (writeIORef ran' True) >> readIORef ran' >>= (@?= True),
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
              @?= (1200, 12 + 8 + allocKeyTtlMarginSec, stacked, stacked)
    ]
