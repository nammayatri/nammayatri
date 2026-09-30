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
import "rider-app" SharedLogic.SharedCab.Allocation (ClaimPush (..), FindingBooking (..), RankedCandidate (..), claimPush, claimTimerKind, claimTimerSec, claimable, clearsWhenMoving, closable, crossedMaxAttempts, eligibleCandidates, isFreshPosition, isMissedCabOutcome, isSkipped, silentCab, silentReleaseMult, skippedWhileFinding, skipsPlateOnClose, standTimerOnClaim, ticketsCountedAtConfirm, withoutSkipped)
import "rider-app" SharedLogic.SharedCab.Allocation.Types
import "rider-app" SharedLogic.SharedCab.AllocationSchedule (thenReschedule)
import "rider-app" SharedLogic.SharedCab.FindingTimeout (findingTimeoutRefund)
import "rider-app" SharedLogic.SharedCab.RefundDecision (Refund (..))
import "rider-app" SharedLogic.SharedCab.RefundPolicy (cancellableStatus)
import "rider-app" SharedLogic.SharedCab.SessionState (Session (..), SessionStatus (ACTIVE))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 9 25) 36000

standing :: AllocationState
standing = AllocationState {vehicleNumber = "ML05A1234", driverId = Just "d1", vehicleTripId = Just "t1", allocatedAt = t0, expiresAt = Just (addUTCTime 180 t0), attempts = 0, timerKind = StandTimer}

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
      rcMoving = True,
      rcAtStop = False
    }

-- a cab at `pos` doing `speed` m/s whose last fix was `agoSec` before t0, with S1 as its upcoming board stop
cabFix :: Text -> LatLong -> Maybe Double -> Int -> LT.VehicleTrackingOnRouteResp
cabFix plate pos speed agoSec =
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
        upcomingStops = Just [LT.UpcomingStop (LT.Stop "S1" stop "S1" 1 0 0) (addUTCTime 120 t0) LT.Upcoming Nothing]
      }

finding :: FindingBooking
finding = FindingBooking {bookingId = Id "b", riderId = Id "r", routeCode = "R1", boardStopCode = "S1", dropStopCode = "S9", seats = 1, findingSince = t0}

claimedAs :: LT.VehicleTrackingOnRouteResp -> [(Bool, Bool, Bool)]
claimedAs veh = [(standTimerOnClaim c, c.rcMoving, c.rcAtStop) | c <- eligibleCandidates t0 defaultAllocationConfig [veh] [(candidate "P1").rcSession] finding]

tests :: TestTree
tests =
  testGroup
    "SharedCab allocation timers and LTS timestamps"
    [ testCase "a running timer keeps the allocation" $
        timerExpiry (addUTCTime 179 t0) (Just standing) @?= Nothing,
      testCase "an expired stand timer releases as StandTimeout, a moving one as MovingTimeout" $
        (timerExpiry (addUTCTime 181 t0) (Just standing), timerExpiry (addUTCTime 181 t0) (Just standing {timerKind = MovingTimer}))
          @?= (Just StandTimeout, Just MovingTimeout),
      testCase "a cab claimed while moving has no timer to lapse" $
        timerExpiry (addUTCTime 3600 t0) (Just standing {expiresAt = Nothing}) @?= Nothing,
      testCase "timer mode follows the cab: moving above 1 m/s, no speed reads as stationary" $
        map isMovingSpeed [Just 5.0, Just 0.2, Nothing] @?= [True, False, False],
      testCase "a vanished alloc key releases without blame" $
        (timerExpiry t0 Nothing, blameFor TimerLost, countsTowardAttempts TimerLost) @?= (Just TimerLost, BlameNone, False),
      testCase "LTS chrono RFC 3339 with nanoseconds and Z" $
        parseLtsTimestamp "2026-09-25T10:00:00.123456789Z" @?= Just (addUTCTime 0.123456789 t0),
      testCase "offsets, a space separator and epoch seconds also parse" $
        map parseLtsTimestamp ["2026-09-25T15:30:00+05:30", "2026-09-25 10:00:00Z", "1790330400"]
          @?= [Just t0, Just t0, Just t0],
      testCase "allocation_closed outcomes use the 05 §7 spellings" $
        map outcomeText [StandTimeout, MovingTimeout, DriverCancelled, PassedStop BlameDriver, SeatLost, RouteChanged, SessionClosed, TimerLost, RiderSkipped SkipFull]
          @?= ["STAND_TIMEOUT", "MOVING_TIMEOUT", "DRIVER_CANCELLED", "PASSED_STOP", "SEAT_LOST", "ROUTE_CHANGED", "SESSION_CLOSED", "TIMER_LOST", "RIDER_SKIPPED"],
      testCase "R15: rider fresh and at the stop when the cab passed -> the driver's miss" $
        blameAt (Just (RiderFix nearStop (addUTCTime (-30) t0))) @?= BlameDriver,
      testCase "R15: rider fresh but away from the stop -> the rider's no-show" $
        blameAt (Just (RiderFix farAway (addUTCTime (-30) t0))) @?= BlameRider,
      testCase "R15: rider position unknown -> nobody" $
        blameAt Nothing @?= BlameNone,
      testCase "R15: rider position stale, even at the stop -> nobody" $
        blameAt (Just (RiderFix nearStop (addUTCTime (-61) t0))) @?= BlameNone,
      testCase "R15: PASSED_STOP carries its blame and always counts as an attempt" $
        map (\b -> (blameFor (PassedStop b), countsTowardAttempts (PassedStop b))) [BlameDriver, BlameRider, BlameNone]
          @?= [(BlameDriver, True), (BlameRider, True), (BlameNone, True)],
      testCase "R19: skipping a full cab is nobody's fault and not an attempt" $
        (blameFor (RiderSkipped SkipFull), countsTowardAttempts (RiderSkipped SkipFull)) @?= (BlameNone, False),
      testCase "R19: skipping for any other reason is the rider's and counts" $
        (blameFor (RiderSkipped SkipOther), countsTowardAttempts (RiderSkipped SkipOther)) @?= (BlameRider, True),
      testCase "R19: the API reasons map onto the skip reasons" $
        map skipReason [API.FULL, API.OTHER] @?= [SkipFull, SkipOther],
      testCase "R19: a skipped cab is never offered to that booking again" $
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
      testCase "R19: the claim's under-lock check sees only the plate the rider skipped" $
        map (`isSkipped` ["ML05B2222"]) ["ML05A1111", "ML05B2222"] @?= [False, True],
      testCase "R16: the fallback push fires on the close that crosses maxAttempts, not before or after" $
        map (uncurry (crossedMaxAttempts 2)) [(0, 1), (1, 2), (2, 3), (0, 0)] @?= [False, True, False, False],
      testCase "R17: only the two timer outcomes push the missed-cab copy" $
        map isMissedCabOutcome [StandTimeout, MovingTimeout, DriverCancelled, PassedStop BlameRider, SeatLost, RouteChanged, SessionClosed, TimerLost, RiderSkipped SkipOther]
          @?= [True, True, False, False, False, False, False, False, False],
      testCase "R44: skips bind while FINDING, and stop binding in FALLBACK (attempts or clock)" $
        [ skippedWhileFinding defaultAllocationConfig t0 0 t0 ["A"],
          skippedWhileFinding defaultAllocationConfig t0 2 t0 ["A"],
          skippedWhileFinding defaultAllocationConfig (addUTCTime 599 t0) 1 t0 ["A"],
          skippedWhileFinding defaultAllocationConfig (addUTCTime 600 t0) 1 t0 ["A"]
        ]
          @?= [["A"], [], ["A"], []],
      testCase "R32: only a stationary cab within atStopRadiusM of the board stop gets the stand timer" $
        map claimedAs [cabFix "P1" nearStop (Just 0) 5, cabFix "P1" farAway (Just 0) 5, cabFix "P1" nearStop (Just 8) 5, cabFix "P1" farAway Nothing 5]
          @?= [[(True, False, True)], [(False, False, False)], [(False, True, True)], [(False, False, False)]],
      testCase "M5: a stationary cab gets a bounded wait, at the stop the stand timer and away the allocation window; a moving cab none" $
        map (\veh -> [claimTimerSec defaultAllocationConfig c | c <- eligibleCandidates t0 defaultAllocationConfig [veh] [(candidate "P1").rcSession] finding]) [cabFix "P1" nearStop (Just 0) 5, cabFix "P1" farAway (Just 0) 5, cabFix "P1" nearStop (Just 8) 5]
          @?= [[Just defaultAllocationConfig.standTimerSec], [Just defaultAllocationConfig.allocationWindowSec], [Nothing]],
      testCase "R31: a missed cab is skipped for the booking, no other close is" $
        map skipsPlateOnClose [StandTimeout, MovingTimeout, DriverCancelled, PassedStop BlameRider, SeatLost, RouteChanged, SessionClosed, TimerLost, CabSilent, RiderSkipped SkipOther]
          @?= [True, True, False, False, False, False, False, False, False, False],
      testCase "R38: a silent cab is released only past silentReleaseMult x ltsMaxAgeSec, and only while the route is otherwise reporting" $
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
      testCase "R38: a silent-cab release is nobody's fault and not an attempt" $
        (outcomeText CabSilent, blameFor CabSilent, countsTowardAttempts CabSilent, countsTowardDriverMisses CabSilent) @?= ("CAB_SILENT", BlameNone, False, False),
      testCase "R64: only a cab that sat at the stop (or is moving) gets the stand-timer kind; a cab away from the stop gets its own" $
        map (\veh -> [claimTimerKind c | c <- eligibleCandidates t0 defaultAllocationConfig [veh] [(candidate "P1").rcSession] finding]) [cabFix "P1" nearStop (Just 0) 5, cabFix "P1" farAway (Just 0) 5, cabFix "P1" nearStop (Just 8) 5]
          @?= [[StandTimer], [AwayTimer], [StandTimer]],
      testCase "R64: the away timer lapses as AwayTimeout, which blames nobody, skips no cab and is not a driver miss" $
        ( timerExpiry (addUTCTime 181 t0) (Just standing {timerKind = AwayTimer}),
          blameFor AwayTimeout,
          skipsPlateOnClose AwayTimeout,
          isMissedCabOutcome AwayTimeout,
          countsTowardDriverMisses AwayTimeout,
          outcomeText AwayTimeout
        )
          @?= (Just AwayTimeout, BlameNone, False, False, False, "AWAY_TIMEOUT"),
      testCase "R75 (user decision 2026-09-30): a stand timeout is the rider's no-show, not a driver miss; a driver cancel still is" $
        ( map blameFor [StandTimeout, MovingTimeout, DriverCancelled, AwayTimeout],
          map countsTowardDriverMisses [StandTimeout, MovingTimeout, DriverCancelled, AwayTimeout]
        )
          @?= ([BlameRider, BlameRider, BlameDriver, BlameNone], [False, False, True, False]),
      testCase "R64: stand and away timers are cleared once the cab moves, the moving timer is not" $
        map clearsWhenMoving [StandTimer, AwayTimer, MovingTimer] @?= [True, True, False],
      testCase "R67: a claim on a stationary cab at the stop pushes ARRIVING, any other claim pushes ASSIGNED" $
        map (\veh -> [claimPush c | c <- eligibleCandidates t0 defaultAllocationConfig [veh] [(candidate "P1").rcSession] finding]) [cabFix "P1" nearStop (Just 0) 5, cabFix "P1" farAway (Just 0) 5, cabFix "P1" nearStop (Just 8) 5]
          @?= [[PushArriving], [PushAssigned], [PushAssigned]],
      testCase "R63: a FINDING booking is cancelled once its current stint is older than findingTimeoutSec, so a late release still gets its reallocation" $
        -- (age of the stint, age of the booking)
        map (\(stint, age) -> findingTimeoutAction (addUTCTime age t0) 1200 (addUTCTime (age - stint) t0) t0) [(0, 1300), (1200, 1300), (1201, 1300), (60, 3000), (10, 3600), (10, 3601), (10, 7200)]
          @?= [KeepFinding, KeepFinding, CancelNoCab, KeepFinding, KeepFinding, CancelNoCab, CancelNoCab],
      testCase "R63: a booking already cancelled is not cancelled (or refunded) again" $
        map cancellableStatus [BS.CONFIRMED, BS.CANCELLED, BS.COUNTER_CANCELLED, BS.CANCEL_INITIATED, BS.FAILED, BS.RESCHEDULED] @?= [True, False, False, False, False, False],
      testCase "R68: a booking with a parent (rescheduled) never counted its tickets at confirm, so none are reversed" $
        (ticketsCountedAtConfirm (Nothing :: Maybe ()), ticketsCountedAtConfirm (Just ())) @?= (True, False),
      testCase "R61: the next tick is scheduled even when the tick body throws" $ do
        ran <- newIORef False
        r <- try (thenReschedule (throwIO (userError "db down")) (writeIORef ran True)) :: IO (Either IOException ())
        (,) (either (const True) (const False) r) <$> readIORef ran >>= (@?= (True, True))
        ran' <- newIORef False
        thenReschedule (pure ()) (writeIORef ran' True) >> readIORef ran' >>= (@?= True),
      testCase "R63/R54: a finding-timeout cancel refunds in full, unless a no-show is already booked against the booking" $
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
      testCase "R83: a findingTimeoutSec that already covers the stacked timers stays the key's TTL, no warning owed" $
        -- defaults: 1200 vs 180 + 90 + 10
        (allocKeyTtl defaultAllocationConfig, allocKeyTtlShort defaultAllocationConfig) @?= (1200, False),
      testCase "R83: a findingTimeoutSec below stand + moving + margin is raised to their sum and flagged (e2e's 25)" $
        let cfg = defaultAllocationConfig {findingTimeoutSec = 25, standTimerSec = 12, movingTimerSec = 8}
         in (allocKeyTtl cfg, allocKeyTtlShort cfg) @?= (12 + 8 + allocKeyTtlMarginSec, True),
      testCase "R83: the exact boundary findingTimeoutSec == stand + moving + margin is not a breach; one below is" $
        let bound = defaultAllocationConfig.standTimerSec + defaultAllocationConfig.movingTimerSec + allocKeyTtlMarginSec
            atBound = defaultAllocationConfig {findingTimeoutSec = bound}
            belowBound = defaultAllocationConfig {findingTimeoutSec = bound - 1}
         in ( (allocKeyTtl atBound, allocKeyTtlShort atBound),
              (allocKeyTtl belowBound, allocKeyTtlShort belowBound)
            )
          @?= ((bound, False), (bound, True))
    ]
