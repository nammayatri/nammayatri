{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabAllocationTests (tests) where

import qualified "rider-app" API.Types.UI.SharedCab as API
import "beckn-spec" BecknV2.FRFS.Enums (ServiceTierType (AC))
import Data.Text (Text)
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import "rider-app" Domain.Action.UI.SharedCab (skipReason)
import qualified "rider-app" Domain.Types.FRFSTicketBookingStatus as BS
import qualified "rider-app" Domain.Types.FRFSTicketStatus as TS
import "mobility-core" Kernel.External.Maps.Types (LatLong (..))
import "mobility-core" Kernel.Types.Id (Id (..))
import qualified "rider-app" SharedLogic.External.LocationTrackingService.Types as LT
import "rider-app" SharedLogic.SharedCab.Allocation (RankedCandidate (..), closable, isSkipped, withoutSkipped)
import "rider-app" SharedLogic.SharedCab.Allocation.Types
import "rider-app" SharedLogic.SharedCab.SessionState (Session (..), SessionStatus (ACTIVE))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 9 25) 36000

standing :: AllocationState
standing = AllocationState {vehicleNumber = "ML05A1234", allocatedAt = t0, expiresAt = Just (addUTCTime 180 t0), attempts = 0, timerKind = StandTimer}

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
      rcMoving = True
    }

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
      testCase "R19: the claim's under-lock check sees only the plate the rider skipped" $
        map (`isSkipped` ["ML05B2222"]) ["ML05A1111", "ML05B2222"] @?= [False, True],
      testCase "garbage is not a timestamp" $
        parseLtsTimestamp "yesterday" @?= Nothing
    ]
